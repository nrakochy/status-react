(ns status-im.chat.models.message
  (:require [re-frame.core :as re-frame]
            [status-im.utils.clocks :as clocks]
            [status-im.constants :as constants]
            [status-im.chat.events.console :as console-events]
            [status-im.chat.events.requests :as requests-events]
            [status-im.chat.models :as chat-model]
            [status-im.chat.models.commands :as commands-model]
            [status-im.chat.models.unviewed-messages :as unviewed-messages-model]
            [status-im.chat.utils :as chat-utils]
            [status-im.data-store.messages :as messages-store]
            [status-im.utils.datetime :as datetime-utils]
            [status-im.utils.clocks :as clocks-utils]
            [taoensso.timbre :as log]))

(defn- get-current-account
  [{:accounts/keys [accounts current-account-id]}]
  (get accounts current-account-id))

(def receive-interceptors
  [(re-frame/inject-cofx :message-exists?)
   (re-frame/inject-cofx :get-last-stored-message)
   (re-frame/inject-cofx :pop-up-chat?)
   (re-frame/inject-cofx :get-last-clock-value)
   (re-frame/inject-cofx :random-id)
   (re-frame/inject-cofx :get-stored-chat)
   re-frame/trim-v])

(defn- lookup-response-ref
  [access-scope->commands-responses account chat contacts response-name]
  (let [available-commands-responses (commands-model/commands-responses :response
                                                                        access-scope->commands-responses
                                                                        account
                                                                        chat
                                                                        contacts)]
    (:ref (get available-commands-responses response-name))))

(defn receive
  [{:keys [db message-exists? get-stored-chat get-last-stored-message
           pop-up-chat? get-last-clock-value now random-id]}
   messages]
  (reduce
   (fn [{:keys [db] :as current-cofx} message]
     (let [{:keys [access-scope->commands-responses] :contacts/keys [contacts]} db
           {:keys [from group-id chat-id content-type content message-id timestamp clock-value]
            :or   {clock-value 0}} message
           chat-identifier (or group-id chat-id from)
           current-account (get-current-account db)]
       ;; proceed with adding message if message is not already stored in realm,
       ;; it's not from current user (outgoing message) and it's for relevant chat
       ;; (either current active chat or new chat not existing yet)
       (if (and (not (message-exists? message-id))
                (not= from (:public-key current-account))
                (pop-up-chat? chat-identifier))

         (let [group-chat?      (not (nil? group-id))
               chat-exists?     (get-in db [:chats chat-identifier])
               cofx-for-chat    (assoc current-cofx :get-stored-chat get-stored-chat
                                       :now now)
               fx               (if chat-exists?
                                  (chat-model/upsert-chat cofx-for-chat {:chat-id    chat-identifier
                                                                         :group-chat group-chat?})
                                  (chat-model/add-chat cofx-for-chat chat-identifier))
               command-request? (= content-type constants/content-type-command-request)
               command          (:command content)
               enriched-message (cond-> (assoc (chat-utils/check-author-direction
                                                (get-last-stored-message chat-identifier)
                                                message)
                                               :chat-id chat-identifier
                                               :timestamp (or timestamp now)
                                               :clock-value (clocks-utils/receive
                                                             clock-value
                                                             (get-last-clock-value chat-identifier)))
                                  (and command command-request?)
                                  (assoc-in [:content :content-command-ref]
                                            (lookup-response-ref access-scope->commands-responses
                                                                 current-account
                                                                 (get-in fx [:db :chats chat-identifier])
                                                                 contacts
                                                                 command)))
               update-db-fx       #(-> %
                                       (chat-utils/add-message-to-db chat-identifier chat-identifier enriched-message
                                                                     (:new? enriched-message))
                                       (unviewed-messages-model/add-unviewed-message chat-identifier message-id)
                                       (assoc-in [:chats chat-identifier :last-message] enriched-message))]
           (cond-> (-> fx
                       (update :db update-db-fx)
                       (update :save-entities #(conj (or % []) [:message (dissoc enriched-message :new?)])))

             command
             (update :dispatch-n concat [[:request-command-message-data enriched-message :short-preview]
                                         [:request-command-preview enriched-message]])

             command-request?
             (requests-events/add-request chat-identifier enriched-message)))
         current-cofx)))
   {:db db}
   messages))



;;;; Send message

(def send-interceptors
  [(re-frame/inject-cofx :random-id)
   (re-frame/inject-cofx :random-id-seq)
   (re-frame/inject-cofx :get-stored-chat)
   (re-frame/inject-cofx :save-entities)
   re-frame/trim-v])

(defn- prepare-command
  [identity chat-id clock-value
   {request-params  :params
    request-command :command
    :keys           [prefill prefillBotDb]
    :as             request}
   {:keys [id params command to-message handler-data content-type]}]
  (let [content  (if request
                   {:command        request-command
                    :params         (assoc request-params :bot-db (:bot-db params))
                    :prefill        prefill
                    :prefill-bot-db prefillBotDb}
                   {:command (:name command)
                    :scope   (:scope command)
                    :params  params})
        content' (assoc content
                        :handler-data handler-data
                        :type (name (:type command))
                        :content-command (:name command)
                        :content-command-scope-bitmask (:scope-bitmask command)
                        :content-command-ref (:ref command)
                        :bot (or (:bot command)
                                 (:owner-id command)))]
    {:message-id   id
     :from         identity
     :to           chat-id
     :timestamp    (datetime-utils/now-ms)
     :content      content'
     :content-type (or content-type
                       (if request
                         constants/content-type-command-request
                         constants/content-type-command))
     :outgoing     true
     :to-message   to-message
     :type         (:type command)
     :has-handler  (:has-handler command)
     :clock-value  (clocks-utils/send clock-value)
     :show?        true}))

;; TODO(alwx): it's actually a prepare-command! handler
(defn send-command
  [{{:keys [current-public-key network-status] :as db} :db
    :keys [get-stored-chat random-id-seq]} add-to-chat-id params]
  (let [{{:keys [handler-data
                 command]
          :as   content} :command
         chat-id         :chat-id} params
        clock-value (messages-store/get-last-clock-value chat-id)
        request (:request handler-data)
        hidden-params (->> (:params command)
                           (filter :hidden)
                           (map :name))
        command' (->> (prepare-command current-public-key chat-id clock-value request content)
                      (chat-utils/check-author-direction db chat-id))
        preview (get-in db [:message-data :preview (:message-id command')])
        params' (assoc params :command command')]
    (cond-> {:db (-> db
                     (chat-utils/add-message-to-db add-to-chat-id chat-id params'))
             :update-message-overhead [chat-id network-status]
             :save-entities [[:message (cond-> (-> command'
                                                   (assoc :chat-id chat-id)
                                                   (update-in [:content :params]
                                                              #(apply dissoc % hidden-params))
                                                   (dissoc :to-message :has-handler :raw-input))
                                         preview
                                         (assoc :preview (pr-str preview)))]] }

      true
      (as-> cofx'
          (chat-model/upsert-chat (assoc cofx' :get-stored-chat get-stored-chat)
                                  {:chat-id chat-id}))

      ;; TODO(alwx): not needed more likely
      true
      (chat-model/set-chat-ui-props {:sending-in-progress? false})

      (:to-message command')
      (assoc :chat-requests/mark-as-answered {:chat-id chat-id
                                              :message-id (:to-message command')})

      true
      (assoc :dispatch-n [[:send-command-protocol! params']])

      (chat-utils/console? chat-id)
      (as-> cofx'
          (let [messages (console-events/console-respond-command-messages params' random-id-seq)]
            (update cofx' :save-entities conj messages))))))

;; TODO(alwx) remove this once send-message events are refactored; also make use of it
(defn invoke-console-command-handler
  [cofx [{:keys [chat-id command] :as command-params}]]
  (let [fx-fn (get console-events/console-commands->fx (-> command :command :name))]
    (-> (fx-fn cofx command)
        (send-command chat-id command-params))))

;; TODO(alwx): unified send for both commands and messages
(defn process-command
  [{:keys [db random-id] :as cofx} {:keys [command message chat-id] :as params}]
  (let [{:keys [command] :as content} command]
    (-> {:db db}

        (as-> cofx'
            (cond
              (and (= constants/console-chat-id chat-id)
                   (console-events/commands-names (:name command)))
              (invoke-console-command-handler (merge cofx cofx') params)

              (:has-handler command)
              ;; TODO(alwx): should be restructured
              (assoc cofx' :dispatch-n [[:invoke-command-handlers! params]])

              :else
              (merge cofx' (send-command cofx' chat-id params))))

        (chat-model/set-chat-ui-props {:sending-in-progress? false}))))
