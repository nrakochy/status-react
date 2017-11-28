(ns status-im.chat.models.message
  (:require [re-frame.core :as re-frame]
            [status-im.utils.clocks :as clocks]
            [status-im.constants :as constants]
            [status-im.chat.utils :as chat-utils]
            [status-im.chat.models :as chat-model]
            [status-im.chat.models.commands :as commands-model]
            [status-im.chat.models.unviewed-messages :as unviewed-messages-model]
            [status-im.chat.events.requests :as requests-events]
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
                                          :clock-value (clocks/receive
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