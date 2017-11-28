(ns status-im.chat.events.receive-message
  (:require [re-frame.core :as re-frame]
            [taoensso.timbre :as log]
            [status-im.utils.handlers :as handlers]
            [status-im.chat.models.message :as message-model]
            [status-im.data-store.chats :as chat-store]
            [status-im.data-store.messages :as messages-store]))

;;;; Coeffects

(re-frame/reg-cofx
 :pop-up-chat?
 (fn [cofx]
   (assoc cofx :pop-up-chat? (fn [chat-id]
                               (or (not (chat-store/exists? chat-id))
                                   (chat-store/is-active? chat-id))))))

(re-frame/reg-cofx
 :message-exists?
 (fn [cofx]
   (assoc cofx :message-exists? messages-store/exists?)))

(re-frame/reg-cofx
 :get-last-clock-value
 (fn [cofx]
   (assoc cofx :get-last-clock-value messages-store/get-last-clock-value)))


;;;; FX

(handlers/register-handler-fx
 :chat-received-message/add-protocol-message
 message-model/receive-interceptors
 (fn [cofx [{:keys [from to payload]}]]
   (message-model/receive cofx [(merge payload
                                       {:from    from
                                        :to      to
                                        :chat-id from})])))

(handlers/register-handler-fx
 :chat-received-message/add
 message-model/receive-interceptors
 (fn [cofx [messages]]
   (message-model/receive cofx messages)))

(handlers/register-handler-fx
 :chat-received-message/add-when-commands-loaded
 message-model/receive-interceptors
 (fn [{:keys [db] :as cofx} [chat-id message]]
   (if (and (:status-node-started? db)
            (get-in db [:contacts/contacts chat-id :jail-loaded?]))
     (message-model/receive cofx [message])
     {:dispatch-later [{:ms 400 :dispatch [:chat-received-message/add-when-commands-loaded chat-id message]}]})))
