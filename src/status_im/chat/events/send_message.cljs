(ns status-im.chat.events.send-message
  (:require [re-frame.core :as re-frame]
            [status-im.constants :as constants]
            [status-im.data-store.chats :as chats-store]
            [status-im.chat.utils :as chat-utils]))

;;;; Coeffects



;;;; Helper methods


;;;; Effects

(re-frame/reg-fx
 :update-message-overhead
 (fn [[chat-id network-status]]
   (if (= network-status :offline)
     (chats-store/inc-message-overhead chat-id)
     (chats-store/reset-message-overhead chat-id))))

(re-frame/reg-fx
 :add-command-message-to-db
 #_(fn [[add-to-chat-id {:keys [chat-id command handler]}]]
     (chat-utils/add-message-to-db db add-to-chat-id chat-id command)
     (when handler (handler))))

;;;; Handlers
