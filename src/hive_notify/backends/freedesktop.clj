(ns hive-notify.backends.freedesktop
  "freedesktop Notifications over the session D-Bus, in-process (dbus-java).
   `ask!` sends Notify with one action per choice and waits on the SAME
   connection for ActionInvoked / NotificationClosed on that id."
  (:require [hive-dsl.result :refer [rescue]])
  (:import [org.freedesktop.dbus.connections.impl DBusConnection DBusConnectionBuilder]
           [org.freedesktop.dbus.interfaces DBusSigHandler]
           [org.freedesktop.dbus.matchrules DBusMatchRuleBuilder]
           [org.freedesktop.dbus.messages DBusSignal Message MethodCall]
           [org.freedesktop.dbus.types UInt32 Variant]
           [java.util HashMap]
           [java.util.concurrent CompletableFuture TimeUnit TimeoutException]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def ^:private service "org.freedesktop.Notifications")
(def ^:private object-path "/org/freedesktop/Notifications")
(def ^:private call-timeout-ms 5000)

(defn- call!
  "Call `member` on the notification service; returns the reply parameters.
   Throws on an error reply or no reply."
  [^DBusConnection conn member sig & args]
  (let [^MethodCall mc (.createMethodCall (.getMessageFactory conn)
                                          service object-path service member
                                          (byte 0) sig (object-array args))]
    (.sendMessage conn mc)
    (let [^Message reply (.getReply mc (long call-timeout-ms))]
      (cond
        (nil? reply)
        (throw (ex-info "no reply" {:member member}))

        (instance? org.freedesktop.dbus.messages.Error reply)
        (throw (ex-info "error reply" {:member member :name (.getName reply)}))

        :else (vec (.getParameters reply))))))

(defn- on-signal!
  "Register `f` for `member` signals of the notification service on `conn`;
   `f` receives the signal's parameters as a vector."
  [^DBusConnection conn member f]
  (.addGenericSigHandler conn
                         (-> (DBusMatchRuleBuilder/create)
                             (.withInterface service)
                             (.withMember member)
                             (.build))
                         (reify DBusSigHandler
                           (handle [_ s] (f (vec (.getParameters ^DBusSignal s)))))))

(defn- notify-params [{:keys [summary body choices]} app timeout-ms]
  (let [hints (doto (HashMap.) (.put "urgency" (Variant. (byte 2))))]
    [(str app) (UInt32. 0) "dialog-question" (str summary) (str body)
     (into-array String (mapcat (fn [[id label]] [(name id) (str label)]) choices))
     hints (int timeout-ms)]))

(defn available?
  "True when a session bus is reachable and the notification service answers
   GetCapabilities with 'actions'. Never throws."
  []
  (rescue false
    (with-open [conn (.build (DBusConnectionBuilder/forSessionBus))]
      (let [[caps] (call! conn "GetCapabilities" "")]
        (boolean (some #{"actions"} caps))))))

(defn ask!
  "Show `question` (body already escaped) with one button per choice and block
   until a button is clicked, the notification is dismissed, or `timeout-ms`
   passes. On the deadline or an interrupt (another surface answered) the
   notification is closed. Returns {:status :answered :raw id}
   | {:status :dismissed :reason n} | {:status :timed-out}
   | {:status :cancelled} | {:status :unavailable}. Never throws."
  [question app timeout-ms]
  (rescue {:status :unavailable}
    (with-open [conn (.build (DBusConnectionBuilder/forSessionBus))]
      (let [result (CompletableFuture.)
            nid    (promise)
            mine?  (fn [id] (= (long (.longValue ^Number id))
                               (deref nid call-timeout-ms -1)))]
        (with-open [_ (on-signal! conn "ActionInvoked"
                                  (fn [[id action]]
                                    (when (mine? id)
                                      (.complete result {:status :answered :raw (str action)}))))
                    _ (on-signal! conn "NotificationClosed"
                                  (fn [[id reason]]
                                    (when (mine? id)
                                      (.complete result {:status :dismissed
                                                         :reason (.longValue ^Number reason)}))))]
          (let [[id]   (apply call! conn "Notify" "susssasa{sv}i"
                              (notify-params question app timeout-ms))
                close! #(rescue nil (call! conn "CloseNotification" "u" id))]
            (deliver nid (.longValue ^Number id))
            (try
              (.get result (long timeout-ms) TimeUnit/MILLISECONDS)
              (catch TimeoutException _
                (close!)
                {:status :timed-out})
              (catch InterruptedException _
                (close!)
                (.interrupt (Thread/currentThread))
                {:status :cancelled}))))))))
