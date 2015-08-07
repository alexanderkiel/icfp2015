(ns icfp2015.server
  "A websocket server for the visualisation."
  (:require [clojure.core.async :refer [<! >! chan put! close! go go-loop]]
            [clojure.tools.logging :refer [debug]]
            [org.httpkit.server :refer [run-server]]
            [chord.http-kit :refer [with-channel]]))

(def ch (chan))

(defn handler [req]
  (debug "Incoming ws request")
  (with-channel req ws-ch
    (go-loop []
      (debug "Wait for msg from ch...")
      (if-let [msg (<! ch)]
        (do
          (debug "Send" msg)
          (>! ws-ch msg)
          (recur))
        (do
          (debug "Close ws")
          (close! ws-ch))))))

(def stop (run-server handler {:port 5011}))

(comment (stop))

(comment
  (put! ch "hi2")
  (close! ch)
  )
