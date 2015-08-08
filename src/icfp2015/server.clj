(ns icfp2015.server
  "A websocket server for the visualisation."
  (:require [clojure.core.async :refer [<! >! chan put! close! alts! go-loop]]
            [clojure.tools.logging :refer [info debug]]
            [org.httpkit.server :refer [run-server]]
            [chord.http-kit :refer [with-channel]]))

(def ch (chan))

(defn handler [req]
  (debug "Incoming ws request")
  (with-channel req ws-ch
    (go-loop []
      (debug "Wait for msg from ch...")
      (let [[msg port] (alts! [ws-ch ch])]
        (if msg
          (do (condp = port
                ch
                (do (debug "Send" msg)
                    (>! ws-ch msg))
                ws-ch
                (debug "Skip client msg" msg))
              (recur))
          (do (debug "Close ws")
              (close! ws-ch)))))))

(defn start [port]
  (info "Start ws server on port" port)
  (run-server handler {:port port}))
