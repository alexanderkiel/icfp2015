(ns user
  (:use plumbing.core)
  (:require [clojure.core.async :refer [put! close!]]
            [clojure.pprint :refer [pprint]]
            [criterium.core :refer [quick-bench]]
            [loom.graph :as g]
            [loom.label :as l]
            [icfp2015.server :as server]
            [icfp2015.io :refer [read-problem]]
            [icfp2015.core :refer :all]
            [icfp2015.tetris :refer :all]
            [org.httpkit.client :as http]
            [clojure.data.json :as json]
            [clj-time.format :as f]
            [loom.alg :as ga]))

(defonce stop (server/start 5011))

(def b (atom {}))
(add-watch b :send (fn [_ _ _ b] (put! server/ch b)))

(defn find-bottom-nodes
  "Finds six bottom nodes of board starting with first unit."
  [board]
  (let [g (graph board (first (:units board)))
        nodes (g/nodes g)]
    (->> (into []
               (comp
                 (remove-nodes-xf g :sw :se)
                 (take 6))
               nodes)
         (assoc board :units))))

(comment
  (def p0 (read-problem "problems/problem_0.json"))
  (reset! b (problem->board p0))

  (swap! b #(spawn % (first (:units p0))))
  (swap! b #(spawn % (second (:units p0))))
  (swap! b #(spawn % (nth (:units p0) 2)))
  (swap! b #(spawn % (nth (:units p0) 3)))
  (swap! b #(spawn % (nth (:units p0) 17)))
  (swap! b #(spawn % {:members [[0 1] [1 2]], :pivot [0 1]}))

  (swap! b #(update-in % [:units 0] move-east))
  (swap! b #(update-in % [:units 0] move-west))
  (swap! b #(update-in % [:units 0] move-south-east))
  (swap! b #(update-in % [:units 0] move-south-west))
  (swap! b #(update-in % [:units 0] turn-cw))
  (swap! b #(update-in % [:units 0] turn-ccw))

  ;; Show all childs of first unit
  (swap! b (fn [b] (update b :units #(into % (moves b (first (:units b)))))))

  ;; show neigbors of current unit
  (first (:units @b))
  (unit-neighbors @b (first (:units @b)))
  (swap! b #(assoc-in % [:units 1] {:pivot [0 0], :members (seq (unit-neighbors @b (first (:units @b))))}))

  )

(defn- timed [f]
  (fn [& args] (time (apply f args))))

;; ---- Game ------------------------------------------------------------------

(def game (atom {}))

(defn init-game! [phrases problem seed-idx]
  (reset! game (add-phrases (prepare-game problem seed-idx) phrases))
  :ok)

(defn init-but-keep-commands! [problem seed-idx]
  (let [cmds (:commands @game)]
    (reset! game (assoc (prepare-game problem seed-idx) :commands cmds)))
  :ok)

(defn step-game! []
  (swap! game (partial step (timed naive-placement) stupid-path))
  (reset! b (:board @game))
  :ok)

(defn step-game-test! []
  (swap! game (partial step (timed naive-placement) test-path))
  (reset! b (:board @game))
  :ok)


(defn step-game-best! [depth]
  (swap! game (partial step (timed naive-placement) (partial best-path depth)))
  (reset! b (:board @game))
  :ok)

(defn play-game! []
  (swap! game (partial play naive-placement stupid-path))
  :ok)

(defn play-game-best! [depth]
  (swap! game (partial play naive-placement (partial best-path depth)))
  :ok)

(defn step-game2! []
  (swap! game (partial step naive-placement-sample stupid-path))
  (reset! b (:board @game))
  :ok)

(defn show-game! []
  (reset! b (:board @game))
  :ok)

(defn micro-step! []
  (swap! game micro-step)
  (reset! b (:board @game))
  :ok)

(defn show-next-start-node! []
  (reset! b (assoc (:board @game) :units [((:start-nodes @game) (first (:unit-stack @game)))]))
  :ok)

(defn show-pruned-graph! []
  (let [game @game
        unit (first (:unit-stack game))
        game (prune-game game unit)
        graph ((:graphs game) unit)
        board (assoc (:board game) :marked (mapcat :members (g/nodes graph)))]
    (reset! b board))
  :ok)

(comment
  (def p0 (read-problem "problems/problem_3.json"))
  (init-game! ["Ei!","ia! ia!", "r'lyeh", "yuggoth"] p0 0)
  (show-game!)
  (step-game-best! 5)

  (:sourceSeeds p0)



  (show-next-start-node!)
  (show-pruned-graph!)
  (step-game!)
  (step-game-best! 5)
  (step-game-test!)

  (show-next-start-node!)
  (step-game2!)
  (show-game!)

  (play-game!)
  (play-game-best! 5)

  (init-but-keep-commands! p0 0)
  (micro-step!)

  (best-path 5 @game {:members #{[0 0]}, :pivot [1 0]} {:members #{[1 24]}, :pivot [1 23]})

  (best-path 10 @game {:members #{[0 0]}, :pivot [0 0]} {:members #{[9 9]}, :pivot [9 9]})
  (stupid-path @game {:members #{[0 0]}, :pivot [0 0]} {:members #{[9 9]}, :pivot [9 9]})


  (let [u (second (first (:start-nodes @game)))]
    (walk-graph (second (first (:graphs @game))) #{}  u [:w :w :sw]))
  (select-keys (swap! game spawn-next) [:board :unit-stack])
  (select-keys @game [:commands :board :unit-stack :finished])
  (select-keys @game [:commands :finished])
  (letter-to-cmd (first (@game :commands)))

  (apply str (:commands @game))

  ((cmd-move :lock) (first (:units @b)))

  (:board @game)
  (:start-nodes @game)
  (pprint *1)
  (:commands @game)
  (nodes-to-prune @game (first (:unit-stack @game))))

(defn parse-date [date]
  (f/parse (f/formatters :date-time) date))

(defn tail-submissions [tag-regex n]
  (->> (-> @(http/get "https://davar.icfpcontest.org/teams/305/solutions"
                      {:as :text
                       :basic-auth ["" (System/getenv "API_TOKEN")]})
           (:body)
           (json/read-str :key-fn keyword))
       (map #(update % :createdAt parse-date))
       (sort-by :createdAt)
       (reverse)
       (filter #(re-find tag-regex (:tag %)))
       (take n)))

(defn submit-game [tag game]
  (let [solution
        [{"problemId" (:problem-id game),
          "seed" (:seed game),
          "tag" tag,
          "solution" (apply str (:commands game))}]
        resp @(http/post "https://davar.icfpcontest.org/teams/305/solutions"
                         {:body (json/write-str solution)
                          :headers {"Content-Type" "application/json"}
                          :basic-auth ["" (System/getenv "API_TOKEN")]})]
    (:status resp)))

(comment
  (submit-game "Alex" @game)
  (pprint (tail-submissions #"Alex" 2))
  (submit-game "Georg" @game)/
  (pprint (tail-submissions #"Georg" 6))
  )

(defn solve-problem [phrases path-gen problem seed-idx]
  (let [game (add-phrases (prepare-game problem seed-idx) phrases)]
    (play naive-placement path-gen game)))

(defn solve [phrases path-gen problem]
  (doseq [game (pmap (partial solve-problem phrases path-gen problem)
                     (range (count (:sourceSeeds problem))))]
    (submit-game "Alex Auto" game)))

(comment
  (def p0 (read-problem "problems/problem_1.json"))
  (init-game! ["Ei!","ia! ia!", "r'lyeh", "yuggoth"] p0 0)
  (:sourceSeeds p0)
  (play-game!)
  (play-game-best! 5)
  (submit-game "Alex" @game)
  (pprint (tail-submissions #"Alex" 2))
  )

(comment
  (solve ["Ei!","ia! ia!", "r'lyeh", "yuggoth"]
         ;(partial best-path 5)
         stupid-path
         (read-problem "problems/problem_1.json"))


  )


(comment
; ------- GEORGS SUBMIT AREA
  (def p0 (read-problem "problems/problem_1.json"))
  (init-game! to-try p0 0)
  (set! *print-length* 1000)
  (pprint (:phrases @game))

  (set! *print-length* 20)
  (let [game (solve-problem ["Ei!","ia! ia!", "r'lyeh", "yuggoth","eee" "!!!"]
                            (partial best-path 6)
                            (read-problem "problems/problem_1.json") 0)]
    (println (apply str (:commands game)))
    (println "Powerscore: " (:powerscore game) "\tMovescore: " (:movescore game))
    ;(submit-game "Georg 1 Test" game)
    )
    (pprint (tail-submissions #"Georg 1 Test" 1))

  ; ---- Phrase test

  (def to-try ["Ron"
               "Tony"
               "Leslie"
               "Barb"
               "Looks like our handler isn't here yet."
               "Robin"
               "ICFP"
               "Excellent!"
               "The fate of the world is in your hands."
               "Conway"
               "Cocke"
               "Hopcroft"
               "Backus"
               "Bigboote"
               "Planet 10"
               "monkeyboy!"
               "Yuggoth"])
    (let [game (solve-problem ["ICFP"]
                            (partial best-path 5)
                            (read-problem "problems/problem_3.json") 0)]
    (println (apply str (:commands game)))
    (println "Powerscore: " (:powerscore game) "\tMovescore: " (:movescore game))
    ;(submit-game "Georg 1 Test" game)
    )
  )
