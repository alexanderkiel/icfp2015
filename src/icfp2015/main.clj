(ns icfp2015.main
  (:use plumbing.core)
  (:require
    [clojure.pprint :refer [pprint]]
    [icfp2015.io :refer [read-problem]]
    [icfp2015.core :refer :all]
    [icfp2015.tetris :refer :all]
    [org.httpkit.client :as http]
    [clojure.data.json :as json]
    [clj-time.format :as f]))

;; ---- Game ------------------------------------------------------------------

(def game (atom {}))

(defn init-game! [problem seed-idx]
  (reset! game (prepare-game problem seed-idx))
  :ok)

(defn play-game! []
  (swap! game (partial play naive-placement stupid-path))
  :ok)

(defn play-game-best! [depth]
  (swap! game (partial play naive-placement (partial best-path depth)))
  :ok)

(defn add-phrases! [phrases]
  (swap! game #(add-phrases % phrases))
  (:phrases @game))

(defn parse-date [date]
  (f/parse (f/formatters :date-time) date))

(defn tail-submissions [tag-regex n]
  (->> (-> @(http/get "https://davar.icfpcontest.org/teams/305/solutions"
                      {:as         :text
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
          "seed"      (:seed game),
          "tag"       tag,
          "solution"  (apply str (:commands game))}]
        resp @(http/post "https://davar.icfpcontest.org/teams/305/solutions"
                         {:body       (json/write-str solution)
                          :headers    {"Content-Type" "application/json"}
                          :basic-auth ["" (System/getenv "API_TOKEN")]})]
    (:status resp)))

(defn solve-problem [phrases path-gen problem seed-idx]
  (let [game (add-phrases (prepare-game problem seed-idx) phrases)]
    (play naive-placement path-gen game)))

(defn -main [& args]
  (println "I C Frantic People")

  (let [argmap (apply hash-map args)
        problemfile (get argmap "-f" "problems/problem_0.json")
        seedIdx (read-string (get argmap "-s" "0"))
        phrases (get argmap "-p" ["Ei!", "ia! ia!", "r'lyeh", "yuggoth","Planet 10"])
        depth (read-string (get argmap "-d" "5"))
        simple (get argmap "-simple" false)
        submit (get argmap "-submit" false)
        name (get argmap "-name" "Georg Script")
        problem (read-problem problemfile)
        game (solve-problem phrases (if simple stupid-path (partial best-path depth))
                            problem seedIdx)
        ]
    (println argmap)
    (println  phrases  simple depth problemfile seedIdx submit)

    (println (apply str (:commands game)))
    (println "Powerscore: " (:powerscore game) "\tMovescore: " (:movescore game))

    (if submit
      (let [_ (println "Submitting...")]
        (submit-game name game))
      )
    ;(pprint (tail-submissions #"Georg" 6))
    )
  )

(comment
  (-main "pla"))