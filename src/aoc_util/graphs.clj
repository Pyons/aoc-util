(ns aoc-util.graphs
  (:import
   [java.util HashSet LinkedList]))

(defn bfs [G s]
  (let [black-nodes (HashSet.)
        queue (doto (LinkedList.) (.add s))]
    (while (seq queue)
      (let [u (.pop queue)]
        (prn :parent u)
        (.add black-nodes u)
        (doseq [v (G u)]
          (when-not (.contains black-nodes v)
            (prn v)
            (.add queue v)
            (.add black-nodes v)))))))

(comment

  ;; Adjacent List
  (def g {:1 [:2 :5]
          :2 [:1 :5 :3 :4]
          :3 [:2 :4]
          :4 [:2 :5 :3 :7]
          :5 [:4 :1 :2 :6]})

  (seq  (LinkedList. [1]))

  (bfs g :3))
