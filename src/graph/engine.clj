(ns graph.engine)

(defn add-vertex
  "Ajoute un sommet a un graphe"
  [graph id]
  (if (get graph id)
    graph
    (assoc graph id #{})))

(defn add-edge
  "Ajoute un arc a un graphe"
  [graph src dst]
  (update graph src (fn [dests] (conj dests dst))))


(defn all-edges
  "Retourne tout les arc du `graph`"
  [graph]
  (if (seq graph)
    (let [[src, dsts] (first graph)]
      (lazy-cat (map (fn[dst] [src, dst]) dsts) (all-edges (rest graph))))
    ()))

(defn remove-edge
  "suprime un arc d'un graph"
  [graph first end]
  (if (graph first)
    (update graph first #(disj % end))
    graph))