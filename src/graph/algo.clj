(ns graph.algo
  (:require
    [graph.engine :as e]))

(defn transpose
  "Creer le graphe transposé de `graph`"
  [graph]
  (loop [edges (e/all-edges graph) res {}]
    (if (seq edges)
      (let [[src, dst] (first edges)]
        (recur (rest edges) (-> res
                                (e/add-vertex src)
                                (e/add-vertex dst)
                                (e/add-edge dst src))))
      res)))

(defn dfs
  "Fait un parcours en profondeur du graph `graph`"
  [graph vert fvert facc init visited]
  (loop [stack (list vert), visited visited, acc init]
    (if (seq stack)
      (let [acc' (facc acc (fvert (first stack)))
            visited' (conj visited (first stack))
            succs (clojure.set/difference (get graph (first stack)) visited')]
        (recur (distinct (concat succs (rest stack))) visited' acc'))
      acc)))

(defn augment [bigraph src visited match]
  (loop [dests (get bigraph src) visited visited match match]
    (if (seq dests)
      (if (visited (first dests))
        (recur (rest dests) visited match)
        ;;pas encore visité
        (if-let [msrc (get match (first dests))]
          (let [[found,visited',match'] (augment bigraph msrc (conj visited (first dests)) match)]
            (if found
              [true, visited', (assoc match' (first dests) src)]
              (recur (rest dests) visited' match')))
          ;;pas encore de match pour (first dests)
          [true, (conj visited (first dests)), (assoc match (first dests) src)]))
      [false, visited, match])))

(defn max-matching
  "defini le couplage maximale d'un graphe"
  [bigraph]
  (loop [verts (keys bigraph) match {}]
    (if (seq verts)
      (let [[_,_,match'] (augment bigraph (first verts) #{} match)]
        (recur (rest verts) match'))
      (if (= (count bigraph) (count match))
        match
        nil))))

(defn cmgraph
  [doms]
  (reduce (fn [res val] (let [[first end] val]
                          (assoc (e/remove-edge res end first) first #{end})))
          doms
          (max-matching doms)))

(defn dfs-stack-from
  [graph vert visited stk]
  (let [visited (conj visited vert)
        dests (clojure.set/difference (get graph vert) visited)]
    (loop [dests dests, visited visited, stk stk]
      (if (seq dests)
        (if (visited (first dests))
          (recur (rest dests) visited stk)
          (let [[stk', visited'] (dfs-stack-from graph (first dests) visited stk)]
            (recur (rest dests) visited' stk')))
        [(cons vert stk) visited]))))

(defn dfs-stack [graph]
  (loop [verts (keys graph) visited #{} stk ()]
    (if (seq verts)
      (if (visited (first verts))
        (recur (rest verts) visited stk)
        (let [[stk' visited'] (dfs-stack-from graph (first verts) visited stk)]
          (recur (rest verts) visited' stk')))
      stk)))

(defn scc-add [doms comp vert]
  (let [[var, vals] comp]
    (if (contains? doms vert)
      [(conj var vert) vals]
      [var (conj vals vert)])))

(defn  compute-scc [doms tgraph stk]
  (loop [stk stk, visited #{}, comps []]
    (if (seq stk)
      (if (visited (first stk))
        (recur (rest stk) visited comps)
        (let [[comp, visited'] (dfs tgraph
                                    (first stk)
                                    identity
                                    (fn [[comp, visited] vert]
                                      [(scc-add doms comp vert) (conj visited vert)])
                                    [(scc-add doms [#{} #{}] (first stk)) (conj visited (first stk))]
                                    (conj visited (first stk)))]
          (recur (rest stk) visited' (conj comps comp))))
      comps)))

(defn scc
  "Donne les composantes fortement connexe d'un graphe a partir des doms"
  [doms]
  (let [cmgraph (cmgraph doms)]
    (compute-scc doms (transpose cmgraph) (dfs-stack cmgraph)))
    )

