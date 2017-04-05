(ns mrsudoku.solveur
  (:require [mrsudoku.grid :as g]))


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

(defn transpose
  "Creer le graphe transposé de `graph`"
  [graph]
  (loop [edges (all-edges graph) res {}]
    (if (seq edges)
      (let [[src, dst] (first edges)]
        (recur (rest edges) (-> res
                                (add-vertex src)
                                (add-vertex dst)
                                (add-edge dst src))))
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

(defn remove-edge
  "suprime un arc d'un graph"
  [graph first end]
  (if (graph first)
    (update graph first #(disj % end))
    graph))

(defn cmgraph
  [doms]
  (reduce (fn [res val] (let [[first end] val]
                          (assoc (remove-edge res end first) first #{end})))
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

(defn scc [doms cmgraph]
  (compute-scc doms (transpose cmgraph) (dfs-stack cmgraph)))

(defn doms-from-component
  [doms component]
  (let [[cvars cvals] component]
    (reduce (fn [res var]
              (assoc res var cvals)) doms cvars)))

(defn doms-from-scc [scc]
  (reduce doms-from-component {} scc))

(defn isolated-values [scc]
  (reduce (fn [res component]
            (let [[cvars cvals] component]
              (if (empty? cvars)
                (clojure.set/union res cvals)
                res))) #{} scc))

(defn isolated-vars [scc]
  (reduce (fn [res component]
            (let [[cvars cvals] component]
              (if (empty? cvals)
                (clojure.set/union res cvars)
                res))) #{} scc))

(defn has-value [doms ivars value]
  (reduce (fn [vars [var vals]]
            (if (and (contains? vals value) (ivars var))
              (conj vars var)
              vars)) #{} doms))

(defn add-value [doms vars value]
  (reduce (fn [doms var]
            (update doms var (fn [values]
                               (if (seq values)
                                 (conj values value)
                                 #{value}))))
          doms vars))

(defn isolated-vars-doms
  "docstring"
  [doms new-vars]
  (loop [vars new-vars]
    (if (seq vars)
      (if (= 1 (count (get doms (first vars))))
        #{(first vars)}
        (recur (rest vars)))
      new-vars)))

(defn access [doms scc]
  (let [doms' (doms-from-scc scc)
        isolated (isolated-values scc)
        ivars (isolated-vars scc)]
    (reduce (fn [ndoms value]
              (add-value ndoms (isolated-vars-doms doms (has-value doms ivars value)) value))
            doms' isolated)))

(defn alldiff [doms]
  (if-let [doms' (scc doms (cmgraph doms))]
    (access doms doms')
    nil))

(defn mkvar
  [x y]
  (keyword (str x y)))

(defn generate-doms-sudoku
  "Genere les domaines de contraintes du sudoku"
  [grid size]
  (let [vals (set (range 1 (inc size)))]
    (reduce (fn [res x] (reduce (fn [res y] (let [cell (g/cell grid x y)]
                                              (if-let [val (:value cell)]
                                                (assoc res (mkvar x y) #{val})
                                                (assoc res (mkvar x y) vals)))) res (range 1 (inc size))))
            {} (range 1 (inc size)))))

(defn alldif-col
  [doms col]
  (reduce (fn [res row] (let [var (mkvar col row)]
                          (assoc res var (get doms var)))) {} (range 1 10)))

(defn alldif-row
  [doms row]
  (reduce (fn [res col] (let [var (mkvar col row)]
                          (assoc res var (get doms var)))) {} (range 1 10)))

(defn alldiff-block
  [doms pos]
  (reduce (fn [res [col row]] (let [var (mkvar col row)]
                          (assoc res var (get doms var)))) {} pos))

(defn mkvartype
  [type num]
  (keyword (str type num)))

(defn partition-doms
  "genere les partitions de la grille de sudoku "
  [doms]
  (clojure.set/union (reduce (fn [res col] (assoc res (mkvartype "col" col) (alldif-col doms col))) {} (range 1 10))
                     (reduce (fn [res row] (assoc res (mkvartype "row" row) (alldif-row doms row))) {} (range 1 10))
                     (reduce (fn [res block] (assoc res (mkvartype "block" block) (alldiff-block doms (g/reduce-block (fn [acc index cx cy cell] (conj acc [cx,cy])) [] {} block)))) {} (range 1 10))))



(defn fusion-case
  "retourne la paire variable valeur correspondante au domaine dans part"
  [x y part]
  (let [var (mkvar x y)
        num-block (+ (inc (int (/ (dec x) 3))) (* (int (/ (dec y) 3)) 3))]
    (clojure.set/intersection (get (get part (mkvartype "col" x)) var)
                              (get (get part (mkvartype "row" y)) var)
                              (get (get part (mkvartype "block" num-block)) var))))

(defn fusion-doms
  "Prend le domaine partitionner et retourne le nouveau domaine"
  [part]
  (reduce (fn [res x] (reduce (fn [res y] (assoc res (mkvar x y) (fusion-case x y part))) res (range 1 10))) {} (range 1 10)))


(defn new-doms
  "prend la partition des somaine set retourne le nombre de modification
  retourne nil si ce n'est pas possible"
  [part]
  (loop [vars (keys part) part' part]
    (if (seq vars)
      (if-let [doms' (alldiff (get part' (first vars)))]
        (recur (rest vars) (update part' (first vars) (fn [_] doms')))
        nil)
      part')))

(defn sudoku-ok
  "retourne si le sudoku est bon"
  [doms]
  (reduce (fn [res [var vals]] (if (= 1 (count vals))
                                 res
                                 false)) true doms))

(defn test-sudoku
  [doms]
  (loop [doms doms stop false]
    (if stop
      (if (sudoku-ok doms)
        doms
        nil)
      (if-let [part' (new-doms (partition-doms doms))]
        (let [doms' (fusion-doms part')]
          (recur doms' (= doms doms')))
          nil))))


(defn update-grid
  "Met a jour la grille de sudoku a partir des contraintes"
  [grid doms]
  (reduce (fn [res x] (reduce (fn [res y] (let [var (mkvar x y)]
                                            (if (= 1 (count (get doms var)))
                                              (g/change-cell res x y (g/mk-cell (first (get doms var))))
                                              res))) res (range 1 10))) grid (range 1 10)))
