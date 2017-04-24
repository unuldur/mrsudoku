(ns csp.alldiff
  (:require
    [graph.algo :as ga]))

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
  (if-let [doms' (ga/scc doms)]
    (access doms doms')
    nil))