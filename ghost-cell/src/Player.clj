(ns Player
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn output
  [msg]
  (println msg)
  (flush))

(defn debug
  [msg]
  (binding [*out* *err*]
    (pp/pprint msg)
    (flush)))

(defn get-links
  [num-links]
  (vec (repeatedly num-links
                   (fn []
                     {:factories [(read) (read)]
                      :distance (read)}))))

(defn get-entity
  []
  (let [entity-id   (read)
        entity-type (read)]
    (cond
      (= entity-type 'FACTORY) {:id           entity-id
                                :type         ::factory
                                :owner        (read)
                                :num-cyborgs  (read)
                                :prod-rate    (read)
                                :prod-timeout (read)
                                :unused-arg   (read)}
      (= entity-type 'TROOP)   {:id          entity-id
                                :type        ::troop
                                :owner       (read)
                                :origin      (read)
                                :destination (read)
                                :num-cyborgs (read)
                                :turns-left  (read)}
      (= entity-type 'BOMB)    {:id          entity-id
                                :type        ::bomb
                                :owner       (read)
                                :origin      (read)
                                :destination (read)
                                :turns-left  (read)
                                :unused-arg  (read)})))

(defn get-entities
  [num-entities]
  (vec (repeatedly num-entities get-entity)))

(defn lengthen
  [lst n]
  (take n (cycle lst)))

(defn move
  [origs dests num-cyborgs]
  (->> [origs (lengthen dests (count origs)) num-cyborgs]
       (apply map #(str/join " " ["MOVE" (:id %1) (:id %2) %3]))
       (str/join ";")
       (output)))

(defn wait
  []
  (output "WAIT"))

(defn src-facts
  "Returns the factories to attack from"
  [factories]
  factories)

(defn attack-fact?
  [factory]
  (not= (:prod-rate factory) 0))

(defn target-facts
  "Returns the factories to attack"
  [factories]
  (let [attackable-factories (filter attack-fact? factories)]
    (if (< 0 (count attackable-factories))
      attackable-factories
      factories)))

(defn -main
  [& args]
  (let [factoryCount (read)
        linkCount (read)
        links (get-links linkCount)]
    (while true
      (let [entityCount (read)
            entities (get-entities entityCount)
            factories (filter #(= (:type %) ::factory) entities)
            troops (filter #(= (:type %) ::troop) entities)
            my-factories (filter #(= (:owner %) 1) factories)
            enemy-factories (filter #(not= (:owner %) 1) factories)]
        (if (and (< 0 (count enemy-factories))
                 (< 0 (count my-factories)))
          (let [orig-facts (src-facts my-factories)
                dest-facts (target-facts enemy-factories)]
            (move orig-facts dest-facts (map #(:num-cyborgs %) orig-facts)))
          (wait))))))
