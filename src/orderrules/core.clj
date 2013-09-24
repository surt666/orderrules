(ns orderrules.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defrel needs fo neededfo)
(fact needs :clear nil)
(fact needs :bb :clear)
(fact needs :dtv :clear)
(fact needs :tlf :bb)
(fact needs :youbio nil)

(defrel canstart fo neededfostatus)
(fact canstart :clear nil)
(fact canstart :youbio nil)
(fact canstart :bb :aktiv)
(fact canstart :bb :kunsignal)
(fact canstart :dtv :aktiv)
(fact canstart :dtv :kunsignal)

(defrel hwstatusok fo status)
(fact hwstatusok :clear nil)
(fact hwstatusok :bb :sent)
(fact hwstatusok :bb :received)
(fact hwstatusok :tlf :sent)
(fact hwstatusok :tlf :received)

(defrel teknikerok fo status)
(fact teknikerok :clear :done)
(fact teknikerok :bb :done)
(fact teknikerok :tlf :done)

(defrel hdok val)
(fact hdok true)

(defn handlingsdato-ok? [hd]
  true)

(defn create-subscription? [o al]
  (= '(_0) (run* [q]
         (fresh [needed status]
                (conde
                 [(membero {:a needed :s status} al)]
                 [(== needed nil)])
                (needs (:a o) needed)
                (hdok (handlingsdato-ok? (:hd o)))
                (canstart (:a o) status)))))

(def o {:a :bb :hd "11-09-2013"})

(def l [{:s :aktiv :a :clear}])

(create-subscription? o l)
