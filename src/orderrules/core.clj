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

(defrel hdok val)
(fact hdok true)

(defn handlingsdato<today? [hd]
  true)

(defn cancreate [o al]
  (run* [q]
        (fresh [needed status]
               (conde
                [(membero {:a needed :s status} al)]
                [(== needed nil)])
               (needs (:a o) needed)
               (hdok (handlingsdato<today? (:hd o)))
               (canstart (:a o) status))))

(def o {:a :youbio :hd "11-09-2013"})

(def l [{:s :aktiv :a :clear}])

(cancreate o l)
