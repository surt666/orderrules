(ns orderrules.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clj-time.core :as c]
            [clj-time.format :as f]
            [clj-time.local :as l]))

(def custom-formatter (f/formatter  "dd-MM-YYYY"))

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
(fact canstart :tlf :aktiv)
(fact canstart :tlf :kunsignal)

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

(defrel handlingsdato fo handling antal-dage hw tekniker)
(fact handlingsdato :clear :opret 0 false false)
(fact handlingsdato :clear :opret 10 false true)
(fact handlingsdato :clear :skift 0 false false)
(fact handlingsdato :clear :skift 10 false true)
(fact handlingsdato :clear :opsig :last-day-next-month false false)
(fact handlingsdato :clear :opsig :last-day-next-month false true)
(fact handlingsdato :bb :opret 0 false false)
(fact handlingsdato :bb :opret 10 false true)
(fact handlingsdato :bb :opret 10 true true)
(fact handlingsdato :bb :opret 3 true false)
(fact handlingsdato :bb :skift 0 false false)
(fact handlingsdato :bb :skift 10 false true)
(fact handlingsdato :bb :skift 10 true true)
(fact handlingsdato :bb :skift 3 true false)
(fact handlingsdato :bb :opsig :last-day-next-month false false)
(fact handlingsdato :bb :opsig :last-day-next-month false true)
(fact handlingsdato :bb :opsig :last-day-next-month true true)
(fact handlingsdato :bb :opsig :last-day-next-month true false)

(defn handlingsdato-ok? [o]
  (let [hdt (f/parse custom-formatter (:hd o))
        odt (f/parse custom-formatter (:od o))
        dt (if (c/after? hdt odt) hdt odt)
        nu (l/local-now)
        res (first (run 1 [q]
                         (handlingsdato (:a o) (:h o) q (:hw o) (:tek o))))
        tid (cond
              (= res :last-day-next-month) (c/last-day-of-the-month (c/plus nu (c/months 1)))
              :default (c/minus dt (c/days res)))]
    (if (c/after? nu tid)
      [true tid]
      [false tid])))

(defn create-subscription? [o al]
  (= '(_0) (run* [q]
         (fresh [needed status]
                (conde
                 [(membero {:a needed :s status} al)]
                 [(== needed nil)])
                (needs (:a o) needed)
                (hdok (get (handlingsdato-ok? o) 0))
                (canstart (:a o) status)))))

(def o {:a :bb :hd "30-09-2013" :od "01-09-2013" :h :opret :hw false :tek true})

(def l [{:s :aktiv :a :clear}])

(create-subscription? o l)
