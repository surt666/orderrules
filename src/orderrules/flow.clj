(ns orderrules.flow
  (:require [orderrules.core :refer :all]
            [clojure.core.async :refer (<! <!! >! >!! go chan close!)]
            [clj-time.core :as c]
            [clj-time.format :as f]
            [clj-time.local :as l]))

(declare ordre)
(declare abonnementer)

(defn step! [stepfn kundeid ordrer abonnementer]
  (let [val (apply stepfn [kundeid ordrer abonnementer])]
    val))

(defn strip-abons
  "Strip everything not needed for logic model"
  [abons]
  (mapv #(assoc {} :aftaletype (:aftaletype %) :status (:status %)) abons))

(defn provisioner [ordre]
  (prn "provisioner " (:varenr ordre))
  (let [abon (first (filter #(= (:varenr ordre) (:varenr %)) @abonnementer))
        abons (filter #(not= (:varenr ordre) (:varenr %)) @abonnementer)]
    (reset! abonnementer (conj abons (assoc abon :status "kunsignal")))
    :ok))

(defn can-activate? [o abons]
  (let [abon (when o (first (filter #(= (:varenr o) (:varenr %)) abons)))
        nu (l/local-now)
        hd (when (:handlingsdato o) (f/parse custom-formatter (:handlingsdato o)))]
    (and abon  (= "kunsignal" (:status abon)) (:handlingsdato o) (c/after? nu hd))))

(defn aktiver [ordre]
  (prn "aktiver " (:varenr ordre))
  (let [abon (first (filter #(= (:varenr ordre) (:varenr %)) @abonnementer))
        abons (filter #(not= (:varenr ordre) (:varenr %)) @abonnementer)]
    (reset! abonnementer (conj abons (assoc abon :status "aktiv")))
    :ok))

(defn aktiver-ordre [o abons]
  (if (can-activate? o abons)
    (aktiver o)
    :done))

(defn bestil-hw [ordre]
  (prn "bestil hw " (:varenr ordre))
  (swap! abonnementer (fn [a] (conj a {:varenr (:varenr ordre) :aftaletype (:aftaletype ordre) :pg-type (:pg-type ordre) :hw true} )))
  :ok)

(defn fakturer-ydelse [ordre]
  (prn "fakturer ydelse " (:varenr ordre))
  (swap! abonnementer (fn [a] (conj a {:varenr (:varenr ordre) :aftaletype (:aftaletype ordre) :pg-type (:pg-type ordre) :faktureret true})))
  :ok)

(defn bestil-hw-ordre [ordre abons]
  (let [can-provision? (when ordre (provision-subscription? ordre (strip-abons abons)))
        ydelse (first (filter #(= (:varenr ordre) (:varenr %)) abons))]
    (if (and can-provision? (:hw ordre) (nil? ydelse))
      (bestil-hw ordre)
      :done)))

(defn fakturer-ydelse-ordre [ordre abons]
  (let [nu (l/local-now)
        hd (when (:handlingsdato ordre) (f/parse custom-formatter (:handlingsdato ordre)))
        ydelse (first (filter #(= (:varenr ordre) (:varenr %)) abons))
        ga-abon (first (filter #(and (= "ga" (:pg-type %)) (= (:aftaletype ordre) (:aftaletype %)) (= (:varenr %) "1401009")) abons))] ;;TODO find GA fra ordre uden hardkodning
    (if (and (:handlingsdato ordre) (c/after? nu hd) (or (= "aktiv" (:status ga-abon)) (= "kunsignal" (:status ga-abon))) (or (nil? ydelse) (and ydelse (not (:faktureret ydelse))))) ;;TODO faktureret gemmes bare som abon i test. Not the way
      (fakturer-ydelse ordre)
      :done)))

(defn provisioner-ordre [ordre abons]
  (let [can-provision? (when ordre (provision-subscription? ordre (strip-abons abons)))
        abon (first (filter #(= (:varenr ordre) (:varenr %)) abons))]
  ;  (prn abon ordre can-provision? (not (nil? abon)) (not= "aktiv" (:status abon)) (not= "kunsignal" (:status abon)))
    (if (and ordre can-provision? (not (nil? abon)) (not= "aktiv" (:status abon)) (not= "kunsignal" (:status abon)))
      (provisioner ordre)
      :done)))

(defn extract-order [ordrer aftaletype pg-type]
  (let [o (first (filter #(and (= aftaletype (:aftaletype %)) (= pg-type (:pg-type %))) ordrer))
        hw (not (empty? (filter #(and (= aftaletype (:aftaletype %)) (:hw %)) ordrer)))
        tekniker (not (empty? (filter #(and (= aftaletype (:aftaletype %)) (:tekniker %)) ordrer)))]
    (when o (assoc o :hw hw :tekniker tekniker))))

(defn extract-abonnement [abons aftaletype pg-type & [varenr]]
  (if varenr
    (filter #(and (= aftaletype (:aftaletype %)) (= pg-type (:pg-type %)) (= varenr (:varenr %))) abons)
    (filter #(and (= aftaletype (:aftaletype %)) (= pg-type (:pg-type %))) abons)))

(defn create-ga-afventer [ordre]
  (let [o {:status "afventer", :varenr (:varenr ordre), :abonnementsid "20002101", :pg-type "ga", :aftaletype (:aftaletype ordre)}]
    (if (empty? (filter #(= (:varenr %) (:varenr o)) @abonnementer))
      (do
        (prn "opret ga afventer " (:varenr ordre))
        (swap! abonnementer (fn [a] (conj a o)))
        :ok)
      :done)))

(defn create-ga-luk [ordre]
  (let [o {:status "lukket", :varenr (:varenr ordre), :abonnementsid "20002101", :pg-type "ga", :aftaletype (:aftaletype ordre)}
        l (filter #(and (not= (:status %) "lukket") (= (:varenr %) (:varenr o))) @abonnementer)] ;; skal tage hensyn til alle åbne abonnement stati
    (if (not (empty? l))
      (do
        (prn "luk ga " (:varenr ordre))
        (swap! abonnementer (fn [a] (conj (filter #(not= (:varenr %) (:varenr o)) a) o)))
        :ok)
      :done)))

(defn skift-ga [o ga-abon]
  (let [abon (assoc ga-abon :varenr (:varenr o))
        abons (filter #(not= (:varenr %) (:varenr ga-abon)) @abonnementer)
        can-provision? (when o (provision-subscription? o (strip-abons abons)))]
    (if (and o ga-abon (= "SKIFT" (:handling o)) (not= (:varenr o) (:varenr ga-abon)) can-provision?)
      (do
        (prn "skift ga " (:varenr o))
        ; (provisioner o)
        (reset! abonnementer (conj abons abon))
        :ok)
      :done)))

(defn create-ga-afventer-ordre [ga-abon ordre]
  (if (and ordre (empty? ga-abon) (= "OPRET" (:handling ordre)))
    (create-ga-afventer ordre)
    :done))

(defn create-ga-luk-ordre [ga-abon ordre]
  (if (and ordre (not (empty? ga-abon)) (= "LUK" (:handling ordre)))
    (create-ga-luk ordre)
    :done))

(defn opret-clear-abon-ga-afventer-step [kundeid ordrer abons]
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer "clear" "ga")
        ga-abon (extract-abonnement abons "clear" "ga" (:varenr o))]
    (create-ga-afventer-ordre ga-abon o)))

(defn opret-bb-abon-ga-afventer-step [kundeid ordrer abons]
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer "bb" "ga")
        ga-abon (extract-abonnement abons "bb" "ga" (:varenr o))]
    (create-ga-afventer-ordre ga-abon o)))

(defn opret-tlf-abon-ga-afventer-step [kundeid ordrer abons]
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer "tlf" "ga")
        ga-abon (extract-abonnement abons "tlf" "ga" (:varenr o))]
    (create-ga-afventer-ordre ga-abon o)))

(defn skift-tlf-ga-abon-step [kundeid ordrer abons]
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer "tlf" "ga")
        ga-abon (first (extract-abonnement abons "tlf" "ga"))]
    (skift-ga o ga-abon)))

(defn luk-tlf-abon-ga-step [kundeid ordrer abons]
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer "tlf" "ga")
        ga-abon (extract-abonnement abons "tlf" "ga" (:varenr o))]
    (create-ga-luk-ordre ga-abon o)))

(defn provisioner-bb-ga-abon-step [kundeid ordrer abons]
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer "bb" "ga")]
    (provisioner-ordre o abons)))

(defn provisioner-tlf-ga-abon-step [kundeid ordrer abons]
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer "tlf" "ga")]
    (provisioner-ordre o abons)))

(defn aktiver-clear-abon-ga-step [kundeid ordrer abons]
  (let [o (extract-order ordrer "clear" "ga")]
    (aktiver-ordre o abons)))

(defn aktiver-bb-abon-ga-step [kundeid ordrer abons]
  (let [o (extract-order ordrer "bb" "ga")]
    (aktiver-ordre o abons)))

(defn aktiver-tlf-abon-ga-step [kundeid ordrer abons]
  (let [o (extract-order ordrer "tlf" "ga")]
    (aktiver-ordre o abons)))

(defn opret-bb-ky-hw [kundeid ordrer abons]
  (let [o (extract-order ordrer "bb" "ky")]
    (bestil-hw-ordre o abons)))

(defn opret-tlf-ky-hw [kundeid ordrer abons]
  (let [o (extract-order ordrer "tlf" "ky")]
    (bestil-hw-ordre o abons)))

(defn opret-tlf-ky-fakturering [kundeid ordrer abons]
  (let [o (extract-order ordrer "tlf" "ky")]
    (fakturer-ydelse-ordre o abons)))

(defn find-kunde-ordrer [kundeid]
  (get-in ordre [:bestilling :handlinger]))

(defn find-kunde-abonnementer [kundeid]
  @abonnementer)

(def steps [opret-clear-abon-ga-afventer-step opret-bb-abon-ga-afventer-step opret-tlf-abon-ga-afventer-step provisioner-bb-ga-abon-step provisioner-tlf-ga-abon-step skift-tlf-ga-abon-step aktiver-clear-abon-ga-step aktiver-bb-abon-ga-step aktiver-tlf-abon-ga-step opret-tlf-ky-hw opret-bb-ky-hw opret-tlf-ky-fakturering luk-tlf-abon-ga-step])

(defn async-order [kundeid ordrer abons]
  (mapv #(go (apply %1 [kundeid ordrer abons])) steps))

(def ordre (clojure.walk/keywordize-keys {"id" "2950aa52-f3ac-4f64-9d3b-922ac35adfdb"
                                          "bestilling" {"kundeid" "606125929"
                                                        "handlinger" [{"index" 0
                                                                       "handling" "SKIFT"
                                                                       "handlingsdato" "09-02-2014"
                                                                       "ordredato" "03-01-2014"
                                                                       "varenr" "1401009"
                                                                       "aftaletype" "tlf"
                                                                       "pg-type" "ga"
                                                                       "hw" false
                                                                       "tekniker" false
                                                                       "abonnementsid" "20000135"
                                                                       "installation" {"amsid" "452232"
                                                                                       "instnr" "155671"}}
                                                                      {"varenr" "1417015"
                                                                       "handling" "OPRET"
                                                                       "handlingsdato" "03-02-2014"
                                                                       "ordredato" "06-02-2013"
                                                                       "aftaletype" "tlf"
                                                                       "pg-type" "ky"
                                                                       "hw" true
                                                                       "tekniker" false
                                                                       "installation" {"amsid" "452232"
                                                                                       "instnr" "155671"}
                                                                       "index" 1
                                                                       "aftalenr" "1101927"}
                                                                      {"varenr" "1317015"
                                                                       "handling" "OPRET"
                                                                       "handlingsdato" "04-10-2013"
                                                                       "ordredato" "06-02-2013"
                                                                       "aftaletype" "bb"
                                                                       "pg-type" "ga"
                                                                       "hw" true
                                                                       "tekniker" false
                                                                       "installation" {"amsid" "452232"
                                                                                       "instnr" "155671"}
                                                                       "index" 3
                                                                       "aftalenr" "1301927"}
                                                                      ;; {"varenr" "1427015"
                                                                      ;;  "handling" "LUK"
                                                                      ;;  "handlingsdato" "03-10-2013"
                                                                      ;;  "ordredato" "06-02-2013"
                                                                      ;;  "aftaletype" "tlf"
                                                                      ;;  "pg-type" "ga"
                                                                      ;;  "hw" false
                                                                      ;;  "tekniker" false
                                                                      ;;  "installation" {"amsid" "452232"
                                                                      ;;                  "instnr" "155671"}
                                                                      ;;  "index" 4
                                                                      ;;  "aftalenr" "1101927"}
                                                                      ]
                                                        "info" {"betaler-brev" false
                                                                "klient-funktion" "k2uiBestil"
                                                                "juridisk-brev" true
                                                                "salgskanal" "K"
                                                                "klient-system" "SPOC"
                                                                "overstyret-salgskanal" "K"
                                                                "klient-bruger" "a65973"
                                                                "ordre-kvittering-email" "youseekvitteringer@gmail.com"}}
                                          "status" "Afsluttet"
                                          "ordredato" "06-02-2013"
                                          "version" "3.0"
                                          "k2orderid" "S01TSPOC----------------79959793"}))

(def abonnementer (atom [{:varenr "1101201"
                          :aftaletype "clear"
                          :pg-type "ga"
                          :status "aktiv"
                          :abonnementsid "20000101"}
                         ;; {:varenr "1301201"
                         ;;  :aftaletype "bb"
                         ;;  :pg-type "ga"
                         ;;  :status "aktiv"
                         ;;  :abonnementsid "20000102"}
                         {:varenr "1401201"
                          :aftaletype "tlf"
                          :pg-type "ga"
                          :status "aktiv"
                          :abonnementsid "20000103"}
                         ;; {:varenr "1427015"
                         ;;  :aftaletype "tlf"
                         ;;  :pg-type "ga"
                         ;;  :status "aktiv"
                         ;;  :abonnementsid "20000103"}
                         ]))

(defn handle-order [data]
  (loop [res [:ok]]
    (if (empty? (filter #(= :ok %) res))
      res
      (let [channels (apply async-order [(get data 0) (get-in (get data 1) [:bestilling :handlinger]) (find-kunde-abonnementer (get data 0))])
            nyres (mapv <!! channels)]
        (prn "R" nyres)
        (mapv close! channels)
        (recur nyres)))))

(defn demo []
  (handle-order ["606125929" ordre]))
