(defproject orderrules "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.logic "0.8.5"]
                 [org.clojure/core.typed "0.2.21"]
                 [clj-time "0.6.0"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [expectations "1.4.56"]]
  :core.typed {:check [orderrules.flow]})
