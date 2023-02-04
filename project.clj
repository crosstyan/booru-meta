(defproject booru_meta "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clj-commons/byte-streams "0.3.2"]
                 [babashka/fs "0.2.14"]
                 [org.typedclojure/typed.clj.runtime "1.0.32"]
                 [com.google.guava/guava "31.1-jre"]
                 [clj-http "3.12.3"]
                 [progrock "0.1.2"]
                 [org.clojure/data.json "2.4.0"]
                 [cheshire "5.11.0"]
                 [net.mikera/imagez "0.12.0"]
                 [techascent/tech.ml.dataset "7.000-beta-25"]
                 [org.clojure/core.async "1.6.673"]
                 [org.clj-commons/hickory "0.7.3"]
                 [org.clj-commons/byte-streams "0.3.2"]
                 [bites "0.3.11"]
                 [com.novemberain/monger "3.5.0"]
                 [org.clojure/tools.cli "1.0.214"]
                 [clojupyter "0.3.6"]
                 [clojure-term-colors "0.1.0"]
                 [metosin/malli "0.10.1"]
                 ;; can't decide which one to use
                 ;; I'll take malli for now
                 [metosin/spec-tools "0.10.5"]
                 [org.clojure/spec.alpha "0.3.218"]]
  :profiles {:dev {:dependencies [[org.typedclojure/typed.clj.checker "1.0.32"]]}}
  :repl-options {:init-ns booru-meta.core}
  :main booru-meta.core)
