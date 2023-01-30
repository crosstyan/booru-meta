(defproject booru_meta "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [babashka/fs "0.2.14"]
                 [org.typedclojure/typed.clj.runtime "1.0.32"]
                 [com.google.guava/guava "31.1-jre"]
                 [clj-http "3.12.3"]
                 [progrock "0.1.2"]
                 [org.clojure/data.json "2.4.0"]
                 [cheshire "5.11.0"]]
  :profiles {:dev {:dependencies [[org.typedclojure/typed.clj.checker "1.0.32"]]}}
  :repl-options {:init-ns booru-meta.core})
