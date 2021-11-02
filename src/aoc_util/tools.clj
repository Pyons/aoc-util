(ns aoc-util.tools
  "Tools to make Advent of Code work with the repl
  You can submit, retrieve and read the puzzle 

  By default it tries to infer the current puzzle to use, from the namespace e.g. se.2020.day2
  Year: 2020
  Day: 2

  Puzzle inputs are stored `resources/puzzle/{YEAR}/{DAY}.txt`
  Inputs are cached and reused they never change

  Puzzle descriptions are stored `resources/puzzle/{YEAR}/{DAY}.md`"
  (:gen-class
   :name aoc_util.tools
   :prefix "-"
   :main false
   :methods [#^{:static true} [get [String] java.util.List]
             #^{:static true} [open [String]]
             #^{:static true} [parseNS [String] java.util.List]
             #^{:static true} [submit [String Integer Integer] String]
             #^{:static true} [downloadDescription [String] String]
             #^{:static true} [downloadPuzzle [Integer Integer] java.io.File]])
  (:import [java.net CookieManager URI]
           [java.time ZonedDateTime Period]
           [java.awt Desktop]
           [io.github.furstenheim CopyDown])
  (:require [aoc-util.utils :refer [str->int]]
            [clojure.java.io :as io :refer [reader make-parents]]
            [clojure.string :as str]
            [hato.client :as hc]
            [hickory.core :as hi]
            [hickory.render :as hr]
            [hickory.select :as hs]
            [lambdaisland.regal :as regal]
            [tick.core :as t]))

(def ^{:doc "Advent of Code Server" :private true}
  host "https://adventofcode.com")

(defn- older-than?
  "Checks if a `ZonedDateTime` z is older than `Period` p"
  [^ZonedDateTime z ^Period p]
  (t/<
   (t/>> z p)
   (t/zoned-date-time)))

(defn parse-ns [ns]
  (let [number-cpt [:capture [:+ :digit]]
        r (regal/regex
           [:cat :start
            [:* [:cat [:+ :word] "."]]
            [:capture [:+ :digit]]
            "."
            [:alt
             [:cat "day" number-cpt]
             [:cat "d" number-cpt]
             number-cpt]
            :end])
        [_ year d1? d2? d3?] (re-find r (str ns))]
    (mapv str->int [year (or d1? d2? d3?)])))

(defn -parseNS [ns]
  (parse-ns ns))

(defn- last-modified [file]
  (-> file .lastModified (t/new-duration :seconds) t/inst))

(def ^:private read-session-key
  (delay
   (let [file (io/file "resources/session-key.cookie")]
     (if (.exists file)
       (let [cookie (slurp file)
             modified (t/zoned-date-time (last-modified file))]
         (if (older-than? modified (t/new-period 1 :months))
           (throw (Exception.  "Session key expired, pls renew"))
           (str/trim cookie)))
       (throw (Exception. "session-key.cookie file not found"))))))

(defn- add-cookies [^CookieManager cm ^String url cookies]
  (let [cookie-list (map #(str/join "=" %) cookies)]
    (doto cm
      (.put (URI. url) {"Set-Cookie" [(str/join ";" cookie-list)]}))))

(def ^:private Cookie-Manager
  (delay
   (-> (CookieManager.)
       (add-cookies host {"session" @read-session-key}))))

(defn download-puzzle
  "Puzzle id `year/day`"
  ^java.io.File
  [^Integer year ^Integer day]
  (let [puzzle-id (format "%s/%s" year day)
        puzzle-path (format "resources/puzzle/%s.%s" puzzle-id "txt")
        puzzle (io/file puzzle-path)]
    (if (.exists puzzle)
      puzzle
      (let [input (:body (hc/get (format "%s/%s/day/%s/input" host year day)
                                 {:http-client {:cookie-handler @Cookie-Manager}}))]
        (make-parents (format "resources/puzzle/%s.%s" puzzle-id "txt"))
        (spit (format "resources/puzzle/%s.%s" puzzle-id "txt") input)
        (io/file puzzle-path)))))

(defn -downloadPuzzle [year day]
  (download-puzzle year day))

(defn get!
  "puzzle-id `{ns}.{year}.day{x}` e.g. *ns*
  defaults to current namespace
  default parser `identity`"
  ([] (get! *ns* identity))
  ([parser] (get! *ns* parser))
  ([puzzle-id parser]
   (let [[year day] (parse-ns puzzle-id)]
     (println (format "input for: year %s day %s" year day))
     (with-open [rdr (reader (download-puzzle year day))]
       (mapv parser (line-seq rdr))))))

(defn -get
  ([^String ns]
   (get! ns identity)))

(defn submit!
  "Takes the namespace `{ns}.{year}.day{x}`, which part [1 2] and the answer"
  ([^Integer part ^Integer answer]
   (submit! *ns* part answer))
  ([puzzle-id ^Integer part ^Integer answer]
   (let [[year day] (parse-ns puzzle-id)
         _ (println (format "submitting for: year %s day %s part %s" year day part))
         req (hc/post
              (format "%s/%s/day/%s/answer" host year day)
              {:body (format "level=%s&answer=%s" part answer)
               :content-type "application/x-www-form-urlencoded"
               :http-client {:cookie-handler @Cookie-Manager
                             :redirect-policy :normal}})
         site (-> req :body hi/parse hi/as-hickory)]
     (-> (hs/select (hs/child (hs/tag "main")
                              hs/first-child
                              (hs/tag "p")) site) first :content first str/trim))))

(defn -submit [puzzle-id part answer]
  (submit! puzzle-id part answer))

(defn submit!1 [answer]
  (submit! *ns* 1 answer))

(defn submit!2 [answer]
  (submit! *ns* 2 answer))

(defn download-examples
  ([] (download-examples *ns*))
  ([ns]
   (let [[year day] (parse-ns ns)
         day-ctx (hc/get (format "%s/%s/day/%s" host year day)
                         {:http-client {:cookie-handler @Cookie-Manager}})
         site (-> day-ctx :body hi/parse hi/as-hickory)
         code-tags (hs/select (hs/child (hs/tag "code")) site)
         examples (map #(-> % :content first) code-tags)]
     (doseq [code examples]
       (println code))
     examples)))

(defn download-description
  "Download puzzle description"
  ([] (download-description *ns*))
  ([ns]
   (let [[year day] (parse-ns ns)
         puzzle-id (format "%s/%s" year day)
         doc-name (format "resources/puzzle/%s.%s" puzzle-id "md")
         day-ctx (hc/get
                  (format "%s/%s/day/%s" host year day)
                  {:http-client {:cookie-handler @Cookie-Manager}})
         site (-> day-ctx :body hi/parse hi/as-hickory)
         converter (CopyDown.)
         [main] (hs/select (hs/child (hs/tag "main")) site)
         md (.convert converter (hr/hickory-to-html main))]
     (make-parents doc-name)
     (spit doc-name md)
     doc-name)))

(defn -downloadDescription [ns]
  (download-description ns))

(defn open
  "Opens the problem with the default browser"
  ([] (open *ns*))
  ([ns]
   (let [[year day] (parse-ns ns)
         url (URI. (format "%s/%s/day/%s" host year day))]
     (.browse (Desktop/getDesktop) url))))

(defn -open [ns] (open ns))

(comment

  (download-description "ns.2020.day2")

  (get! "2020.23" identity)

  (open "ns.2020.day2")

  ;; gets the input for the puzzle
  (get! "ns.2020.day2" identity))
