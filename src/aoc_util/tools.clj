(ns aoc-util.tools
  "Tools to make Advent of Code work with the repl
  You can submit, retrieve and read the puzzle 

  By default it tries to infer the current puzzle to use, from the namespace e.g. se.2020.day2
  Year: 2020
  Day: 2

  Puzzle inputs are stored `resources/puzzle/{YEAR}/{DAY}.txt`
  Inputs are cached and reused as they never change

  Puzzle descriptions are stored `resources/puzzle/{YEAR}/{DAY}.md`"
  (:gen-class
   :name aoc_util.tools
   :prefix "-"
   :main false
   :methods [#^{:static true} [get [String] java.util.List]
             #^{:static true} [open [String] java.lang.Object]
             #^{:static true} [parseNS [String] java.util.List]
             #^{:static true} [submit [String Integer Integer] String]
             #^{:static true} [downloadDescription [String] String]
             #^{:static true} [downloadPuzzle [Integer Integer] java.io.File]])
  (:require
   [aoc-util.utils :refer [parse-int]]
   [clojure.java.io :as io :refer [file make-parents reader]]
   [clojure.pprint :refer [pprint] :as pp]
   [clojure.reflect :as reflect]
   [clojure.inspector :as insp]
   [clojure.string :as str]
   [hato.client :as hc]
   [hickory.core :as hi]
   [hickory.render :as hr]
   [clojure.java.javadoc :as jdoc]
   [hickory.select :as hs]
   [lambdaisland.regal :as regal]
   [tick.core :as t])
  (:import
   [io.github.furstenheim CopyDown]
   [java.awt Desktop]
   [java.io BufferedReader StringReader]
   [java.net CookieManager URI]
   [java.time Period ZonedDateTime]))

(def ^{:doc "Advent of Code Server" :private true}
  host "https://adventofcode.com")

(def ^:private session-cookie-path
  "resources/session-key.cookie")

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
    (mapv parse-int [year (or d1? d2? d3?)])))

(defn -parseNS [ns]
  (parse-ns ns))

(defn- increase-day
  "Increases the namespace"
  [ns]
  (let [[_ day] (parse-ns ns)
        next-day (inc day)
        re (re-pattern (str day "$"))]
    (cond
      (< day 0) (throw (Exception. "Advent of Code days start at 1"))
      (>= day 25) (throw (Exception. "Advent of Code ends at 25"))
      :else (str/replace ns re (str next-day)))))

(defn create-next-day
  "Creates the next Advent of Code day"
  ([] (create-next-day (increase-day *ns*)))
  ([ns]
   (let [next-day ns
         path (format "src/%s.clj" (str/replace next-day #"\." "/"))
         f (file path)
         template [(list 'ns (symbol next-day)
                         '(:require [aoc-util.tools :refer [create-next-day
                                                            download-description
                                                            get!
                                                            open-browser
                                                            submit-first!
                                                            submit-second!]]
                                    [aoc-util.utils :refer [parse-int line-process] :as utils]
                                    [clojure.edn :refer [read-string] :as edn]
                                    [clojure.string :as st]))
                   '(def input (get!))
                   '(comment (download-description)
                             (submit-first!))
                   '(comment (download-description)
                             (submit-second!))
                   '(comment (create-next-day))]]
     (make-parents path)
     (with-open [f (io/writer f)]
       (doseq [line template]
         (pprint line f)
         (.newLine f))))))

(defn- last-modified [file]
  (-> file .lastModified (t/new-duration :seconds) t/inst))

(defn set-session-cookie!
  "Write your session cookie to the right file, for later use"
  [^String cookie]
  (make-parents session-cookie-path)
  (spit session-cookie-path cookie))

(def ^:private read-session-key
  (delay
    (let [file (io/file session-cookie-path)]
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

(defn parse-input
  "Equivalent to get! but for strings,
  helps to use the examples from the puzzle-description"
  ([s] (parse-input s identity))
  ([s parser]
   (with-open [rdr (BufferedReader. (StringReader. s))]
     (mapv parser (line-seq rdr)))))

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

(defn submit-first! [answer]
  (submit! *ns* 1 answer))

(defn submit-second! [answer]
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

(defn open-browser
  "Opens the problem with the default browser"
  ([] (open-browser *ns*))
  ([ns]
   (let [[year day] (parse-ns ns)
         url (URI. (format "%s/%s/day/%s" host year day))]
     (.browse (Desktop/getDesktop) url))))

(defn -open [ns]
  (open-browser ns))

(comment

  (jdoc/javadoc #"sdf")
  (ancestors (type #"sdf"))

  (->> (reflect/reflect java.io.InputStream) :members (sort-by :name) (pp/print-table [:name :flags :parameter-types :return-type]))


  (reflect/reflect "sdfsdf")
  (set-session-cookie! "Test")
  (download-description "ns.2020.day2")

  (get! "2020.23" identity)

  (open-browser "ns.2020.day2")
  (parse-ns "ns.2020.day2")
  (create-next-day "ns.2020.day2")

  ;; gets the input for the puzzle
  (get! "ns.2020.day2" identity))
