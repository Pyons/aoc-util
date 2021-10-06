(ns aoc-util.tools
  "Tools to make Advent of Code work with the repl
  You can submit, retrieve and read the puzzle 

  By default it tries to infer the current puzzle to use, from the namespace e.g. se.2020.day2
  Year: 2020
  Day: 2

  Puzzle inputs are stored `resources/puzzle/{YEAR}/{DAY}.txt`
  Inputs are cached and reused they never change

  Puzzle descriptions are stored `resources/puzzle/{YEAR}/{DAY}.md`"
  (:import [java.net CookieManager URI]
           [java.time ZonedDateTime Period]
           [io.github.furstenheim CopyDown])
  (:require [aoc-util.utils :refer [str->int]]
            [clojure.java.io :refer [resource reader file make-parents]]
            [clojure.string :as str]
            [hato.client :as hc]
            [hickory.core :as hi]
            [hickory.render :as hr]
            [hickory.select :as hs]
            [lambdaisland.regal :as regal]
            [tick.core :as t]))

(def ^{:doc "Advent of Code Server"}
  host "https://adventofcode.com")

(defn- older-than?
  "Checks if a ZonedDateTime is older than Period"
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

(defn- last-modified [path]
  (-> path file .lastModified (t/new-duration :seconds) t/inst))

(def read-session-key
  (delay
   (if-let [path (resource "session-key.cookie")]
     (let [cookie (slurp path)
           modified (t/zoned-date-time (last-modified path))]
       (if (older-than? modified (t/new-period 1 :months))
         (throw (Exception.  "Session key expired, pls renew"))
         (str/trim cookie)))
     (throw (Exception. "session-key.cookie file not found")))))

(defn- add-cookies [^CookieManager cm ^String url cookies]
  (let [cookie-list (map #(str/join "=" %) cookies)]
    (doto cm
      (.put (URI. url) {"Set-Cookie" [(str/join ";" cookie-list)]}))))

(def Cookie-Manager
  (delay (-> (CookieManager.)
             (add-cookies host {"session" @read-session-key}))))

(defn- download-puzzle
  "Puzzle id `year/day`"
  [year day]
  (let [puzzle-id (format "%s/%s" year day)
        puzzle-path (resource (format "puzzle/%s.%s" puzzle-id "txt"))]
    (or
     puzzle-path
     (let [input (:body (hc/get (format "%s/%s/day/%s/input" host year day)
                                {:http-client {:cookie-handler @Cookie-Manager}}))]
       (make-parents (format "resources/puzzle/%s.%s" puzzle-id "txt"))
       (spit (format "resources/puzzle/%s.%s" puzzle-id "txt") input)
       (resource (format "puzzle/%s.%s" puzzle-id "txt"))))))

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
         md (.convert converter
                      (hr/hickory-to-html main))]
     (make-parents doc-name)
     (spit doc-name md)
     (resource (format "puzzle/%s.%s" puzzle-id "md")))))

(comment

  (download-description "ns.2020.day2")

  ;; gets the input for the puzzle
  (get! "ns.2020.day2" identity))
