(ns aoc-util.nvim-socket
  "Allows to send commands to the current running nvim instance"
  (:require [clojure.java.shell :as sh]
            [clojure.java.io :refer [file]]
            [clojure.string :as str])
  (:import [java.lang ProcessHandle]
           [java.io BufferedReader StringReader]
           [com.ensarsarajcic.neovim.java.unix.socket UnixDomainSocketRpcConnection]
           [com.ensarsarajcic.neovim.java.corerpc.message RequestMessage$Builder]
           [com.ensarsarajcic.neovim.java.corerpc.client RpcClient]))

(defn- string-reader [s]
  (-> s
      StringReader.
      BufferedReader.))

(defn find-socket []
  (let [nvim-process-id (loop [cnt 0 pr (ProcessHandle/current)]
                          (let [process-info (.info pr)
                                cmd (.get (.command process-info))]
                            (cond
                              (> cnt 10) (throw (Exception. "Couldn't find nvim process"))
                              (re-find #"nvim$" cmd) (.pid pr)
                              :else (recur (inc cnt) (.get (.parent pr))))))
        lsof-out (:out (sh/sh "lsof" "-p" (str nvim-process-id)))
        filter-lsof (->> lsof-out
                         string-reader
                         line-seq
                         (filter #(re-find #"/nvim.*STREAM \(LISTEN\)$" %)) first)]
    (-> filter-lsof (str/split #"\s+") (nth 8))))

(def socket (delay (find-socket)))
(def socket-file (delay (file @socket)))
(def socket-connection (delay (UnixDomainSocketRpcConnection. @socket-file)))
(def client (RpcClient/getDefaultAsyncInstance))

(defn exec
  "Ex command `cmd` to send to nvim socket"
  [^String cmd]
  (.attach client @socket-connection)
  (.send client (doto
                 (RequestMessage$Builder. "nvim_exec")
                  (.addArgument cmd)
                  (.addArgument false))))

(defn snd-keys
  "Sends the keys strokes `k` to nvim e.g. 'jjjj'"
  [^String k]
  (.attach client @socket-connection)
  (.send client (doto
                 (RequestMessage$Builder. "nvim_feedkeys")
                  (.addArgument k)
                  (.addArgument "")
                  (.addArgument false))))

(defn get-line
  "Sends the keys strokes `k` to nvim e.g. 'jjjj'"
  [consumer]
  (.attach client @socket-connection)
  (.send client (doto
                 (RequestMessage$Builder. "nvim_get_current_line")) consumer))

(defn edit-file
  "Opens the file in nvim :edit f
  f is the path to the file"
  [^String f]
  (exec (format "edit %s" f)))

(comment

  (snd-keys "gg")
  (snd-keys "j,ee")
  (snd-keys ",ff")
  (edit-file "deps.edn")

  (get-line consumer)

  (find-socket))
