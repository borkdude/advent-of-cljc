(ns aoc.y2017.d07.mfikes
  (:refer-clojure :exclude [read read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d07.data :refer [input answer-1 answer-2]]
   #?(:clj [clojure.java.io :as io])
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]
   [clojure.tools.reader :as r]
   [clojure.tools.reader.reader-types :as rt])
  #?(:clj (:import (java.io PushbackReader StringReader))))


;;; Reading support

;; Note that we wrap the protocol definitsions here in a function to
;; work around https://github.com/borkdude/advent-of-cljc/issues/39

(defn define-reading-protocols []

  (defprotocol IClosable
    "Protocol for closing entities."
    (-close [this] "Closes this entity."))

  (defprotocol IReader
    "Protocol for reading."
    (-read [this] "Returns available characters as a string or nil if EOF."))

  (defprotocol IBufferedReader
    "Protocol for reading line-based content. Instances of IBufferedReader must
     also satisfy IReader."
    (-read-line [this] "Reads the next line."))

  (defprotocol IPushbackReader
    "Protocol for readers that support undo. Instances of IPushbackReader must
    also satisfy IBufferedReader."
    (-unread [this s] "Pushes a string of characters back on to the stream.")))

(define-reading-protocols)

(deftype ^:private Reader [raw-read raw-close buffer pos]

  IReader
  (-read [_]
    (if-some [buffered @buffer]
      (do
        (reset! buffer nil)
        (subs buffered @pos))
      (raw-read)))

  IBufferedReader
  (-read-line [this]
    (loop []
      (if-some [buffered @buffer]
        (if-some [n (string/index-of buffered "\n" @pos)]
          (let [rv (subs buffered @pos n)]
            (reset! pos (inc n))
            rv)
          (if-some [new-chars (raw-read)]
            (do
              (reset! buffer (str (subs buffered @pos) new-chars))
              (reset! pos 0)
              (recur))
            (do
              (reset! buffer nil)
              (let [rv (subs buffered @pos)]
                (if (= rv "")
                  nil
                  rv)))))
        (if-some [new-chars (raw-read)]
          (do
            (reset! buffer new-chars)
            (reset! pos 0)
            (recur))
          nil))))

  IPushbackReader
  (-unread [_ s]
    (swap! buffer #(str s %))
    (reset! pos 0))

  IClosable
  (-close [_]
    (raw-close)))

(defn string-reader
  [s]
  #?(:clj  (PushbackReader. (StringReader. s))
     :cljs (let [content (volatile! s)]
             (->Reader
               (fn [] (let [return @content]
                        (vreset! content nil)
                        return))
               (fn [])
               (atom nil)
               (atom 0)))))

(defn- adapt-pushback-reader
  [pushback-reader]
  (reify
    rt/Reader
    (read-char [this]
      (when-some [characters (-read pushback-reader)]
        (when (> (count characters) 1)
          (-unread pushback-reader (subs characters 1)))
        (subs characters 0 1)))
    (peek-char [this]
      (when-some [ch (rt/read-char this)]
        (-unread pushback-reader ch)
        ch))
    rt/IPushbackReader
    (unread [this ch]
      (when (some? ch)
        (-unread pushback-reader ch)))))

(defn read
  "Reads the first object from an IPushbackReader.
  Returns the object read. If EOF, throws if eof-error? is true.
  Otherwise returns sentinel. If no reader is provided, *in* will be used.
  Opts is a persistent map with valid keys:
     :read-cond - :allow to process reader conditionals, or
                  :preserve to keep all branches
     :features - persistent set of feature keywords for reader conditionals
     :eof - on eof, return value unless :eofthrow, then throw.
            if not specified, will throw"
  ([reader]
   (r/read (#?(:clj identity :cljs adapt-pushback-reader) reader)))
  ([opts reader]
   (r/read opts (#?(:clj identity :cljs adapt-pushback-reader) reader)))
  ([reader eof-error? eof-value]
   (r/read (#?(:clj identity :cljs adapt-pushback-reader) reader) eof-error? eof-value)))



;;; Problem proper

(def reader (string-reader input))

(s/def ::weight (s/coll-of int? :kind list? :min-count 1 :max-count 1))
(s/def ::program (s/alt ::solo (s/cat :name symbol? :weight ::weight)
                        ::deps (s/cat :name symbol? :weight ::weight ::arrow #{'->} :held (s/* symbol?))))

(s/check-asserts true)
(def data (->> (repeatedly #(read {:eof nil} reader))
            (take-while some?)
            (s/assert (s/* ::program))
            (s/conform (s/* ::program))
            (map second)
            (map #(update % :weight first))))

(defn solve-1 []
  (first (set/difference (into #{} (map :name data)) (into #{} (flatten (keep :held data))))))

(let [index (set/index data [:name])]
  (defn lookup [name]
    (first (index {:name name}))))

(defn weight [name]
  (let [program (lookup name)]
    (apply + (:weight program) (map weight (:held program)))))

(defn solve-2 []
  (some (fn [program]
          (when-let [held (:held program)]
            (let [weights (map weight held)]
              (when-not (apply == weights)
                (let [unique-weight ((set/map-invert (frequencies weights)) 1)]
                  (+ (:weight (lookup (nth held (.indexOf weights unique-weight))))
                     (- (apply min weights) (apply max weights))))))))
    data))

(deftest part-1
  (is (= (str answer-1)
        (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
        (str (solve-2)))))
