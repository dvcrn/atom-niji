(ns niji.core
  (:require [cljs.nodejs :as node])
  (:import goog.dom.DomHelper))

;; reference to atom shell API
(def ashell (node/require "atom"))

;; js/atom is not the same as require 'atom'.
(def commands (.-commands js/atom))
(def workspace (.-workspace js/atom))
(def views (.-views js/atom))

;; get atom.CompositeDisposable so we can work with it
(def composite-disposable (.-CompositeDisposable ashell))

;; Atom for holding all disposables objects
(def disposables (atom []))

;; Initialise new composite-disposable so we can add stuff to it later
(def subscriptions (new composite-disposable))
(swap! disposables conj subscriptions)

(def color-classes (atom []))
(doseq [i (range 1 9)]
  (swap! color-classes conj (str "niji-" i)))

(defn toggle [] (.log js/console "niji got toggled!"))

;; Dispose all disposables
(defn deactivate []
    (.log js/console "Deactivating niji...")
    (doseq [disposable @disposables]
      (.dispose disposable)))

(defn serialize [] nil)

(def brackets-normalized '("(" ")" "[" "]"))
(def brackets-matching
  {"(" ")"
   "[" "]"})

(defn get-color-class []
  (let [result (first @color-classes)
        classes-without-first (subvec @color-classes 1)]
    (reset! color-classes (conj classes-without-first result))
    result))

(defn get-closing-node [dom-nodes node-index]
  (let [is-sub (atom 0)
        start-node (get dom-nodes node-index)
        opening-bracket (.-innerHTML start-node)
        closing-bracket (get brackets-matching opening-bracket)]

    (loop [index (inc node-index)]
      (if (not (< index (count dom-nodes)))
        nil
        ;; check if we hit a closing bracket
        (let [element (get dom-nodes index)]

          (if (= closing-bracket (.-innerHTML element))
            ;; check if we hit the closing bracket of a sub-node
            ;; if we hit a sub-bracket, decrease our sub-index by 1

            ;; if we didn't hit a sub-bracket (we are at level 0), return :
            (if (= @is-sub 0)
              (do
                ; (println "found closing bracket")
                element)
              (do
                ; (println "Found bracket was from other match")
                (swap! is-sub dec)
                ; (println (str "decreasing to " @is-sub))
                (recur (inc index))))

            ;; if it is not a closing bracket, it is a opening one
            (do
              ; (println "found another opening bracket")
              (swap! is-sub inc)
              ; (println (str "Increasing to " @is-sub))
              (recur (inc index)))))))))


(defn parse-brackets [dom-nodes]
  (let [dom-nodes (into [] dom-nodes)]
    (doseq [i (range 0 (count dom-nodes))]
      (let [element (get dom-nodes i)]
        (if (not (.contains (.-classList element) "niji-bracket"))
          (.add (.-classList element) "niji-bracket"))

        (if (contains? brackets-matching (.-innerHTML element))
          (let [start-element element
                end-element (get-closing-node dom-nodes i)
                color (get-color-class)]
            (if (nil? end-element)
              (.log js/console "!!!!! bracket error... sorry")
              (do
                ; (.log js/console "-----")
                ; (.log js/console start-element)
                ; (.log js/console end-element)
                ; (.log js/console (str "applying " color))
                (.add (.-classList start-element) color)
                (.add (.-classList end-element) color)))))))))


(defn on-file-load [texteditor]
  (let [editor-view (.getView views texteditor)
        all-elements (.querySelectorAll editor-view "::shadow *")
        all-elements-cljs (js->clj (.call js/Array.prototype.slice all-elements))]
    (->>
      all-elements-cljs
      (filter
        (fn [el]
          (let [content (.-innerHTML el)]
            (and
              (= (.-length content) 1)
              (some #{content} brackets-normalized)))))
      (parse-brackets))))


(defn activate [state]
  (.log js/console "Niji activated")

  (let [observe-disposable (.observeTextEditors workspace on-file-load)]
    (swap! disposables conj observe-disposable))

  (.add subscriptions
        (.add commands "atom-workspace" "niji:toggle" toggle)))

;; live-reload
;; calls stop before hotswapping code
;; then start after all code is loaded
;; the return value of stop will be the argument to start
(defn stop []
  (let [state (serialize)]
    (deactivate)
    state))

(defn start [state]
  (activate state))
