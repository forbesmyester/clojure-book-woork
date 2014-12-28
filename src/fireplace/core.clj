; (ns fireplace.core
;   (:gen-class))

; import for Java classes
; require for Clojure code
; ns combines both
; (require '[clojure.string :as str])
(ns fireplace.core
  (:require
      [clojure.string :as str]
      [clojure.core.typed :refer [ann ann-datatype HMap AnyInteger check-ns]]))

(ann times2 [AnyInteger -> (HMap :mandatory {:a AnyInteger})])

(defn times2
  "Multiplies a number by 2"
  [x]
  { :a (* 2 x) }
  )
(times2 12)

(doc ann-datatype)

(defn multi [f args]
  (let [res (atom {})
        done? #(= (count args) (count (keys (deref res))))
       ]
    (map-indexed
      (fn [i v]
        (print (str "(" i "/" v ")"))
        (swap! res assoc-in [i] v) 1)
      args)
    (deref res)
  ))

(multi #(+ 1 %) [1 2 3 4])

; java.string.indexOfAny
(defn indexed [col] (map-indexed vector col))
(defn index-filter [search, string]
  (when search
    (for [[idx elt] (indexed string) :when (search elt)] idx)
    )
  )
(index-filter #{\r} "abracadabra")
(defn javaStringIndexOfAny [search string]
  (first (index-filter search string)))
(javaStringIndexOfAny #{\r} "abracadabra")

(defrecord u-user [name friendSet])
(defrecord g-group [name userSet invitesSet])
(defrecord rg-requestGroup [name])
(defrecord gjr-groupJoinRequest [name votesFor])
(defrecord loadHolder [key record])

(defn dataforFriendsScreen [] 1)
(defn dataforGroupsScreen [] 1)
(defn dataforShowGroupScreen [] 1)
(defn dataforOtherUserScreen [] 1)
(defn dataforProfileScreen [] 1)


(def a (atom {:txt "hi"}))
(reset! a "Goodbye world!")
(deref a)

(defn e [words]
  (let [[w1, w2, w3] (str/split words #"\s+")]
    (str/join " " [w1, w2, w3, '...'])))

(e "hi there bob how are you")

; Six rules of recursion
; ======================
; 1. Avoid direct recursion. The JVM cannot optimize recursive calls, and Clojure programs that recurse will blow their stack.
; 2. Use recur when you are producing scalar values or small, fixed sequences.  Clojure will optimize calls that use an explicit recur .
; 3. When producing large or variable-sized sequences, always be lazy. (Do not recur.) Then, your callers can consume just the part of the sequence they actually need.
; 4. Be careful not to realize more of a lazy sequence than you need.  5. Know the sequence library. You can often write code without using recur or the lazy APIs at all.
; 6. Subdivide. Divide even simple-seeming problems into smaller pieces, and you will often find solutions in the sequence library that lead to more general, reusable code.

(defn recur-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (recur next (+ current next) (dec n))))]
    (fib 0N 1N n)))
(recur-fibo 2222)

(defn lazy-seq-fibo
  ([]
   (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b]
   (let [n (+ a b)]
     (lazy-seq
       (cons n (lazy-seq-fibo b n))))))
(nth (lazy-seq-fibo) 4000)

(defn fibo-inner-seq [] (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])) ; The 'N' here is important!
(nth (fibo-inner-seq) 5000)
(defn fibo []
  (map first (fibo-inner-seq))
  )
(nth (fibo) 5000)

(defn by-pairs-impl [c] (partition 2 1 c))
(defn is-pair [two] (every? #(= :h %) two))
(defn head-in-a-row [coll]
  (count (filter is-pair (by-pairs-impl coll)))
  )
(head-in-a-row [:h :h :t :h :t :h :h :t])

(def message (ref "Hi There"))
(dosync (ref-set message "Hi There Bob"))
(dosync (alter message str "+")) ; less concurrent
(dosync (commute message str "+")) ; more concurrent
(deref message)

(def counter (agent 0 :validator number?))
(send counter inc)
(send counter (fn [_] "boo")) ; fails validation function
(deref counter)
(clear-agent-errors counter)
(agent-errors counter)
(await-for 200 counter)

(defrecord Message [sender text])
(def messages (ref ()))
(def backup-agent (agent "/tmp/output-log.clj"))
(defn add-message-with-backup [msg]
  (dosync
    (let [snapshot (commute messages conj msg)]
      (send-off backup-agent (fn [filename]
                               (spit filename snapshot)
                               filename))
      snapshot)))
(add-message-with-backup (Message. "John" "Johnston"))
(deref messages)

;; Loops
(defn count-to [in-n]
  (loop [ar [] c 0]
    (if (> c in-n)
      ar
      (recur (cons (- in-n c) ar) (inc c)))))

(cons 2 (cons 1 nil))
(count-to 4)

(defn fillit [r g b x y w h]
  (let [
        frm (java.awt.Frame.)
        vis (.setVisible frm true)
        gfx (.getGraphics frm)
        clr (java.awt.Color. r g b)
        ]
    (do
      (.setSize frm 320 240)
      (.setColor gfx clr)
      (.fillRect gfx x y w h)
      )))

(fillit 255 122 122 20 20 50 50)

(def re #"\d")
(re-find re "abc3ed")
(clojure.string/split "abc3ed" re)

;; Types / Protocols
(defprotocol Smegable
  (smeg [this a b c]))

(defprotocol Toastable
  (toast [this]))

(deftype Rd [n]
  Smegable
  (smeg [this a b c] (+ (.n this) a b c)))

(extend-type Rd
  Toastable
  (toast [this] (+ (.n this) 1)))

(def rd (->Rd 5))
(.n rd)
(smeg rd 1 2 3)
(toast rd)

; Find a position in any collection

(defn pos-1-cmp [isMap item coll]
  (if isMap
    (= (second (first coll)) item)
    (= (first coll) item)))

(defn pos-1 [item coll]
  (loop [cl coll i 0]
    (when (seq cl)
      (if (cmp (map? coll) item cl)
        (if (map? coll)
          (first (first cl))
          i)
        (recur (next cl) (inc i))))))

(defn pos-2-seq [coll]
  (if (map? coll)
    (seq coll)
    (seq (zipmap coll (iterate #(+ 1 %) 0)))))

(defn pos-3-seq [coll]
  (if (map? coll)
    coll
    (map vector (iterate inc 0) coll)
    ))

(defn pos-3 [item coll]
  (first (for [[k v] (pos-3-seq coll) :when (= item v)] k)))

(defn pos-p [pred coll]
  (first (for [[k v] (pos-3-seq coll) :when (pred v)] k)))

(defn s [f]
  [(f [1 2 3]) (f {:a 1 :b 2 :c 3})])
(defn t [f]
  {
   :map (pos-3 2 {:a 1 :b 2 :c 3 :d 2})
   :vec (pos-3 2 [1 2 3 2])
   })
(defn tp [f]
  {
   :map (pos-p #(= 2 %) {:a 1 :b 2 :c 3 :d 2})
   :vec (pos-p #(= 2 %) [1 2 3 2])
   })

(t pos-3)
(tp pos-3)
(s pos-3-seq)

; Infinite Triangle Numbers

(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(defn triangle-iter [n & t]
  [(+ 1 (first n)) (triangle (first n))])

(defn triangle-first [n] [n (triangle n)])

(take 11 (map #(% 1) (iterate triangle-iter (triangle-first 1))))

;; Misc

(defn fnth [n]
  (comp #(last %) #(take (+ n 1) %)))
((fnth 1) [0 1 2 3 4 5])

(def plus5 (partial + 5))
(plus5 2 3)
(def not-equal-five (complement #(= 5 %)))
(not-equal-five 6)

;; Pre/Post conditions...

(defn slope [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  "This has a silly post condition..."
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(slope [0 0] [1.1 2])
(slope [1 1] [1 1])

;; Bot on Grid

(def bearings [{:x 0 :y 1}    ; north
               {:x 1 :y 0}    ; east
               {:x 0 :y -1}   ; south
               {:x -1 :y 0}]) ; west

(defn bot [x y bearing-num]
  {:coords
   [x y]
   :bearing
   ([:north :east :south :west] bearing-num)
   :forward
   (fn [] (bot (+ x (:x (bearings bearing-num)))
               (+ y (:y (bearings bearing-num)))
               bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(:coords ((:forward ((:turn-right ((:forward (bot 5 5 0))))))))

(def world [[ 1   1   1   1   1]
            [ 999 999 999 999 1]
            [ 1   1   1   1   1]
            [ 1   999 999 999 999]
            [ 1   1   1   1   1]])

(defn neighbors
  ([within-limit at] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] within-limit at)) 
  ([deltas within-limit at] 
   (filter 
     (fn[neighbour] 
       (every? #(< -1 % within-limit) neighbour)) 
     (map #(vec (map + at %)) deltas)))) 

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (:cost cheapest-nbr 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
            coll)))

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps :routes routes]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})
                   (into rest-work-todo
                         (map
                           (fn [w]
                             (let [[y x] w]
                               [(total-cost newcost step-est size y x) w]))
                           nbr-yxs)))))))))
(astar [0 0] 900 world)

; Chapter 8

(defmacro unless [condition & body] `(if (not ~condition) (do ~@body)))
(macroexpand-1 '(unless (= 1 1) (print "HI")))
(defn xxx [x] (unless (= 1 x) (print "HI")))
(xxx 1)
(xxx 2)

; Chapter 9

;; Prototyplical Inheritance.
(ns fozz.udp (:refer-clojure :exclude [get]))
(defn beget [o p] (assoc o ::prototype p))
(def put assoc)
(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))
(def cat {:likes-dogs true, :ocd-bathing true})
(def morris (beget {:likes-9lives true} cat))
(def post-traumatic-morris (beget {:likes-dogs nil} morris))
(get cat :likes-dogs)
(get morris :likes-dogs)
(get post-traumatic-morris :likes-dogs)

;; Multi Methods
(defmulti compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx [m] (get m :c-compiler))
(def clone (partial beget {}))
(def unix
  {:os ::unix, :c-compiler "cc", :home "/home", :dev "/dev"})
(def osx (-> (clone unix)
             (put :os ::osx)
             (put :c-compiler "gcc")
             (put :home "/Users")))
(compiler unix)
(compiler osx)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
