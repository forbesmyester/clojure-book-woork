(ns fireplace.core)

(defn e [x] (+ x 2))

(defn ^:export times2 [x]
  (* 2 x)
  )

(format "hi %s" "there")

(defn -main [&args]
  (apply str (map [\ "world" "hello"] [2 0 1])))

(set! *main-cli-fn* -main)

