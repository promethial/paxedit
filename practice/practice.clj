;;; Paxedit
;;; practice.clj, place to experiment with Paxedit

;;; Implicit Expressions in Clojure

;;; Clojure let

(let [x 100
      y (+ 1 2
           4 5
           5 6)
      z 678]
  (+ x y z))

;;; Clojure map

{:title "Apollo 13"
 :actors ["Tom Hanks"
          "Kevin Bacon"
          "Bill Paxton"
          "Gary Sinise"
          "Ed Harris"]
 :director "Ron Howard"
 :running-time 140}
