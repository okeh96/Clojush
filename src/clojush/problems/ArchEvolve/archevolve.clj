(ns clojush.problems.ArchEvolve.archevolve
  (:use [clojush.pushgp.pushgp]))


(defn forward
  "As in Hornby's 'forward' instruction, this adds one unit of length
   of construction material extending from the current location and 
   change the current state location to the end of the construction."
   [length]
   ; Take the current state location 
   (println "Moving forward")
   )

(defn branch
	"At the current node, create 'n' new branches in random directions"
	[branches]
	(println "Branching")
	)

(defn connectRandomPoint
	"Connect the current point to a randomly selected point in the space"
	[pointa pointb]
	(println "Connecting points")
	)

(defn calc-error
	"Access headless DynamoBim from command line to build model."
	[individual]
	(assoc individual :errors [0 0 0]))

(pushgp
	{:error-function calc-error
	 :atom-generators (list 'forward
	 						'branch
	 						'connectRandomPoint)
	 :population-size 100
	 :use-single-thread true})