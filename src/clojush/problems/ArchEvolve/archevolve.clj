(ns clojush.problems.ArchEvolve.archevolve
  (:use [clojush.pushgp.pushgp]
  		[clojush.pushstate]
  		[clojush.util]
  		[clojush.random])
  (:import [opengl/jogl]))

;================ Implementation 1: Node Generation =================

(defn node
	"Returns a function that pushes a node vector with three values (x,y,x)."
	[type]
	(fn [state]
		(if (not (or (empty? (rest (type state))) 
					 (empty? (rest (rest (type state))))))
			(let [x_val (stack-ref type 0 state)
				  y_val (stack-ref type 1 state)
				  z_val (stack-ref type 2 state)]
			  (println "Creating node")
			  (->> (pop-item type state)
			  	   (pop-item type)
             	   (pop-item type)
             	   (push-item [x_val y_val z_val] type)))
			state)))

(define-registered create_node (with-meta (node :node) {:stack-types [:float]}))

(defn connect_node
	[]
	(println "Connecting nodes")
	)

;================ Implementation 2: Tree-based Growth ===============

(defn forward
  "Adds one unit of length of construction material extending from the 
   current location and change the current state location to the end 
   of the construction."
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
	(let [errors []]
	(assoc individual :errors [0 0 0])))

;====================================================================

(def argmap
	{:error-function calc-error
	 :atom-generators (list (fn [] (lrand 10))
	 						'in1
	 						'float_div
                          	'float_mult
                          	'float_add
                          	'float_sub
                          	;'create_node
                          	'connect_node
	 						;'forward
	 						;'branch
	 						;'connectRandomPoint)
	 						)
	 :population-size 100
	 :use-single-thread true})