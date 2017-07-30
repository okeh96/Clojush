(ns clojush.interpreter
  (:require [clojush.graphs.utils :refer [profile-fn]])
  (:use [clojush pushstate globals util]
        [clojush.instructions tag input-output]
        [clojush.experimental.tagged-code-macros]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push interpreter

(def time-atom
   {:total (atom 0)
    :literal-type (atom 0)
    :empty-vector (atom 0)
    :input (atom 0)
    :tag (atom 0)
    :tagged-code-macro (atom 0)
    :instruction (atom 0)
    :recognize (atom 0)
    :recognize-literal (atom 0)
    :swap (atom 0)})

(defmacro time [name form]
  `(let [start# (System/currentTimeMillis)
         res# ~form]
     (swap! (~name time-atom) + (- (System/currentTimeMillis) start#))
     res#))

(defn execute-instruction
  "Executes a single Push instruction."
  [instruction state]
  ;; for debugging only, e.g. for stress-test
  ;(def debug-recent-instructions (cons instruction debug-recent-instructions))
  ;(def debug-recent-state state)
  (time :swap (swap! point-evaluations-count inc))
  (if (= instruction nil) ;; tests for nil and ignores it
    state
    (let [literal-type (time :recognize-literal (recognize-literal instruction))]
      (cond
        literal-type (time :literal-type (push-item instruction literal-type state))
        (time :recognize (and (vector? instruction) (= [] instruction))) (time :empty-vector (push-item [] :vector_integer (push-item [] :vector_float (push-item [] :vector_string (push-item [] :vector_boolean state)))))
        (time :recognize (and (symbol? instruction) (re-seq #"in\d+" (name instruction)))) (time :input (handle-input-instruction instruction state))
        (time :recognize (tag-instruction? instruction))  (time :tag (handle-tag-instruction instruction state))
        (time :recognize (tagged-code-macro? instruction)) (time :tagged-code-macro (handle-tag-code-macro instruction state))
        (time :recognize (contains? @instruction-table instruction)) (time :instruction ((instruction @instruction-table) state))
        :else (throw (Exception. (str "Undefined instruction: " (pr-str instruction))))))))

(def saved-state-sequence (atom []))



; (def execute-instruction-wrapped
;   (profile-fn execute-instruction [:generation :compute-errors :error-function :evaluate-program-for-behaviors :run-push :eval-push :execute-instruction]))


(defn eval-push
  "Executes the contents of the exec stack, aborting prematurely if execution limits are
   exceeded. The resulting push state will map :termination to :normal if termination was
   normal, or :abnormal otherwise."
  ([state] (eval-push state false false false))
  ([state print-steps] (eval-push state print-steps false false))
  ([state print-steps trace] (eval-push state print-steps trace false))
  ([state print-steps trace save-state-sequence]
    ;(when (empty? @global-atom-generators)
    ;  (println "global-atom-generators is empty. You should do something like: (reset! global-atom-generators '(exec_if boolean_not true false))"))
    (loop [iteration 1
           s state
           time-limit (if (zero? @global-evalpush-time-limit)
                        0
                        (+' @global-evalpush-time-limit (System/nanoTime)))]
      (if (or (> iteration @global-evalpush-limit)
              (and (empty? (:exec s)) (empty? (:environment s)))
              (and (not (zero? time-limit))
                   (> (System/nanoTime) time-limit)))
        (assoc s :termination (if (and (empty? (:exec s)) (empty? (:environment s)))
                                :normal
                                :abnormal))
        (if (empty? (:exec s))
          (let [s (end-environment s)]
            (when print-steps
              (printf "\nState after %s steps (last step: %s):\n"
                      iteration "end_environment_from_empty_exec")
              (state-pretty-print s))
            (when save-state-sequence
              (swap! saved-state-sequence #(conj % s)))
            (recur (inc iteration) s time-limit))
          (let [exec-top (top-item :exec s)
                s (pop-item :exec s)]
            (let [s (if (seq? exec-top)
                      (assoc s :exec (concat exec-top (:exec s)))
                      (let [execution-result (time :total (execute-instruction exec-top s))]
                        (cond
                          (= trace false) execution-result
                          (= trace true) (assoc execution-result
                                                :trace
                                                (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))
                          (= trace :changes) (if (= execution-result s)
                                               execution-result
                                               (assoc execution-result
                                                      :trace
                                                      (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))))))]
              (when print-steps
                (printf "\nState after %s steps (last step: %s):\n"
                        iteration (if (seq? exec-top) "(...)" exec-top))
                (state-pretty-print s))
              (when save-state-sequence
                (swap! saved-state-sequence #(conj % s)))
              (recur (inc iteration) s time-limit))))))))

(def eval-push-wrapped
  (profile-fn eval-push [:generation :compute-errors :error-function :evaluate-program-for-behaviors :run-push :eval-push]))

(defn run-push
  "The top level of the push interpreter; calls eval-push between appropriate code/exec
   pushing/popping. The resulting push state will map :termination to :normal if termination was
   normal, or :abnormal otherwise."
  ([code state]
    (run-push code state false false false))
  ([code state print-steps]
    (run-push code state print-steps false false))
  ([code state print-steps trace]
    (run-push code state print-steps trace false))
  ([code state print-steps trace save-state-sequence]
    (let [s (if @global-top-level-push-code (push-item code :code state) state)]
      (let [s (push-item (not-lazy code) :exec s)]
        (when print-steps
          (printf "\nState after 0 steps:\n")
          (state-pretty-print s))
        (when save-state-sequence
          (reset! saved-state-sequence [s]))
        (let [s (eval-push-wrapped s print-steps trace save-state-sequence)]
          (if @global-top-level-pop-code
            (pop-item :code s)
            s))))))
