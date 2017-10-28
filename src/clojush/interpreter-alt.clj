(ns clojush.interpreter-alt
    (:require [clojure.future :refer :all]
              [clojure.spec.alpha :as s]

              [clojush.bench.interpreter-bench :refer [grab-call-inputs]]
              [clojush.bench.core-bench :refer [configurations]]
              [clojush.test.core-test :refer [reset-globals!]]
              [clojush.interpreter :refer [eval-push]]))


(s/def ::tag integer?)
(def instructions
  (clojure.set/union
    #{:boolean_and
      :boolean_or
      :boolean_not
      :boolean_xor
      :boolean_invert_first_then_and
      :boolean_invert_second_then_and
      :boolean_frominteger
      :boolean_fromfloat
      :char_allfromstring
      :char_frominteger
      :char_fromfloat
      :char_isletter
      :char_isdigit
      :char_iswhitespace
      :exec_noop
      :exec_do*range
      :exec_do*count
      :exec_do*times
      :exec_while
      :exec_do*while
      :exec_if
      :exec_when
      :exec_k
      :exec_s
      :exec_y
      :genome_pop
      :genome_dup
      :genome_swap
      :genome_rot
      :genome_flush
      :genome_eq
      :genome_stackdepth
      :genome_yank
      :genome_yankdup
      :genome_shove
      :genome_empty
      :genome_gene_dup
      :genome_gene_randomize
      :genome_gene_replace
      :genome_gene_delete
      :genome_rotate
      :genome_gene_copy
      :genome_gene_copy_range
      :genome_toggle_silent
      :genome_silence
      :genome_unsilence
      :genome_close_inc
      :genome_close_dec
      :genome_new
      :genome_parent1
      :genome_parent2
      :autoconstructive_integer_rand
      :autoconstructive_boolean_rand
      :genome_genesis
      :genome_uniform_instruction_mutation
      :genome_uniform_integer_mutation
      :genome_uniform_float_mutation
      :genome_uniform_tag_mutation
      :genome_uniform_string_mutation
      :genome_uniform_boolean_mutation
      :genome_uniform_close_mutation
      :genome_uniform_silence_mutation
      :genome_uniform_deletion
      :genome_uniform_addition
      :genome_uniform_addition_and_deletion
      :genome_uniform_combination_and_deletion
      :genome_alternation
      :genome_two_point_crossover
      :genome_uniform_crossover
      :genome_instruction_eq
      :genome_close_eq
      :genome_silent_eq
      :genome_autoconstructing
      :genome_if_autoconstructing
      :print_exec
      :print_integer
      :print_float
      :print_code
      :print_boolean
      :print_string
      :print_char
      :print_newline}
    (into #{}
      (for [s ["integer" "float"]
            o ["add"
               "sub"
               "mult"
               "div"
               "mod"
               "lt"
               "lte"
               "gt"
               "gte"
               "fromboolean"
               "fromstring"
               "fromchar"
               "min"
               "max"
               "inc"
               "dec"]]
           (keyword (str s "_" o))))
    (into #{}
      (for [s ["exec" "integer" "float" "boolean" "string" "char"]
            o ["pop"
               "dup"
               "dup_times"
               "dup_items"
               "swap"
               "rot"
               "flush"
               "eq"
               "stackdepth"
               "yank"
               "yankdup"
               "shove"
               "empty"]]
        (keyword (str s "_" o))))
    #{:float_tan
      :float_sin
      :float_cos
      :float_frominteger
      :integer_fromfloat
      :boolean_rand
      :integer_rand
      :float_rand
      :code_rand
      :string_rand
      :char_rand
      :string_frominteger
      :string_fromfloat
      :string_fromboolean
      :string_fromchar
      :string_concat
      :string_conjchar
      :string_take
      :string_substring
      :string_first
      :string_last
      :string_nth
      :string_rest
      :string_butlast
      :string_length
      :string_reverse
      :string_parse_to_chars
      :string_split
      :string_emptystring
      :string_contains
      :string_containschar
      :string_indexofchar
      :string_occurrencesofchar
      :string_replace
      :string_replacefirst
      :string_replacechar
      :string_replacefirstchar
      :string_removechar
      :string_setchar
      :exec_string_iterate
      :print_exec
      :print_integer
      :print_float
      :print_code
      :print_boolean
      :print_string
      :print_char
      :integer_tagged_instruction
      :integer_untag_instruction
      :integer_tag_exec_instruction
      :integer_tag_code_instruction
      :integer_tag_integer_instruction
      :integer_tag_float_instruction
      :integer_tag_boolean_instruction
      :integer_tag_char_instruction
      :integer_tag_string_instruction}))


(s/def ::instruction instructions)

(s/def ::float float?)
(s/def ::string string?)
(s/def ::integer integer?)
(s/def ::boolean boolean?)
(s/def ::char char?)

(s/def ::item
  (s/keys
   :req [(or ::instruction
             ::tag
             ::float
             ::string
             ::integer
             ::boolean
             ::char)]))
(s/def ::push-program
  (s/coll-of
    (s/keys
      :req [(or ::item ::push-program)])))
(s/def ::close integer?)
(s/def ::silent boolean?)
(s/def ::uuid string?)
(s/def ::plush-gene
  (s/keys :req [::item ::close ::silent ::random-insertion ::uuid]))

(s/def ::plush-genome (s/coll-of ::plush-gene))

(s/def ::exec-stack ::push-program)
(s/def ::genome-stack (s/coll-of ::plush-genome))
(s/def ::output-stack (s/coll-of string?))
(s/def ::string-stack (s/coll-of string?))
(s/def ::integer-stack (s/coll-of integer?))
(s/def ::boolean-stack (s/coll-of boolean?))
(s/def ::char-stack (s/coll-of char?))
(s/def ::input-stack (s/coll-of string?))
(s/def ::float-stack (s/coll-of float?))
(s/def ::tag-stack (s/map-of integer? ::item))
(s/def ::termination #{:abnormal :normal})
(s/def ::push-state
  (s/keys
   :req [::exec-stack
         ::float-stack
         ::output-stack
         ::string-stack
         ::integer-stack
         ::genome-stack
         ::tag-stack
         ::input-stack
         ::boolean-stack
         ::char-stack]
   :opt [::termination]))
(def current-instructions
  (into #{}
    (map #(symbol (name %))
         instructions)))
(s/def :current/instruction current-instructions)

(s/def :current/item
  (s/or
    :instruction :current/instruction
    :tag (s/and symbol? #(clojure.string/starts-with? (name %) "tag"))
    ::float ::float
    ::string ::string
    ::integer ::integer
    ::boolean ::boolean
    ::char ::char))

(s/def :current/push-program
  (s/coll-of
    (s/or :item :current/item
          :push-program :current/push-program)))

(s/def :current.plush/instruction :current/item)
(s/def :current/uuid #(instance? java.util.UUID %))
(s/def :current/plush-gene
  (s/keys :req-un [:current.plush/instruction ::close ::silent ::random-insertion :current/uuid]))


(s/def :current/plush-genome (s/coll-of :current/plush-gene))

(s/def :current/exec :current/push-program)

(s/def :current/float (s/nilable ::float-stack))
(s/def :current/output (s/nilable ::output-stack))
(s/def :current/string (s/nilable ::string-stack))
(s/def :current/genome (s/nilable (s/coll-of :current/plush-genome)))
(s/def :current/string (s/nilable ::string-stack))
(s/def :current/termination (s/nilable ::termination))
(s/def :current/tag (s/nilable (s/map-of integer? :current/item)))
(s/def :current/input (s/nilable ::input-stack))
(s/def :current/boolean (s/nilable ::boolean-stack))
(s/def :current/char (s/nilable ::char-stack))
(s/def :current/push-state
  (s/keys
    :req-un [:current/exec
             :current/float
             :current/output
             :current/string
             :current/genome
             :current/tag
             :current/input
             :current/boolean
             :current/char]
    :opt-un [:current/termination]))

(s/def ::max-iterations int?)
(s/def ::options (s/keys :req [::max-iterations]))



(s/fdef current->new-push-state
  :args (s/cat :state ::current/push-state)
  :ret ::push-state)
(defn current->new-push-state [state]
  (let [{:keys []}]))




(reset-globals!)
(def total-args (concat (:jan-13 configurations) [":use-single-thread" "true"]))
(def inputs-outputs (grab-call-inputs #'eval-push 1000 total-args))

; clojush.core=> (def total-args (concat (:jan-13 configurations) [":max-generations" "5" ":use-single-thread" "true"]))


(def g inputs-outputs)

(defn filter-kv [f coll]
  (reduce-kv (fn [m k v] (if (f v) (assoc m k v) m)) {} coll))

(reduce clojure.set/union (map #(->> % :output (filter-kv (fn [x] (or (keyword? x) (seq x)))) keys set) g))
(clojure.pprint/pprint (reduce merge (map #(->> % :output (filter-kv (fn [x] (or (keyword? x) (seq x))))) g)))
(clojure.pprint/pprint (reduce merge (map #(->> % :input first (filter-kv (fn [x] (or (keyword? x) (seq x))))) g)))


; to get timings

(ns clojush.core-bench
  (:require [libra.bench :refer :all]

            [clojush.pushgp.record :as r]

            [clojush.core :refer [-main]]
            [clojush.test.core-test :refer [reset-globals!]]))
(defn call-main [args log]
  (reset-globals!)
  (let [new-gen r/new-generation!
        gen-start (atom nil)
        run-start (atom nil)]
    (with-redefs [r/new-generation!
                    (fn [i]
                      (println i)
                      (swap! gen-start
                        (fn [start-time]
                          (println start-time)
                          (when start-time
                            (let [gen-time (/ (- (System/nanoTime) start-time) 1e9)]
                              (swap! run-start (fn [t] (+ t gen-time)))
                              (log {:i i :gen-time  gen-time})))
                          (System/nanoTime)))
                      (new-gen i))]
     (reset! run-start (System/nanoTime))
     (with-out-str (apply -main args))
     (log {:run-time (/ (- (System/nanoTime) @run-start) 1e9)}))))

(def configurations
  {:jan-13
    ["clojush.problems.software.replace-space-with-newline"
      ":autoconstructive" "true"
      ":autoconstructive-genome-instructions" ":uniform"
      ":autoconstructive-diversification-test" ":size-and-instruction"
      ":autoconstructive-si-children" "2"
      ":autoconstructive-integer-rand-enrichment" "10"
      ":autoconstructive-boolean-rand-enrichment" "10"
      ":max-points" "1600"
      ":final-report-simplifications" "0"
      ":report-simplifications" "0"
      ":max-genome-size-in-initial-program" "400"
      ":evalpush-limit" "1600"
      ":parent-selection" ":leaky-lexicase"]
   :nth-prime
    ["clojush.problems.integer-regression.nth-prime"
     ":final-report-simplifications" "0"
     ":report-simplifications" "0"]})

(defn args [n]
  (concat (:jan-13 configurations) [":max-generations" (str n)]))

(defn measure- [n]
  (-> n
    args
    (call-main println)))

(map measure- (take 10 (repeat 0)))

; (do
;   (def zeros (doall (map measure- (repeat 500 0) )))
;   (def ones (doall (map measure- (repeat 500 1) ))))


THings to get
1. args from other stacks
2. self as instructions
