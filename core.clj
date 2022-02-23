;; (ns push307.core
;;   (:gen-class))

;; (comment
;;   (NAME: Jack Kaffenbarger    
;;    * -> not currently in use, will be tested in technical report for helpfulness
;;
;;    New Stacks: :bool, :row-col, :output-row-col
;;
;;    New Instructions in Use: contains-queen?, get-row, get-col, return-row, return-col
;;                             exec-if, exec-dup-n, exec-do-range, get-rowcol-length
;;                             check-index-queen*, inc-integer*, dec-integer*
;;
;;    New Genetic Operator/Parent Selection: uniform-mutation*, lexicase-selection*
;;
;;    New Error Function: nqueens-error-calculator
;;    
;;    New GP Modifications: culling*, gp-simplification*
;;
;;))

;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '(integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

; An example Plushy genome
(def example-plushy-genome
  '(3 5 integer_* exec_dup "hello" 4 "world" integer_- close))

; An example Push program
; This is the program tha would result from the above Plushy genome
(def example-push-program
  '(3 5 integer_* exec_dup ("hello" 4 "world" integer_-)))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:genome '(3 5 integer_* exec_dup "hello" 4 "world" integer_- close)
   :program '(3 5 integer_* exec_dup ("hello" 4 "world" integer_-))
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37})

;;;;;;;;;;
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; The exception is `close`, which is only used in translation from Plushy to Push

(def default-instructions
  (list
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   'exec_dup
   'contains-queen?
   'get-row
   'get-col
   'return-row
   'return-col
   'exec-if
   'exec-dup-n
   'exec-do-range
   'get-rowcol-length
   'close
   0
   1))

; number of code blocks opened by instructions (default = 0)
(def opened-blocks
  {'exec_dup 1
   'exec-if 2
   'exec-dup-n 1
   'exec-do-range 1})

;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :input {}})

;the -1 -1 might be bad
(def empty-nqueens-state
  {:exec '()
   :integer '()
   :string '()
   :bool '()
   :row-col '()
   :output-row-col '(-1 -1)
   :input {}})

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (update state stack conj item))

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (update state stack pop))

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty? (state stack)) :no-stack-item
      (first (state stack))))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (state stack)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map with keys {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;
;; Instructions
(def my-state
  {:exec '()
   :integer '()
   :string '()
   :input {:in1 4}})

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (push-to-stack state :exec ((state :input) :in1)))

(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

;;;; This is an example of what would be necessary to implement integer_+
;;;; without the useful helper function make-push-instruction.
;; (defn integer_+_without_helpers
;;   [state]
;;   (if (< (count (:integer state)) 2)
;;     state
;;     (let [arg1 (peek-stack state :integer)
;;           arg2 (peek-stack (pop-stack state :integer) :integer)
;;           popped-twice (pop-stack (pop-stack state :integer) :integer)]
;;       (push-to-stack popped-twice :integer (+' arg1 arg2)))))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: if `first` is the top integer on the stack, and `second` is the 
  second, the result pushed to the stack should be (second - first)."
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator (returns stack with numerator on top), 
   to avoid divide-by-zero errors."
  [state]
  (if (= 0 (first (state :integer))) (pop-stack state :integer)
      (make-push-instruction state quot [:integer :integer] :integer)))

(defn exec_dup
  "Duplicates the top of the :exec stack"
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn contains-queen?
  "Check the row-col stack, pop off the top and if the list contains a 
   1, return true, else return false"
  [state]
  (if (empty-stack? state :row-col) state
      (let [result (peek-stack state :row-col)
            popped-once (pop-stack state :row-col)]
        (push-to-stack popped-once :bool (apply not= result)))))

(defn get-row
  "Pop off the top of integer stack, if the integer is in bounds return that row"
  [state]
  (if (empty-stack? state :integer) state
      ;now we need to check that the row is in bounds
      (let [index (peek-stack state :integer)]
        (if (< index 0) state
            (if (>= index (count ((state :input) :in1))) state
                ;index is in bounds
                (push-to-stack (pop-stack state :integer) :row-col (nth ((state :input) :in1) index)))))))

(defn get-col
  "Pop off the top of integer stack, if the integer is in bounds return that col"
  [state]
  (if (empty-stack? state :integer) state
      ;now we need to check that the col is in bounds
      (let [index (peek-stack state :integer)]
        (if (< index 0) state
            (if (>= index (count ((state :input) :in1))) state
                ;index is in bounds
                (push-to-stack (pop-stack state :integer) :row-col (map #(nth % index) ((state :input) :in1))))))))

(defn return-row
  "Push top of :integer stack to the row slot on the :output-row-col stack"
  [state]
  (if (empty-stack? state :integer) state
      (let [item (peek-stack state :integer)]
        (push-to-stack (pop-stack (pop-stack state :output-row-col) :integer) :output-row-col item))))

(defn return-col
  "Push top of :integer stack to the row slot on the :output-row-col stack"
  [state]
  (if (empty-stack? state :integer) state
      (let [item (peek-stack state :integer)
            row (peek-stack state :output-row-col)
            new-state (pop-stack (pop-stack (pop-stack state :output-row-col) :output-row-col) :integer)]
        (push-to-stack (push-to-stack new-state :output-row-col item) :output-row-col row))))

;; (defn push-output
;;   "Take the top of integer stack, and push to :output-row-col"
;;   [state]
;;   (if (empty-stack? state :integer) state
;;       (let [item (peek-stack state :integer)]
;;         (push-to-stack (pop-stack state :integer) :output-row-col item))))

(defn exec-if
  "NOOP unless at least two items on the exec stack and one on the bool stack. If True, remove second item on 
   exec stack, otherwise remove the first item"
  [state]
  (if (empty-stack? state :bool) state
      (if (nil? (second (state :exec))) state
          (let [truth-value (peek-stack state :bool)
                first-item (peek-stack state :exec)]
            (if (not truth-value) (pop-stack state :exec)
                (push-to-stack (pop-stack (pop-stack state :exec) :exec) :exec first-item))))))

(defn exec-dup-n
  "Copy the top of the :exec stack, and paste based on the top number on the :integer stack.
   Max of 50 to prevent running out of memory"
  [state]
  (if (empty-stack? state :integer) state
      (loop [new-state state
             count (peek-stack state :integer)]
        (if (< count 1) (pop-stack new-state :integer)
            (if (> count 50) (recur new-state 50)
                (recur (exec_dup new-state) (dec count)))))))

(defn get-rowcol-length
  "Return the length of the nxn input"
  [state]
  (push-to-stack state :integer (dec (count ((state :input) :in1)))))

(defn exec-do-range
  "Executes the top item on the :exec stack a number of times that depends on the top
   two integers, while also pushing the loop counter onto the :integer stack for 
   possible access during the execution of the loop"
  [state]
  (if (empty-stack? state :exec) state
      (if (nil? (second (state :integer))) state
          (let [code (peek-stack state :exec)
                destination-index (peek-stack state :integer)
                new-state (pop-stack (pop-stack state :integer) :exec)
                current-index (peek-stack new-state :integer)]
            (if (= destination-index current-index) (push-to-stack new-state :exec code)
                (let [updated-index (if (< destination-index current-index) (dec current-index) (inc current-index))]
                  (push-to-stack (push-to-stack new-state :exec (list updated-index destination-index 'exec-do-range code)) :exec code)))))))

(defn check-index-queen
  "Take the top number off the :integer stack and check the top row/col on the row/col stack.
   Push true if queen, false if not queen (1)"
  [state]
  (if (empty-stack? state :integer) state
      (if (empty-stack? state :row-col) state
          (let [index (peek-stack state :integer)
                row-col (peek-stack state :row-col)
                new-state (pop-stack (pop-stack state :integer) :row-col)]
            ;need to fix the bounds checking
            (if (> (count row-col) index) new-state
                (if (< (count row-col) index) new-state
                    (if (= 1 (nth row-col index))
                      (push-to-stack new-state :bool true)
                      (push-to-stack new-state :bool false))))))))

(defn inc-integer
  "Increment the top of :integer stack"
  [state]
  (if (empty-stack? state :integer) state
      (push-to-stack (pop-stack state :integer) :integer (inc (peek-stack state :integer)))))

(defn dec-integer
  "Decrement the top of :integer stack"
  [state]
  (if (empty-stack? state :integer) state
      (push-to-stack (pop-stack state :integer) :integer (dec (peek-stack state :integer)))))

;;;;;;;;;
;; Interpreter

(defn push-list-exec
  "Helper function that takes a program, and pushes it onto the exec stack"
  [program start-state]
  (loop [program (reverse program)
         start-state start-state]
    (if (empty? program) start-state
        (recur (rest program) (push-to-stack start-state :exec (first program))))))

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Or, if the next element is a nested list, needs to unwrap that list onto
  the exec stack.
  Returns the new Push state."
  [push-state]
  (let [item (peek-stack push-state :exec)
        return-state (pop-stack push-state :exec)]
    (if (int? item) (push-to-stack return-state :integer item)
        (if (string? item) (push-to-stack return-state :string item)
            (if (list? item) (push-list-exec item return-state)
                (if (boolean? item) (push-to-stack return-state :bool item)
                    (if (= clojure.lang.BigInt (type item)) (push-to-stack return-state :integer item)
                        ;otherwise the item is an instruction
                        (if (symbol? item) ((-> item symbol resolve) return-state)
                            (println "ERROR")))))))))

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing.
   To avoid infinite execution, you will need to enforce some maximum number
  of interpreter steps before terminating the program. You can choose this limit."
  [program start-state]
  (loop [start-state (push-list-exec program start-state)
         step-limit 0]
    (if (empty-stack? start-state :exec) start-state
        (if (= 5000 step-limit) start-state
            (recur (interpret-one-step start-state) (inc step-limit))))))

;;;;;;;;;
;; Translation from Plushy genomes to Push programs

(defn translate-plushy-to-push
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opened-blocks
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opened-blocks %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else

;;;;;;;;;
;; GP

(defn make-random-plushy-genome
  "Creates and returns a new plushy genome. Takes a list of instructions and
  a maximum initial Plushy genome size."
  [instructions max-initial-plushy-size]
  (loop [length (inc (rand-int max-initial-plushy-size))
         plushy-genome '()]
    (if (= length 0) plushy-genome
        (recur (dec length) (conj plushy-genome (nth instructions (rand-int (count instructions))))))))

(defn lexicase-helper
  "Given a population and a test case, return individuals that passed the test case"
  [population test-case-index]
  (filter #(zero? (nth (% :errors) test-case-index)) population))

(defn lexicase-selection
  "Iterates through test cases randomly, continuing until one program is left. If, after a test case,
   no individuals in the population survive, backtrack one test and randomly select, or if the first
   test case eliminates the population, randomly select an individual from the population."
  [population]
  (loop [test-case-index (shuffle (take (count ((first population) :errors)) (range)))
         population population
         new-population (lexicase-helper population (first test-case-index))
         c 1]
    (if (empty? new-population) (rand-nth population)
        (if (= c (count test-case-index)) (rand-nth new-population)
            (recur test-case-index new-population (lexicase-helper new-population (nth test-case-index c)) (inc c))))))

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size (10)."
  [population]
  (loop [individual (nth population (rand-int (count population)))
         c 0]
    (if (= 10 c) individual
        (let [new-individual (nth population (rand-int (count population)))]
          (if (> (individual :total-error) (new-individual :total-error))
            (recur new-individual (inc c))
            (recur individual (inc c)))))))

(defn crossover
  "Crosses over two Plushy genomes (note: not individuals) using uniform crossover.
  Returns child Plushy genome."
  [prog-a prog-b]
  (loop [length 0
         prog-c '()]
    (if (= length (max (count prog-a) (count prog-b))) (reverse prog-c)
        (recur (inc length) (if (= (rand-int 2) 1)
                                ;attempt to add the nth element from one of the programs, and if the index
                                ;is out of range iterate to the next step
                              (if (nil? (nth prog-a length nil)) prog-c
                                  (cons (nth prog-a length) prog-c))
                              (if (nil? (nth prog-b length nil)) prog-c
                                  (cons (nth prog-b length) prog-c)))))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the Plushy genomes) with some prob. Returns child Plushy genome."
  [prog instructions]
  ;if passed an empty genome, 10% of the time return a random instruction, otherwise return '()
  (if (empty? prog) (if (> (rand-int 10) 0) '() (nth instructions (rand-int (count instructions))))
      (loop [c 0
             new-prog (if (> (rand-int 10) 0) '() (list (nth instructions (rand-int (count instructions)))))]
        ;new-prog is initialized, with 10% of the time the first slot being filled with a random instruction
        ;then iterate through the rest of the slots
        (if (= c (count prog)) (reverse new-prog)
            (recur (inc c) (if (> (rand-int 10) 0) ;10% chance to add a new gene for each "slot"
                             (cons (nth prog c) new-prog)
                             (cons (nth instructions (rand-int (count instructions))) (cons (nth prog c) new-prog))))))))

(defn uniform-deletion
  "Randomly deletes instructions from Plushy genomes at some rate. Returns
   child Plushy genome."
  [prog]
  (if (empty? prog) prog
      (loop [c 0
             child-prog '()]
        (if (= c (count prog)) (reverse child-prog)
            ;delete an instruction 10% of the time
            (recur (inc c) (if (> (rand-int 10) 0) (cons (nth prog c) child-prog) child-prog))))))

(defn uniform-mutation
  "Randomly replace each instruction at a rate of 4%. Returns the child Plushy genome"
  [prog instructions]
  (if (empty? prog) prog
      (loop [chance (rand-int 25)
             child-prog '()
             c 0]
        (if (= c (count prog)) (reverse child-prog)
            (recur (rand-int 25) (if (= 4 chance)
                                   ;add a random instruction
                                   (cons (nth instructions (rand-int (count instructions))) child-prog)
                                   ;add the original instruction
                                   (cons (nth prog c) child-prog)) (inc c))))))

(defn select-and-vary-helper
  "Takes a genome, and creates and returns a child individual"
  [genome]
  {:genome genome :program (translate-plushy-to-push genome) :errors [] :total-error 0})

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program/genome). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover, 20% to uniform-addition,
  20% to uniform-deletion, and 10% to uniform mutation."
  [population instructions]
  (let [num (rand-int 10)]
    (if (< num 2) (select-and-vary-helper (uniform-deletion ((lexicase-selection population) :genome)))
        (if (< num 4) (select-and-vary-helper (uniform-addition ((lexicase-selection population) :genome) instructions))
            (if (< num 5) (select-and-vary-helper (uniform-mutation ((lexicase-selection population) :genome) instructions))
                (select-and-vary-helper (crossover ((lexicase-selection population) :genome) ((lexicase-selection population) :genome))))))))

;;;;;;;;;;
;; The error function
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  (+ (+ (* x (* x x)) x) 3))

(defn my-abs
  "Takes an integer and returns the absolute value of it"
  [value]
  (if (> value 0) value (*' -1 value)))

(defn error-calculator
  "Given a program and a test case, run the program and return the difference between
   target-function output and whatever is at the top of the integer stack. If there 
   is nothing on top of the integer stack, return 1000"
  [program test-case]
  (let [new-state (interpret-push-program program (assoc empty-push-state :input {:in1 test-case}))
        value (peek-stack new-state :integer)]
    (if (= :no-stack-item value) 1000
        (my-abs (-' (target-function test-case) value)))))

(defn regression-error-function
  "Takes an individual and evaluates it on some test cases.
  This will need to translate each individual's Plushy genome into a Push
  program before executing the Push program (see translate-plushy-to-push).
  For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors. You may also want to set
  :program to be the Push program translated from the Plushy genome, though
  this isn't mandatory.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual]
  (loop [new-individual (assoc individual :program (translate-plushy-to-push (individual :genome)))
         test-cases [-8 -5 -4 -3 -2 -1 0 1 2 3 4 5 8]
         c 0]
    (if (= c (count test-cases)) (assoc new-individual :total-error (apply +' (new-individual :errors)))
        (recur (assoc new-individual :errors (conj (new-individual :errors)
                                                   (error-calculator (new-individual :program) (get test-cases c))))
               test-cases (inc c)))))

(defn error-row
  "Given a state, calculate the number of queens on the given row"
  [state]
  (let [row (peek-stack (get-row (assoc state :integer (state :output-row-col))) :row-col)]
    (if (= :no-stack-item row) 10
        (count (filter odd? row)))))

(defn error-col
  "Given a state, calculate the number of queens on the given col"
  [state]
  (let [col (peek-stack (get-col (assoc state :integer (list (second (state :output-row-col))))) :row-col)]
    (if (= :no-stack-item col) 10
        (count (filter odd? col)))))

(defn nqueens-error-calculator
  "Given a program and a test case, run the program and return the number of queen
   conflicts (in rows and cols). If there is not two items on row-col stack, error of 30. 
   Diags are not considered, because a placed queen that conflicts with a one diagonal and one
   row/col is not as 'fit' as a placed queen that conflicts with both a row and a column."
  [program test-case]
  (let [new-state (interpret-push-program program (assoc empty-nqueens-state :input {:in1 test-case}))]
    ;(println new-state)
    (if (nil? (second (new-state :output-row-col))) 30
        (+ (error-row new-state) (error-col new-state)))))

(defn nqueens-error-function
  "Takes an individual and evaluates it on some test cases."
  [individual]
  (loop [new-individual (assoc individual :program (translate-plushy-to-push (individual :genome)))
         test-cases ['((0 0 1 0) (1 0 0 0) (0 0 0 0) (0 1 0 0))
                     '((0 0 0 0) (1 0 0 0) (0 0 0 1) (0 1 0 0))
                     '((0 0 1 0) (0 0 0 0) (0 0 0 1) (0 1 0 0))
                     '((0 0 1 0) (1 0 0 0) (0 0 0 1) (0 0 0 0))
                     '((0 0 0 0 0) (0 1 0 0 0) (0 0 0 0 1) (0 0 1 0 0) (1 0 0 0 0))
                     '((0 0 0 1 0) (0 0 0 0 0) (0 0 0 0 1) (0 0 1 0 0) (1 0 0 0 0))
                     '((0 0 0 1 0) (0 1 0 0 0) (0 0 0 0 0) (0 0 1 0 0) (1 0 0 0 0))
                     '((0 0 0 1 0) (0 1 0 0 0) (0 0 0 0 1) (0 0 0 0 0) (1 0 0 0 0))
                     '((0 0 0 1 0) (0 1 0 0 0) (0 0 0 0 1) (0 0 1 0 0) (0 0 0 0 0))
                     '((0 0 0 0 0 0) (0 0 1 0 0 0) (1 0 0 0 0 0) (0 0 0 0 0 1) (0 0 0 1 0 0) (0 1 0 0 0 0))
                     '((0 0 0 0 1 0) (0 0 0 0 0 0) (1 0 0 0 0 0) (0 0 0 0 0 1) (0 0 0 1 0 0) (0 1 0 0 0 0))
                     '((0 0 0 0 1 0) (0 0 1 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 1) (0 0 0 1 0 0) (0 1 0 0 0 0))
                     '((0 0 0 0 1 0) (0 0 1 0 0 0) (1 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 1 0 0) (0 1 0 0 0 0))
                     '((0 0 0 0 1 0) (0 0 1 0 0 0) (1 0 0 0 0 0) (0 0 0 0 0 1) (0 0 0 0 0 0) (0 1 0 0 0 0))
                     '((0 0 0 0 1 0) (0 0 1 0 0 0) (1 0 0 0 0 0) (0 0 0 0 0 1) (0 0 0 1 0 0) (0 0 0 0 0 0))
                     '((0 0 0 0 0 0 0) (0 0 0 1 0 0 0) (0 1 0 0 0 0 0) (0 0 0 0 0 0 1) (0 0 0 0 1 0 0) (0 0 1 0 0 0 0) (1 0 0 0 0 0 0))
                     '((0 0 0 0 0 1 0) (0 0 0 0 0 0 0) (0 1 0 0 0 0 0) (0 0 0 0 0 0 1) (0 0 0 0 1 0 0) (0 0 1 0 0 0 0) (1 0 0 0 0 0 0))
                     '((0 0 0 0 0 1 0) (0 0 0 1 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 1) (0 0 0 0 1 0 0) (0 0 1 0 0 0 0) (1 0 0 0 0 0 0))
                     '((0 0 0 0 0 1 0) (0 0 0 1 0 0 0) (0 1 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 1 0 0) (0 0 1 0 0 0 0) (1 0 0 0 0 0 0))
                     '((0 0 0 0 0 1 0) (0 0 0 1 0 0 0) (0 1 0 0 0 0 0) (0 0 0 0 0 0 1) (0 0 0 0 0 0 0) (0 0 1 0 0 0 0) (1 0 0 0 0 0 0))
                     '((0 0 0 0 0 1 0) (0 0 0 1 0 0 0) (0 1 0 0 0 0 0) (0 0 0 0 0 0 1) (0 0 0 0 1 0 0) (0 0 0 0 0 0 0) (1 0 0 0 0 0 0))
                     '((0 0 0 0 0 1 0) (0 0 0 1 0 0 0) (0 1 0 0 0 0 0) (0 0 0 0 0 0 1) (0 0 0 0 1 0 0) (0 0 1 0 0 0 0) (0 0 0 0 0 0 0))
                     '((0 0 0 0 0 0 0 0) (0 1 0 0 0 0 0 0) (0 0 0 0 0 0 1 0) (0 0 1 0 0 0 0 0) (0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 1) (0 0 0 0 1 0 0 0) (1 0 0 0 0 0 0 0))
                     '((0 0 0 1 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 1 0) (0 0 1 0 0 0 0 0) (0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 1) (0 0 0 0 1 0 0 0) (1 0 0 0 0 0 0 0))
                     '((0 0 0 1 0 0 0 0) (0 1 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 1 0 0 0 0 0) (0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 1) (0 0 0 0 1 0 0 0) (1 0 0 0 0 0 0 0))
                     '((0 0 0 1 0 0 0 0) (0 1 0 0 0 0 0 0) (0 0 0 0 0 0 1 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 1) (0 0 0 0 1 0 0 0) (1 0 0 0 0 0 0 0))
                     '((0 0 0 1 0 0 0 0) (0 1 0 0 0 0 0 0) (0 0 0 0 0 0 1 0) (0 0 1 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1) (0 0 0 0 1 0 0 0) (1 0 0 0 0 0 0 0))
                     '((0 0 0 1 0 0 0 0) (0 1 0 0 0 0 0 0) (0 0 0 0 0 0 1 0) (0 0 1 0 0 0 0 0) (0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 1 0 0 0) (1 0 0 0 0 0 0 0))
                     '((0 0 0 1 0 0 0 0) (0 1 0 0 0 0 0 0) (0 0 0 0 0 0 1 0) (0 0 1 0 0 0 0 0) (0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0))
                     '((0 0 0 1 0 0 0 0) (0 1 0 0 0 0 0 0) (0 0 0 0 0 0 1 0) (0 0 1 0 0 0 0 0) (0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 1) (0 0 0 0 1 0 0 0) (0 0 0 0 0 0 0 0))
                     '((0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 1 0 0 0 0 0) (0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0) (0 0 0 0 0 1 0 0 0) (0 0 1 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0))
                     '((0 0 0 0 1 0 0 0 0) (0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 1 0 0 0 0 0) (0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0) (0 0 0 0 0 1 0 0 0) (0 0 1 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0))
                     '((0 0 0 0 1 0 0 0 0) (0 0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 0) (0 0 0 1 0 0 0 0 0) (0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0) (0 0 0 0 0 1 0 0 0) (0 0 1 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0))
                     '((0 0 0 0 1 0 0 0 0) (0 0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0) (0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0) (0 0 0 0 0 1 0 0 0) (0 0 1 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0))
                     '((0 0 0 0 1 0 0 0 0) (0 0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 1 0 0 0 0 0) (0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0) (0 0 0 0 0 1 0 0 0) (0 0 1 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0))
                     '((0 0 0 0 1 0 0 0 0) (0 0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 1 0 0 0 0 0) (0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0) (0 0 0 0 0 1 0 0 0) (0 0 1 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0))
                     '((0 0 0 0 1 0 0 0 0) (0 0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 1 0 0 0 0 0) (0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0) (0 0 0 0 0 0 0 0 0) (0 0 1 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0))
                     '((0 0 0 0 1 0 0 0 0) (0 0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 1 0 0 0 0 0) (0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0) (0 0 0 0 0 1 0 0 0) (0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0))
                     '((0 0 0 0 1 0 0 0 0) (0 0 0 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 1 0 0 0 0 0) (0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0) (0 0 0 0 0 1 0 0 0) (0 0 1 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0))]
         c 0]
    (if (= c (count test-cases)) (assoc new-individual :total-error (apply +' (new-individual :errors)))
        (recur (assoc new-individual :errors (conj (new-individual :errors)
                                                   (nqueens-error-calculator (new-individual :program) (get test-cases c))))
               test-cases (inc c)))))

(defn initialize-population
  "Takes a population size, an error function, instructions, and max-initial-plushy-size
   and returns a population"
  [population-size error-function instructions max-initial-plushy-size]
  (loop [population '()
         genome (make-random-plushy-genome instructions max-initial-plushy-size)]
    (if (= population-size (count population)) (map error-function population)
        (recur (cons
                {:genome genome
                 :program (translate-plushy-to-push genome)
                 :errors []
                 :total-error 0}
                population) (make-random-plushy-genome instructions max-initial-plushy-size)))))


(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).

-------------------------------------------------------
               Report for Generation 3
-------------------------------------------------------
Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
Best program size: 33
Best total error: 727
Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)
  "
  [population generation]
  (loop [population population
         best-individual (first population)
         best-error (best-individual :total-error)]
    (if (empty? population)
      (do (println "-------------------------------------------------------")
          (println "               Report for Generation" generation)
          (println "-------------------------------------------------------")
          (println "Best genome: " (best-individual :genome))
          (println "Best program: " (best-individual :program))
          (println "Best program size: " (count (best-individual :genome)))
          (println "Best total error: " best-error)
          (println "Best errors: " (best-individual :errors)))
      ;loop through population, and find the lowest total error individual
      (if (< ((first population) :total-error) best-error)
        (recur (rest population) (first population) ((first population) :total-error))
        (recur (rest population) best-individual best-error)))))

(defn no-error?
  "Takes a population, and runs true if there is an individual with no error, otherwise
   returns false"
  [population]
  (loop [population population]
    (if (empty? population) false
        (if (= 0 ((first population) :total-error)) true
            (recur (rest population))))))

(defn evolve
  "Takes an old population and returns an evolved one of the same size, using select-and-vary"
  [population instructions error-function]
  (loop [new-population '()
         c 0]
    (if (= c (count population)) (map error-function new-population)
        (recur (cons (select-and-vary population instructions) new-population) (inc c)))))

(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-plushy-size (max size of randomly generated Plushy genomes)"
  [{:keys [population-size max-generations error-function instructions max-initial-plushy-size]
    :as argmap}]
  (loop [population (initialize-population population-size error-function instructions max-initial-plushy-size)
         generation 0]
    (report population generation)
    (if (no-error? population) :SUCCESS
        (if (= max-generations generation) nil
            (recur (evolve population instructions error-function) (inc generation))))))

;;;;;;;;;
;; GP Modifications

(defn culling
  "Given a population, get rid of individuals who perform the same or better than the 75th percentile
   and fill with random individuals. To be clear, this does not remove the top 25%, it removes all 
   individuals that performed as well as the top 25%"
  [population]
  (let [cutoff-index (quot (count population) 4)
        cutoff-value ((nth (sort-by :total-error population) cutoff-index) :total-error)
        new-population (filter #(<= cutoff-value (% :total-error)) population)
        random-population (initialize-population (- (count population) (count new-population)) nqueens-error-function default-instructions 50)]
    (distinct (concat new-population random-population))))

(defn simplify-genome-helper
  "Given an index and a genome, remove that index"
  [genome index]
  (loop [new-genome '()
         index index
         c 0]
    (if (= c (count genome)) (reverse new-genome)
        (if (not= c index)
          (recur (cons (nth genome c) new-genome) index (inc c))
          (recur new-genome index (inc c))))))

(defn simplify-genome
  "Given a genome, remove 1-4 random instructions"
  [genome]
  (if (< (count genome) 4) genome
      (loop [genome genome
             remove-index (rand-int (count genome))
             c (inc (rand-int 4))]
        (if (= c 0) genome
            (recur (simplify-genome-helper genome remove-index) (rand-int (- (count genome) 1)) (dec c))))))

(defn genome-compare
  "If the two genome behave the same on the test cases, return true, otherwise return false"
  [genomeA, genomeB]
  (let [individualA  {:genome genomeA
                      :program '()
                      :errors []
                      :total-error 0}
        individualB {:genome genomeB
                     :program '()
                     :errors []
                     :total-error 0}]
    (if (= ((nqueens-error-function individualA) :errors) ((nqueens-error-function individualB) :errors)) true
        false)))

(defn gp-simplification
  "Loop over 1000 steps, make a new genome that has 1-4 instructions removed, in behavior is 
   the same on test cases then update the genome."
  [genome]
  (loop [genome genome
         new-genome (simplify-genome genome)
         c 0]
    (if (= c 1000) genome
        (if (genome-compare genome new-genome)
          (recur new-genome (simplify-genome new-genome) (inc c))
          (recur genome (simplify-genome genome) (inc c))))))

;;;;;;;;;;
;; The main function call
;; You can call this in a REPL, or alternatively from the command line
;; by running:
;;   lein run

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'push307.core)]
    ; The above line is necessary to allow `lein run` to work
    (push-gp {:instructions default-instructions
              :error-function nqueens-error-function
              :max-generations 200
              :population-size 200
              :max-initial-plushy-size 50})))
