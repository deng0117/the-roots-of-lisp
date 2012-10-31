(ns ch4.hw11-parser-and-client-server-architecture
  (:use clojure.test))


(declare eval-)

(defn self-eval? 
  "test if variable evaluate to itself or not,
  with side effect printing exception message"
  [x]
  (try
    (or 
      (string? x)
      (number? x)
      (char? x)
      (keyword? x)
      (true? x)
      (false? x)
      (nil? x)
      (empty? x)
      (vector? x))
    (catch Exception e 
      (println "self-eval?")
      (println (.getMessage e))
      false)))

(defn math? [x]
  (cond
    (= '+ x) +
    (= '- x) -
    (= '* x) *
    (= '/ x) /
    :else false))

(defn get-math-op [x]
  (let [op (math? x)]
    (if op
      op
      (throw (Exception. (str x " is not math operator"))))))

(defn not-list? [x]
  (not (list? x)))

(defn fnfirst [x]
  (first (nfirst x)))

(defn fnnext [x]
  (first (nnext x)))

(defn fnnfirst [x]
  (fnext (nfirst x)))

(defn get-env-var 
  "from local environment get variable value"
  [env var]
  (cond 
    (contains? env var) (get env var) 
    (contains? env :parent) (recur (get env :parent) var)
    :else (throw (Exception. (str var " is unbound")))))

(defn create-local-env 
  "create loval environment"
  [env vars values]
  (loop [local-env {}
         vars vars
         values values]
    (cond
      (or (empty? vars) (empty? values)) 
      (assoc local-env
             :parent
             env)
      
      :else (recur (assoc local-env 
                          (first vars) 
                          (first values))
                   (next vars)
                   (next values)))))

(defn eval-arg 
  "evaluate anonymous function arguments"
  [args env-]
  (map (fn [x]
         (eval- x :env env-))
       args))

(defn eval-cond [cond-pairs env-]
  (if (eval- (first cond-pairs) :env env-)
    (eval- (fnext cond-pairs) :env env-)
    (eval-cond (nnext cond-pairs) env-)))


(def global-env {})

(defn eval-
  "http://lib.store.yahoo.net/lib/paulgraham/jmc.lisp"
  [exp & 
   {:keys [env]
    :or {env global-env}}]
         
  (cond
    (self-eval? exp) exp  ;var evaluate to self
    (not-list? exp) (get-env-var env exp)  ;var in env
    
    (not-list? (first exp))
    (cond 
      (= 'quote (first exp)) (fnext exp)  ;quote
      (= '= (first exp)) (= (eval- (fnext exp) :env env)
                            (eval- (fnnext exp) :env env))  ;equal
      (= 'first (first exp)) (first (eval- (fnext exp) :env env))  ;first
      (= 'next (first exp)) (next (eval- (fnext exp) :env env))  ;next
      (= 'cons (first exp)) (cons (eval- (fnext exp) :env env) 
                                  (eval- (fnnext exp) :env env))  ;cons
      ;cond
      (= 'cond (first exp)) (eval-cond (next exp) env)
      
      ;math
      (math? (first exp))
      (let [args (eval-arg (next exp) env)
            op (get-math-op (first exp))]
        (reduce op args))
           
      :else (eval- (cons (get-env-var env (first exp))
                         (next exp)) :env env))
           
    ;anonymous function
    (= 'fn (ffirst exp)) 
    (let [local-env (create-local-env env
                                      (fnfirst exp)
                                      (eval-arg (next exp) env))]
      (eval- (fnnfirst exp)
             :env local-env))))




;test code
(deftest test-sub
  "test self-defined function"
  []  
  (testing 
    "test multi first next"
    (is (= 2 
           (fnfirst '([1 2 3] 20 30))))
    (is (= 30 
           (fnnext '(10 20 30 40))))
    (is (= 3 
           (fnnfirst '([1 2 3 4] 20 30 40)))))
  
  (testing 
    "test get environment variable"
    (is (= 100 
           (get-env-var {:local 100} :local)))
    (is (= 1000
           (get-env-var {:local 100
                         :parent {:global 1000}} :global)))
    (is (= 10000
           (get-env-var {:local 100
                         :parent {:local 1000
                                  :parent {:global 10000}}} :global)))
    (is (= 100 
           (get-env-var {:x 100
                         :parent {:x 10000}} :x))))
  
  (testing 
    "test set environment variable"
    (is (= {:parent {:x 1, :y 2}} 
           (create-local-env {:x 1, :y 2} 
                             '() 
                             '())))
    (is (= {:x 1, :y 2, :parent {}} 
           (create-local-env {} 
                             '(:x :y) 
                             '(1 2))))
    (is (= {:x 1, :y 2, :parent {:x 100}} 
           (create-local-env {:x 100} 
                             '(:x :y) 
                             '(1 2))))))


(deftest test-eval
  "test clojure eval"
  []
  (is (let [vector- [1 2 3 4 5 6 7 8 9]]
        (= vector- (map eval- vector-))))
  
  (is (= '(1 2 3) 
         (eval- '(quote (1 2 3)))))
  
  (is (= 1 
         (first '(1 2 3 4)) 
         (eval- '(first '(1 2 3 4)))))
  (is (= '(2 3 4) 
         (next '(1 2 3 4)) 
         (eval- '(next '(1 2 3 4)))))
  (is (= '(100 1 2 3 4) 
         (cons 100 '(1 2 3 4)) 
         (eval- '(cons 100 '(1 2 3 4)))))
  
  (testing
    "test cond"
    (is (= "foo" 
           (cond true "foo")
           (eval- '(cond true "foo"))))
    (is (= "foo" 
           (cond false "first"
                 true "foo")
           (eval- '(cond false "first"
                         true "foo"))))
    (is (= "foo" 
           (cond false "first"
                 nil "second"
                 true "foo")
           (eval- '(cond false "first"
                         nil "second"
                         true "foo")))))
  (testing
    "test math"
    (is (+ 1 2)
        (eval- '(+ 1 2)))
    (is (+ 1 2 3)
        (eval- '(+ 1 2 3)))
    (is (- 1 2)
        (eval- '(- 1 2)))
    (is (* 1 2 3 4)
        (eval- '(* 1 2 3 4))))
  
  (testing
    "test anonymous function"
    (is (= 100 
           ((fn [x] x) 100) 
           (eval- '((fn [x] x) 100))))
    (is (= 'x
           ((fn [x] (quote x)) 100) 
           (eval- '((fn [x] (quote x)) 100))))
    (is (= true
           ((fn [x] (= x 100)) 100) 
           (eval- '((fn [x] (= x 100)) 100))))
    (is (= 1 
           ((fn [x] (first '(1 2 3 4))) 100) 
           (eval- '((fn [x] (first '(1 2 3 4))) 100))))
    (is (= '(2 3 4) 
           ((fn [x] (next '(1 2 3 4))) 100) 
           (eval- '((fn [x] (next '(1 2 3 4))) 100))))
    (is (= '(100) 
           ((fn [x] (cons x '())) 100) 
           (eval- '((fn [x] (cons x '())) 100))))
    (is (= '(100 200) 
           ((fn [x y] (cons x (cons y '()))) 100 200) 
           (eval- '((fn [x y] (cons x (cons y '()))) 100 200))))
    (is (= 100
           ((fn [x] (cond true x)) 100) 
           (eval- '((fn [x] (cond true x)) 100))))
    (is (= 101
           ((fn [x] (+ x 1)) 100) 
           (eval- '((fn [x] (+ x 1)) 100))))
    (is (= 300
           ((fn [x y] (+ x y)) 100 200) 
           (eval- '((fn [x y] (+ x y)) 100 200))))
    (is (= 10300
           ((fn [x y] (+ x y (* 10000 1))) 100 200) 
           (eval- '((fn [x y] (+ x y (* 10000 1))) 100 200))))
    (is (= 303
           ((fn [x y z] (+ x y (* z 1))) 100 200 3) 
           (eval- '((fn [x y z] (+ x y (* z 1))) 100 200 3))))
    (is (= 100
           ((fn [x] (cond false "foo"
                          true x)) 100) 
           (eval- '((fn [x] (cond false "foo"
                                  true x)) 100))))))


