(ns org.subluminal.test-class-file
  (:use (clojure test)
        (org.subluminal [class-file :as c])))

(deftest type-parser
  (is (= (c/full-parse c/<field-descriptor> "D") :double))
  (is (= (c/full-parse c/<field-descriptor> "V") :void))
  (is (= (c/full-parse c/<field-descriptor> "[[I")
         [:array [:array :int]]))
  (is (= (c/full-parse c/<field-descriptor> "Ljava/lang/Object;")
         'java.lang.Object))
  (is (= (c/full-parse c/<field-descriptor> "[Lcom/example/Outer$Inner;")
         [:array 'com.example.Outer$Inner]))
  (is (thrown? IllegalArgumentException
               (c/full-parse c/<field-descriptor> "java/lang/Object")))
  (is (thrown? IllegalArgumentException
               (c/full-parse c/<field-descriptor> ""))))

(deftest method-parser
  (is (= (c/full-parse c/<method-descriptor> "(II)V")
         [:method :void [:int :int]]))
  (is (= (c/full-parse c/<method-descriptor>
                       "([Ljava/lang/String;)Ljava/lang/Object;")
         [:method 'java.lang.Object [[:array 'java.lang.String]]])))

(deftest type-specifier
  (is (= (normalize-type-specifier Void/TYPE) :void))
  (is (= (normalize-type-specifier (class (int-array 10))) [:array :int]))
  (is (= (normalize-type-specifier Object) 'java.lang.Object))
  (is (= (normalize-type-specifier [:array [:array String]])
         [:array [:array 'java.lang.String]]))
  (is (= (normalize-type-specifier 'java.io.File) 'java.io.File))
  (is (= (type-descriptor-string :void) "V"))
  (is (= (type-descriptor-string [:array :int]) "[I"))
  (is (= (type-descriptor-string 'java.lang.Object) "Ljava/lang/Object;"))
  (is (= (type-descriptor-string [:array [:array 'java.lang.String]])
         "[[Ljava/lang/String;"))
  (is (= (type-descriptor-string 'java.io.File) "Ljava/io/File;")))

(deftest method-descriptor
  (is (= (normalize-method-descriptor
           [:method Integer [:double [:array Object]]])
         [:method 'java.lang.Integer [:double [:array 'java.lang.Object]]]))
  (is (= (normalize-method-descriptor
           [:method Void/TYPE [(class (long-array 10))]])
         [:method :void [[:array :long]]]))
  (is (= (method-descriptor-string
           [:method 'java.lang.Integer [:double [:array 'java.lang.Object]]])
         "(D[Ljava/lang/Object;)Ljava/lang/Integer;"))
  (is (= (method-descriptor-string
           [:method :void [[:array :long]]])
         "([J)V")))

(deftest interval-arithmetic
  (let [before {:pc [2 5]}
        [_ after] ((advance [10 20]) before)]
    (is (= (:pc after) [12 25])))
  (is (bound-interval? [50 55] [100 110] [-100 100]))
  (is (bound-interval? [100 110] [50 55] [-100 100]))
  (is (not (bound-interval? [20 40] [50 60] [0 30])))
  (is (not (bound-interval? [100 120] [10 20] [0 1000]))))

(deftest instruction-size
  (is (= (ins-size [0 0] {:op :aconst-null :args []} {}) [1 1]))
  (is (= (ins-size [10 20] {:op :aload :args [1]} {}) [2 2]))
  (is (= (ins-size [500 550] {:op :tableswitch :args [nil 'default 2 4
                                                     'second 'third 'fourth]}
                   {'default 1000 'second 1050 'third 2000 'fourth 0})
         [(+ 13 12) (+ 13 12 3)]))
  (is (= (ins-size [40000 40000] {:op :ifeq :args ['start]}
                   {'start [20000 24000]})
         [3 3]))
  (is (= (ins-size [40000 40000] {:op :ifeq :args ['start]}
                   {'start [0 20000]})
         [3 8]))
  (is (= (ins-size [40000 40000] {:op :ifeq :args ['start]} {})
         [3 8]))
  (is (= (ins-size [0 10000] {:op :ifeq :args ['end]}
                   {'end [20000 40000]})
         [3 8])))

(deftest instructions-misc
  (is (= (load-store-op :aload 0) :aload-0))
  (is (= (load-store-op :istore 1) :istore-1))
  (is (== (sizeof-desc :void) 0))
  (is (== (sizeof-desc :int) 1))
  (is (== (sizeof-desc :long) 2))
  (is (== (sizeof-desc 'java.lang.Object) 1))
  (is (== (sizeof-desc [:array :int]) 1))
  (is (== (method-weight [:method :void [:int :int :long]]) 4))
  (is (== (method-weight [:method :long ['java.lang.Object [:array :long]]])
          2)))
