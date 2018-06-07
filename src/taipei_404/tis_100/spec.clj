(ns taipei-404.tis-100.spec
  [:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.pprint :as pp]])

(defn char-set [& params]
  (into #{}
        (mapcat (fn [param]
                  (if (vector? param)
                    (let [[min-char max-char] param
                          min-int (int min-char)
                          max-int (int max-char)]
                      (map char (range min-int (inc max-int))))
                    [param])))
        params))
; (char-set \x [\0 \4] \y [\A \D])

; Macro contagion from clojure.spec.alpha.cat  :-(
(defmacro token [text]
  `(clojure.spec.alpha/cat
    ~@(mapcat (fn [c]
                [(keyword (str c)) #{c}])
              (seq (str/lower-case text)))))

(s/def ::NEWLINE #{\newline})

(s/def ::SPACE #{\space \,})
(s/def ::SPACE* (s/* ::SPACE))
(s/def ::SPACE+ (s/+ ::SPACE))

(s/def ::COMMENT (s/cat :sharp #{\#}
                        :text (s/* (complement #{\newline}))))
; (s/conform ::COMMENT (seq "# abc"))

(s/def ::LABEL (s/+ (char-set [\a \z]
                              [\A \Z]
                              [\0 \9]
                              \_ \-)))
(s/def ::COLUMN #{\:})

(s/def ::ACC (token "acc"))
(s/def ::NIL (token "nil"))
(s/def ::UP (token "up"))
(s/def ::DOWN (token "down"))
(s/def ::LEFT (token "left"))
(s/def ::RIGHT (token "right"))
(s/def ::ANY (token "any"))
(s/def ::LAST (token "last"))
(s/def ::NUMBER
  (s/cat :sign (s/? #{\- \+})
         :digits (s/& (s/+ (char-set [\0 \9]))
                      #(<= 1 (count %) 3))))
; (s/conform ::ACC (seq "acc"))
; (s/conform ::NUMBER (seq "-123"))

(s/def ::NOP (token "nop"))
(s/def ::MOV (token "mov"))
(s/def ::SWP (token "swp"))
(s/def ::SAV (token "sav"))
(s/def ::ADD (token "add"))
(s/def ::SUB (token "sub"))
(s/def ::NEG (token "neg"))
(s/def ::JMP (token "jmp"))
(s/def ::JEZ (token "jez"))
(s/def ::JNZ (token "jnz"))
(s/def ::JGZ (token "jgz"))
(s/def ::JLZ (token "jlz"))
(s/def ::JRO (token "jro"))
(s/def ::HCF (token "hcf"))

(s/def ::source-operand
  (s/alt :acc ::ACC
         :nil ::NIL
         :up ::UP
         :down ::DOWN
         :left ::LEFT
         :right ::RIGHT
         :any ::ANY
         :last ::LAST
         :number ::NUMBER))
(s/def ::destination-operand
  (s/alt :acc ::ACC
         :nil ::NIL
         :up ::UP
         :down ::DOWN
         :left ::LEFT
         :right ::RIGHT
         :any ::ANY
         :last ::LAST))
(s/def ::0-arity-cmd
  (s/alt :nop ::NOP
         :swp ::SWP
         :sav ::SAV
         :neg ::NEG
         :hcf ::HCF))
(s/def ::1-arity-cmd
  (s/alt :add ::ADD
         :sub ::SUB
         :jro ::JRO))
(s/def ::2-arity-cmd ::MOV)
(s/def ::label-cmd
  (s/alt :jmp ::JMP
         :jnz ::JNZ
         :jgz ::JGZ
         :jlz ::JLZ))

(s/def ::label-declaration
  (s/cat :label ::LABEL
         :column ::COLUMN))

(s/def ::command
  (s/alt :cmd-alone ::0-arity-cmd
         :cmd-src (s/cat :cmd ::1-arity-cmd
                         :_ ::SPACE+
                         :src ::source-operand)
         :cmd-src-dst (s/cat :cmd ::2-arity-cmd
                             :_ ::SPACE+
                             :src ::source-operand
                             :_ ::SPACE+
                             :dst ::destination-operand)
         :cmd-label (s/cat :cmd ::label-cmd
                           :_ ::SPACE+
                           :label ::LABEL)))

(s/def ::command-line
  (s/cat :_ ::SPACE*
         :label-declaration (s/? ::label-declaration)
         :_ ::SPACE*
         :command (s/? ::command)
         :_ ::SPACE*
         :comment (s/? ::COMMENT)))

(s/def ::program
  (s/* (s/cat :command-line ::command-line
              :newline ::NEWLINE)))

; (def prog "mov up, acc
;            loop:
;            sub 1 # 1 is removed from acc
;            mov 1 down
;            jnz loop")
;
; (pp/pprint (s/conform ::program (seq prog)))
