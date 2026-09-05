;;;; rosette-blc/src/package.lisp --- Public API.

(defpackage #:rosette-blc
  (:use #:cl)
  (:export
   ;; term constructors / predicates
   #:tvar #:tlam #:tapp
   #:term-p
   #:term-var-p #:term-lam-p #:term-app-p
   #:term-index #:term-body #:term-fun #:term-arg
   ;; the BLC encoding
   #:blc-encode
   #:blc-length
   #:blc-decode
   ;; bitstring <-> "0010" string helpers
   #:bits->string
   #:string->bits
   ;; the classic combinators and Church numerals as terms
   #:combinator-i
   #:combinator-k
   #:combinator-s
   #:church-numeral
   ;; structural sharing -- de Bruijn helpers
   #:closed-p
   #:node-size
   #:count-subterm
   ;; structural sharing -- 1. CSE / let-factoring (pure BLC)
   #:blc-cse-factor
   #:blc-cse-expand
   #:blc-length-shared
   ;; structural sharing -- 2. minimal DAG / back-reference encoding
   #:blc-dag-encode
   #:blc-dag-decode
   #:blc-dag-length
   ;; BLC2 -- Levenshtein-coded de Bruijn indices (Tromp's gblc.c)
   #:levenshtein-encode
   #:levenshtein-decode
   #:levenshtein-length
   #:blc2-encode
   #:blc2-decode
   #:blc2-length
   #:blc2-length-shared))
