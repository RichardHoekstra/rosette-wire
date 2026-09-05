;;;; distribution-compiler/src/package.lisp --- public API.

(defpackage #:rosette-distribution-compiler
  (:use #:cl)
  (:import-from #:rosette-ship #:library-closure)
  (:import-from #:rosette-toml #:read-toml #:toml-get)
  (:import-from #:rosette-symmetric-crypto #:sha256-hex #:string->bytes)
  (:export
   #:distribution-error #:distribution-error-code
   #:distribution-error-detail
   #:distribution-definition #:distribution-definition-p
   #:make-distribution-definition #:read-distribution-definition
   #:distribution-definition-name #:distribution-definition-version
   #:distribution-definition-roots #:distribution-definition-expected-systems
   #:distribution-definition-forbidden-systems
   #:distribution-definition-forbidden-prefixes
   #:distribution-definition-max-systems
   #:distribution-definition-public-license
   #:distribution-definition-target-language
   #:distribution-definition-language-profile
   #:distribution-definition-required-floor
   #:distribution-definition-candidate-floors
   #:distribution-definition-source-prefix
   #:distribution-definition-public-prefix
   #:distribution-definition-source-environment-prefix
   #:distribution-definition-public-environment-prefix
   #:distribution-definition-forbidden-tokens
   #:distribution-definition-cut-policy-id
   #:distribution-definition-export-policy-id
   #:distribution-definition-license-grants-id
   #:distribution-plan #:distribution-plan-p #:compile-distribution-plan
   #:distribution-plan-definition #:distribution-plan-systems
   #:distribution-plan-root-closures #:distribution-plan-id
   #:distribution-plan->form
   #:lower-distribution-name #:lower-distribution-text))
