;;;; rosette-proof-witness/src/delta.lisp --- Delta reports.

(in-package #:rosette-proof-witness)

(defstruct (delta-report
            (:constructor %make-delta-report
                (&key name baseline candidate runtime-ns speedup
                      register-count occupancy memory-traffic numerics metadata)))
  "Adapter/runtime delta report for compiler witness publication."
  (name :delta :type keyword)
  baseline
  candidate
  runtime-ns
  speedup
  register-count
  occupancy
  memory-traffic
  numerics
  (metadata nil :type list))

(defun make-delta-report (&key (name :delta) baseline candidate runtime-ns speedup
                               register-count occupancy memory-traffic numerics
                               metadata)
  "Construct a delta report without retaining caller-owned metadata."
  (%make-delta-report
   :name name
   :baseline baseline
   :candidate candidate
   :runtime-ns runtime-ns
   :speedup speedup
   :register-count register-count
   :occupancy occupancy
   :memory-traffic memory-traffic
   :numerics numerics
   :metadata (rosette-metadata-core:copy-plist
              metadata
              :label "Delta report METADATA")))

(defun delta-report-complete-p (report)
  "Return true when REPORT carries every mandatory delta field.

This is a property of the report itself, so it lives beside the struct.
`rosette-adapter-protocol:adapter-delta-report-complete-p' delegates here so the
two cannot drift.  Completeness is the ONLY content property this library
decides: whether a measured delta is *acceptable* depends on a tolerance that
is not in the report (the `numerics' plist has no closed vocabulary across
producers -- :max-error, :max-abs-error, :max-abs-diff, :ok all occur), so
`publish-delta-report' takes that judgement as an explicit ACCEPT predicate
rather than inventing a threshold."
  (and (delta-report-p report)
       (not (null (delta-report-runtime-ns report)))
       (not (null (delta-report-speedup report)))
       (not (null (delta-report-register-count report)))
       (not (null (delta-report-occupancy report)))
       (not (null (delta-report-memory-traffic report)))
       (not (null (delta-report-numerics report)))
       t))

(defun delta-report->plist (report)
  "Return REPORT as a stable machine-readable plist."
  (check-type report delta-report)
  (list :name (delta-report-name report)
        :baseline (delta-report-baseline report)
        :candidate (delta-report-candidate report)
        :runtime-ns (delta-report-runtime-ns report)
        :speedup (delta-report-speedup report)
        :register-count (delta-report-register-count report)
        :occupancy (delta-report-occupancy report)
        :memory-traffic (delta-report-memory-traffic report)
        :numerics (delta-report-numerics report)
        :metadata (copy-list (delta-report-metadata report))))
