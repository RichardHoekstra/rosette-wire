;;;; composition-sel4.lisp --- Composition authority lowering to Microkit data.

(in-package #:composition-sel4)

(define-condition composition-sel4-error (error)
  ((code :initarg :code :reader composition-sel4-error-code)
   (detail :initarg :detail :reader composition-sel4-error-detail))
  (:report (lambda (condition stream)
             (format stream "Composition seL4 lowering ~A: ~A"
                     (composition-sel4-error-code condition)
                     (composition-sel4-error-detail condition)))))

(defun %refuse (code control &rest arguments)
  (error 'composition-sel4-error :code code
         :detail (apply #'format nil control arguments)))

(defun %field (name object)
  (cdr (assoc name object :test #'string=)))

(defstruct (sel4-lowering-policy (:constructor %make-sel4-lowering-policy))
  base-endpoint-cap node-levels orchestrator-level service-modes
  max-pds max-channels max-capabilities)

(defun %valid-levels-p (levels)
  (and (listp levels)
       (every (lambda (entry)
                (and (consp entry) (stringp (car entry))
                     (typep (cdr entry) '(integer 0 *))))
              levels)
       (= (length levels)
          (length (remove-duplicates levels :key #'car :test #'string=)))))

(defun %valid-service-modes-p (modes)
  (and (listp modes)
       (every (lambda (entry)
                (and (consp entry) (stringp (car entry))
                     (member (cdr entry) '(:ppcall :async))))
              modes)
       (= (length modes)
          (length (remove-duplicates modes :key #'car :test #'string=)))))

(defun make-sel4-lowering-policy
    (&key (base-endpoint-cap #x1000) node-levels (orchestrator-level 0)
          service-modes (max-pds 128) (max-channels 512)
          (max-capabilities 1024))
  (unless (and (typep base-endpoint-cap '(integer 0 #xffffffffffffffff))
               (%valid-levels-p node-levels)
               (typep orchestrator-level '(integer 0 *))
               (%valid-service-modes-p service-modes)
               (every (lambda (value) (typep value '(integer 1 *)))
                      (list max-pds max-channels max-capabilities)))
    (%refuse :invalid-policy "malformed levels, modes, endpoint base, or bounds"))
  (%make-sel4-lowering-policy
   :base-endpoint-cap base-endpoint-cap :node-levels (copy-tree node-levels)
   :orchestrator-level orchestrator-level
   :service-modes (copy-tree service-modes)
   :max-pds max-pds :max-channels max-channels
   :max-capabilities max-capabilities))

(defstruct protection-domain name wire-node priority level)
(defstruct authority-grant holder capability channel endpoint-slot)
(defstruct microkit-channel id source target mode kind)

(defstruct (sel4-authority-description
            (:constructor %make-sel4-authority-description))
  id composition-id status policy protection-domains authority-grants channels
  deadlock-verdict information-flow-verdict violations system-xml)

(defun sel4-authority-description-safe-p (description)
  (and (sel4-authority-description-p description)
       (eq :safe (sel4-authority-description-status description))))

(defun %policy-form (policy)
  (list :base-endpoint-cap (sel4-lowering-policy-base-endpoint-cap policy)
        :node-levels (copy-tree (sel4-lowering-policy-node-levels policy))
        :orchestrator-level
        (sel4-lowering-policy-orchestrator-level policy)
        :service-modes (copy-tree (sel4-lowering-policy-service-modes policy))
        :max-pds (sel4-lowering-policy-max-pds policy)
        :max-channels (sel4-lowering-policy-max-channels policy)
        :max-capabilities (sel4-lowering-policy-max-capabilities policy)))

(defun %pd-form (pd)
  (list :name (protection-domain-name pd)
        :wire-node (protection-domain-wire-node pd)
        :priority (protection-domain-priority pd)
        :level (protection-domain-level pd)))

(defun %grant-form (grant)
  (list :holder (authority-grant-holder grant)
        :capability (authority-grant-capability grant)
        :channel (authority-grant-channel grant)
        :endpoint-slot (authority-grant-endpoint-slot grant)))

(defun %channel-form (channel)
  (list :id (microkit-channel-id channel)
        :source (microkit-channel-source channel)
        :target (microkit-channel-target channel)
        :mode (microkit-channel-mode channel)
        :kind (microkit-channel-kind channel)))

(defun %verdict-form (verdict)
  (list :deadlock-free (not (null (sv:verdict-deadlock-free verdict)))
        :information-flow-secure
        (not (null (sv:verdict-info-flow-secure verdict)))
        :b1 (sv:verdict-b1 verdict)
        :order (copy-list (sv:verdict-order verdict))
        :violations (copy-tree (sv:verdict-violations verdict))))

(defun %description-content
    (composition-id status policy pds grants channels deadlock flow violations xml)
  (list :schema :rosette-sel4-authority-description/v1
        :composition-id composition-id :status status
        :activation-authority :absent
        :policy (%policy-form policy)
        :protection-domains (mapcar #'%pd-form pds)
        :authority-grants (mapcar #'%grant-form grants)
        :channels (mapcar #'%channel-form channels)
        :deadlock-verdict (%verdict-form deadlock)
        :information-flow-verdict (%verdict-form flow)
        :violations violations :system-xml xml))

(defun sel4-authority-description->form (description)
  (check-type description sel4-authority-description)
  (list :id (sel4-authority-description-id description)
        :content
        (%description-content
         (sel4-authority-description-composition-id description)
         (sel4-authority-description-status description)
         (sel4-authority-description-policy description)
         (sel4-authority-description-protection-domains description)
         (sel4-authority-description-authority-grants description)
         (sel4-authority-description-channels description)
         (sel4-authority-description-deadlock-verdict description)
         (sel4-authority-description-information-flow-verdict description)
         (sel4-authority-description-violations description)
         (sel4-authority-description-system-xml description))))

(defun %node-level (policy node-id)
  (let ((entry (assoc node-id (sel4-lowering-policy-node-levels policy)
                      :test #'string=)))
    (if entry (cdr entry) 0)))

(defun %service-mode (policy service-id)
  (let ((entry (assoc service-id
                      (sel4-lowering-policy-service-modes policy)
                      :test #'string=)))
    (or (cdr entry)
        (%refuse :undeclared-service-mode
                 "service ~A requires explicit :PPCALL or :ASYNC policy"
                 service-id))))

(defun %node-map (nodes policy)
  (loop for node in nodes for index from 0
        for wire-id = (%field "id" node)
        collect
        (cons wire-id
              (make-protection-domain
               :name (intern (format nil "PD~D" index) :keyword)
               :wire-node wire-id :priority 0
               :level (%node-level policy wire-id)))))

(defun %pd (node-id node-map)
  (or (cdr (assoc node-id node-map :test #'string=))
      (%refuse :unknown-node "unknown node ~A" node-id)))

(defun %step-node-map (steps)
  (loop for step in steps
        collect (cons (%field "id" step) (%field "node" step))))

(defun %data-flow-channels (steps node-map)
  (let ((step-nodes (%step-node-map steps)) (channels nil))
    (dolist (target steps)
      (let ((target-node (%field "node" target)))
        (dolist (binding (%field "bindings" target))
          (let* ((source (%field "source" binding))
                 (kind (%field "kind" source)))
            (when (string= kind "step")
              (let* ((source-step (%field "step" source))
                     (source-node (cdr (assoc source-step step-nodes
                                              :test #'string=))))
                (when (and source-node (not (string= source-node target-node)))
                  (push (make-microkit-channel
                         :id (format nil "flow:~A:~A" source-step
                                     (%field "id" target))
                         :source (protection-domain-name
                                  (%pd source-node node-map))
                         :target (protection-domain-name
                                  (%pd target-node node-map))
                         :mode :async :kind :data-flow)
                        channels))))))))
    (remove-duplicates (nreverse channels)
                       :test #'equal :key #'%channel-form)))

(defun %service-channels (services node-map policy)
  (loop for service in services
        for id = (%field "id" service)
        for consumer = (%field "consumer" service)
        for provider = (%field "provider" service)
        for provider-node = (and (listp provider) (%field "node" provider))
        when provider-node
          collect (make-microkit-channel
                   :id (concatenate 'string "service:" id)
                   :source (protection-domain-name
                            (%pd (%field "node" consumer) node-map))
                   :target (protection-domain-name
                            (%pd provider-node node-map))
                   :mode (%service-mode policy id) :kind :service)))

(defun %orchestrator-channels (steps node-map)
  (loop for node-id in (sort (remove-duplicates
                              (mapcar (lambda (step) (%field "node" step)) steps)
                              :test #'string=)
                             #'string<)
        for index from 0
        collect (make-microkit-channel
                 :id (format nil "orchestrator:~D" index)
                 :source :rosette :target (protection-domain-name
                                            (%pd node-id node-map))
                 :mode :ppcall :kind :orchestrator)))

(defun %call-edges (channels)
  (loop for channel in channels
        when (eq :ppcall (microkit-channel-mode channel))
          collect (list (microkit-channel-source channel)
                        (microkit-channel-target channel))))

(defun %flow-edges (channels)
  (mapcar (lambda (channel)
            (list (microkit-channel-source channel)
                  (microkit-channel-target channel)))
          channels))

(defun %assign-priorities (pds channels)
  (let* ((names (mapcar #'protection-domain-name pds))
         (order (sv:progress-potential names (%call-edges channels))))
    (if order
        (loop for name in order for priority from 1
              do (setf (protection-domain-priority
                         (find name pds :key #'protection-domain-name))
                       priority))
        (loop for pd in (sort (copy-list pds) #'string<
                              :key (lambda (item)
                                     (string (protection-domain-name item))))
              for priority from 1
              do (setf (protection-domain-priority pd) priority))))
  pds)

(defun %make-system (pds edges)
  (let ((system (sv:make-system)))
    (dolist (pd pds)
      (sv:add-pd system (protection-domain-name pd)
                 :priority (protection-domain-priority pd)
                 :level (protection-domain-level pd)))
    (dolist (edge edges) (sv:add-ppcall system (first edge) (second edge)))
    system))

(defun %authority-grants (nodes node-map policy)
  (let ((channel 0) (grants nil))
    (dolist (node nodes)
      (let* ((holder (protection-domain-name
                      (%pd (%field "id" node) node-map)))
             (contract (%field "contract" node)))
        (dolist (capability (%field "capabilities" contract))
          (push (make-authority-grant
                 :holder holder :capability capability :channel channel
                 :endpoint-slot
                 (cap:endpoint-capability
                  (sel4-lowering-policy-base-endpoint-cap policy) channel))
                grants)
          (incf channel))))
    (nreverse grants)))

(defun %xml-escape (value)
  (with-output-to-string (stream)
    (loop for character across (string-downcase (string value))
          do (case character
               (#\& (write-string "&amp;" stream))
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (#\" (write-string "&quot;" stream))
               (otherwise (write-char character stream))))))

(defun %emit-system-xml (pds channels)
  (with-output-to-string (stream)
    (write-string "<system>\n" stream)
    (dolist (pd pds)
      (format stream "  <protection_domain name=\"~A\" priority=\"~D\"/>~%"
              (%xml-escape (protection-domain-name pd))
              (protection-domain-priority pd)))
    (dolist (channel channels)
      (format stream "  <channel id=\"~A\"><end pd=\"~A\" id=\"0\"~A/><end pd=\"~A\" id=\"0\"/></channel>~%"
              (%xml-escape (microkit-channel-id channel))
              (%xml-escape (microkit-channel-source channel))
              (if (eq :ppcall (microkit-channel-mode channel))
                  " pp=\"true\"" "")
              (%xml-escape (microkit-channel-target channel))))
    (write-string "</system>\n" stream)))

(defun lower-composition-to-sel4
    (composition &key (policy (make-sel4-lowering-policy)))
  "Lower a Rosette-accepted Composition into a non-activating authority plan."
  (unless (and (rw:composition-p composition)
               (sel4-lowering-policy-p policy))
    (%refuse :invalid-input "expected Composition and lowering policy"))
  (let ((validation (rw:validate-composition composition)))
    (unless (eq :pass (rw:wire-receipt-verdict validation))
      (%refuse :rosette-refusal "Rosette validation refused the Composition")))
  (let* ((value (rw:composition->value composition))
         (nodes (%field "nodes" value)) (steps (%field "steps" value))
         (services (%field "serviceBindings" value))
         (node-map (%node-map nodes policy))
         (orchestrator
           (make-protection-domain
            :name :rosette :wire-node nil :priority 0
            :level (sel4-lowering-policy-orchestrator-level policy)))
         (pds (cons orchestrator (mapcar #'cdr node-map)))
         (channels
           (append (%orchestrator-channels steps node-map)
                   (%service-channels services node-map policy)
                   (%data-flow-channels steps node-map)))
         (grants (%authority-grants nodes node-map policy)))
    (when (> (length pds) (sel4-lowering-policy-max-pds policy))
      (%refuse :pd-budget "~D PDs exceed the policy bound" (length pds)))
    (when (> (length channels) (sel4-lowering-policy-max-channels policy))
      (%refuse :channel-budget "~D channels exceed the policy bound"
               (length channels)))
    (when (> (length grants) (sel4-lowering-policy-max-capabilities policy))
      (%refuse :capability-budget "~D capabilities exceed the policy bound"
               (length grants)))
    (%assign-priorities pds channels)
    (let* ((call-system (%make-system pds (%call-edges channels)))
           (flow-system (%make-system pds (%flow-edges channels)))
           (deadlock (sv:referee call-system))
           (flow (sv:referee flow-system))
           (safe (and (sv:verdict-deadlock-free deadlock)
                      (sv:priority-monotone-p call-system)
                      (sv:verdict-info-flow-secure flow)))
           (violations
             (append
              (mapcar (lambda (item) (cons :deadlock item))
                      (sv:verdict-violations deadlock))
              (mapcar (lambda (item) (cons :information-flow item))
                      (remove-if-not
                       (lambda (item) (eq :info-leak (first item)))
                       (sv:verdict-violations flow)))))
           (xml (%emit-system-xml pds channels))
           (status (if safe :safe :refused))
           (composition-id (rw:composition-id composition))
           (content (%description-content
                     composition-id status policy pds grants channels
                     deadlock flow violations xml)))
      (%make-sel4-authority-description
       :id (cid:content-id-long content) :composition-id composition-id
       :status status :policy policy :protection-domains pds
       :authority-grants grants :channels channels
       :deadlock-verdict deadlock :information-flow-verdict flow
       :violations violations :system-xml xml))))

(defun verify-sel4-authority-description (description composition)
  "Re-lower from the Composition and policy. SAFE and refused descriptions are
both verifiable evidence; only SAFE is eligible for a later human activation."
  (and (sel4-authority-description-p description)
       (rw:composition-p composition)
       (handler-case
           (let ((fresh
                   (lower-composition-to-sel4
                    composition
                    :policy (sel4-authority-description-policy description))))
             (and (string= (sel4-authority-description-id description)
                           (sel4-authority-description-id fresh))
                  (equal (sel4-authority-description->form description)
                         (sel4-authority-description->form fresh))))
         (error () nil))))
