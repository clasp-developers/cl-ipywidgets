
(in-package #:cl-ipywidgets)
(defun extract-message-content (msg)
  (myjson:parse-json-from-string (cl-jupyter:message-content msg)))



(defgeneric cl-ipykernel:on-msg (target callback &key &allow-other-keys))


;;; ------------------------------------------------------------
;;;
;;; For debugging in slime
;;;
;;; In the jupyter notebook use (cl-ipywidgets::save-context)
;;;      That will save the context of that message.
;;; Then in slime evaluate (in-context (form))

(defparameter *debug-parent-msg* nil)
(defparameter *debug-shell* nil)
(defparameter *debug-kernel* nil)
(defparameter *debug-env* nil)

(defun save-jupyter-cell-state ()
  (setf *debug-env* (mapcar #'symbol-value cl-jupyter:*special-variables*)))

(defmacro in-jupyter-cell (form)
  `(progv ',cl-jupyter:*special-variables* ',*debug-env*
     ,form))
