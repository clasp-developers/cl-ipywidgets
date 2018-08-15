(in-package :cl-ipywidgets)

(defclass-widget-register output (domwidget)
  ((msg-id :initarg :msg-id :accessor msg-id
	   :type unicode
	   :initform (unicode "")
	   :metadata (:sync t
                      :json-name "msg_id"
                      :help "Parent message id of messages to capture."))
   (outputs :initarg :outputs :accessor outputs
	    :type tuple
	    :initform nil
	    :metadata (:sync t
                       :json-name "outputs"
                       :help "The output messages synced from the frontend.")))
  (:default-initargs
   :view-name (unicode "OutputView")
   :model-name (unicode "OutputModel")
   :view-module (unicode "@jupyter-widgets/output")
   :model-module (unicode "@jupyter-widgets/output")
   :view-module-version (unicode *jupyter-widgets-output-version*)
   :model-module-version (unicode *jupyter-widgets-output-version*)
   )
  (:metaclass traitlets:traitlet-class)
  (:documentation
   "Widget used as a context manager to display output.

    This widget can capture and display stdout, stderr, and rich output.  To use
    it, create an instance of it and display it.

    You can then use the widget as a context manager: any output produced while in the
    context will be captured and displayed in the widget instead of the standard output
    area.

    You can also use the .capture() method to decorate a function or a method. Any output 
    produced by the function will then go to the output widget. This is useful for
    debugging widget callbacks, for example.

    Example::
        import ipywidgets as widgets
        from IPython.display import display
        out = widgets.Output()
        display(out)

        print('prints to output area')

        with out:
            print('prints to output widget')

        @out.capture()
        def func():
            print('prints to output widget')
    "   ))


#|| TODO

    # PY3: Force passing clear_output and clear_kwargs as kwargs
    def capture(self, clear_output=False, *clear_args, **clear_kwargs):
        """
        Decorator to capture the stdout and stderr of a function.

        Parameters
        ----------

        clear_output: bool
            If True, clear the content of the output widget at every
            new function call. Default: False

        wait: bool
            If True, wait to clear the output until new output is
            available to replace it. This is only used if clear_output
            is also True.
            Default: False
        """
        def capture_decorator(func):
            @wraps(func)
            def inner(*args, **kwargs):
                if clear_output:
                    self.clear_output(*clear_args, **clear_kwargs)
                with self:
                    return func(*args, **kwargs)
            return inner
        return capture_decorator


||#
#+(or)
(defmethod %enter ((self output))
  (%flush self)
  (let ((ip (get-ipython)));FIXME: Does this function exist somewhere?
    (when (and (slot-exists-p ip 'kernel) (slot-exists-p (kernel ip) 'parent-header))
      (setf (msg-id self) ))))

#+or
(defmethod %exit ((self output) etype evalue tb)
  (let ((ip (get-ipython)))
    (%flush self)
    (setf (msg-id self) "")
    ip))


(defmethod %flush ((self output))
  (finish-output)
  (values))

(defmethod %append-stream-output ((self output) text &key stream-name)
  "Append a stream output."
  (setf (outputs self) (append (outputs self)
                               (list (list (cons "output_type" "stream")
                                           (cons "name" stream-name)
                                           (cons "text" text))))))

(defmethod append-stdout ((self output) text)
  "append text to the stdout stream"
  (%append-stream-output self text :stream-name "stdout"))

(defmethod append-stderr ((self output) text)
  "append text to the stderr stream"
  (%append-stream-output self text :stream-name "stderr"))

(defmethod append-display-data ((self output) display-object)
  (error "Finish implementing me"))






(defmacro with-output (widget &body body)
  (let ((swidget (gensym "WIDGET")))
    `(let ((,swidget ,widget))
       (%enter ,swidget)
       (handler-case
	   (progn ,@body (%exit ,swidget))
	 (condition (c)
	   (%on-condition ,swidget c))))))


