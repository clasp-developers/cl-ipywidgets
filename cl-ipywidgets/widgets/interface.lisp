(in-package #:cl-ipywidgets)


(defparameter *kernel-start-callbacks* nil
  "Push into this list thunks that get called when the kernel starts")

(defun kernel-start-hook (kernel)
  (cl-jupyter:logg 2 "In kernel-start-hook kernel -> ~a~%" kernel)
  (let ((comm-manager (cl-ipykernel:make-comm-manager kernel)))
    (cl-jupyter:logg 2 "Just did make-comm-manager~%")
    (setf (gethash kernel cl-ipykernel:*kernel-comm-managers*) comm-manager)
    (cl-jupyter:logg 2 "register-target for kernel: ~a~%" kernel)
    (cl-ipykernel:register-target comm-manager "jupyter.widget" #'handle-comm-open)
    (loop for start-callback in *kernel-start-callbacks*
          do (funcall start-callback))))

(defun kernel-shutdown-hook (kernel)
  (cl-jupyter:logg 2 "In kernel-shutdown-hook kernel -> ~a~%" kernel)
  (let ((comm-manager (gethash kernel cl-ipykernel:*kernel-comm-managers*)))
    (if comm-manager
        (remhash kernel cl-ipykernel:*kernel-comm-managers*)
        (warn "The kernel ~a was shutdown but no comm-manager could be found for it" kernel))))

(defun handle-comm-open (shell identities msg)
  (cl-jupyter:logg 2 "[Shell] handling 'comm_open' - parsing message~%")
  (cl-jupyter:send-status-update (cl-jupyter:kernel-iopub (cl-jupyter:kernel shell)) msg "busy" :key (cl-jupyter:kernel-key shell))'
  (cl-jupyter:logg 2 "[Shell] done sending busy~%")
  (unwind-protect
       (progn
         (cl-jupyter:logg 2 "[Shell] Parsing message~%")
         (cl-jupyter:logg 2 "  ==> msg = ~W~%" msg)
         (let* ((kernel (cl-jupyter:kernel shell))
                (manager (gethash kernel cl-ipykernel:*kernel-comm-managers*)))
           ;; Should I pass identities for ident??????
           ;; I have no idea what the stream is
           (cl-ipykernel:comm-open manager :I-dont-know-what-to-pass-for-stream :i-dont-know-what-to-pass-for-ident msg)))
    (cl-jupyter:logg 2 "    Unwound after parse-json-from-string or comm-open~%"))
  ;; status back to idle
  (cl-jupyter:send-status-update (cl-jupyter:kernel-iopub (cl-jupyter:kernel shell)) msg "idle" :key (cl-jupyter:kernel-key shell)))

(defun handle-comm-msg (shell identities msg)
  (cl-jupyter:logg 2 "[Shell/handle-comm-msg] handling 'comm_msg' - parsing message~%")
  (cl-jupyter:send-status-update (cl-jupyter:kernel-iopub (cl-jupyter:kernel shell)) msg "busy" :key (cl-jupyter:kernel-key shell))'
  (cl-jupyter:logg 2 "[Shell/handle-comm-msg] done sending busy~%")
  (unwind-protect
       (progn
         (cl-jupyter:logg 2 "[Shell/handle-comm-msg] Parsing message~%")
         (cl-jupyter:logg 2 "  ==> msg = ~W~%" msg)
         (let* ((kernel (cl-jupyter:kernel shell))
                (manager (gethash kernel cl-ipykernel:*kernel-comm-managers*)))
           ;; Should I pass identities for ident??????
           ;; I have no idea what the stream is
           (cl-ipykernel:comm-msg manager :I-dont-know-what-to-pass-for-stream identities msg)))
    (cl-jupyter:logg 2 "[Shell/handle-comm-msg]    Unwound stack after parse-json-from-string or comm-msg~%"))
  ;; status back to idle
  (cl-jupyter:send-status-update (cl-jupyter:kernel-iopub (cl-jupyter:kernel shell)) msg "idle" :key (cl-jupyter:kernel-key shell)))

(defun handle-comm-close (shell identities msg)
  (cl-jupyter:logg 2 "[Shell] handling 'comm_close' - parsing message~%")
  (cl-jupyter:send-status-update (cl-jupyter:kernel-iopub (cl-jupyter:kernel shell)) msg "busy" :key (cl-jupyter:kernel-key shell))'
  (cl-jupyter:logg 2 "[Shell] done sending busy~%")
  (unwind-protect
       (progn
         (cl-jupyter:logg 2 "[Shell] Parsing message~%")
         (cl-jupyter:logg 2 "  ==> msg = ~W~%" msg)
         (let* ((kernel (cl-jupyter:kernel shell))
                (manager (gethash kernel cl-ipykernel:*kernel-comm-managers*)))
           ;; Should I pass identities for ident??????
           ;; I have no idea what the stream is
           (cl-ipykernel:comm-close manager :I-dont-know-what-to-pass-for-stream :i-dont-know-what-to-pass-for-ident msg)))
    (cl-jupyter:logg 2 "    Unwinding after parse-json-from-string or comm-close~%"))
  ;; status back to idle
  (cl-jupyter:send-status-update (cl-jupyter:kernel-iopub (cl-jupyter:kernel shell)) msg "idle" :key (cl-jupyter:kernel-key shell)))


;;; Carries out the action of load_ipython_extension
;;; It registers a comm-manager for the kernels that have been started already
;;; and calls register-target on them
(eval-when (:execute :load-toplevel)
  (loop for kernel in cl-jupyter:*started-kernels*
        do (kernel-start-hook kernel)))

(defun cl-jupyter-widget-display-hook (widget iopub parent-msg execution-count key)
  (cl-jupyter:logg 2 "Entered cl-jupyter-widget-display-hook ~%")
  (if (and widget (typep widget 'cljw:widget))
      (progn
	(cl-jupyter:logg 1 "cl-jupyter-widget-display-hook: TEST The widget result ~s is about to be displayed~%" widget)
        (ipython-display widget iopub parent-msg execution-count key)
        (cl-jupyter:logg 1 "Displayed widget~%")
        t)
    (progn
      (cl-jupyter:logg 1 "cl-jupyter-widget-display-hook: Did not recognize ~s as a widget - returning to shell~%" widget)
      nil)))

(eval-when (:load-toplevel :execute)
  (setf cl-jupyter:*kernel-start-hook* 'kernel-start-hook)
  (setf cl-jupyter:*kernel-shutdown-hook* 'kernel-shutdown-hook)
  (setf cl-jupyter:*handle-comm-open-hook* 'handle-comm-open)
  (setf cl-jupyter:*handle-comm-msg-hook* 'handle-comm-msg)
  (setf cl-jupyter:*handle-comm-close-hook* 'handle-comm-close)
  (setf cl-jupyter:*cl-jupyter-widget-display-hook* 'cl-jupyter-widget-display-hook)
  (setf cl-jupyter:*sort-encoded-json* nil))



#|
    def send(self, stream, msg_or_type, content=None, parent=None, ident=None,
             buffers=None, track=False, header=None, metadata=None):
        """Build and send a message via stream or socket.

        The message format used by this function internally is as follows:

        [ident1,ident2,...,DELIM,HMAC,p_header,p_parent,p_content,
         buffer1,buffer2,...]

        The serialize/deserialize methods convert the nested message dict into this
        format.

        Parameters
        ----------

        stream : zmq.Socket or ZMQStream
            The socket-like object used to send the data.
        msg_or_type : str or Message/dict
            Normally, msg_or_type will be a msg_type unless a message is being
            sent more than once. If a header is supplied, this can be set to
            None and the msg_type will be pulled from the header.

        content : dict or None
            The content of the message (ignored if msg_or_type is a message).
        header : dict or None
            The header dict for the message (ignored if msg_to_type is a message).
        parent : Message or dict or None
            The parent or parent header describing the parent of this message
            (ignored if msg_or_type is a message).
        ident : bytes or list of bytes
            The zmq.IDENTITY routing path.
        metadata : dict or None
            The metadata describing the message
        buffers : list or None
            The already-serialized buffers to be appended to the message.
        track : bool
            Whether to track.  Only for use with Sockets, because ZMQStream
            objects cannot track messages.


        Returns
        -------
        msg : dict
            The constructed message.
        """
        if not isinstance(stream, zmq.Socket):
            # ZMQStreams and dummy sockets do not support tracking.
            track = False

        if isinstance(msg_or_type, (Message, dict)):
            # We got a Message or message dict, not a msg_type so don't
            # build a new Message.
            msg = msg_or_type
            buffers = buffers or msg.get('buffers', [])
        else:
            msg = self.msg(msg_or_type, content=content, parent=parent,
                           header=header, metadata=metadata)
        if self.check_pid and not os.getpid() == self.pid:
            get_logger().warning("WARNING: attempted to send message from fork\n%s",
                msg
            )
            return
        buffers = [] if buffers is None else buffers
        for buf in buffers:
            if not isinstance(buf, memoryview):
                try:
                    # check to see if buf supports the buffer protocol.
                    memoryview(buf)
                except TypeError:
                    raise TypeError("Buffer objects must support the buffer protocol.")

        if self.adapt_version:
            msg = adapt(msg, self.adapt_version)
        to_send = self.serialize(msg, ident)
        to_send.extend(buffers)
        longest = max([ len(s) for s in to_send ])
        copy = (longest < self.copy_threshold)

        if buffers and track and not copy:
            # only really track when we are doing zero-copy buffers
            tracker = stream.send_multipart(to_send, copy=False, track=True)
        else:
            # use dummy tracker, which will be done immediately
            tracker = DONE
            stream.send_multipart(to_send, copy=copy)

        if self.debug:
            pprint.pprint(msg)
            pprint.pprint(to_send)
            pprint.pprint(buffers)

        msg['tracker'] = tracker

        return msg


|#

(defun send-comm-open (content)
  (let* ((msg (cl-jupyter:make-message cl-jupyter:*parent-msg* "comm_open" nil content))
         (shell cl-jupyter:*shell*))
    #++(let ((json-str (encode-json-to-string content :indent 4)))
      (cl-jupyter:logg 2 "Sending comm_open~%")
      (cl-jupyter:logg 2 "parent-msg -> ~s~%" *parent-msg*)
      (cl-jupyter:logg 2 "content:   ~s~%" content)
      (cl-jupyter:logg 2 "json:  ---> ~%")
      (cl-jupyter:logg 2 "~s~%" json-str))
    (cl-jupyter:message-send
     (cl-jupyter:kernel-iopub (cl-jupyter:kernel shell))
     msg
     :identities '("comm_open")
     :key (cl-jupyter:kernel-key shell))))

(defun send-comm-msg (content)
  (let* ((msg (cl-jupyter:make-message cl-jupyter:*parent-msg* "comm_msg" nil content))
         (shell cl-jupyter:*shell*))
    #++(let ((json-str (encode-json-to-string content :indent 4)))
      (cl-jupyter:logg 2 "Sending comm_msg~%")
      (cl-jupyter:logg 2 "parent-msg -> ~s~%" cl-jupyter:*parent-msg*)
      (cl-jupyter:logg 2 "content:   ~s~%" content)
      (cl-jupyter:logg 2 "json:  ---> ~%")
      (cl-jupyter:logg 2 "~s~%" json-str))
    (cl-jupyter:message-send
     (cl-jupyter:kernel-iopub (cl-jupyter:kernel shell))
     msg
     :identities '("comm_msg")
     :key (cl-jupyter:kernel-key shell))))
