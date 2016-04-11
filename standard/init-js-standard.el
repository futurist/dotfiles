;; Enable lexical-binding by default, so can inter-process pass arg  -*- lexical-binding: t; -*-


;; standard format for javascript
(defvar standard-format-proc-name "NodeStandard")
(defvar standard-format-proc-port nil)
(defvar standard-format-command
  (let* ((script-file (or load-file-name
                          (and (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))
         (bin-file (expand-file-name "../standard/standard.js" (file-name-directory (file-truename script-file))))
         (standard-itself (list (if (file-exists-p bin-file) bin-file "standard"))))
    (if (eq system-type 'windows-nt000) standard-itself (cons "node" standard-itself)))
  "The command to be run to start the Standard server. Should be a
list of strings, giving the binary name and arguments.")


(defun js2-mark-parent-statement2 (&optional arg)
  (interactive "P")
  (let* ((cur (point))
         (count (and (car arg)  (log (car arg) 4) ))
         (back (js2-backward-sws))
         (last (when (looking-at-p "[\t \n\r]") (forward-char -1)))
         (cur-node (js2-node-at-point))
         (parent-statement (if (null count)
                               (if (region-active-p) (js2-node-parent-stmt cur-node) cur-node)
                             (while (and (> count 0)  (setq cur-node (js2-node-parent-stmt cur-node)) )
                               (decf count))
                             cur-node
                             ))
         (beg (and parent-statement (js2-node-abs-pos parent-statement)))
         (end (and parent-statement (+ beg (js2-node-len parent-statement)))))
    (when (and beg end (/= (- end beg) (- (point-max) (point-min))))
      (transient-mark-mode '(4))
      (goto-char beg)
      (set-mark-command nil)
      (goto-char end))
    ))

(defun standard-format-before-cursor(&optional arg)
  (interactive "P")
  (let* ((col (current-column))
         (line (line-number-at-pos)))
    (when (not (region-active-p))
      (skip-chars-backward "\t \n\r")
      (forward-char -1)
      (js2-mark-parent-statement2 arg))
    (standard-format-region (region-beginning) (region-end) nil `(,line ,col))
    )
  )

(defun standard-format-region(start end not-jump-p pos-list)
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end) current-prefix-arg nil)
                 (list (+ (line-beginning-position) (current-indentation)) (line-end-position) current-prefix-arg nil)
                 ))
  (let ((cur-buffer (buffer-name))
        (errorsign "#!!#")
        (error-pos nil)
        success result get-formatted)
    (goto-char start)
    (skip-chars-forward "\t\n \r" end)
    (push-mark)
    (setq start (point))
    (goto-char end)
    (skip-chars-backward "\t\n \r" start)
    (setq end (point))
    (setq result (buffer-substring start end))
    ;; ***** old style: using shell-command to replace region
    ;; (setq result (with-temp-buffer
    ;;                (insert result)
    ;;                (setq success (eq 0 (shell-command-on-region (point-min) (point-max) "standard-format -" " *temp*" t)))
    ;;                (buffer-string))
    ;;       )
    ;; **** old end

    ;; **** new style: using nodejs server to format
    (setf get-formatted
          (lambda (formatted)
            (setq success (not (string-prefix-p errorsign formatted) ))
            (switch-to-buffer cur-buffer)
            (if (not success)
                (progn (deactivate-mark)
                       (string-match "\"index\":\\([0-9]+\\)" formatted)
                       (setq error-pos (+ start (string-to-number (or (match-string 1 formatted) "")) ))
                       (unless not-jump-p  (goto-char error-pos))
                       (message "standard-format error: %s" (car (split-string formatted errorsign t)) ) )
              (delete-region start end)
              (when (string-prefix-p ";" formatted) (setq formatted (substring formatted 1)))
              (insert formatted)
              (delete-char -1)  ;; standard-format will add new line, don't need it
              (js2-indent-region start (point))
              ;; try to restore previous position
              (when pos-list
                (goto-line (car pos-list))
                (move-to-column (car (cdr pos-list)) nil)
              )
            )))
    (standard-format-run result get-formatted)
    )
  )

    (defun standard-format-result (errorlist)
      "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
      ;; (decode-coding-string "\\225\\357" 'utf-8) convert  unibyte string to Chinese!!!
      ;; (message "-------%s -%s -%s" (buffer-name) errorlist (prog1 (buffer-string) (kill-buffer)))
      (when (not errorlist)
        (let ((result (prog2 (search-forward "\n\n" nil t) (buffer-substring (point) (point-max)) (kill-buffer))))
          ;; (message "%s" (decode-coding-string result 'utf-8)))
          (setf result (decode-coding-string result 'utf-8))
          (message "%s" result)
          )
        )
      )



(defun standard-format-run(data done)
  (let* (;; (text "if(a==b)'这是测试sdj要新的';")
         ;; (data (list (cons "text" text) ))
         (server "http://localhost")
         (method "POST-PLAIN")
         callback runner local-done
         )
    (setf local-done (lambda(s)
                       (let ((result (prog2 (search-forward "\n\n" nil t) (buffer-substring (point) (point-max)) (kill-buffer))))
                         (setf result (decode-coding-string result 'utf-8))
                         (funcall done result)
                         )
                       ))
    (setf callback (lambda ()
                     (funcall runner)
                     ))
    (setf runner (lambda()
                   ;; (my-url-http 'standard-format-result server method `,data) ; using backquote to quote the value of data
                   (my-url-http local-done server method `,data) ; using backquote to quote the value of data
                   ))
    (if (or (get-process standard-format-proc-name) standard-format-proc-port)
        (funcall runner)
      (standard-start-server callback)
      ))
  )

(defun standard-start-server (cb-success)
  (let* ((cmd (append standard-format-command))
         (proc (apply #'start-process standard-format-proc-name nil cmd))
         (all-output ""))
    (message "%s" cmd)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc (lambda (_proc _event)
                                 (delete-process proc)
                                 (setf standard-format-proc-port nil)
                                 (message "standard: %s" (concat "Could not start Standard server\n" all-output))
                                 ))
    (set-process-filter proc (lambda (proc output)
                               (if (and (not (string-match "Listening on port \\([0-9][0-9]*\\)" output))
                                        (not (string-match "EADDRINUSE 0.0.0.0:\\([0-9][0-9]*\\)" output))
                                        )
                                   (setf all-output (concat all-output output))
                                 (setf standard-format-proc-port (string-to-number (match-string 1 output)))
                                 (set-process-sentinel proc (lambda (proc _event)
                                                              (delete-process proc)
                                                              (setf standard-format-proc-port nil)
                                                              (message "standard server exit %s" _event)))
                                 (set-process-filter proc nil)
                                 (funcall cb-success)
                                 )))))




(provide 'init-js-standard)
