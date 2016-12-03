;; Enable lexical-binding by default, so can inter-process pass arg  -*- lexical-binding: t; -*-


;; js2-format for javascript
(defvar js2-format-proc-name "JS2FORMAT")
(defvar js2-format-proc-port nil)
(defvar js2-format-command
  (let* ((script-file (or load-file-name
                          (and (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))
         (bin-file (expand-file-name "./formatter.js" (file-name-directory (file-truename script-file))))
         (standard-itself (list (if (file-exists-p bin-file) bin-file (list "standard")))))
    (if (eq system-type 'windows-nt000) standard-itself (cons "node" standard-itself)))
  "The command to be run to start the js2-format server. Should be a
list of strings, giving the binary name and arguments.")

(defun js2-format-mark-statement (&optional arg)
  (interactive "P")
  (let* ((cur (point))
         (count (and (car arg)  (log (car arg) 4) ))
         (back (js2-backward-sws))
         (last (when (looking-at-p "[\t \n\r]") (forward-char -1)))
         (region-beg (if (region-active-p) (region-beginning) (point-max)))
         (region-end (if (region-active-p) (goto-char (region-end)) (point-min)))
         (cur-node (js2-node-at-point))
         (parent cur-node)
         beg end)
    ;; blacklist: 33=prop-get, 39=name, 128=scope, 108=arrow function
    (while (and parent (or
                        (memq (js2-node-type parent) '(33 39))
                        (and (= (js2-node-type parent) 108) (eq (js2-function-node-form parent) 'FUNCTION_ARROW))
                        (<= region-beg (js2-node-abs-pos parent))))
      (setq parent (js2-node-parent-stmt parent)))
    (setq beg (and parent (js2-node-abs-pos parent)))
    (setq end (and parent (+ beg (js2-node-len parent))))
    (when (and beg end (/= (- end beg) (- (point-max) (point-min))))
      (transient-mark-mode '(4))
      (goto-char beg)
      (set-mark-command nil)
      (goto-char end))))

(defun js2-format-buffer (&optional arg)
  (interactive "P")
  (let ((cur (point)) start)
    (goto-char (point-min))
    ;; skip # line for cli.js
    (while (and (not (eobp)) (looking-at-p "\\s *\#")) (next-line 1))
    (skip-chars-forward "\r\n[:blank:]")
    (setq start (point))
    (goto-char cur)
    (save-excursion
      (let* ((col (current-column))
             (line (line-number-at-pos)))
        (js2-format-region start (point-max) nil `(,line ,col) t)))))

(defun js2-format-before-cursor (&optional arg)
  (interactive "P")
  (save-excursion
    (let* ((col (current-column))
           (line (line-number-at-pos)))
      (when (not (region-active-p))
        (skip-chars-backward "\t \n\r")
        (forward-char -1)
        (js2-format-mark-statement arg))
      (js2-format-region (region-beginning) (region-end) nil `(,line ,col)))))

(defun js2-format-region (start end &optional not-jump-p pos-list reset-after)
  (interactive (progn
                 (when (not (region-active-p))
                   (js2-format-mark-statement))
                 (list (region-beginning) (region-end) current-prefix-arg nil)))
  (save-excursion
    (let ((kill-ring nil)
          (cur-buffer (buffer-name))
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
      (setf get-formatted
            (lambda (formatted)
              (setq success (not (string-prefix-p errorsign formatted) ))
              (switch-to-buffer cur-buffer)
              (if (string= "" formatted)
                  (message "js2-format return nil")
                (if (not success)
                    (progn (deactivate-mark)
                           (string-match "\"index\":\\([0-9]+\\)" formatted)
                           (setq error-pos (+ start (string-to-number (or (match-string 1 formatted) "")) ))
                           (unless not-jump-p  (goto-char error-pos))
                           (message "js2-format error: %s" (car (split-string formatted errorsign t)) ) )
                  (delete-region start end)
                  (when (string-prefix-p ";" formatted) (setq formatted (substring formatted 1)))
                  (insert formatted)
                  (delete-char -1)  ;; js2-format will add new line, don't need it
                  (js2-indent-region start (point))
                  ;; try to restore previous position
                  (when pos-list
                    (goto-line (car pos-list))
                    (move-to-column (car (cdr pos-list)) nil)
                    )
                  ;; js2-mode-reset after format
                  (when reset-after
                    (js2-mode-reset))))))
      (js2-format-run result get-formatted))))

(defun js2-format-result (errorlist)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  ;; (decode-coding-string "\\225\\357" 'utf-8) convert  unibyte string to Chinese!!!
  ;; (message "-------%s -%s -%s" (buffer-name) errorlist (prog1 (buffer-string) ))
  (when (not errorlist)
    (let ((result (prog2 (search-forward "\n\n" nil t) (buffer-substring (point) (point-max)) (kill-buffer))))
      ;; (message "%s" (decode-coding-string result 'utf-8)))
      (setf result (decode-coding-string result (symbol-value 'buffer-file-coding-system))))))



(defun js2-format-run (data done)
  (let* (;; (text "if(a==b)'这是测试sdj要新的';")
         ;; (data (list (cons "text" text) ))
         (host "http://localhost")
         (method "POST-BASE64")
         server callback runner local-done)
    (setf local-done (lambda(s)
                       (let ((result (prog2 (search-forward "\n\n" nil t) (buffer-substring (point) (point-max)) (kill-buffer))))
                         (setf result (decode-coding-string result (symbol-value 'buffer-file-coding-system)))
                         (funcall done result))))
    (setf callback (lambda ()
                     (funcall runner)))
    (setf runner (lambda()
                   ;; (my-url-http 'js2-format-result server method `,data) ; using backquote to quote the value of data
                   (setq server (concat host ":" (number-to-string (or js2-format-proc-port 8000))))
                   ;; using backquote to quote the value of data
                   (my-url-http local-done server method `,data)))
    (if (or (get-process js2-format-proc-name) js2-format-proc-port)
        (funcall runner)
      (js2-format-start-server callback))))

(defun js2-format-start-server (cb-success)
  (let* ((cmd (append js2-format-command))
         (proc (apply #'start-process js2-format-proc-name nil cmd))
         (all-output ""))
    (message "%s" cmd)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc (lambda (_proc _event)
                                 (delete-process proc)
                                 (setf js2-format-proc-port nil)
                                 (message "js2-format: %s" (concat "Could not start node server\n" all-output))))
    (set-process-filter proc
                        (lambda (proc output)
                          (if (and (not (string-match "Listening on port \\([0-9][0-9]*\\)" output))
                                   (not (string-match "EADDRINUSE .*:\\([0-9][0-9]*\\)" output))
                                   )
                              (setf all-output (concat all-output output))
                            (setf js2-format-proc-port (string-to-number (match-string 1 output)))
                            (set-process-sentinel proc (lambda (proc _event)
                                                         (delete-process proc)
                                                         (setf js2-format-proc-port nil)
                                                         (message "js2-format server exit %s" _event)))
                            (set-process-filter proc nil)
                            (funcall cb-success))))))




(provide 'js2-format)
