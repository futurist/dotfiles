;; Enable lexical-binding by default, so can inter-process pass arg  -*- lexical-binding: t; -*-

;; Disable below line in purcell's init.el

;; default fg color: #E5E5DE
;; (require 'init-themes)
;; (require 'init-flycheck)
;; (require 'init-sessions)
;; (require 'init-paredit)
;; (require 'init-lisp)

(defconst *is-a-windows* (eq system-type 'windows-nt))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq tramp-auto-save-directory "~/tramp-autosave")
;; (setq tramp-chunksize "500")
;; (setq tramp-default-method "plink")


(setq debug-on-error t)

(after-load "expand-region"
  (setq expand-region-fast-keys-enabled nil)
  )

(require-package 'use-package)

(require-package 'monokai-theme)

;; http://stackoverflow.com/questions/4462393/how-do-i-configure-emacs-for-editing-html-files-that-contain-javascript
(require-package 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js2-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(require-package 'yasnippet)
(yas-global-mode 1)

(require-package 'smartparens)
(require 'smartparens-config)
(add-hook 'js-mode #'smartparens-mode)
(add-hook 'js2-mode #'smartparens-mode)
(sp-use-smartparens-bindings)


(require-package 'dash)
(use-package js2-refactor
  :defer t
  :diminish js2-refactor-mode
  :commands js2-refactor-mode
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))


;; ternjs for eamcs
(add-to-list 'load-path "~/.emacs.d/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; add tern-auto-complete
(eval-after-load 'tern
  '(progn
     (require-package 'tern-auto-complete)
     (tern-ac-setup)))


;; when it's windows, setting below
(setq default-font-family "Consolas")
(setq default-font-size 143)

;; when it's mac, setting below
(when *is-a-mac*
  (setq default-font-family "Source Code Pro")
  (setq default-font-size 183)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default line-spacing 15)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy t)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("ff02e8e37c9cfd192d6a0cb29054777f5254c17b1bf42023ba52b65e4307b76a" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(display-buffer-reuse-frames t)
 '(safe-local-variable-values
   (quote
    ((no-byte-compile t)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (:family ,default-font-family :foundry "outline" :slant normal :weight normal :height ,default-font-size :width normal)))))

(load-theme 'monokai t)
;; (tool-bar-mode nil)

;; save buffer when outof focus

(defun save-current-file ()
  (if (and (buffer-file-name) (file-exists-p (buffer-file-name)))
      (save-buffer)
    )
  )
;; (add-hook 'focus-out-hook 'save-current-file)

(defun do-lines-in-region (fun &optional arg start end)
  "Invoke function FUN on the text of each line from START to END."
  (interactive)
  (if (use-region-p)
      (setq start (region-beginning)
            end (region-end)
            )
    )
  (deactivate-mark)
  (save-excursion
    (goto-char end)
    (re-search-forward "\n" nil t)
    (setq end (point))
    (goto-char start)
    (while (and (< (point) end)
                (progn (unless (current-line-empty-p) (funcall fun arg))
                       (re-search-forward "\n" end t))
                )
      )
    ))

(defun count-lines-in-region (&optional skip-empty)
  (when (use-region-p)
    (let ((count 0)
          (start (region-beginning))
          (end (region-end))
          )
      (deactivate-mark)
      (save-excursion
        (goto-char end)
        (re-search-forward "\n" nil t)
        (setq end (point))
        (goto-char start)
        (while (and (< (point) end)
                    (progn (incf count)
                           (re-search-forward "\n" end t)))
          )
        )
      (or count 0)
      ))
  )


(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))


(defun remove-add-last-comma(&optional arg)
  (interactive "P")
  (if (use-region-p)
      (funcall 'do-lines-in-region '__remove-add-last-comma arg)
    (__remove-add-last-comma arg)
      )
  )

(defun __remove-add-last-comma(&optional arg)
  (let ((has-comma (eq 59 (get-byte (1- (line-end-position))))));ASCII 59 = ;
    (save-excursion
      (if (and has-comma (eq arg nil))
          (progn
            (end-of-line)
            (delete-backward-char 1)
            )
        )
      (if (and (not has-comma) (consp arg))
              (progn
                (end-of-line)
                (insert ";")
                )
        )
      ))
  )



(defun my-url-http (callback url &optional method args)
  "Send ARGS to URL as a POST request."
  ;; usage: (my-url-http 'callback "http://baidu.com" "POST" '(("post" . "1") ("text" . "just a test")))
  (setq method (or method "GET"))
  (let ((url-request-method (car (split-string method "-")))
        (url-request-extra-headers (cond
                                    ((string= method "POST") '(("Content-Type" . "application/x-www-form-urlencoded")))
                                    ((string= method "POST-PLAIN") '(("Content-Type" . "text/plain")))
                                    ((string= method "POST-JSON") '(("Content-Type" . "application/json")))
                                    ))
        (url-request-data
         (cond
          ((not args) "")
          ((string= method "POST")  (mapconcat (lambda (arg)
                                         (concat (url-hexify-string (car arg))
                                                 "="
                                                 (url-hexify-string (cdr arg))))
                                       args
                                       "&"))
          ((and t) args)
               )))

    (url-retrieve url callback)
    ))


;; standard format for javascript
(defvar standard-format-proc-name "NodeStandard")
(defvar standard-format-proc-port nil)
(defvar standard-format-command
  (let* ((script-file (or load-file-name
                          (and (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))
         (bin-file (expand-file-name "./standard/standard.js" (file-name-directory (file-truename script-file))))
         (tern-itself (list (if (file-exists-p bin-file) bin-file "standard"))))
    (if (eq system-type 'windows-nt000) tern-itself (cons "node" tern-itself)))
  "The command to be run to start the Tern server. Should be a
list of strings, giving the binary name and arguments.")


(defun js2-mark-parent-statement2 ()
  (interactive)
  (let* ((cur (point))
         (back (js2-backward-sws))
         (last (when (looking-at-p "[\t \n\r]") (forward-char -1)))
         (parent-statement (if t
                               (js2-node-parent-stmt (js2-node-at-point))
                             ;; (forward-char 1)
                             (js2-node-at-point)))
         (beg (when parent-statement (js2-node-abs-pos parent-statement)))
         (end (when parent-statement (+ beg (js2-node-len parent-statement)))))
    (when (and beg end)
      (transient-mark-mode '(4))
      (goto-char beg)
      (set-mark-command nil)
      (goto-char end))
    ))

(defun standard-format-before-cursor()
  (interactive)
  (let* ((col (current-column))
             (line (line-number-at-pos))
             )
        (skip-chars-backward "\t \n\r")
        (forward-char -1)
        (js2-mark-parent-statement2)
        (standard-format-region (region-beginning) (region-end) nil `(,line ,col))
        )
  (run-at-time
   "0 sec" nil
   '(lambda()
      ))
  )

(defun standard-format-region(start end not-jump-p pos-list)
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end) current-prefix-arg nil)
                 (list (+ (line-beginning-position) (current-indentation)) (line-end-position) current-prefix-arg nil)
                 ))
  (let ((cur-buffer (buffer-name))
        (errorsign "#!!#")
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
                       (unless not-jump-p (goto-char (+ start (string-to-number (or (match-string 1 formatted) "")) )))
                       (message "standard-format error: %s" (car (split-string formatted errorsign t)) ) )
              (delete-region start end)
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
    (if (and (get-process standard-format-proc-name))
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
                                        ;; (not (string-match "EADDRINUSE 0.0.0.0:\\([0-9][0-9]*\\)" output))
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



(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (next-line 1)
        ;; (forward-char pos)
        ))))



(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )


(defun my-delete-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (progn (delete-region (car bounds) (cdr bounds)) t)
      )))


(defun er/delete-char-or-word(&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (consp arg)
        (let* ((int (car arg)) (count (or (log int 4) 1)))
          (progn
           (if (> count 0) (if (my-delete-thing-at-point 'word) (decf count)) )
           (if (> count 0) (if (my-delete-thing-at-point 'symbol) (decf count)) )
           (if (> count 0) (progn (sp-kill-sexp (round count)) (message "%s" count)))
           )
          ;; (er/expand-region (or (log count 4) 1))
          ;; (kill-region (region-beginning) (region-end))
          )
      (sp-delete-char (or arg 1))
      ))
  )


(defun delete-backword-or-ws ()
  (interactive)
  (let (
        (start (point))
        (end (save-excursion (skip-chars-backward "\t\n \r") (+ 1 (point))))
        )
    (if (and (thing-at-point 'whitespace) (>= start end))
        (if (> start end)
            (delete-region start end)
          (if (= start end)
              (delete-char -1)
            )
          )
      (sp-backward-kill-word 1)
      (pop kill-ring)
      (setq kill-ring-yank-pointer kill-ring)
      )
    )
  )

(defun ac-trigger-isearch ()
  "Trigger ac isearch using -"
  (interactive)
  (if (ac-menu-live-p)
      (ac-isearch)
    (insert "-")
    ))

(defun ac-move-next-item ()
  (interactive)
  (if (ac-menu-live-p)
      (ac-next)
    (next-line 1)
    )
  )
(defun ac-move-prev-item ()
  (interactive)
  (if (ac-menu-live-p)
      (ac-previous)
    (next-line -1)
    )
  )


(defun mark-current-sentence ()
  "Mark current sentence where point is"
  (interactive)
  (forward-sentence)
  (deactivate-mark)
  (set-mark-command nil)
  (backward-sentence)
  )



(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col nil) (start nil) (end nil))
    (setq col (current-column))
    (beginning-of-line) (setq start (point))
    (end-of-line) (forward-char) (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (next-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (next-line -1)
      (forward-char col))
    )
  )

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun kill-paragraph-or-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-paragraph 1)
    )
  )

(defun kill-line-or-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (sp-kill-hybrid-sexp 1)
    )
  )

(defun highlight-next-line(arg)
  "highlight next line"
  (interactive "^P")
  (unless (region-active-p)
    (set-mark-command nil)
    (transient-mark-mode '(4))
    )
  (next-line 1)
  )
(defun highlight-prev-line(arg)
  "highlight prev line"
  (interactive "^P")
  (unless (region-active-p)
    (set-mark-command nil)
    (transient-mark-mode '(4))
    )
  (next-line -1)
  )

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position))))
      )
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))
;; optional key binding


(defun move-parent-forward (arg)
  (interactive "p")
  (setq arg (or arg 1))
  (save-excursion
    (sp-end-of-sexp)
    (forward-char 1)
    (if (eq arg 1)(kill-sexp 1)
      (if (eq arg 2) (let ((char (read-char-exclusive "input a char to zap:")))
                       (if (eq char 13)
                           (sp-kill-hybrid-sexp 1)
                         (zap-up-to-char 1 char))
                       )
        (if (eq arg 3) (sp-kill-hybrid-sexp 1))
        )
      )
    (backward-char 1)
    (yank)
    (pop kill-ring)
    (setq kill-ring-yank-pointer kill-ring)
    ))

(defun move-parent-backward (arg)
  (interactive "p")
  (setq arg (or arg 1))
  (save-excursion
    (when (eq arg 1)
      (sp-end-of-sexp)
      (backward-kill-sexp 1)
      )
    (when (eq arg 2) (sp-kill-hybrid-sexp 1))
    (when (eq arg 3) (sp-kill-hybrid-sexp 1))
    (forward-char 1)
    (yank)
    (pop kill-ring)
    (setq kill-ring-yank-pointer kill-ring)
    )
  )

(defun move-parent-forward-symbol ()(interactive)
       (move-parent-forward 1))
(defun move-parent-forward-up-to-char ()(interactive)
       (move-parent-forward 2))
(defun move-parent-backwrad-symbol()(interactive)
       (move-parent-backward 2))


;; -- define a new command to join multiple lines together --
(defun join-lines () (interactive)
       (next-line)
       (join-line)
       (delete-horizontal-space)
       (insert " ")
       )

(defun select-current-pair()
  (interactive)
  (sp-up-sexp)
  (deactivate-mark)
  (set-mark-command nil)
  (transient-mark-mode '(4))
  (sp-backward-sexp)
  )

(defun sp-backward-delete-all (&optional arg)(interactive)
       (let ((old (point)))
         (sp-beginning-of-sexp arg)
         (if (eq old (point))
             (progn
               ;; (move-beginning-of-line nil)
               (back-to-indentation)
               (delete-region old (point)))
           (delete-region (point) old))
         )
       )


(defun search-selection (&optional arg)
  "search for selected text"
  (interactive "P")
  (when (and (thing-at-point 'symbol) (eq arg nil) (not (region-active-p)))
    ;; (sp-select-next-thing-exchange)
    )
  (if (region-active-p)
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end)))
            )
        (deactivate-mark)
        (setq isearch-string selection)
        (isearch-mode t nil nil nil)
        (isearch-yank-string selection)
        )
    (isearch-forward)
    )
  )

(defun newline-before(&optional arg)
  (interactive)
  (move-beginning-of-line nil)
  (sanityinc/open-line-with-reindent 1)
  )

(defun goto-first-reference ()
  (interactive)
  (set-mark-command nil)
  (deactivate-mark)
  (eval
   `(progn
      (goto-char (point-min))
      (search-forward-regexp
       (rx symbol-start ,(thing-at-point 'symbol) symbol-end))
      (beginning-of-thing 'symbol))))

(defun my-backward-sexp()
  (interactive)
  (let ((old (point))) (sp-beginning-of-sexp nil)
       (if (eq old (point)) (sp-backward-up-sexp nil))
       )
  )

(require-package 'youdao-dictionary)
;; FIX Problem for youdao-dict
;; https://github.com/xuchunyang/youdao-dictionary.el/issues/1#issuecomment-71359418
(defun url-cookie-expired-p (cookie)
  "Return non-nil if COOKIE is expired."
  (let ((exp (url-cookie-expires cookie)))
    (and (> (length exp) 0)
         (condition-case ()
             (> (float-time) (float-time (date-to-time exp)))
           (error nil)))))
;; Enable Cache
(setq url-automatic-caching t)


;; setting for auto-complete
;; press - to trigger isearch
(setq ac-show-menu-immediately-on-auto-complete t)
(global-set-key (kbd "-") 'ac-trigger-isearch)


;; multiple-cursors keybinding
(global-set-key (kbd "C-0") 'mc/mark-next-like-this)
(global-set-key (kbd "C-9") 'mc/skip-to-next-like-this)


;; custom functions
(add-hook 'js2-mode-hook
          (lambda()
            (define-key js2-mode-map (kbd "C-M-h") 'js2-mark-defun)
            (define-key js2-mode-map (kbd "C-M-]") 'js2-mark-parent-statement2)
            (define-key js2-mode-map (kbd "C-x C-;") 'remove-add-last-comma)
            (define-key js2-mode-map (kbd "C-'") 'standard-format-before-cursor)
            ))
(define-key global-map (kbd "C-x ^") 'maximize-window)
(define-key global-map (kbd "C-x j") 'standard-format-region)
(define-key global-map (kbd "C-M-]") 'select-current-pair)
(global-set-key (kbd "C-c C-k") 'copy-line)
(global-set-key (kbd "C-x C-k") 'whole-line-or-region-kill-region)
;; (global-set-key (kbd "C-S-k") 'whole-line-or-region-kill-region)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
(define-key global-map (kbd "M-.") 'goto-first-reference)
(define-key global-map (kbd "C-s") 'search-selection)
(global-set-key (kbd "C-M-j") 'delete-indentation)
(global-set-key (kbd "C-S-j") 'join-lines)
;; (global-set-key (kbd "C-j") 'join-lines)
(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-<backspace>") 'delete-backword-or-ws)
(global-set-key (kbd "C-S-<backspace>") 'sp-backward-delete-all)
(global-set-key (kbd "C-c C-;") 'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-S-l") 'mark-paragraph)
;; move lines
(global-set-key (kbd "C-x C-n") 'move-line-down)
(global-set-key (kbd "C-x C-p") 'move-line-up)
(global-set-key (kbd "C-S-n") 'highlight-next-line)
(global-set-key (kbd "C-S-p") 'highlight-prev-line)
(global-set-key (kbd "C-n") 'ac-move-next-item)
(global-set-key (kbd "C-p") 'ac-move-prev-item)


;; some short keys for default
;; (define-key global-map "\C-x\C-u" 'undo)
(global-set-key (kbd "C-S-r") 'anzu-query-replace-at-cursor-thing)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
;; (global-set-key (kbd "C-c C-c") 'whole-line-or-region-kill-ring-save)
;; (global-set-key (kbd "C-c C-x") 'whole-line-or-region-kill-region)
;; (global-set-key (kbd "C-c C-v") 'whole-line-or-region-yank)
;; (global-set-key (kbd "C-S-h") 'backward-kill-sentence)
;; restore 'kill-sentence and bind 'paredit-kill to C-k
;; (after-load 'paredit
;;   (define-key paredit-everywhere-mode-map [remap kill-sentence] nil)
;;   (define-key paredit-mode-map [remap kill-sentence] nil)
;;   )
;; (global-set-key (kbd "C-S-k") 'kill-sentence)
(global-set-key (kbd "C-k") 'kill-line-or-region)
(global-set-key (kbd "M-k") 'kill-paragraph-or-region)


;; smartparents keybinding
(global-set-key (kbd "C-d") 'er/delete-char-or-word)
(global-set-key (kbd "M-]") 'sp-forward-sexp)
(global-set-key (kbd "M-[") 'sp-backward-sexp)
(global-set-key (kbd "M-s") 'sp-unwrap-sexp)
(global-set-key (kbd "C-{") 'my-backward-sexp)
(global-set-key (kbd "C-}") 'sp-end-of-sexp)
(global-set-key (kbd "C-M-'") 'sp-rewrap-sexp)
;; (global-set-key (kbd "C-M-<left>") 'sp-forward-slurp-sexp)
;; (global-set-key (kbd "C-M-<right>") 'sp-forward-barf-sexp)
(global-set-key (kbd "M-<right>") 'move-parent-backwrad-symbol)
(global-set-key (kbd "M-<left>") 'move-parent-forward-symbol)
(global-set-key (kbd "C-M-<left>") 'move-parent-forward-up-to-char)


;; (global-set-key (kbd "M-] ]") 'paredit-wrap-square)
;; (global-set-key (kbd "M-] }") 'paredit-wrap-curly)


(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "<f8>") 'flycheck-mode)

(global-set-key (kbd "<M-return>") 'sanityinc/newline-at-end-of-line)
(global-set-key (kbd "<C-M-return>") 'newline-before)

;; (setq-default custom-enabled-themes '(sanityinc-solarized-dark))

(when *is-a-mac*
  ;; bash complete not run on windows
  (require-package 'bash-completion)
  (bash-completion-setup)

  (setq initial-frame-alist '((top . 0) (left . 280) (width . 120) (height . 42)))
  (setq default-frame-alist '((top . 0) (left . 280) (width . 120) (height . 42)))
  )

(when *is-a-windows*
  ;; (defcustom gnutls-trustfiles "./cacert.pem"
  ;;   "gnutls-trustfiles location of cacert.pem."
  ;;   :type '(string)
  ;;   :group 'tools
  ;;   )
  ;; (customize-option 'gnutls-trustfiles)

  (setq tramp-default-method "plink")

  (setq w32-lwindow-modifier 'meta)
  (setq w32-rwindow-modifier 'meta)


  (set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp"))

  (defun e-maximize ()
    "Maximize emacs window in windows os"
    (interactive)
    (w32-send-sys-command 61488))        ; WM_SYSCOMMAND #xf030 maximize
  (defun e-minimize ()
    "Minimize emacs window in windows os"
    (interactive)
    (w32-send-sys-command #xf020))    ; #xf020 minimize
  (defun e-normal ()
    "Normal emacs window in windows os"
    (interactive)
    (w32-send-sys-command #xf120))    ; #xf120 normalimize

  (e-maximize)
  ;; (global-set-key (kbd "M-SPC M-x") 'emacs-maximize)

  )
