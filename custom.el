;; Disable below line in purcell's init.el

;; default fg color: #E5E5DE
;; (require 'init-themes)
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

(server-start)

;; bookmark with abbrevs mode
;; (require 'bookmark)
;; (setq bookmark-save-flag 1)             ;auto save bookmark when changed
(defun bookmark-to-abbrevs ()
  "Create abbrevs based on `bookmark-alist'.
use C-x a e to expand bookmark"
  ;; (bookmark-maybe-load-default-file)    ;load default bookmark
  (when (boundp 'bookmark-alist)
    (dolist (bookmark bookmark-alist)
    (let* ((name (car bookmark))
           (file (bookmark-get-filename name)))
      (define-abbrev global-abbrev-table name file)))
    )
  )
;; add minibuffer for abbrev
;; (add-hook 'minibuffer-setup-hook 'abbrev-mode)
;; (setq save-abbrevs nil)
(add-hook 'bookmark-load-hook 'bookmark-to-abbrevs)                   ;use C-x a e to expand bookmark


;; linum mode with highlight
(require-package 'hlinum)
(face-spec-set 'linum-highlight-face
               '((t (:inherit default :foreground "#bbbbbb"
                              :background "#333333"))))
(hlinum-activate)
(global-linum-mode t)                   ;(linum-mode) for all buffer


;; remove er +/- overlay
(after-load "expand-region"
  (setq expand-region-fast-keys-enabled nil)
  )

(require-package 'use-package)

(require-package 'monokai-theme)

;; editing html file mode
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
;; yasnippet <tab> conflict with ac, change below
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

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


;; (require-package 'projectile)
;; (porojectile-global-mode)

(require-package 'neotree)
(global-set-key [f9] 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda() (setq neo-smart-open t)))

(require-package 'web-beautify)

;; ternjs for eamcs
(add-to-list 'load-path "~/.emacs.d/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)

;; add tern-auto-complete
(eval-after-load 'tern
  '(progn
     (require-package 'tern-auto-complete)
     (tern-ac-setup)))

(global-set-key (kbd "<f8>") 'flycheck-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (tern-mode t)
            (form-feed-mode t)
            ;; (projectile-mode t)
            (define-key js2-mode-map (kbd "C-c C-m be") 'web-beautify-js)
            (flycheck-select-checker 'javascript-standard)))


;; when it's windows, setting below
(defvar default-font-family "Source Code Pro")
(defvar default-font-size 120)
;; (set-face-attribute 'default (selected-frame) :height 140)


;; when it's mac, setting below
(when *is-a-mac*
  (setq default-font-family "Source Code Pro")
  (setq default-font-size 140)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default line-spacing 0.2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(display-buffer-reuse-frames t)
 '(safe-local-variable-values (quote ((no-byte-compile t))))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (:family "Source Code Pro" :foundry "outline" :slant normal :weight normal :height ,default-font-size :width normal)))))


(load-theme 'monokai t)

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



;; align rule for js2-mode
;; align var = abc; {a:1, b:2} etc
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(js-var
                  (regexp . "\\(\\s-*\\)[=:]\\(\\s-*\\)")
                  (group . (1 2))
                  ;; (spacing . (1 1))
                  (modes quote (js2-mode)))))



(add-to-list 'load-path (expand-file-name "standard" user-emacs-directory))
(require 'init-js-standard)

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
        (progn (kill-region (car bounds) (cdr bounds)) t)
      )))


(defun er/delete-char-or-word(&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (consp arg)
        (let* ((int (car arg)) (count (or (log int 4) 1))
               (is-js2-mode (string= "js2-mode" major-mode))
               (is-pair (looking-at-p "[][(){}'\"]"))
               )
          (progn
            (when (> count 0) (if (and (not is-pair) (my-delete-thing-at-point 'word)) (decf count)) )
            (when (> count 0)  (if (and (not is-pair) (my-delete-thing-at-point 'filename)) (decf count)) )
            (when (> count 0)  (if (and is-js2-mode is-pair) (js2r-kill) (sp-kill-sexp (round count))))
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

(defun select-current-pair-content(&optional arg)
  (interactive "p")
  (sp-beginning-of-sexp arg)
  (deactivate-mark)
  (set-mark-command nil)
  (sp-end-of-sexp nil)
  )

(defun select-current-pair()
  (interactive)
  (when (not (region-active-p))
    (if (looking-at-p "[]})]") (forward-char -1)
      (if(looking-at-p "[[{(]") (forward-char 1))
      ))
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
  ;; (sanityinc/open-line-with-reindent 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command)
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

(defun my-backward-sexp(&optional arg)
  (interactive "p")
  (let ((old (point))) (sp-beginning-of-sexp arg)
       (if (eq old (point)) (sp-backward-up-sexp arg))
       )
  )

(require-package 'youdao-dictionary)
;; FIX Problem for youdao-dict
;; https://github.com/xuchunyang/youdao-dictionary.el/issues/1#issuecomment-71359418
;; after load url-cookie.el, replace this func to avoid y2038 problem in 32-bit os
(eval-after-load 'url-cookie
'(defun url-cookie-expired-p (cookie)
  "Return non-nil if COOKIE is expired."
  (let ((exp (url-cookie-expires cookie)) year)
    (and (> (length exp) 0)
         (string-match "\\([1-9][0-9]\\{3\\}\\)" exp)
         (setq year (match-string 1 exp))
         (message "cookie from init %s" year)
         (if (and year (setq year (string-to-number year)) (>= year 2038)) nil
           (> (float-time) (float-time (date-to-time exp))))
         ))))


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
            (define-key js2-mode-map (kbd "C-M-;") '(lambda(arg)(interactive "P") (if arg (select-current-pair-content) (js2-mark-parent-statement2))))
            (define-key js2-mode-map (kbd "C-x C-;") 'remove-add-last-comma)
            ;; (define-key js2-mode-map (kbd "C-'") 'standard-format-before-cursor)
            (define-key js2-mode-map (kbd "C-' l") 'align)
            ))
(define-key global-map (kbd "<down>") 'scroll-up-line)
(define-key global-map (kbd "<up>") 'scroll-down-line)
(define-key global-map (kbd "C-x ^") 'maximize-window)
(define-key global-map (kbd "C-x j") 'standard-format-region)
(define-key global-map (kbd "C-M-;") '(lambda(arg)(interactive "P") (if arg (select-current-pair-content) (select-current-pair))))
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
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

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
