;; Disable below line in purcell's init.el

;; (require 'init-themes)
;; (require 'init-flycheck)
;; (require 'init-sessions)
;; (require 'init-paredit)
;; (require 'init-lisp)


(require 'tramp)
(setq tramp-auto-save-directory "~/emacs/tramp-autosave")
;; (setq tramp-chunksize "500")
;; (setq tramp-default-method "plink")


(setq debug-on-error t)

(require-package 'monokai-theme)

(require-package 'yasnippet)
(yas-global-mode 1)

(require-package 'smartparens)
(require 'smartparens-config)
(add-hook 'js-mode #'smartparens-mode)
(add-hook 'js2-mode #'smartparens-mode)
(sp-use-smartparens-bindings)


(defconst *is-a-windows* (eq system-type 'windows-nt))


(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)


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
 '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight normal :height 163 :width normal)))))

(load-theme 'monokai t)
;; (tool-bar-mode nil)

;; save buffer when outof focus

(defun save-current-file ()
  (if (and (buffer-file-name) (file-exists-p (buffer-file-name)))
      (save-buffer)
    )
  )
(add-hook 'focus-out-hook 'save-current-file)




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




(defun delete-backword-or-ws ()
  (interactive)
  (let (
        (start (point))
        (end (save-excursion (skip-chars-backward "\t\n \r") (+ 1 (point))))
        )
    (message "%s, %s" start end)
    (if (thing-at-point 'whitespace)
        (if (> start end)
            (delete-region start end)
          (if (= start end)
              (delete-char -1)
            (sp-backward-kill-word 1))
          )
      (sp-backward-kill-word 1)
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

(defun highlight-next-line()
  "highlight next line"
  (interactive)
  (if (region-active-p)()
    (set-mark-command nil))
  (next-line 1)
)
(defun highlight-prev-line()
  "highlight prev line"
  (interactive)
  (if (region-active-p)()
    (set-mark-command nil))
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
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
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

(defun sp-backward-delete-all (&optional arg)(interactive)
       (let (
             (old (point))
             )
         (sp-beginning-of-sexp arg)
         (delete-region (point) old)
         )
       )


(defun search-selection (&optional arg)
  "search for selected text"
  (interactive "P")
  (when (and (eq arg nil) (not (region-active-p)))
    (sp-select-next-thing-exchange)
    )
  (if (region-active-p)
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end)))
            )
        (deactivate-mark)
        (isearch-mode t nil nil nil)
        (isearch-yank-string selection)
        )
    (isearch-forward)
    )
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


;; setting for auto-complete
;; press - to trigger isearch
(setq ac-show-menu-immediately-on-auto-complete t)
(global-set-key (kbd "-") 'ac-trigger-isearch)


;; multiple-cursors keybinding
(global-set-key (kbd "M-D") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "M-N") 'mc/skip-to-next-like-this)


;; custom functions
(define-key global-map (kbd "C-,") 'goto-first-reference)
(define-key global-map (kbd "C-s") 'search-selection)
(global-set-key (kbd "C-M-j") 'delete-indentation)
(global-set-key (kbd "C-S-j") 'join-lines)
(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-<backspace>") 'delete-backword-or-ws)
(global-set-key (kbd "C-S-<backspace>") 'sp-backward-delete-all)
(global-set-key (kbd "C-c C-;") 'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-M-l") 'mark-current-sentence)
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
(global-set-key (kbd "C-c C-c") 'whole-line-or-region-kill-ring-save)
(global-set-key (kbd "C-c C-x") 'whole-line-or-region-kill-region)
(global-set-key (kbd "C-c C-v") 'whole-line-or-region-yank)
;; (global-set-key (kbd "C-S-h") 'backward-kill-sentence)
;; restore 'kill-sentence and bind 'paredit-kill to C-k
;; (after-load 'paredit
;;   (define-key paredit-everywhere-mode-map [remap kill-sentence] nil)
;;   (define-key paredit-mode-map [remap kill-sentence] nil)
;;   )
;; (global-set-key (kbd "C-S-k") 'kill-sentence)
(global-set-key (kbd "C-j") 'kill-line-or-region)
(global-set-key (kbd "M-k") 'kill-paragraph-or-region)
(global-set-key (kbd "C-k") 'whole-line-or-region-kill-region)


;; smartparents keybinding
(global-set-key (kbd "M-]") 'sp-forward-sexp)
(global-set-key (kbd "M-[") 'sp-backward-sexp)
(global-set-key (kbd "M-s") 'sp-unwrap-sexp)
(global-set-key (kbd "C-M-]") 'sp-rewrap-sexp)
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

(global-set-key (kbd "<M-RET>") 'sanityinc/newline-at-end-of-line)



;; (setq-default custom-enabled-themes '(sanityinc-solarized-dark))

(when *is-a-mac*
;; bash complete not run on windows
  (require-package 'bash-completion)
  (bash-completion-setup)

  (setq initial-frame-alist '((top . 0) (left . 280) (width . 120) (height . 46)))
  (setq default-frame-alist '((top . 0) (left . 280) (width . 120) (height . 46)))
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
