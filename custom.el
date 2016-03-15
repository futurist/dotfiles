;; Disable below line in purcell's init.el

;; (require 'init-themes)
;; (require 'init-flycheck)
;; (require 'init-sessions)
;; (require 'init-paredit)
;; (require 'init-lisp)



(setq debug-on-error t)

(require-package 'yasnippet)
(yas-global-mode 1)

(require-package 'smartparens)
(require 'smartparens-config)
(add-hook 'js-mode #'smartparens-mode)
(add-hook 'js2-mode #'smartparens-mode)


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
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
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

(load-theme 'wombat t)
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
        (forward-line 1)
        (forward-char pos)))))



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
            (backward-kill-word 1))
          )
      (backward-kill-word 1)
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
    (forward-line 1)
    )
  )
(defun ac-move-prev-item ()
  (interactive)
  (if (ac-menu-live-p)
      (ac-previous)
    (forward-line -1)
    )
  )


(defun mark-current-sentence ()
  "Mark current sentence where point is"
  (interactive)
  (forward-sentence)
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
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
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

(defun kill-line-or-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line)
    )
  )

(defun highlight-next-line()
  "highlight next line"
  (interactive)
  (if (region-active-p)()
    (set-mark-command nil))
  (forward-line 1)
)
(defun highlight-prev-line()
  "highlight prev line"
  (interactive)
  (if (region-active-p)()
    (set-mark-command nil))
  (forward-line -1)
)


;; setting for auto-complete
;; press - to trigger isearch
(setq ac-show-menu-immediately-on-auto-complete t)
(global-set-key (kbd "-") 'ac-trigger-isearch)


;; multiple-cursors keybinding
(global-set-key (kbd "C-S-d") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-S-s") 'mc/skip-to-next-like-this)


;; custom functions
(global-set-key (kbd "C-S-j") 'duplicate-line-or-region)
(global-set-key (kbd "<C-backspace>") 'delete-backword-or-ws)
(global-set-key (kbd "<M-backspace>") 'delete-backword-or-ws)
(global-set-key (kbd "C-c C-;") 'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-S-l") 'mark-current-sentence)
;; move lines
(global-set-key (kbd "C-x C-n") 'move-line-down)
(global-set-key (kbd "C-x C-p") 'move-line-up)
(global-set-key (kbd "C-S-n") 'highlight-next-line)
(global-set-key (kbd "C-S-p") 'highlight-prev-line)
(global-set-key (kbd "C-n") 'ac-move-next-item)
(global-set-key (kbd "C-p") 'ac-move-prev-item)


;; some short keys for default
(define-key global-map "\C-x\C-u" 'undo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo )
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "C-M-j") 'delete-indentation)
(global-set-key (kbd "C-S-h") 'backward-kill-sentence)
;; restore 'kill-sentence and bind 'paredit-kill to C-k
;; (after-load 'paredit
;;   (define-key paredit-everywhere-mode-map [remap kill-sentence] nil)
;;   (define-key paredit-mode-map [remap kill-sentence] nil)
;;   )
(global-set-key (kbd "C-S-k") 'kill-sentence)
(global-set-key (kbd "C-k") 'kill-line-or-region)



(global-set-key (kbd "M-] ]") 'paredit-wrap-square)
(global-set-key (kbd "M-] }") 'paredit-wrap-curly)


(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "<f8>") 'flycheck-mode)

(global-set-key (kbd "<M-RET>") 'sanityinc/newline-at-end-of-line)



;; (setq-default custom-enabled-themes '(sanityinc-solarized-dark))

(when *is-a-mac*
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
