(setq debug-on-error t)
(require-package 'yasnippet)
(yas-global-mode 1)


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
            (paredit-backward-kill-word))
          )
      (paredit-backward-kill-word)
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


;; setting for auto-complete
;; press - to trigger isearch
(setq ac-show-menu-immediately-on-auto-complete t)
(global-set-key (kbd "-") 'ac-trigger-isearch)


;; multiple-cursors keybinding
(global-set-key (kbd "C-S-d") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-S-n") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-S-p") 'mc/skip-to-previous-like-this)

;; custom functions
(global-set-key (kbd "C-S-j") 'duplicate-line-or-region)
(global-set-key (kbd "<C-backspace>") 'delete-backword-or-ws)
(global-set-key (kbd "<M-backspace>") 'delete-backword-or-ws)
(global-set-key (kbd "C-c C-;") 'comment-or-uncomment-line-or-region)



;; some short keys for default
(define-key global-map "\C-x\C-u" 'undo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo )
(global-set-key (kbd "C-M-j") 'delete-indentation)
(global-set-key (kbd "C-S-h") 'backward-kill-sentence)
(global-unset-key (kbd "C-S-k"))
(global-set-key (kbd "C-S-k") 'kill-sentence)


(global-set-key (kbd "M-] ]") 'paredit-wrap-square)
(global-set-key (kbd "M-] }") 'paredit-wrap-curly)


(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "<f8>") 'flycheck-mode)

(global-set-key (kbd "<M-RET>") 'sanityinc/newline-at-end-of-line)



;; (setq-default custom-enabled-themes '(sanityinc-solarized-dark))

;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 120) (height . 34)))

;; (setq default-frame-alist '((top . 0) (left . 0) (width . 120) (height . 34)))


(when *is-a-windows*
  ;; (defcustom gnutls-trustfiles "./cacert.pem"
  ;;   "gnutls-trustfiles location of cacert.pem."
  ;;   :type '(string)
  ;;   :group 'tools
  ;;   )
  ;; (customize-option 'gnutls-trustfiles)


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
