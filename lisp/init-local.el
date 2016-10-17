;;; init-local.el --- based on purcell's .emacs.d  -*- no-byte-compile: t; -*-

;; Copyright (C) 2016 Micheal Yang

;; Maintainer: noambitions@126.com
;; Keywords: lisp, languages
;; Package: emacs.custom

;;; Commentary:

;; Disable below line in purcell's init.el
;; (require 'init-themes)
;; (require 'init-sessions)
;; (require 'init-paredit)
;; (require 'init-lisp)

;; default fg color: #E5E5DE

;; May need to cleanup .elc file and re-compile:
;; $ find site-lisp/ -name "*.elc" -print | xargs rm -f
;; $ emacs --batch --eval "(byte-recompile-directory \"site-lisp/\" 0)"
;;; Code:

;; no sound
(setq ring-bell-function 'ignore)

(menu-bar-mode -1)

(defconst *is-a-windows* (eq system-type 'windows-nt))

(when *is-a-windows*

  ;; UTF-8 settings
  ;; (set-language-environment "UTF-8")
  ;; (prefer-coding-system 'utf-8)

  (set-language-environment 'chinese-gbk)
  (prefer-coding-system 'utf-8-unix)

  ;; (set-default-coding-systems 'gbk)
  ;; (set-buffer-file-coding-system 'gbk)
  ;; (set-clipboard-coding-system 'gbk)
  ;; (set-file-name-coding-system 'gbk)
  ;; (set-keyboard-coding-system 'gbk)
  ;; (set-next-selection-coding-system 'gbk)
  ;; (set-terminal-coding-system 'gbk)
  ;; (setq locale-coding-system 'gbk)

  (set-fontset-font t 'gb18030 '("Microsoft Yahei" . "unicode-bmp"))
  ;; (set-fontset-font t 'han (font-spec :family "Microsoft Yahei" :size 16))
  (setq face-font-rescale-alist '( ("Microsoft Yahei" . 1)("WenQuanYi Zen Hei" . 1)))

  ;; (push '("\\*shell" gbk-dos . gbk-dos) process-coding-system-alist)
  (add-hook 'shell-mode-hook '(lambda()
                                (set-buffer-process-coding-system 'gbk-dos 'gbk-dos)
                                ))

  )

(setq tramp-auto-save-directory "~/tramp-autosave")
;; (setq tramp-chunksize "500")
;; (setq tramp-default-method "plink")

(set-face-attribute 'default t :foreground "#E5E5DE") ;; set default color for fg

;; disable Ido auto merge when C-x C-f
(setq ido-auto-merge-delay-time 9999)

;; M-s SPC (isearch-toggle-lax-whitespace) to toggle it when C-s
(add-hook 'isearch-mode-hook
          (function
           (lambda ()
             (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
             (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
             (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
             (define-key isearch-mode-map "\C-j" 'isearch-edit-string))))
;; space will match any string
(setq search-whitespace-regexp ".*?")
;; show case-fold status, CFS if case-insensitive
;; (add-to-list 'minor-mode-alist '(case-fold-search " CFS"))

;; ChangeLog mode settings
(setq change-log-version-info-enabled t)
(defun change-log-sort ()
  "Sort a changelog in the format used by change-log-mode by date"
  (interactive)
  (goto-char (point-max))
  (do ((expression "^[0-9]\\{4\\}-[0-1][0-9]-[0-3][0-9]")
       (pos (point) (point))
       (list nil (cons
                  (cons
                   (match-string 0)
                   (buffer-substring (match-beginning 0) pos)) list)))
      ((not (re-search-backward expression nil t))
       (erase-buffer)
       (mapc (lambda (item)
               (insert (cdr item)))
             (nreverse (sort* list #'string< :key 'car))))))
(add-hook 'change-log-mode-hook
          (lambda()
             (define-key change-log-mode-map (kbd "C-' s") 'change-log-sort)))

;; (setq debug-on-error t)

;; prevent Chinese date problems
(setq system-time-locale "C")

(when *is-a-mac*
  ;; using TCP instead of UNIX socket to start server
  (setq server-auth-dir "/tmp/emacsserver")
  (setq server-use-tcp t)
  (toggle-frame-fullscreen)
;; ;; set below in your .bash_profile
;; export EMACS_SERVER_FILE=/tmp/emacsserver/server
;; alias e='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n -f $EMACS_SERVER_FILE -a "/Applications/Emacs.app/Contents/MacOS/Emacs" '
  )
(server-start)  ;; server seems not stable in windows

;; Helper function 
(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
(global-set-key (kbd "C-c +") 'my-increment-number-decimal)

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )


(defun get-windows-special-folder (name)
  "Get windows current user's special folder location from registry.
Name should be AppData, Cache, Desktop, Personal, Programs, Start Menu, Startup etc."
  (let ((val (shell-command-to-string (concat "reg query \"HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders\" /v \"" name "\""))))
    (trim-string (car (last (split-string val "_SZ\t"))))
    )
  )


(defun get-newest-image-file (dir)
  "Get newest image file in dir."
  (let ((image-types '("jpg" "png" "gif")))
    (expand-file-name (car (last (mapcar #'car
                                         (sort (remove-if '(lambda(x) (and (nth 1 x) (null (member (car x) image-types))) ) (directory-files-and-attributes dir))
                                               #'(lambda (x y) (time-less-p (nth 6 x) (nth 6 y))))))) dir))
  )

;; 


;; go-mode

(defvar GO-MODE-GOPATH (expand-file-name "" "~/GoCode")
  "GOPATH for go-mode.")

(defvar hugo-content-dir "~/Hugo/Sites/12345.menu/content/"
  "Path to Hugo's content directory")

(defvar org-snapshot-image-dir (cond (*is-a-windows* (convert-standard-filename (concat (get-windows-special-folder "Personal") "\\Scrshot")))
                                (*is-a-mac* "~/Desktop"))
  "Copy snapshot images from this location.")

(defun org-insert-snapshot ()(interactive)
       "Copy newest snapshot into current buffer-file dir and insert into org."
       (let ((file (get-newest-image-file org-snapshot-image-dir)))
         (when file
           (shell-command (format "cp %s %s" file (file-name-directory (buffer-file-name)) ))
           (insert (format "#+CAPTION:\n#+NAME:\n#+ATTR_HTML: :width 300px\n[[./%s.%s]]" (file-name-base file) (file-name-extension file)))))
       )

(bind-key "C-' i" 'org-insert-snapshot)

(require-package 'go-mode)
(require-package 'go-eldoc)
;; (require-package 'company-go)
(require-package 'go-autocomplete)
(add-hook 'go-mode-hook (lambda()
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (setenv "GOPATH" GO-MODE-GOPATH)
                          (setq compilation-environment (list (concat "GOPATH=" GO-MODE-GOPATH)))
                          (setq compile-command "go build -v && go test -v && go vet")
                          (define-key (current-local-map) "\C-c\C-c" '(lambda(arg)(interactive "P") (if arg
                                                                                                   (compile compile-command)
                                                                                                 (shell-command (format "go run %s" (shell-quote-argument buffer-file-name))))))
                          ;; disable company-mode and use ac-mode for go
                           ;; (set (make-local-variable 'company-backends) '(company-go)) ;only load go backends
                          (company-mode -1)
                          (auto-complete-mode 1)
                           (go-eldoc-setup)))


;; Install extensions if they're missing
(require-package 'swiper)
(defun swiper-selection (start end)
  (interactive "r")
  (swiper (when (region-active-p)
            (deactivate-mark)
            (buffer-substring start end))))

(require-package 'restclient)
;; (require-package 'nodejs-repl)  ;;buggy!!! will freeze emacs
(require-package 'editorconfig)
(editorconfig-mode +1)

(require-package 'goto-last-change)
(bind-key "C-x C-/" 'goto-last-change)

;; linum mode with highlight
(require-package 'hlinum)
(face-spec-set 'linum-highlight-face
               '((t (:inherit default :foreground "#bbbbbb"
                              :background "#333333"))))
(hlinum-activate)
(global-linum-mode -1)                   ;(linum-mode) for all buffer


;; remove er +/- overlay
(after-load "expand-region"
  (setq expand-region-fast-keys-enabled nil)
  )

(require-package 'use-package)

(require-package 'monokai-theme)

;; package from github/xahlee
(require-package 'xah-find)
(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall (and 'text-mode))
    (setq buffer-offer-save t)))
;; (setq initial-major-mode (quote text-mode))

(require-package 'ascii)

;; markdown mode
(add-hook 'markdown-mode-hook '(lambda()
                                 (whitespace-cleanup-mode -1)
                                 (bind-key "C-' w" 'whitespace-cleanup)

                                 ;; earmuff function for markdown mode
                                 (defvar markdown-earmuff-char-list '("*" "_" "\`")
                                   "Earmuff chars for Org-mode.")
                                 (dolist (char markdown-earmuff-char-list)
                                   (define-key markdown-mode-map (kbd char) `(lambda (arg) (interactive "P") (insert-earmuffs ,char nil arg))))
                                 (define-key markdown-mode-map (kbd "<backspace>") '(lambda () (interactive) (delete-earmuffs nil markdown-earmuff-char-list)))

                                 (define-key markdown-mode-map
                                   (kbd "C-c C-v")
                                   '(lambda()(interactive)
                                      (let ((html-file (replace-regexp-in-string "\.md$" ".html" buffer-file-name)))
                                        (shell-command (format "marked %s -o %s"
                                                               (shell-quote-argument buffer-file-name)
                                                               html-file))
                                        (find-file html-file))
                                      ))
                                 ))

;; package from Harry Schwartz
;; (add-to-list 'load-path "~/.emacs.d/download/org-mode/lisp")
;; (add-to-list 'load-path "~/.emacs.d/download/org-mode/contrib/lisp" t)
(require-package 'embrace)
(require-package 'ox-twbs)
(require-package 'org-download)
(require-package 'ox-reveal)
(require-package 'ox-ioslide)
(setq org-reveal-root (concat "file:///" (expand-file-name "~/download/reveal.js")))
(setq org-startup-with-inline-images nil)

(global-set-key (kbd "C-S-SPC") #'embrace-commander)
(add-hook 'org-mode-hook #'embrace-org-mode-hook)

(require-package 'ox-gfm)


;; Hugo convert org to md
;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
(defun hugo-ensure-property (property)
  "Make sure that a property exists. If not, it will be created.

Returns the property name if the property has been created,
otherwise nil."
  (if (org-entry-get nil property)
      nil
    (progn (org-entry-put nil property "")
           property)))

(defun hugo-ensure-properties ()
  "This ensures that several properties exists. If not, these
properties will be created in an empty form. In this case, the
drawer will also be opened and the cursor will be positioned
at the first element that needs to be filled.

Returns list of properties that still must be filled in"
  (require 'dash)
  (let ((current-time (format-time-string (org-time-stamp-format t t) (org-current-time)))
        first)
    (save-excursion
      (unless (org-entry-get nil "TITLE")
        (org-entry-put nil "TITLE" (nth 4 (org-heading-components))))

      (setq first (--first it (mapcar #'hugo-ensure-property '("HUGO_TAGS" "HUGO_TOPICS"))))
      (unless (org-entry-get nil "HUGO_DATE")
        (org-entry-put nil "HUGO_DATE" current-time)))

    (when first
      (goto-char (org-entry-beginning-position))
      ;; The following opens the drawer
      (forward-line 1)
      (beginning-of-line 1)
      (when (looking-at org-drawer-regexp)
        (org-flag-drawer nil))
      ;; And now move to the drawer property
      (search-forward (concat ":" first ":"))
      (end-of-line))
    first))

(defun hugo ()
  (interactive)
  (unless (hugo-ensure-properties)
    (let* ((title    (concat "title = \"" (org-entry-get nil "TITLE") "\"\n"))
           (date     (concat "date = \"" (format-time-string "%Y-%m-%d" (apply 'encode-time (org-parse-time-string (org-entry-get nil "HUGO_DATE"))) t) "\"\n"))
           (topics   (concat "topics = [ \"" (mapconcat 'identity (split-string (org-entry-get nil "HUGO_TOPICS") "\\( *, *\\)" t) "\", \"") "\" ]\n"))
           (tags     (concat "tags = [ \"" (mapconcat 'identity (split-string (org-entry-get nil "HUGO_TAGS") "\\( *, *\\)" t) "\", \"") "\" ]\n"))
           (fm (concat "+++\n"
                       title
                       date
                       tags
                       topics
                       "+++\n\n"))
           (file     (org-entry-get nil "HUGO_FILE"))
           (coding-system-for-write buffer-file-coding-system)
           (backend  'md)
           (blog))
      ;; try to load org-mode/contrib/lisp/ox-gfm.el and use it as backend
      (if (require 'ox-gfm nil t)
          (setq backend 'gfm)
        (require 'ox-md))
      (setq blog (org-export-as backend t))
      ;; Normalize save file path
      (unless (string-match "^[/~]" file)
        (setq file (concat hugo-content-dir file))
      (unless (string-match "\\.md$" file)
        (setq file (concat file ".md")))
      (message "%s" file)
      ;; save markdown
      (with-temp-buffer
        (insert fm)
        (insert blog)
        (untabify (point-min) (point-max))
        (write-file file)
        (message "Exported to %s" file))
      ))))

(defun org-insert-title-as-pinyin(arg start end) (interactive "P\nr")
       (let ((str (cond (arg (read-string "Enter chinese"))
                        ((region-active-p) (prog1 (buffer-substring start end) (delete-region start end)))
                        (t (nth 4 (org-heading-components))) )))
         (insert (replace-regexp-in-string
                  " " "-"
                  (trim-string (shell-command-to-string (concat "pinyin -s NORMAL \"" str "\"")))
                  )))
       )

(bind-key "M-g h" #'hugo)
(bind-key "M-g t" #'org-insert-title-as-pinyin)

(defun gp-org-gfm-publish-to-md (plist filename pub-dir)
  "Publish an org file to Github Flavoured Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'gfm filename ".md" plist pub-dir))

(setq org-publish-project-alist
      `(
        ("12345"
         ;; Path to org files
         :base-directory ,(expand-file-name (concat hugo-content-dir "../org-content"))
         :base-extension "org"

         ;; Path to hugo project
         :publishing-directory ,hugo-content-dir
         :recursive t
         :publishing-function gp-org-gfm-publish-to-md
         )

        ))



(defun hrs/de-unicode ()
  "Tidy up a buffer by replacing all special Unicode characters
       (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("\u2013" . "--")
                       ("\u2014" . "---")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

(defun mc/eval-and-replace-region ()
  (interactive)
  (mc/execute-command-for-all-fake-cursors 'eval-and-replace-region))

(defun eval-and-replace-region ()
  "Replace the region with its sexp value."
  (interactive)
  (when (region-active-p)
    (call-interactively 'kill-region)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0))))))

(defun eval-string (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

;; Some short cuts function
(defun insert-earmuffs (char &optional with-space insert-normal-p)
  "Insert earmuffs for char."
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert char)
        (goto-char start)
        (insert char)
        ;; (call-interactively 'exchange-point-and-mark)
        )
    (if (and (not insert-normal-p)
             (or (not with-space)
               (and (> (current-column) 0)
                    (looking-back "[ \t\n]" 1))))
        (let ((space 1))
          (progn
            (insert char)
            ;; you can just UNDO the insert, back to normal char
            (undo-boundary)
            (insert char)
            (when with-space
              (insert " ")
              (incf space))
            (backward-char space)))
      (insert char))
    ))

(defun delete-earmuffs (with-space char-list)
  "Delete earmuffs based on earmuff-char-list var."
  (let ((not-found t))
    (dolist (char char-list)
      (when (and (char-before) (char-after) not-found (string= (string (char-before)) char) (string= (string (char-after)) char) )
        (delete-forward-char (if with-space 2 1) nil)
        (delete-forward-char -1 nil)
        (setq not-found nil)
        )
      )
    (when not-found
      (if (region-active-p)
          (delete-region (region-beginning) (region-end))
        (delete-forward-char -1 nil)))
    )
  )

;; soft wrap long lines
(setq org-startup-truncated nil)
(add-hook 'org-mode-hook
          (lambda ()
            ;; (org-bullets-mode -1)
            (flycheck-mode -1)
            (setq truncate-lines 'nil)
            (load-library "ox-reveal")
            (define-key org-mode-map (kbd "C-'") nil)
            (define-key org-mode-map (kbd "C-' l") 'org-toggle-link-display)
            (define-key org-mode-map (kbd "C-' c") 'org-unindent-buffer)
            (define-key org-mode-map (kbd "C-' e") '(lambda()(interactive) (org-ioslide-export-to-html) (browse-url-of-file (replace-regexp-in-string "\.org$" ".html" (expand-file-name (buffer-file-name))))))
            (defvar org-earmuff-char-list '("*" "=" "/" "~" "+" "_")
              "Earmuff chars for Org-mode.")
            (dolist (char org-earmuff-char-list)
              (define-key org-mode-map (kbd char) `(lambda (arg) (interactive "P") (insert-earmuffs ,char t arg))))
            (define-key org-mode-map (kbd "<backspace>") '(lambda () (interactive) (delete-earmuffs t org-earmuff-char-list)))
            (define-key global-map (kbd "<M-return>") nil)
            (setq-default org-display-custom-times t)
            (setq org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
            (setq org-export-html-postamble-format
                  '( ("zh-CN" "<p class=\"postamble\">最后更新日期: %C</p>")
                     ("en" "<p class=\"postamble\">最后更新日期: %C</p>")
                     ))
            ;; fix Chinese 'creator name
            (add-to-list 'org-export-dictionary '("Created" ("zh-CN" :default "更新")))
            ))

(setq user-full-name "James Yang"
        user-mail-address "jamesyang999@gmail.com"
        calendar-latitude 30.26
        calendar-longitude 120.19
        calendar-location-name "Hangzhou, China")



(setq org-hide-leading-stars t)
(setq org-ellipsis " ⇒⇒")

;; abbrev settings
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...
(setq save-abbrevs t)              ;; save abbrevs when files are saved

;; package from github
(require-package 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; (require-package 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;; command line install: npm install -g lorem-cn
(defun insert-lorem-cn (number arg)
  (interactive "p\nP")
  ;; (message "%s---%s" number arg)
  (insert (shell-command-to-string (format "lorem-cn %s %s" number "-p")))
  )

(bind-key "C-' l l" 'insert-lorem-cn)

(defvar html-file-extensions "\\.\\(aspx\\|php\\|\\sw*html*\\)\\'")
(setq mmm-global-mode nil)
(after-load 'init-mmm
  (mmm-add-classes
   '((html-js2
      :submode js-mode
      :face mmm-default-submode-face
      :front "<script[^>]*>"
      :back "[ \t\n]*</script>"
      )))
  (mmm-add-mode-ext-class 'html-mode html-file-extensions 'html-php)
  (mmm-add-mode-ext-class 'html-mode html-file-extensions 'html-js2)
  (mmm-add-mode-ext-class 'html-mode html-file-extensions 'html-css)
  (add-hook 'html-mode-hook
            #'(lambda()
                (mmm-mode t)
                (define-key mmm-mode-map (kbd "C-c C-v")
                  '(lambda()(interactive)
                     (save-buffer)
                     (browse-url-of-buffer)))
                (define-key mmm-mode-map (kbd "C-' r") '(lambda()(interactive) (mmm-parse-buffer)))
                ))
  )
(defvar browse-url-filename-alist nil
  "Default mapping for preview html file to http host.")


;; ;; editing html file mode
;; (require-package 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags
;;   '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;     (js2-mode  "<script[^>]*>" "</script>")
;;     (css-mode "<style[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


(require-package 'yasnippet)
(require-package 'react-snippets)
(yas-global-mode 1)
;; yasnippet <tab> conflict with ac, change below
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

;; (require-package 'smartparens)
;; (require 'smartparens-config)
;; (add-hook 'js-mode-hook #'smartparens-mode)
;; (add-hook 'js2-mode-hook #'smartparens-mode)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; github/magnars

(require 'mc-cycle-cursors)

(setq mc/cmds-to-run-once
      '(
        mac-mwheel-scroll
        mouse-set-region
        smex
        ))

;;;;;;;;;;;Commonly used;;;;;;;;;;;;;;;;;

(bind-key "C-c C-m a" 'mc/mark-all-dwim)
(bind-key "C-' a" 'align-regexp)

;;;;;;;;;;;HELPER FOR MC;;;;;;;;;;;;;;;;;

;; when C-- C-> (to unmark next)
;; cycle-backward first, then do unmark
(advice-add 'mc/mark-next-like-this :before '(lambda(arg)(interactive "p") (when (< arg 0) (mc/cycle-backward))))
(advice-add 'mc/mark-next-like-this :after '(lambda(arg)(interactive "p") (unless (< arg 0) (mc/cycle-forward))))
(advice-add 'mc/skip-to-next-like-this :before '(lambda()(interactive) (when (> (mc/num-cursors) 1) (mc/cycle-backward))))

(advice-add 'mc/mark-previous-like-this :before '(lambda(arg)(interactive "p") (when (< arg 0) (mc/cycle-forward))))
(advice-add 'mc/mark-previous-like-this :after '(lambda(arg)(interactive "p") (unless (< arg 0) (mc/cycle-backward))))
(advice-add 'mc/skip-to-previous-like-this :before '(lambda()(interactive) (when (> (mc/num-cursors) 1) (mc/cycle-forward))))


(require-package 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(require-package 's)
(require-package 'dash)
(require-package 'tagedit)
(after-load 'tagedit

  (defun te/my-forward-tag ()
    "Alway goto the end tag of currnet scope."
    (interactive)
    (let* ((tag (te/current-tag))
           (in-opening-tag (and (> (point) (te/get tag :beg)) (< (point) (te/inner-beg tag))))
           (in-closing-tag (and (< (point) (te/get tag :end)) (> (point) (te/inner-end tag))))
           )
      (if in-opening-tag
          (te/goto-current-tag-content)
        (goto-char (te/get tag :end))
        )
      )
    )

  (defun te/my-backward-tag ()
    "Always goto the begin of current scope."
    (interactive)
    (let* ((tag (te/current-tag))
           (in-opening-tag (and (> (point) (te/get tag :beg)) (< (point) (te/inner-beg tag))))
           (in-closing-tag (and (< (point) (te/get tag :end)) (> (point) (te/inner-end tag))))
           )
      (if in-opening-tag
          (goto-char (te/get tag :beg))
        (backward-char)
        (te/goto-tag-begging)
        )
      )
    )

  (bind-key "C-M-n" 'te/my-forward-tag tagedit-mode-map)
  (bind-key "C-M-p" 'te/my-backward-tag tagedit-mode-map)

  (defun te/goto-current-tag-content()
    (interactive)
    (goto-char (te/inner-beg (te/current-tag)))
    ;; (newline-and-indent)
    )

  (defun te/goto-tag-begging()
    (interactive)
    (goto-char (te/get (te/current-tag) :beg))
    )

  (defun te/goto-tag-end()
    (interactive)
    (goto-char (1- (te/get (te/current-tag) :end)))
    )

  (defun te/goto-tag-match()
    (interactive)
    (let* ((tag (te/current-tag))
           (in-opening-tag (and (>= (point) (te/get tag :beg)) (<= (point) (te/inner-beg tag))))
           (in-closing-tag (and (<= (point) (te/get tag :end)) (>= (point) (te/inner-end tag))))
           )
      (if in-opening-tag
          (te/goto-tag-end)
        (te/goto-tag-begging)
        )
      )
    )

  (defun te/kill-current-tag(arg)
    (interactive "p")
    (decf arg)
    (let* ((tag (te/current-tag)) (parent tag))
      (dotimes (i arg)
        (setq parent (te/parent-tag parent))
        )
      (kill-region (te/get parent :beg) (te/get parent :end))
      )
    )

  (define-key tagedit-mode-map (kbd "M-]") 'te/goto-current-tag-content)
  (define-key tagedit-mode-map (kbd "C-c C-<backspace>") 'te/kill-current-tag)
  (define-key tagedit-mode-map (kbd "C-%") 'te/goto-tag-match)
  (define-key tagedit-mode-map (kbd "C-^") 'te/goto-tag-begging)
  (define-key tagedit-mode-map (kbd "C-$") 'te/goto-tag-end)
  )
(use-package js2-refactor
  :defer t
  :diminish js2-refactor-mode
  :commands js2-refactor-mode
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(defun tagedit-toggle-experimental()
  (interactive)
  (if tagedit-experimental-features-on?
      (tagedit-disable-experimental-features)
    (tagedit-add-experimental-features)))

(eval-after-load "sgml-mode"
  '(progn
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda ()
                                 (tagedit-mode 1)
                                 (tagedit-add-experimental-features)
                                 (bind-key "C-' t t" 'tagedit-toggle-experimental)
                                 ))))

;; from magnars/.emacs.d/defuns/buffer-defuns.el
(defun untabify-buffer ()
  "Select all then untabify."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Select all then indent region."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(require-package 'company)
(require-package 'company-flx)
;; (require-package 'company-tern)
(require-package 'company-quickhelp)
(require-package 'company-dict)
(require-package 'company-restclient)
(require-package 'company-go)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-limit 30)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
;; (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-minimum-prefix-length 1)
(after-load 'company
  (company-flx-mode +1)
  (setq company-auto-complete-chars "")
  ;; (company-quickhelp-mode 1)
  (setq company-auto-complete t)
  (setq company-flx-limit 99)
  (setq company-dict-dir (concat user-emacs-directory "company-dict/"))
  ;; (setq company-auto-complete-chars '(?\  ?\) ?. ?\t))
  ;; (add-to-list 'company-backends 'company-dict)
  ;; (add-to-list 'company-backends 'company-restclient)
  (define-key global-map (kbd "M-/") 'company-complete)
  (define-key global-map (kbd "C-M-/") 'company-dict)
  (define-key global-map (kbd "M-\\") 'hippie-expand)
  ;; (define-key company-active-map (kbd "<SPC>") '(lambda()(interactive) (self-insert-command 1) (undo-boundary) (company-complete-selection)))
  ;; (define-key company-active-map (kbd "<SPC>") '(lambda()(interactive) (company-abort) (insert " ")))
  (define-key company-active-map (kbd ".") '(lambda()(interactive) (company-abort) (insert ".")))
  (define-key company-active-map (kbd "C-j") 'company-abort)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map "\C-n" 'company-select-next-or-abort)
  (define-key company-active-map "\C-p" 'company-select-previous-or-abort)
  )
;; (after-load 'company-tern
;;   (add-to-list 'company-backends 'company-tern)
;;   ;; (setq company-tern-property-marker "")
;;   )
(after-load 'company-go
  (add-to-list 'company-backends 'company-go)
  )

(require-package 'sws-mode)
(require-package 'jade-mode)
(require-package 'stylus-mode)
(setq auto-mode-alist
      (append
       '(
         ("\\.styl\\'" . stylus-mode)
         ("\\.stylus\\'" . stylus-mode)
         )
      auto-mode-alist)
 )


(defvar projectile-keymap-prefix (kbd "C-x p"))
(require-package 'ag)
(require-package 'flx)
(require-package 'flx-ido)
(require-package 'projectile)
(projectile-global-mode)


(require-package 'neotree)
(global-set-key [f9] 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda() (setq neo-smart-open t)))

(require-package 'form-feed)
(require-package 'js-doc)
(require-package 'web-beautify)
;; (after-load 'skewer-mode
;;   (skewer-setup))

;; ternjs for eamcs
(add-to-list 'load-path "~/.emacs.d/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)

;; add tern-auto-complete
(after-load 'tern
  '(progn
     ;; (require-package 'tern-auto-complete)
     ;; (tern-ac-setup)
     ))
(add-hook 'tern-mode-hook
          (lambda()
            (unbind-key "M-." tern-mode-keymap)
            (unbind-key "C-M-." tern-mode-keymap)
            (unbind-key "M-," tern-mode-keymap)
            (unbind-key "C-c C-r" tern-mode-keymap)
            ;; C-c C-c to get type and C-c C-d to get doc
            )
          )

(global-set-key (kbd "<f8>") 'flycheck-mode)
(add-hook 'js-mode-hook
          (lambda()
            (tern-mode +1)
            (tagedit-mode -1)
            ) )

(defun js2-insert-comma-new-line (arg)
  (interactive "P")
  (goto-char (paredit-current-sexp-end))
  (insert ",")
  (sanityinc/newline-at-end-of-line)
  )

(defun js-comment-block-newline (arg)
  (interactive "P")
  (let ((node (js2-comment-at-point)) start len comment-start)
    (when node
      (setq start (1+ (aref node 2) ))
      (setq len (+ start (aref node 3)))
      (setq comment-start (buffer-substring start (+ start 2)) )
      (sanityinc/newline-at-end-of-line)
      (if (not (string= comment-start "/*"))
          (insert "// ")
        (delete-horizontal-space)
        (insert " * ")))
    (when (null node)
      (sanityinc/newline-at-end-of-line)
      ))
  )

(add-hook 'js2-mode-hook
          (lambda ()
            (set (make-local-variable 'page-delimiter) "//\f")
            ;; (tern-mode +1)
            (form-feed-mode t)
            (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
            (define-key js2-mode-map "@" 'js-doc-insert-tag)
            (define-key js2-mode-map (kbd "C-c C-m be") 'web-beautify-js)
            (define-key js2-mode-map (kbd "C-M-i") 'company-tern)
            (define-key js2-mode-map (kbd "M-.") 'js2-jump-to-definition)
            (define-key js2-mode-map (kbd "M-,") 'js2-mark-parent-statement)
            (define-key js2-mode-map (kbd "C-' c") 'standard-format-buffer)
            (define-key js2-mode-map (kbd "C-M-]") 'js2-insert-comma-new-line)
            (define-key js2-mode-map (kbd "<M-return>") 'js-comment-block-newline)
            (flycheck-select-checker 'javascript-standard)
            ))


;; when it's windows, setting below
(defvar default-font-family "Source Code Pro")
(defvar default-font-size 120)
;; (set-face-attribute 'default (selected-frame) :height 140)


;; when it's mac, setting below
(when *is-a-mac*
  (setq default-font-family "Source Code Pro")
  (setq default-font-size 164)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default line-spacing 0.2)

(defun get-default-font()
  `(default ((t (:family "Source Code Pro" :foundry "outline" :slant normal :weight normal :height ,default-font-size :width normal)))))

(setq cua-enable-cua-keys nil)
(setq display-buffer-reuse-frames t)
(setq avy-timeout-seconds 0.4)
(setq avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (get-default-font))


(load-theme 'monokai t)

;; save buffer when outof focus
(defun save-current-file ()
  (if (and (buffer-file-name) (file-exists-p (buffer-file-name)))
      (save-buffer)
    )
  )
(defun trigger-isearch-when-focus ()
  (if (and (string= "*Open Recent*" (buffer-name)) )
      ;; (goto-line 3)
      (isearch-mode t nil nil nil)
    )
  )

(add-to-list 'recentf-exclude "/recentf\\'")
(add-to-list 'recentf-exclude "ido\\.last\\'")
(add-to-list 'recentf-exclude "\\.tidyrc")
(add-hook 'recentf-dialog-mode-hook 'trigger-isearch-when-focus)
(add-hook 'find-file-hook '(lambda()
                             (recentf-save-list)
                             ))
(add-hook 'focus-out-hook '(lambda()
                             ;; (save-current-file)
                             ))

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


(defun current-line-empty-p (&optional at-point-p)
  (save-excursion
    (unless at-point-p (beginning-of-line))
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
                                    ((string= method "POST-BASE64") '(("Content-Type" . "text/plain")))
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
          ((and t) (base64-encode-string (encode-coding-string args 'utf-8))) ;base64 encode to pass to node
          )))
    (url-retrieve url callback)
    ))



(defvar-local pop-mark-pos-ring nil
  "Store popped mark posision.
TODO: save mark position for each buffer.")

(defun pop-mark-large-redo (ARG)
  (interactive "P")
  (when pop-mark-pos-ring
    (goto-char (pop pop-mark-pos-ring))))

(advice-add 'set-mark-command :before '(lambda(arg)
                                         (if (consp arg)
                                             (push (point) pop-mark-pos-ring)
                                           )
                                         ))

(defun pop-mark-large (ARG)
  (interactive "P")
  (if (and ARG pop-mark-pos-ring)
      (goto-char (pop pop-mark-pos-ring))
    (let ((line (line-number-at-pos)) (count 99))
      (when mark-ring (push (point) pop-mark-pos-ring))
      (pop-to-mark-command)
      (while (and (> (decf count) 0) mark-ring (= line (line-number-at-pos)))
        (setf line (line-number-at-pos))
        (pop-to-mark-command)
        )
      ))
  )

(bind-key "C-' C-SPC" 'pop-mark-large)
(bind-key "C-' C-g" 'pop-mark-large-redo)

(defun paredit-rewrap (&optional ARG)
  "Rewrap current sexp, if ARG, don't remove old delimiter."
  (interactive "P")
  (save-excursion
    (let ((pos (point))
          (start (paredit-current-sexp-start))
          (end (paredit-current-sexp-end))
          (left (read-char-exclusive "Rewrap left char:"))
          (right (read-char-exclusive "Rewrap right char:"))
          )
      (goto-char end)
      (when (null ARG)
        (delete-backward-char 1))
      (insert-char right)
      (goto-char start)
      (when (null ARG)
        (delete-forward-char 1))
      (insert-char left)
      )))

(defun paredit-kill-sexp-forward(&optional N)
  "Kill forward-sexp N times."
  (setq N (or N 1))
  (let ((pos (point)))
    (forward-sexp N)
    (kill-region pos (point) nil)))

(defun paredit-current-sexp-start(&optional arg)
  "Get current sexp start for string/list type."
  (interactive "P")
  (let ((pos (if (paredit-in-string-p)
                 (paredit-enclosing-string-start)
               (paredit-enclosing-list-start))))
    (when (called-interactively-p 'interactive)
      (goto-char pos)(forward-char 1) )
    pos ) )

(defun paredit-current-sexp-end(&optional arg)
  "Get current sexp end for string/list type."
  (interactive "P")
  (let ((pos (if (paredit-in-string-p)
                 (paredit-enclosing-string-end)
               (paredit-enclosing-list-end))))
    (when (called-interactively-p 'interactive)
      (goto-char pos) (forward-char -1) )
    pos ) )

(defun select-current-pair(&optional arg)
  (interactive "P")
  (let ((start (paredit-current-sexp-start))
        (end (paredit-current-sexp-end)))
    (when arg
      (incf start)
      (decf end)
      )
    (goto-char start)
    (deactivate-mark)
    (set-mark-command nil)
    (transient-mark-mode '(4))
    (goto-char end)
    )
  )

(defun paredit-backward-delete-all (&optional arg)(interactive)
       (let ((old (point))
             (state (paredit-current-parse-state)))
         (if (> (car state) 0) (goto-char (1+ (paredit-current-sexp-start))))
         (if (eq old (point))
             (progn
               ;; (move-beginning-of-line nil)
               (back-to-indentation)
               (kill-region old (point)))
           (kill-region (point) old))
         )
       )


;; (require-package 'paredit)
(after-load 'paredit-everywhere
  (advice-add 'paredit-splice-sexp :before '(lambda(&optional arg)
                                              "If in begin of sexp delimiter, then forward-char"
                                              (if (= ?\( (char-syntax (char-after)))
                                                  (forward-char)
                                                )
                                              ))
  (define-key paredit-everywhere-mode-map (kbd "C-d") nil)

  (define-key paredit-everywhere-mode-map (kbd "M-}") nil)

  (define-key paredit-everywhere-mode-map (kbd "C-M-}") 'paredit-current-sexp-end)

  (define-key paredit-everywhere-mode-map (kbd "C-M-{") 'paredit-current-sexp-start)

  (define-key paredit-everywhere-mode-map (kbd "C-M-p") 'paredit-backward-down)
  (define-key paredit-everywhere-mode-map (kbd "C-M-n") 'paredit-forward-up)
  (define-key paredit-everywhere-mode-map (kbd "C-M-d") 'paredit-forward-down)
  )

(dolist (mode '(web html xhtml xml nxml sgml))
  (add-hook (intern (format "%s-mode-hook" mode))
            '(lambda ()
               (tagedit-mode 1)
               )))

(dolist (mode '(ruby espresso js js2))
  (add-hook (intern (format "%s-mode-hook" mode))
            '(lambda ()
               (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
                            (lambda (_ _) nil))
               ;; (enable-paredit-mode)
               )))

(defun js2r-universal-expand(arg &optional is-contract)
  "Expand or contract bracketed list using js2r-refactor.
Currently working on array, object, function, call args."
  (interactive "P")
  (let ((debug-on-error nil)
        (pos (point))
        (pos-array (point-max))
        (pos-object (point-max))
        (pos-function (point-max))
        (pos-call (point-max)))
    (save-excursion
      (ignore-errors
        (js2r--goto-closest-array-start)
        (setq pos-array (- pos (point)))))

    (save-excursion
      (ignore-errors
        (js2r--goto-closest-object-start)
        (setq pos-object (- pos (point)))))

    (save-excursion
      (ignore-errors
        (js2r--goto-closest-function-start)
        (setq pos-function (- pos (point)))))

    (save-excursion
      (ignore-errors
        (js2r--goto-closest-call-start)
        (setq pos-call (- pos (point)))))

    (setq pos (-min (list pos-array pos-object pos-function pos-call)))
    (when (= pos pos-array) (if is-contract
                                (js2r-contract-array)
                              (js2r-expand-array)
                              (when arg (js2r--goto-closest-array-start) (forward-char) (js2r--ensure-just-one-space))))
    (when (= pos pos-object) (if is-contract
                                 (js2r-contract-object)
                               (js2r-expand-object)
                               (when arg (js2r--goto-closest-object-start) (forward-char) (js2r--ensure-just-one-space))))
    (when (= pos pos-function) (if is-contract
                                   (js2r-contract-function)
                                 (js2r-expand-function)
                                 (when arg (js2r--goto-closest-function-start) (forward-char) (js2r--ensure-just-one-space))))
    (when (= pos pos-call) (if is-contract
                               (js2r-contract-call-args)
                             (js2r-expand-call-args)
                             (when arg (js2r--goto-closest-call-start) (forward-char) (js2r--ensure-just-one-space))))
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


(require 'hi-lock)
(defun my/unhighlight-all-in-buffer ()
  "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
  (interactive)
  (unhighlight-regexp t))
(defun my/highlight-symbole()
  (interactive)
  (if (region-active-p)
      (highlight-symbol-add-symbol (buffer-substring (region-beginning) (region-end)))
    (highlight-symbol-at-point)
    )
  )
(define-key search-map "hu" #'my/unhighlight-all-in-buffer) ;paredit use M-s, so add global-map below
(bind-key "C-' u" 'highlight-symbol-remove-all)
(define-key global-map (kbd "M-n") #'highlight-symbol-next)
(define-key global-map (kbd "M-p") #'highlight-symbol-prev)

(advice-add 'ido-kill-buffer
            :around
            '(lambda(oldfun &rest args)
               "If in server edit mode, close client."
               (if server-buffer-clients
                   (server-edit)
                 (apply oldfun args)
                 )
               ))



    (defun mark-current-indentation (&optional ARG)
      "Select surrounding lines with current indentation.
    if ARG is 'C-u', mark forward; if ARG is 'C-u C-u', mark backward."
      (interactive "P")
      (let ((is-forward (not (equal ARG '(16))))
            (is-backward (not (equal ARG '(4))))
            (indentation (if (not (current-line-empty-p))
                             (current-indentation)
                           (skip-chars-forward "\s\t\n")
                           (current-indentation))))
        (if (= indentation 0)
            (mark-whole-buffer)
          (if (not is-forward) (push-mark (point) nil t))
          (while (and (not (bobp)) is-backward
                      (or (current-line-empty-p) (<= indentation (current-indentation))))
            (forward-line -1))
          (if is-backward (forward-line 1) (move-beginning-of-line nil))
          (if (not (use-region-p)) (push-mark (point) nil t))
          (while (and (not (eobp)) is-forward
                      (or (current-line-empty-p) (<= indentation (current-indentation))))
            (forward-line 1))
          (if is-forward (backward-char)))))
(bind-key "C-' C-l" 'mark-current-indentation)

(add-to-list 'load-path (expand-file-name "standard" user-emacs-directory))
(require 'init-js-standard)


(require-package 'powerline)
(powerline-default-theme)



;; magnars version for rename buffer and file
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)


;; location current file in explorer
(require-package 'reveal-in-osx-finder)
(defun locate-current-file-in-explorer (in-tc)
  (interactive "p")
  (when *is-a-windows*
    (let ((cmd (if (= in-tc 1) "tc " "explorer /e,/select,")))
      (cond
       ;; In buffers with file name
       ((buffer-file-name)
        (shell-command (concat "start " cmd  "\"" (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))
       ;; In dired mode
       ((eq major-mode 'dired-mode)
        (shell-command (concat "start " cmd  "\"" (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))
       ;; In eshell mode
       ((eq major-mode 'eshell-mode)
        (shell-command (concat "start " cmd  "\"" (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))
       ;; Use default-directory as last resource
       (t
        (shell-command (concat "start " cmd  "\"" (replace-regexp-in-string "/" "\\\\" default-directory) "\""))))))
  (when *is-a-mac*
    (reveal-in-osx-finder))
  )

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



(defun comment-or-uncomment-line-or-region (arg)
  "Comments or uncomments the current line or region."
  (interactive "p")
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (save-excursion
      (let (cur)
        (setq cur (line-beginning-position))
        (forward-line arg)
        (if (plusp arg)
            (comment-or-uncomment-region cur  (line-beginning-position))
          (comment-or-uncomment-region  (line-beginning-position) cur)
          ) )) )
  )

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))


(defun my-delete-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (progn (kill-region (car bounds) (cdr bounds)) t)
      )))

(defun escape-regexp-in-string(str)
  (setq str (replace-regexp-in-string "'" "\\\'" str 'FIXEDCASE 'LITERAL))
  (setq str (replace-regexp-in-string "\"" "\\\"" str 'FIXEDCASE 'LITERAL))
  )

(defun syntax-forward-syntax-group (&optional arg)
  "Move ARG times to start of a set of the same syntax characters group.
from Google syntax-forward-syntax func."
  (interactive "^p")
  (let* ((cur (point))
         (group '("_w" " -<>"))
         get-group role curgroup
         )
    (setq get-group '(lambda(role) (--first (progn (when (not (consp it)) (setq it `(,it . ,it))) (s-contains? role (car it))) group) ))

    (while (and (> arg 0) (not (eobp)) )
      (setq cur (point))
      (setq role (string (char-syntax (char-after))))
      (setq curgroup (funcall get-group role))
      ;; (message "%s--%s" role curgroup)
      (if (null curgroup) (forward-char)
        (skip-syntax-forward (car curgroup)))
      (if (= cur (point)) (forward-char))
      (when (or (s-contains? role " -<>") (not (looking-at "[[:space:]]*$"))) (skip-syntax-forward " -<>" ))
      (setq arg (1- arg)))
    (while (and (< arg 0) (not (bobp)))
      (setq cur (point))
      (setq role (string (char-syntax (char-before))))
      (setq curgroup (funcall get-group role))
      (if (null curgroup) (forward-char -1)
        (skip-syntax-backward (car curgroup)))
      (if (= cur (point)) (forward-char -1))
      (when (or (s-contains? role " -<>") (not (looking-back "^[[:space:]]*"))) (skip-syntax-backward " -<>" ))
      (setq arg (1+ arg)))
    )
  )


(defun kill-forward-symbol(&optional arg)
  (interactive "p")
  (let ((cur (point)) count to)
    (syntax-forward-syntax-group arg)
    (kill-region (point) cur)
    )
  )

(defun kill-backward-symbol(&optional arg)
  (interactive "p")
  (let ((cur (point)) count to)
    (syntax-forward-syntax-group (* -1 arg))
    (kill-region (point) cur)
    )
  )

(defun my-delete-char-or-word(&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (consp arg)
        (let* ((int (car arg))
               (count (or (round (log int 4)) 1))
               (is-js2-mode (string= "js2-mode" major-mode))
               (syntax-char (char-syntax (following-char)))
               (is-pair (-contains? '(?\( ?\") syntax-char))
               )
          (if (= ?\) syntax-char) (paredit-backward-up))
          (when (> count 0) (if (and (not is-pair) (my-delete-thing-at-point 'word)) (decf count)) )
          (when (> count 0) (if (and (not is-pair) (my-delete-thing-at-point 'filename)) (decf count)) )
          (setq count (min (paredit-count-sexps-forward) count))
          (when (> count 0) (if (and is-js2-mode is-pair) (js2r-kill) (paredit-kill-sexp-forward count)))
          ;; (er/expand-region (or (log count 4) 1))
          ;; (kill-region (region-beginning) (region-end))
          )
      (paredit-forward-delete (or arg 1))
      ))
  )


(defun delete-backword-or-ws (arg)
  (interactive "p")
  (dotimes (count arg)
    (let (
          (start (point))
          (end (save-excursion (skip-chars-backward "\t\n \r") (+ 1 (point))))
          )
      (if (and (thing-at-point 'whitespace) (>= start end))
          (if (> start end)
              (kill-region start end)
            (if (= start end)
                (delete-char -1)
              )
            )
        (paredit-backward-kill-word)
        (pop kill-ring)
        (setq kill-ring-yank-pointer kill-ring)
        )
      ))
  )

(defun ac-trigger-isearch ()
  "Trigger ac isearch using -"
  (interactive)
  (if (ac-menu-live-p)
      (ac-isearch)
    ;; (insert "-")
    (self-insert-command 1)
    ))


(defun mark-current-sentence ()
  "Mark current sentence where point is"
  (interactive)
  (forward-sentence)
  (deactivate-mark)
  (set-mark-command nil)
  (backward-sentence)
  )



(defun kill-paragraph-or-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-paragraph 1)
    )
  )

(defun kill-line-or-region (arg)
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (paredit-kill arg)))

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
  (let ((kill-ring nil))
    (save-excursion
      (goto-char (paredit-current-sexp-end))
      (if (eq arg 1)(kill-sexp 1)
        (if (eq arg 2) (let ((char (read-char-exclusive "input a char to zap:")))
                         (if (eq char 13)
                             (kill-line)
                           (zap-up-to-char 1 char))
                         )
          ;; (if (eq arg 3) (paredit-kill 1))
          (if (eq arg 3) (kill-line))
          )
        )
      (backward-char 1)
      (yank)
      ;; (pop kill-ring)
      ;; (setq kill-ring-yank-pointer kill-ring)
      )))

(defun move-parent-backward (arg)
  (interactive "p")
  (let ((kill-ring nil))
    (save-excursion
      (when (eq arg 1)
        (let ((pos (point)))
          (goto-char (1- (paredit-current-sexp-end)))
          (skip-chars-backward "\n\r \t")
          (kill-region pos (point))
          )
        )
      (when (eq arg 2) (call-interactively 'paredit-kill))
      (when (eq arg 3) (paredit-kill))
      (goto-char (paredit-current-sexp-end))
      (newline-and-indent)
      (yank)
      ;; (pop kill-ring)
      ;; (setq kill-ring-yank-pointer kill-ring)
      ))
  )

(defun move-parent-forward-symbol (arg)(interactive "P")
       (move-parent-forward (if arg 2 1)))
(defun move-parent-forward-paredit (arg)(interactive "p")
       (move-parent-forward 3))
(defun move-parent-backward-sexp(arg)(interactive "p")
       (move-parent-backward 1))


;; -- define a new command to join multiple lines together --
(defun join-lines () (interactive)
       (next-line)
       (join-line)
       (delete-horizontal-space)
       (insert " ")
       )

(defun search-selection (&optional arg)
  "search for selected text"
  (interactive "P")
  (when (and (thing-at-point 'symbol) (equal arg nil) (not (region-active-p)))
    )
  (if (and (not arg) (region-active-p))
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
  (let ((old (point))) (goto-char (1+ (paredit-current-sexp-start)))
       (when (eq old (point))
         (paredit-backward-up arg)
         ;; (goto-char (1+ (paredit-current-sexp-start)))
         )
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
            ;; (message "cookie from init %s" year)
            (if (and year (setq year (string-to-number year)) (>= year 2038)) t
              (> (float-time) (float-time (date-to-time exp))))
            ))))



;; setting for auto-complete
;; press - to trigger isearch
;; (setq ac-show-menu-immediately-on-auto-complete t)
;; (global-set-key (kbd "-") 'ac-trigger-isearch)


;; multiple-cursors keybinding
;; (global-set-key (kbd "C-0") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-9") 'mc/skip-to-next-like-this)

(defun my-indent-region (N)
  "Forward indent N*tab."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N tab-width))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-unindent-region (N)
  "Backward indent N*tab."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N (* -1 tab-width)))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(global-set-key ">" 'my-indent-region)
(global-set-key "<" 'my-unindent-region)

(defun my-max-window-size (restore)
  "Max window size of window."
  (interactive "P")
  (let* ((win (or
               (get-buffer-window "*Completions*")
               (get-buffer-window "*Ido Completions*")
               (get-buffer-window nil))) size)
    (when win
      (if restore
          (balance-windows nil)
        (setq size (window-resizable win 200))
        (window-resize win size))
      )
    )
  )


;; custom functions

;; ido to jump to bookmark
;; C-' space is my custom space
(global-set-key [double-mouse-1] 'er/expand-region)
(global-set-key (kbd "M-]") 'syntax-forward-syntax-group)
(global-set-key (kbd "M-[") '(lambda(arg)(interactive "^p") (syntax-forward-syntax-group (* arg -1))))
(global-set-key (kbd "C-' 2") 'split-window-right)
(global-set-key (kbd "C-' x f") 'xah-find-text)
(global-set-key (kbd "C-' n") 'xah-new-empty-buffer)
(global-set-key (kbd "C-' f") 'recentf-open-files)
(global-set-key (kbd "C-' s") 'my/highlight-symbole)
(global-set-key (kbd "C-' TAB") '(lambda()
                                   (interactive)
                                   (if (region-active-p)
                                       (progn (call-interactively 'indent-rigidly))
                                     (call-interactively 'indent-relative))
                                   ))
(define-key global-map (kbd "C-' o") 'locate-current-file-in-explorer)
(define-key global-map (kbd "C-' c") 'cleanup-buffer)

(global-set-key (kbd "C-x r b")
                (lambda ()
                  (interactive)
                  (bookmark-jump
                   (ido-completing-read "Jump to bookmark: " (bookmark-all-names)))))

(global-set-key (kbd "C-' 8")
                (lambda()
                  (interactive)
                  (when fill-prefix (insert-and-inherit fill-prefix))
                  ))

(add-hook 'js2-mode-hook
          (lambda()
            (advice-add 'js2-jump-to-definition
                        :after
                        '(lambda(&optional ARG)
                           "Move to function name when jump to definition."
                           (when (js2r--is-function-declaration (js2-node-at-point))
                               (forward-word 2)
                               )
                           ))
            (advice-add 'js2r-expand-call-args
                        :after
                        '(lambda()
                           "Format for compact expand call args."
                           (when (consp current-prefix-arg)
                             ;; (js2r--goto-closest-call-start) (forward-char) (js2r--ensure-just-one-space)
                             )))
            (define-key js2-mode-map (kbd "C-c C-m C-e") 'js2r-universal-expand)
            (define-key js2-mode-map (kbd "C-c C-m C-c") '(lambda()(interactive)(js2r-universal-expand current-prefix-arg t)))
            (define-key js2-mode-map (kbd "C-c C-m C-.") 'js2-mark-parent-statement)
            (define-key paredit-everywhere-mode-map (kbd "M-]") nil)
            (define-key js2-mode-map (kbd "M-]") '(lambda()(interactive)(call-interactively 'paredit-current-sexp-end) (forward-char) (newline-and-indent)))
            (define-key js2-mode-map (kbd "C-,") '(lambda()(interactive)(call-interactively 'move-end-of-line) (insert ",") (newline-and-indent)))
            (define-key js2-mode-map (kbd "C-M-h") 'js2-mark-defun)
            (define-key js2-mode-map (kbd "C-' ;") 'remove-add-last-comma)
            (define-key js2-mode-map (kbd "C-' a") 'align)
            ))

(define-key global-map (kbd "C-x j") 'standard-format-region)
;; (global-set-key (kbd "C-c C-k") 'copy-line)
;; (global-set-key (kbd "C-x C-k") 'whole-line-or-region-kill-region)
;; (global-set-key (kbd "C-S-k") 'whole-line-or-region-kill-region)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
(define-key global-map (kbd "M-.") 'goto-first-reference)
(global-set-key (kbd "C-M-j") 'delete-indentation)
(global-set-key (kbd "C-S-j") 'join-lines)
;; (global-set-key (kbd "C-j") 'join-lines)
(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-<backspace>") 'kill-backward-symbol)
(global-set-key (kbd "C-S-<backspace>") 'paredit-backward-delete-all)
;; (global-set-key (kbd "C-:") 'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-S-l") 'mark-paragraph)
;; move lines
(global-set-key (kbd "C-x C-n") 'md/move-lines-down)
(global-set-key (kbd "C-x C-p") 'md/move-lines-up)

;; FIX: move-line with region activated
(advice-add 'md/move-line :after '(lambda(&optional N) (deactivate-mark t)))

(after-load 'mc-mark-more
  (define-key mc/keymap "\C-n" 'mc/skip-to-next-like-this)
  (define-key mc/keymap "\C-p" 'mc/skip-to-previous-like-this))




;; (after-load 'auto-complete
;;   (define-key ac-complete-mode-map "\C-n" 'ac-next)
;;   (define-key ac-complete-mode-map "\C-p" 'ac-previous))

;; some short keys for default
;; (define-key global-map "\C-x\C-u" 'undo)
(global-set-key (kbd "C-S-r") 'anzu-query-replace-at-cursor-thing)
(after-load 'init-editing-utils
  (global-set-key [M-up] nil)
  (global-set-key [M-down] nil)

  (advice-add 'backward-up-sexp :before '(lambda(arg)
                                          (push-mark (point) nil nil)
                                          ))

  (define-key global-map (kbd "C-.") 'select-current-pair)
  ;; remmap the old C-M-. is 'find-tag-regexp
  (define-key global-map (kbd "C-M-.") '(lambda(arg)(interactive "P")
                                          (funcall (global-key-binding (kbd "C-.")) arg)
                                          (kill-ring-save (region-beginning) (region-end))
                                          ))
  (define-key global-map (kbd "C-;") 'avy-goto-word-or-subword-1)
  (after-load 'guide-key
    (guide-key-mode -1))
  (after-load 'indent-guide
    (indent-guide-mode -1))
  (cua-selection-mode -1)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (advice-add 'upcase-region
              :around
              '(lambda(oldfun &rest args)
                 "Only apply upcase-region when region active."
                 (when (region-active-p)
                   (apply oldfun args))))
  (advice-add 'downcase-region
              :around
              '(lambda(oldfun &rest args)
                 "Only apply upcase-region when region active."
                 (when (region-active-p)
                   (apply oldfun args)))))

;; use c to create new file in dired
(after-load 'dired
  (define-key dired-mode-map "c" 'find-file)
  )

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


;; smartparents keybinding
;; (global-set-key (kbd "M-]") 'sp-forward-sexp)
;; (global-set-key (kbd "M-[") 'sp-backward-sexp)

;; (global-set-key (kbd "M-S") 'paredit-unwrap) ; paredit-splice-sexp/string
(global-set-key (kbd "C-{") 'my-backward-sexp)
;; (global-set-key (kbd "C-}") 'sp-end-of-sexp)
(global-set-key (kbd "C-M-'") 'paredit-rewrap)

;; (global-set-key (kbd "C-M-<left>") 'sp-forward-slurp-sexp)
;; (global-set-key (kbd "C-M-<right>") 'sp-forward-barf-sexp)
(global-set-key (kbd "M-<right>") 'move-parent-backward-sexp)
(global-set-key (kbd "M-<left>") 'move-parent-forward-symbol)
(global-set-key (kbd "C-M-<left>") 'move-parent-forward-paredit)


;; (global-set-key (kbd "M-] ]") 'paredit-wrap-square)
;; (global-set-key (kbd "M-] }") 'paredit-wrap-curly)

;; replace exist C-<return> cua-set-rectangle-mark
;; (global-set-key (kbd "<C-S-M-return>") 'cua-set-rectangle-mark) ;; buggy!!! will exit transient-mark-mode
(global-set-key (kbd "<M-return>") 'sanityinc/newline-at-end-of-line)
(global-set-key (kbd "<C-O>") 'sanityinc/newline-at-end-of-line)
(global-set-key (kbd "<C-M-return>") 'newline-before)


;; rebinding existing emacs keys

(define-key global-map (kbd "<down>") 'scroll-up-line)
(define-key global-map (kbd "<up>") 'scroll-down-line)
(define-key global-map (kbd "C-x C-6") 'my-max-window-size)

(define-key global-map (kbd "C-s") 'search-selection)
(define-key global-map (kbd "C-S-s") 'swiper-selection)
;; (global-set-key (kbd "C-M-d") 'paredit-forward-down)
;; (global-set-key (kbd "M-D") 'kill-word)
(global-set-key (kbd "C-d") 'my-delete-char-or-word)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "C-k") 'kill-line-or-region)
(global-set-key (kbd "M-k") 'kill-paragraph-or-region)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(global-set-key (kbd "C-h") 'delete-backword-or-ws)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)


;; (setq-default custom-enabled-themes '(sanityinc-solarized-dark))

(defun osx-open-terminal (&optional dir file)
  (interactive)
  (let ((filename (buffer-file-name)))
  (if (and filename (null dir)) (setq dir (file-name-directory filename)))
  (if (and filename (null file)) (setq file (file-name-nondirectory filename)))
  (if dir
      (let ((script         ; Define script variable using revealpath and text.
             (concat
              ;; "delay 1\n"
              "tell application \"Terminal\"\n"
              " activate\n"
              " do script \"cd '" dir "'\" in front window \n"
              " activate\n"
              "end tell\n")))
        (start-process "osascript-getinfo" nil "osascript" "-e" script)))))

(when *is-a-mac*
  ;; bash complete not run on windows
  (require-package 'bash-completion)
  (bash-completion-setup)

  ;; (advice-add 'reveal-in-osx-finder-as :after 'osx-open-terminal)
  (bind-key "C-' d" 'osx-open-terminal)

  ;; when in graphic GUI, set proper window size
  (when (display-graphic-p)
    (if (> (x-display-pixel-width) 1280)
        (progn
          (setq initial-frame-alist '((top . 0) (left . 280) (width . 143) (height . 48)))
          (setq default-frame-alist '((top . 0) (left . 280) (width . 143) (height . 48))))
      (setq initial-frame-alist '((top . 0) (left . 80) (width . 112) (height . 32)))
      (setq default-frame-alist '((top . 0) (left . 80) (width . 112) (height . 32)))))
  )

(when *is-a-windows*
  ;; (defcustom gnutls-trustfiles "./cacert.pem"
  ;;   "gnutls-trustfiles location of cacert.pem."
  ;;   :type '(string)
  ;;   :group 'tools
  ;;   )
  ;; (customize-option 'gnutls-trustfiles)

  (setq grep-command "~/bin/grep.exe")

  (setq tramp-default-method "plinkx")

  (setq w32-lwindow-modifier 'meta)
  (setq w32-rwindow-modifier 'meta)

  (defvar au3-last-window nil
    "Last activated widnow from autoit3.")

  (defun au3-command-winactivate (arg)
    ;; (start-process "nircmd" nil (expand-file-name "~/win32/nircmd.exe") "win" "activate" "ititle" (concat "" arg  ""))
    (start-process "autoit3" nil (expand-file-name "~/win32/AutoIt3.exe") "/AutoIt3ExecuteLine" (concat "WinActivate('[REGEXPTITLE:" arg  "]')"))
    arg
    )

  (defun au3-activate-last()
    (interactive)
    (when au3-last-window
      (au3-command-winactivate au3-last-window))
    )

  (defun au3-activate-tc()
    (interactive)
    (setq au3-last-window
          (au3-command-winactivate "Total Commander")) )

  (defun au3-activate-chrome()
    (interactive)
    (setq au3-last-window
          (au3-command-winactivate "- Google Chrome$") ))

  (defun au3-activate-cmd()
    (interactive)
    (setq au3-last-window
          (au3-command-winactivate "cmd.exe|sh.exe") ))

  (defun au3-activate-xshell()
    (interactive)
    (setq au3-last-window
          (au3-command-winactivate "Xshell") ))

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

  (defun e-fix-max ()
    "Fix max not max bug in windows"
    (interactive)
    (e-normal) (e-maximize))    ; #xf120 normalimize

  ;; (define-key global-map (kbd "C-' C-' d") 'au3-activate-cmd)
  ;; (define-key global-map (kbd "C-' C-' t") 'au3-activate-tc)
  ;; (define-key global-map (kbd "C-' C-' c") 'au3-activate-chrome)
  ;; (define-key global-map (kbd "C-' C-' s") 'au3-activate-xshell)
  ;; (define-key global-map (kbd "C-' C-' '") 'au3-activate-last)

  ;; Start maximised (cross-platf)
  (add-hook 'window-setup-hook 'toggle-frame-maximized t)

  ;; (global-set-key (kbd "M-SPC M-x") 'emacs-maximize)

  ;; exec autoit
  )



;; ////////////////////////////////////////
;; define some useful macro

;; save to remote custom file
(fset 'my-macro-save-to-remote-dotfile-custom
   [?\C-x ?h ?\M-w ?\C-x ?p ?p ?d ?o ?t ?f ?i ?l ?e ?s return ?i ?n ?i ?t ?- ?l ?o return ?\C-x ?h ?\C-y ?\C-x ?\C-s])

;; save custom.el into remote
(define-key global-map (kbd "C-' m s") 'my-macro-save-to-remote-dotfile-custom)


(provide 'init-local)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init-local.el ends here
