;;; custom.el --- based on purcell's .emacs.d  -*- no-byte-compile: t; -*-

;; Copyright (C) 2016 Micheal.Yang

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

(defconst *is-a-windows* (eq system-type 'windows-nt))

(when *is-a-windows*

  ;; UTF-8 settings
  ;; (set-language-environment "UTF-8")
  ;; (prefer-coding-system 'utf-8)

  (set-language-environment 'chinese-gbk)
  (prefer-coding-system 'utf-8-auto)

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

;; (setq debug-on-error t)

;; prevent Chinese date problems
(setq system-time-locale "C")

(when *is-a-mac*
  ;; using TCP instead of UNIX socket to start server
  (setq server-auth-dir "/tmp/emacsserver")
  (setq server-use-tcp t)
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
(editorconfig-mode -1)

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
(add-to-list 'load-path "~/.emacs.d/download/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/download/org-mode/contrib/lisp" t)
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

;; Some short cuts function
(defun insert-earmuffs (char &optional insert-normal-p)
  "Insert earmuffs for char."
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert char)
        (goto-char start)
        (insert char)
        )
    (let ((pointer-position (point)))
      (if (or insert-normal-p (bolp) (string= (string (char-before)) char) )
          (insert char)
        (if (looking-back "[^ \t\n]" 1)
            (if (not (string= (string (char-after)) char))
                (insert char)
              (forward-char))
          (progn
            (insert (format "%s%s " char char) )
            (goto-char (+ 1 pointer-position)))))
      )))

(defun delete-earmuffs (char-list)
  "Delete earmuffs based on earmuff-char-list var."
  (let ((not-found t))
    (dolist (char char-list)
      (when (and not-found (string= (string (char-before)) char) (string= (string (char-after)) char) )
        (delete-forward-char 1 nil)
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
            (defvar earmuff-char-list '("*" "=" "/" "~" "+" "_")
              "Earmuff chars for Org-mode.")
            (dolist (char earmuff-char-list)
              (define-key org-mode-map (kbd char) `(lambda (arg) (interactive "P") (insert-earmuffs ,char arg))))
            (define-key org-mode-map (kbd "<backspace>") '(lambda () (interactive) (delete-earmuffs earmuff-char-list)))
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


(after-load 'emmet-mode
  ;; (-each '("山东" "北京" "上海" "武汉")
  ;;   (lambda(item) (push item emmet-lorem-words)))
  )

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


;; github/magnars
(require-package 's)
(require-package 'dash)
(require-package 'tagedit)
(after-load 'tagedit
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

  (define-key tagedit-mode-map (kbd "M-'") 'te/goto-current-tag-content)
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

(eval-after-load "sgml-mode"
  '(progn
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda ()
                                 (tagedit-mode 1)
                                 (tagedit-add-experimental-features)
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
(require-package 'company-tern)
(require-package 'company-quickhelp)
(require-package 'company-dict)
(require-package 'company-restclient)
(require-package 'company-go)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-limit 99)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-minimum-prefix-length 2)
(after-load 'company
  (company-flx-mode +1)
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
  (define-key company-active-map (kbd "<SPC>") '(lambda()(interactive) (company-abort) (insert " ")))
  (define-key company-active-map (kbd "C-j") 'company-abort)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map "\C-n" 'company-select-next-or-abort)
  (define-key company-active-map "\C-p" 'company-select-previous-or-abort)
  )
(after-load 'company-tern
  (add-to-list 'company-backends 'company-tern)
  ;; (setq company-tern-property-marker "")
  )
(after-load 'company-go
  (add-to-list 'company-backends 'company-go)
  )

(require-package 'sws-mode)
(require-package 'jade-mode)
(require-package 'stylus-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))


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
     (require-package 'tern-auto-complete)
     ;; (tern-ac-setup)
     ))

(global-set-key (kbd "<f8>") 'flycheck-mode)
(add-hook 'js-mode-hook
          (lambda()
            (tern-mode +1)
            ) )
(add-hook 'js2-mode-hook
          (lambda ()
            (set (make-local-variable 'page-delimiter) "//\f")
            (tern-mode +1)
            (form-feed-mode t)
            (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
            (define-key js2-mode-map "@" 'js-doc-insert-tag)
            (define-key js2-mode-map (kbd "C-c C-m be") 'web-beautify-js)
            (define-key js2-mode-map (kbd "C-M-i") 'company-tern)
            (define-key js2-mode-map (kbd "C-' c") 'standard-format-buffer)
            (define-key js2-mode-map (kbd "<M-return>") '(lambda(arg)(interactive "P")
                                                           (let ((node (js2-comment-at-point)) start len comment-start)
                                                             (when node
                                                               (setq start (1+ (aref node 2) ))
                                                               (setq len (+ start (aref node 3)))
                                                               (setq comment-start (buffer-substring start (+ start 2)) )
                                                               (sanityinc/newline-at-end-of-line)
                                                               (if (string= comment-start "/*")
                                                                   (insert "* ")
                                                                 (insert "// ")))
                                                             (when (null node)
                                                               (sanityinc/newline-at-end-of-line)
                                                               ))
                                                           ))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy nil)
 ;; using M-s SPC to toggle loose/lax
 '(search-whitespace-regexp ".*?")      ;space seperated multi key word instead of "\\s-+"
 '(avy-timeout-seconds 0.4)
 '(avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y))
 ;; '(avy-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
 '(column-number-mode t)
 '(enable-dir-local-variables t)
 '(cua-enable-cua-keys nil)
 '(cua-mode nil nil (cua-base))
 '(display-buffer-reuse-frames t)
 '(safe-local-variable-values '((no-byte-compile t)))
;; '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(url-automatic-caching t))

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
(defun trigger-isearch-when-focus ()
  (if (and (string= "*Open Recent*" (buffer-name)) )
      (isearch-mode t nil nil nil)
    )
  )
(add-hook 'recentf-dialog-mode-hook 'trigger-isearch-when-focus)
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
       (let ((old (point)))
         (goto-char (1+ (paredit-current-sexp-start)))
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
  (define-key paredit-mode-map (kbd "C-d") nil)
  (define-key paredit-everywhere-mode-map (kbd "C-d") nil)

  (define-key paredit-mode-map (kbd "M-}") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-}") nil)

  (define-key paredit-mode-map (kbd "C-M-}") 'paredit-current-sexp-end)
  (define-key paredit-everywhere-mode-map (kbd "C-M-}") 'paredit-current-sexp-end)

  (define-key paredit-mode-map (kbd "C-M-{") 'paredit-current-sexp-start)
  (define-key paredit-everywhere-mode-map (kbd "C-M-{") 'paredit-current-sexp-start)
  )
(add-hook 'js2-mode-hook 'enable-paredit-mode)
(add-hook 'html-mode-hook 'enable-paredit-mode)
(add-hook 'web-mode-hook 'enable-paredit-mode)


(defun js2r-universal-expand(arg)
  "Expand or contract bracketed list using js2r.
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
    (when (= pos pos-array) (if arg (js2r-contract-array) (js2r-expand-array)))
    (when (= pos pos-object) (if arg (js2r-contract-object) (js2r-expand-object)))
    (when (= pos pos-function) (if arg (js2r-contract-function) (js2r-expand-function)))
    (when (= pos pos-call) (if arg (js2r-contract-call-args) (js2r-expand-call-args)))
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
(define-key search-map "hu" #'my/unhighlight-all-in-buffer)
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


(add-to-list 'load-path (expand-file-name "standard" user-emacs-directory))
(require 'init-js-standard)

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
  (save-excursion
    (goto-char (paredit-current-sexp-end))
    (if (eq arg 1)(kill-sexp 1)
      (if (eq arg 2) (let ((char (read-char-exclusive "input a char to zap:")))
                       (if (eq char 13)
                           (kill-line)
                         (zap-up-to-char 1 char))
                       )
        (if (eq arg 3) (paredit-kill 1))
        )
      )
    (backward-char 1)
    (yank)
    (pop kill-ring)
    (setq kill-ring-yank-pointer kill-ring)
    ))

(defun move-parent-backward (arg)
  (interactive "p")
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
    (pop kill-ring)
    (setq kill-ring-yank-pointer kill-ring)
    )
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
(global-set-key (kbd "M-]") 'syntax-forward-syntax-group)
(global-set-key (kbd "M-[") '(lambda(arg)(interactive "^p") (syntax-forward-syntax-group (* arg -1))))
(global-set-key (kbd "C-' 2") 'split-window-right)
(global-set-key (kbd "C-' x f") 'xah-find-text)
(global-set-key (kbd "C-' f") 'recentf-open-files)
(global-set-key (kbd "C-' s") 'highlight-symbol-at-point)
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
            (advice-add 'js2r-expand-call-args
                        :after
                        '(lambda() (js2r--goto-closest-call-start) (forward-char) (js2r--ensure-just-one-space) ))
            (define-key js2-mode-map (kbd "C-c C-m C-e") 'js2r-universal-expand)
            (define-key js2-mode-map (kbd "C-c C-m C-c") '(lambda()(interactive)(js2r-universal-expand t)))
            (define-key paredit-everywhere-mode-map (kbd "M-]") nil)
            (define-key js2-mode-map (kbd "M-]") '(lambda()(interactive)(call-interactively 'paredit-current-sexp-end) (forward-char) (newline-and-indent)))
            (define-key js2-mode-map (kbd "C-M-h") 'js2-mark-defun)
            (define-key js2-mode-map (kbd "C-\"") 'js2-mark-parent-statement2)
            (define-key js2-mode-map (kbd "C-x C-;") 'remove-add-last-comma)
            (define-key js2-mode-map (kbd "C-' l") 'align)
            ))

(define-key global-map (kbd "C-x j") 'standard-format-region)
(global-set-key (kbd "C-c C-k") 'copy-line)
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
(global-set-key (kbd "C-:") 'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-S-l") 'mark-paragraph)
;; move lines
(global-set-key (kbd "C-x C-n") 'md/move-lines-down)
(global-set-key (kbd "C-x C-p") 'md/move-lines-up)

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

  (define-key global-map (kbd "C-.") 'select-current-pair)
  ;; remmap the old C-M-. is 'find-tag-regexp
  (define-key global-map (kbd "C-M-.") '(lambda(arg)(interactive "P")
                                          (funcall (global-key-binding (kbd "C-.")) arg)
                                          (kill-ring-save (region-beginning) (region-end))
                                          ))
  (define-key global-map (kbd "C-;") 'avy-goto-char-2)
  (guide-key-mode -1)
  (cua-selection-mode -1)
  (put 'upcase-region 'disabled t)
  (put 'downcase-region 'disabled t)
  )

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
(global-set-key (kbd "C-M-d") 'kill-forward-symbol)
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

(when *is-a-mac*
  ;; bash complete not run on windows
  (require-package 'bash-completion)
  (bash-completion-setup)

  (advice-add 'reveal-in-osx-finder-as :after
              '(lambda(&optional dir file)
                 (if dir
                     (let ((script         ; Define script variable using revealpath and text.
                            (concat
                             "delay 1\n"
                             "set thePath to POSIX file \"" dir "\"\n"
                             "tell application \"Terminal\"\n"
                             " activate\n"
                             " do script \"cd '" dir "'\" in front window \n"
                             " activate\n"
                             "end tell\n")))
                       (start-process "osascript-getinfo" nil "osascript" "-e" script)))))

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

  (setq tramp-default-method "plink")

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

  (define-key global-map (kbd "C-' C-' d") 'au3-activate-cmd)
  (define-key global-map (kbd "C-' C-' t") 'au3-activate-tc)
  (define-key global-map (kbd "C-' C-' c") 'au3-activate-chrome)
  (define-key global-map (kbd "C-' C-' s") 'au3-activate-xshell)
  (define-key global-map (kbd "C-' C-' '") 'au3-activate-last)

  ;; Start maximised (cross-platf)
  (add-hook 'window-setup-hook 'toggle-frame-maximized t)

  ;; (global-set-key (kbd "M-SPC M-x") 'emacs-maximize)

  ;; exec autoit
  )



;; ////////////////////////////////////////
;; define some useful macro

;; save to remote custom file
(fset 'my-macro-save-to-remote-dotfile-custom
   [?\C-x ?h ?\M-w ?\C-x ?p ?p ?d ?o ?t ?f ?i ?l ?e ?s return ?c ?u ?t ?o ?m return ?\C-x ?h ?\C-y ?\C-x ?\C-s])

;; save custom.el into remote
(define-key global-map (kbd "C-' m s") 'my-macro-save-to-remote-dotfile-custom)


(provide 'custom)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; custom.el ends here
