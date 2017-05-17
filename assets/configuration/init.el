;; ============================================================================================
;; Preconfiguration
;; ============================================================================================
;; == Subconfiguration file
;; Isolate custom
(setq custom-file (concat "~/.emacs.d/custom.el"))
(load custom-file 'noerror)

;; Isolate private variables
(when (file-exists-p "~/.emacs.d/private-variables.el")
  (load-file "~/.emacs.d/private-variables.el"))

;; == Package part
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("local-melpa" . "http://localhost/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("orgmode" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Adding support for use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)

;; Paradox for a better listing
(use-package paradox
  :ensure t
  :config
  (setq paradox-spinner-type 'progress-bar))

;; ============================================================================================
;; Global part
;; ============================================================================================

;; == Startup cleaning
(setq inhibit-startup-message t)
(setq find-file-suppress-same-file-warnings t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defun custom-startup ()
  (org-agenda-list 1)
  (switch-to-buffer "*Org Agenda*"))
(add-hook 'window-setup-hook 'custom-startup)

;; == Uncoding
(setq system-time-locale "en_US.utf8")  ; "C"?
(add-to-list 'file-coding-system-alist
             '("\\.owl\\'" utf-8 . utf-8))
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; == Undo(ing)
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; == Recentf
(use-package recentf
  :config
  (setq recentf-max-menu-items 100)
  (recentf-mode 1)

  ;; Exclude file from recentf
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.ido\\.last" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.recentf" (getenv "HOME")))
  (add-to-list 'recentf-exclude ".*todo.org"))

;; == Backing up
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
  It disables backup creation and auto saving.

  With no argument, this command toggles the mode.
  Non-null prefix argument turns on the mode.
  Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1))) ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited) ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))
(add-to-list 'auto-mode-alist '("\\.gpg$" . sensitive-mode))

;; Starting backup
;; (backups-mode-start)

;; == Copy/paste
(setq mouse-drag-copy-region nil)
(setq x-select-enable-primary nil)
(setq x-select-enable-clipboard t)
(setq select-active-regions t)

;; == Spelling
(setq-default ispell-program-name "aspell")

;; == Minibuffer
(setq read-file-name-completion-ignore-case t
      completion-ignore-case t
      resize-mini-windows t)
(file-name-shadow-mode 1)
(icomplete-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; ============================================================================================
;; Buffer
;; ============================================================================================

;; Open Large file
(use-package vlf :ensure t)

;; Delete trailing-whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Unify the buffer name style
(eval-after-load "uniquify"
  '(progn
     (setq uniquify-buffer-name-style 'forward)))

(use-package ibuffer
  :config

  ;; Some ibuffer subpackage
  (use-package ibuffer-git :ensure t)
  (use-package ibuffer-tramp :ensure t)

  ;; Readable size column
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Last modification time column
  (defun get-modification-time-buffer(b)
    "Retrieve the savetime of the given buffer"
    (if (buffer-file-name b)
        (format-time-string "%Y-%m-%d %H:%M:%S"
                            (nth 5 (file-attributes (buffer-file-name b))))
      ""))
  (define-ibuffer-column last-modification
    (:name "Last modification time" )
    (get-modification-time-buffer buffer))

  ;; Format line
  (setq ibuffer-formats
        '((mark modified read-only
                (name 30 30 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 20 20 :left :elide) " "
                ;; (eproject 16 16 :left :elide)      " "
                (git-status 8 8 :left)" "
                (last-modification 30 30 :left :elide)  " "
                filename-and-process)))
  (defadvice ibuffer-update (around ibuffer-preserve-prev-header activate)
    "Preserve line-header used before Ibuffer if it doesn't set one"
    (let ((prev-line-header header-line-format))
      ad-do-it
      (unless header-line-format
        (setq header-line-format prev-line-header)))
    )

  ;; Marking buffer
  (setq ibuffer-marked-char ?✓)

  ;;  Sorting
  (setq ibuffer-default-sorting-mode 'major-mode)

  ;; Ignore empty groups
  (setq ibuffer-show-empty-filter-groups nil)

  ;; ;; Auto revert
  ;; (add-hook 'ibuffer-mode-hook
  ;;           (lambda ()
  ;;             (my-own-ibuffer-hook)
  ;;             (ibuffer-switch-to-saved-filter-groups "default")
  ;;             (ibuffer-update nil t)))

  ;; Always put the cursor to the origin buffer
  (defun ibuffer-jump-to-last-buffer ()
    (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list)))))
  (add-hook 'ibuffer-hook #'ibuffer-jump-to-last-buffer)

  ;; Some user functions

  ;; Some keyboards
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  )

;; ============================================================================================
;; File / Directory
;; ============================================================================================
;; == Dired configuration
(use-package dired
  :config

  ;; Subpackage
  (use-package dired-dups :ensure t)
  (use-package dired-efap :ensure t)
  (use-package dired-explorer :ensure t)
  (use-package dired-hacks-utils :ensure t)
  (use-package dired-narrow :ensure t)
  (use-package dired-rainbow :ensure t)
  (use-package dired-ranger :ensure t)
  (use-package dired-subtree :ensure t)
  (use-package dired-toggle-sudo :ensure t)
  (use-package diredful :ensure t)
  (use-package dired-single :ensure t)
  (use-package dired+ :ensure t)
  (use-package peep-dired :ensure t)

  ;; ls adaptation
  (when (eq system-type 'darwin)
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program t)
    (setq insert-directory-program "/usr/local/bin/gls"))
  (setq dired-listing-switches "--group-directories-first -alh")

  ;; Ignoring details by default (can be reactivate after)
  (if (boundp 'ls-lisp-ignore-case)
      (setq ls-lisp-ignore-case t))
  (if (boundp 'ls-lisp-dirs-first)
      (setq ls-lisp-dirs-first t))
  (if (boundp 'ls-lisp-format-time-list)
      (setq ls-lisp-format-time-list
	    '("%Y-%m-%d %H:%M"
	      "%Y-%m-%d %H:%M")))
  (if (boundp 'ls-lisp-use-localized-time-format)
      (setq ls-lisp-use-localized-time-format t))

  (add-hook 'dired-load-hook
	    (lambda ()
	      (load "dired-column-widths.el")))
  (setq dired-details-hidden-string "[...] ")

  ;; FIXME: what is that:
  (setq dired-dwim-target t
	dired-recursive-deletes 'top
	dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Omitting
  (setq-default dired-omit-files "^\\.[^.]+")
  (setq-default dired-omit-mode t)

  ;; Compression mode
  (setq auto-compression-mode t)

  ;; Global keys
  (global-set-key (kbd "C-x C-d") 'dired))

;; == Diff
(setq diff-switches "-u")
(autoload 'diff-mode "diff-mode" "Diff major mode" t)
(setq ediff-auto-refine-limit (* 2 14000))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (lambda (&optional arg)
                    (if (> (frame-width) 160)
                    (split-window-horizontally arg)
		    (split-window-vertically arg))))

;; == Image/compressed files
(use-package image+
  :ensure t
  :config
  (use-package image-dired+ :ensure t)
  (setq auto-image-file-mode t))

;; == Tramp
(use-package tramp
  :config
  ;; Global configuration
  (setq tramp-default-method "ssh")
  (setq password-cache-expiry 60)
  (setq tramp-auto-save-directory temporary-file-directory)

  ;; Debug
  ;;(setq tramp-verbose 9)
  (setq tramp-debug-buffer nil))

;; ============================================================================================
;; Completion
;; ============================================================================================
;; == Global completion
(use-package helm
  :ensure t
  :config
  (use-package helm-package :ensure t)

  ;; Some key
  (global-set-key (kbd "C-x C-r") 'helm-recentf))

(use-package counsel :ensure t)

;; == Inner completion
(use-package company
  :ensure t
  :config

  ;; Baseline configuration
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 20
        company-etags-ignore-case t)

  ;; Faces
  (unless (face-attribute 'company-tooltip :background)
    (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
    (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
    (set-face-attribute 'company-preview nil :background "black")
    (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
    (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
    (set-face-attribute 'company-scrollbar-fg nil :background "gray40"))

  ;; Default backends
  (setq company-backends
        '((company-files
           company-yasnippet)))

  ;; Activation
  (global-company-mode t))

;; == Snippets
(use-package yasnippet
  :ensure t
  :config

  ;; Activation
  (yas-global-mode))

;; == Templates
(use-package yatemplate
  :ensure t
  :config
  ;; (setq yatemplate-dir (concat config-basedir "/third_parties/templates"))
  (setq auto-insert-alist nil)
  (yatemplate-fill-alist))

;; ============================================================================================
;; Project management
;; ============================================================================================
;; == Project management
(use-package projectile
  :ensure t
  :config

  ;; Global configuration
  (setq projectile-switch-project-action 'neotree-projectile-action
        projectile-enable-caching t
        projectile-create-missing-test-files t
        projectile-switch-project-action #'projectile-commander
        projectile-ignored-project-function 'file-remote-p
        projectile-mode-line " P")

  ;; Helpers command
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    ;; This requires a snapshot version of Projectile.
    (projectile-run-shell))

  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (projectile-compile-project nil))

  (def-projectile-commander-method ?\C-?
    "Go back to project selection."
    (projectile-switch-project))

  (def-projectile-commander-method ?F
    "Git fetch."
    (magit-status)
    (if (fboundp 'magit-fetch-from-upstream)
        (call-interactively #'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-current)))

  (def-projectile-commander-method ?j
    "Jack-in."
    (let* ((opts (projectile-current-project-files))
           (file (ido-completing-read
                  "Find file: "
                  opts
                  nil nil nil nil
                  (car (cl-member-if
                        (lambda (f)
                          (string-match "core\\.clj\\'" f))
                        opts)))))
      (find-file (expand-file-name
                  file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)
      (cider-jack-in)))

  ;; Globally enabling
  (projectile-global-mode)

  ;; Keymap
  (setq projectile-keymap-prefix (kbd "C-x p"))
  )
(use-package helm-projectile :ensure t)

;; == Git
(use-package magit :ensure t)

;; Magit helpers
(use-package magit-annex :ensure t)
(use-package magit-filenotify :ensure t)
(use-package magit-find-file :ensure t)
(use-package magit-gerrit :ensure t)
(use-package magit-gh-pulls :ensure t)
(use-package magit-gitflow :ensure t)
(use-package magit-popup :ensure t)
(use-package magit-rockstar :ensure t)

;; Magit interfaced with github
(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

;; Gitconfig edition mode
(use-package gitconfig-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("/\.gitconfig\'"   . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("/vcs/gitconfig\'" . gitconfig-mode)))

;; Git timemachine
(use-package git-timemachine :ensure t)


;; == Mercurial
(use-package monky :ensure t)
(use-package hgignore-mode :ensure t)
(use-package hgrc-mode :ensure t)

;; ============================================================================================
;; Compilation
;; ============================================================================================

;; == General
(if (boundp 'compile-auto-highlight)
	(progn
	  (setq compile-auto-highlight t)
	  (setq compilation-finish-functions 'highlight-error-lines)
	  )
  )

;; == On the fly checking
(use-package flycheck :ensure t)
(use-package flycheck-stack :ensure t)

;; ;; == Gradle [FIXME]
;; (use-package gradle-mode
;;   ;; FIXME: change that !
;;   :load-path (lambda () (format "%s/subpart/emacs-gradle-mode" config-basedir))
;;   :config
;;   (setq gradle-gradlew-executable "./gradlew")
;;   (setq gradle-use-gradlew t)
;;   (gradle-mode)
;;   )

;; ============================================================================================
;; Languages
;; ============================================================================================

;; == Java/Groovy/Gradle
(use-package meghanada
  :ensure t
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              ;; meghanada-mode on
              (meghanada-mode t)
              (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  )
(use-package javadoc-lookup
  :ensure t
  :config
  (when (file-exists-p "/usr/share/doc/openjdk-8-jdk/api")
    (javadoc-add-roots "/usr/share/doc/openjdk-8-jdk/api"))

  (javadoc-add-artifacts [org.lwjgl.lwjgl lwjgl "2.8.2"]
                         [com.nullprogram native-guide "0.2"]
                         [org.apache.commons commons-math3 "3.0"]))

(use-package groovy-mode
  :ensure t
  :config

  ;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
  (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
  (autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
  (autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

  ;; Adding groovy keys
  (add-hook 'groovy-mode-hook
            '(lambda ()
               (inf-groovy-keys)))

  ;; ;; can set groovy-home here, if not in environment
  ;; (setq inferior-groovy-mode-hook
  ;;       '(lambda()
  ;;          (setq groovy-home "/Users/sclayman/Downloads/groovy-1.7.1/")))

  ;; .groovy/.gradle files should be in groovy-mode
  (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

  ;; This _might_ not work with Aquamacs (not sure what value it offers)
  (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
  (add-to-list 'interpreter-mode-alist '("gradle" . groovy-mode))
  )

(use-package groovy-imports :ensure t)

;; == Python

;; == R
(use-package ess :ensure t)
(use-package ess-R-data-view :ensure t)
(use-package ess-R-object-popup :ensure t)
(use-package ess-smart-equals :ensure t)
(use-package ess-smart-underscore :ensure t)
(use-package ess-view :ensure t)
(use-package company-statistics :ensure t)

;; ============================================================================================
;; Org-mode
;; ============================================================================================

;; == TODO part

;; == Calendar / Agenda

;; == Project/Org-mode agenda/todo list bindings
(use-package org-projectile
  :ensure t
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org")
  (setq org-agenda-files
        (append org-agenda-files (org-projectile:todo-files))))

;; == Publishing

;; ============================================================================================
;; Mode line part
;; ============================================================================================

;; == Theme (github)
(use-package github-theme
  :ensure t
  :config
  (load-theme 'github t))

;; == TODO buffer part
;; Parenthesis
(use-package smartparens
  :ensure t
  :config

  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)

  (sp-pair "'" nil :actions :rem))

(use-package rainbow-delimiters
  :ensure t
  :config

  (custom-set-faces
   '(rainbow-delimiters-mismatched-face ((t (:foreground "white" :background "red" :weight bold))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "red" :weight bold))))

   ;; show parents (in case of rainbow failing !)
   '(show-paren-match ((t (:foreground "white" :background "green" :weight bold))))
   '(show-paren-mismatch ((t (:foreground "white" :background "red" :weight bold))))
   )

  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Line/fringe
(use-package nlinum
  :ensure t
  :config
  (line-number-mode t)
  (column-number-mode t)
  (global-nlinum-mode t)
  )

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(size-indication-mode t)
(fringe-mode 10)

;; == Mode line
(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :init

  ;; Separator configuration
  (setq spaceline-all-the-icons-separator-type 'wave
        spaceline-separator-dir-left '(left . left)
        spaceline-separator-dir-right '(right . right)
        spaceline-minor-modes-separator " ")

  ;; Spaceline loading
  (spaceline-all-the-icons-theme)

  ;; Some  configurations
  (require 'spaceline-config)
  (spaceline-helm-mode)
  (use-package info+
    :ensure t
    :init
    (spaceline-info-mode))

  ;; Print the battery status
  (use-package fancy-battery
    :ensure t
    :init
    (add-hook 'after-init-hook #'fancy-battery-mode)
    (display-battery-mode -1))

  ;; Paradox support for the mode line
  (spaceline-all-the-icons--setup-paradox))
