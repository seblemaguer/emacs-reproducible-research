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

;; == Main configuration part
;; Download if not exists
(unless (file-exists-p "~/.emacs.d/main.org")
  (url-copy-file
   "https://raw.githubusercontent.com/seblemaguer/emacs-reproducible-research/master/assets/configuration/main.org"
   "~/.emacs.d/main.org" t t t))

;; And now we actually load the main part
(use-package org
  :ensure t
  :config
  (use-package org-plus-contrib :ensure t))

;; Initialise everything now
(org-babel-load-file "~/.emacs.d/main.org")
