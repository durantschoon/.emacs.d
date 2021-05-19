;;; Begin initialization

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	      '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (package-initialize) ; no longer needed

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-verbose t)

;;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))
(put 'scroll-left 'disabled nil)
(put 'magit-diff-edit-hunk-commit 'disabled nil)
