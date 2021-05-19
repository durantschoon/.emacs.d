(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(fci-rule-color "#383838")
 '(fill-column 80)
 '(flycheck-flake8rc ".flake8")
 '(go-guru-scope "...")
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "log" "tmp"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.elc" "*.lof" "*.glo" "*.idx" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.fasl" "*.ufsl" "*.fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.sassc" "*.png" "*.xcf" "*.sqlite*" "*.jar"))
 '(line-number-mode nil)
 '(org-tag-faces
   '(("github" . "SpringGreen3")
     ("jira" . "SpringGreen3")
     ("python" . "SpringGreen3")
     ("javascript" . "SpringGreen3")))
 '(org-tags-column 0)
 '(package-selected-packages
   '(bind-key esup company-emoji company-terraform terraform-mode terraform rg dockerfile-mode prettier-js js-doc blacken pyenv-mode-auto auto-virtualenv helm-idris idris-mode company-tern tern-auto-complete tern xref-js2 git-link rjsx-mode color-theme-modern doom-modeline all-the-icons company-anaconda avy use-package-chords expand-region indium indium-mode projectile-speedbar helm-etags-plus yasnippet-snippets json-mode anaconda-mode emojify emojify-mode buffer-move go-guru helm-company gore-mode company-go company company-mode autopair go-mode solidity-mode pcre2el color-theme-sanityinc-tomorrow color-theme helm-swoop helm-ag ag helm-projectile helm visual-regexp-steroids visual-regexp url-shortener phi-search-mc rainbow-mode osx-plist org-jira clojure-mode octave-mode yasnippet yaml-mode web-mode use-package smartscan sass-mode python-mode projectile pp-c-l phi-search org-bullets multiple-cursors monokai-theme markdown-mode magit livescript-mode js2-mode ido-vertical-mode flycheck exec-path-from-shell emmet-mode edit-server cyberpunk-theme csharp-mode coffee-mode cl-lib-highlight))
 '(projectile-mode t)
 '(safe-local-eval-forms
   '((add-hook 'write-file-hooks 'time-stamp)
     (add-hook 'write-file-functions 'time-stamp)
     (add-hook 'before-save-hook 'time-stamp nil t)
     (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
     (turn-off-auto-fill)))
 '(solarized-termcolors 256 t)
 '(vc-directory-exclusion-list
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(go-guru-hl-identifier-face ((t (:inherit highlight :background "OrangeRed1"))))
 '(helm-selection ((t (:inherit highlight :background "gray31"))))
 '(mode-line ((t (:background "blue4" :foreground "gray90"))))
 '(mode-line-buffer-id ((t (:foreground "gold1" :weight ultra-bold))))
 '(mode-line-inactive ((t (:background "#404045" :foreground "gray60"))))
 '(org-block ((t (:background "#000000"))))
 '(org-block-background ((t (:background "#000000"))))
 '(org-block-begin-line ((t (:foreground "#008ED1" :background "#002E41"))))
 '(org-block-end-line ((t (:foreground "#008ED1" :background "#002E41"))))
 '(show-paren-match ((t (:background "default" :foreground "#afa" :weight ultra-bold))))
 '(show-paren-mismatch ((t (:background "default" :foreground "#cc6666" :weight ultra-bold))))
 '(which-func ((t (:foreground "orange")))))
