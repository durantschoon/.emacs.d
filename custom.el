(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#383838")
 '(fill-column 80)
 '(go-guru-scope "...")
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "log" "tmp")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.elc" "*.lof" "*.glo" "*.idx" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.fasl" "*.ufsl" "*.fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.sassc" "*.png" "*.xcf" "*.sqlite*" "*.jar")))
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (color-theme-modern doom-modeline all-the-icons company-anaconda avy use-package-chords expand-region indium indium-mode projectile-speedbar helm-etags-plus yasnippet-snippets json-mode anaconda-mode emojify emojify-mode buffer-move go-guru helm-company gore-mode company-go company company-mode autopair go-mode solidity-mode pcre2el color-theme-sanityinc-tomorrow color-theme helm-swoop helm-ag ag helm-projectile helm visual-regexp-steroids visual-regexp url-shortener phi-search-mc rainbow-mode osx-plist org-jira clojure-mode octave-mode yasnippet yaml-mode web-mode use-package smartscan sass-mode python-mode projectile pp-c-l phi-search org-bullets multiple-cursors monokai-theme markdown-mode magit livescript-mode js2-mode ido-vertical-mode flycheck exec-path-from-shell emmet-mode edit-server cyberpunk-theme csharp-mode coffee-mode cl-lib-highlight)))
 '(safe-local-eval-forms
   (quote
    ((add-hook
      (quote write-file-hooks)
      (quote time-stamp))
     (add-hook
      (quote write-file-functions)
      (quote time-stamp))
     (add-hook
      (quote before-save-hook)
      (quote time-stamp)
      nil t)
     (add-hook
      (quote before-save-hook)
      (quote delete-trailing-whitespace)
      nil t)
     (turn-off-auto-fill))))
 '(solarized-termcolors 256 t)
 '(vc-directory-exclusion-list
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))))
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
 '(show-paren-match ((t (:background "default" :foreground "#afa" :weight ultra-bold))))
 '(show-paren-mismatch ((t (:background "default" :foreground "#cc6666" :weight ultra-bold))))
 '(which-func ((t (:foreground "orange")))))
