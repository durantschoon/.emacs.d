(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "log" "tmp")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.elc" "*.lof" "*.glo" "*.idx" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.fasl" "*.ufsl" "*.fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.sassc" "*.png" "*.xcf" "*.sqlite*" "*.jar")))
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (helm-swoop helm-ag ag helm-projectile helm visual-regexp-steroids visual-regexp url-shortener phi-search-mc rainbow-mode osx-plist org-jira clojure-mode octave-mode yasnippet yaml-mode web-mode use-package smartscan sass-mode python-mode projectile pp-c-l phi-search org-bullets multiple-cursors monokai-theme markdown-mode magit livescript-mode js2-mode ido-vertical-mode flycheck exec-path-from-shell emmet-mode edit-server cyberpunk-theme csharp-mode coffee-mode cl-lib-highlight)))
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
 '(vc-directory-exclusion-list
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
