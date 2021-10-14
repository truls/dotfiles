(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(display-line-numbers-width 1)
 '(org-agenda-files
   '("~/Nextcloud/bibliography/notes.org" "~/Nextcloud/org/todo.org" "~/Nextcloud/org/gcal.org"))
 '(safe-local-variable-values
   '((blacken-skip-string-normalization . t)
     (blacken-allow-py36 . t)
     (blacken-line-length . 100)
     (TeX-master \` `main)
     (TeX-command-extra-options . "-shell-escape")
     (TeX-master . main)
     (eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (lsp-python-ms-extra-paths "/home/truls/uni/teaching/algdat-tdt4120/inginious-installed")
     (lsp-python-ms-extra-paths vector "/home/truls/uni/teaching/algdat-tdt4120/inginious-installed")
     (lsp-python-ms-extra-paths quote
                                ("/home/truls/uni/teaching/algdat-tdt4120/inginious-installed"))
     (lsp-python-ms-extra-paths . "/home/truls/uni/teaching/algdat-tdt4120/inginious-installed")
     (lsp-python-ms-dir . "/home/truls/uni/teaching/algdat-tdt4120/inginious-installed")
     (TeX-command-extra-options . "-enable-write18")
     (sh-indent-for-case-alt . +)
     (sh-indent-for-case-label . 0)
     (lexical-scoping . t)
     (nameless-current-name . "biblio")
     (haskell-stylish-on-save)
     (eval bibtex-set-dialect 'biblatex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-info ((t (:foreground "pale turquoise" :height 1.5))))
 '(org-document-title ((t (:foreground "pale turquoise" :weight bold :height 2.0))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-tag ((t (:weight bold :height 0.5))))
 '(org-todo ((t (:foreground "Pink" :weight bold)))))
