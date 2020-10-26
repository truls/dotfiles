;; lang-mode-config.el -*- lexical-binding: t -*-
;;
;; Miscellaneous languages
;;

(use-package mutt-mode
  :ensure t
  :mode ("\\.muttrc\\'"
         "\\.neomuttrc\\'"
         "\\neomuttrc\\'")
  :commands mutt-mode)

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile"))

(use-package sh-script
  :no-require t
  :config
  (add-hook 'sh-mode-hook #'flycheck-mode))

(use-package c-mode
  :no-require t
  :config
  ;; C-mode hook to avoid indenting after extern "C"
  (add-hook 'c-mode-common-hook
            (lambda()
              (c-set-offset 'inextern-lang 0))))

(use-package cc-mode
  :no-require t
  :config
  (add-hook 'c++-mode-hook (lambda ()
                            (c-set-offset 'innamespace 0))))

;;
;; gitignore-mode
;;
(use-package gitignore-mode
  :ensure t
  :pin melpa
  :mode "\\.gitignore\\'")

;;
;; solidity-mode
;;
(use-package solidity-mode
  :ensure t
  :mode "\\.sol\\'"
  :init
  :config
  (add-hook 'solidity-mode-hook
            (lambda ()
              (c-set-offset 'arglist-intro '+)
              (c-set-offset 'arglist-close 0))))

;;
;; sass-mode
;;
(use-package ssass-mode
  ;; https://github.com/AdamNiederer/ssass-mode/pull/4 was
  ;; accepted. Waiting for a new release to be tagged.
  :pin melpa
  :ensure t
  :mode ("\\.scss\\'" "\\.sass\\'"))

;;
;; web-mode
;;
(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook #'lsp-deferred)
  :mode ("\\.tsx\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"))

;; `javascript' mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js2-strict-trailing-comma-warning nil)
  (add-hook 'js2-mode-hook #'lsp-deferred)
  (add-hook 'js2-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (setq typescript-indent-level 2))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.mrk\\'")
  :config
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'text-auto-fill))


(use-package ess
  :ensure t
  :init (require 'ess-site))

(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :ensure t)

(use-package haskell-mode
  :ensure t
  :bind (:map haskell-mode-map
              ("C-c h" . haskell-hoogle))
  :config
  (add-hook 'haskell-mode-hook #'lsp-deferred)
  :after lsp-haskell)

(use-package slurm-script-mode
  :load-path "~/misc/projects/slurm.el")

(use-package julia-mode
  :ensure t)

(use-package ess
  :disabled t
  :config
  (ess-toggle-underscore nil))

(provide 'lang-mode-config)
