;; lang-mode-config.el -*- lexical-binding: t -*-
;;
;; Miscellaneous languages
;;

(use-package mutt-mode
  :straight t
  :mode ("\\.muttrc\\'"
         "\\.neomuttrc\\'"
         "\\neomuttrc\\'")
  :commands mutt-mode)

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile"))

(use-package sh-script
  :no-require t
  :config
  (add-hook 'sh-mode-hook #'flycheck-mode))

;; (use-package c-ts-mode
;;   :no-require t
  ;; :config
  ;; C-mode hook to avoid indenting after extern "C"
  ;; (add-hook c-mode-common-hook
  ;;           (lambda()
  ;;             (c-set-offset 'inextern-lang 0)))
  ;; )

;; (use-package cc-ts-mode
;;   :no-require t
  ;; :config
  ;; (add-hook 'c++-ts-mode-hook (lambda ()
  ;;                               (c-set-offset 'innamespace 0)))
  ;; )

;;
;; git-modes
;;
(use-package git-modes
  :straight t
  :config
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

;;
;; solidity-mode
;;
(use-package solidity-mode
  :straight t
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
  :straight t
  :mode ("\\.scss\\'" "\\.sass\\'"))

;;
;; web-mode
;;
(use-package web-mode
  :straight t
  :config
  (add-hook 'web-mode-hook #'lsp-deferred)
  :mode ("\\.tsx\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"))

;; `javascript' mode
(use-package js2-mode
  :straight t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js2-strict-trailing-comma-warning nil)
  (add-hook 'js2-ts-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(use-package typescript-ts-mode
  :straight t
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :init
  (setq typescript-indent-level 2))

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" "\\.mrk\\'")
  :init
  (setq markdown-command "pandoc"))


(use-package ess
  :straight t
  ;; TODO: We just want lazy loading here. Add other extensions also
  :mode ("\\.R\\'" . R-mode)
  :init
  (require 'ess-site)
  (setq ess-use-flymake nil
        ;; Disable lsp-ui sideline diagnostics with R as the R
        ;; language server is a bit quirky
        lsp-ui-sideline-show-diagnostics nil))

(use-package ess-smart-equals
  :straight t
  :init   (setq ess-smart-equals-extra-ops '())
  :after  (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
  :config (ess-smart-equals-activate))

;;
;; Polymode for R
;;
(use-package poly-R
  :straight t
  :mode
  ("\\.Snw\\'" . poly-noweb+r-mode)
  ("\\.[rR]nw\\'" . poly-noweb+r-mode)
  ("\\.[rR]md\\'" . poly-markdown+r-mode)
  ("\\.rapport\\'" . poly-rapport-mode)
  ("\\.[rR]html\\'" . poly-html+r-mode)
  ("\\.[rR]brew\\'" . poly-brew+r-mode)
  ("\\.[Rr]cpp\\'" . poly-r+c++-mode)
  ("\\.cpp[rR]\\'" . poly-c++r-mode))

(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :straight t)

(use-package haskell-mode
  :straight t
  :bind (:map haskell-mode-map
              ("C-c h" . haskell-hoogle))
  :after lsp-haskell)

(use-package slurm-script-mode
  :load-path "~/misc/projects/slurm.el")

(use-package julia-mode
  :straight t)

(use-package groovy-mode
  :straight t
  :mode ("\\.groovy\\'" "\\.gradle\\'"))

;;
;; Ansible config
;;
(use-package ansible
  :straight t
  :commands ansible)

(use-package ansible-doc
  :straight t
  :commands ansible-doc ansible-doc-mode)

(use-package ansible-vault
  :straight t)

(use-package jinja2-mode
  :straight t)

;;
;; Python config
;;
(use-package python-mode
  :straight t
  :bind (:map python-mode-map
              ("C-<backspace>" . backward-kill-word)))

;;
;; Protocol buffers
;;
(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'"))

;;
;; go-mode
;;
(use-package go-ts-mode
  :straight t
  :interpreter
  ("go" . go-ts-mode)
  :init
  (setq go-ts-mode-indent-offset 4)
  (let ((my/go-hook-fun
         (lambda () (setq-local whitespace-style
                           (-filter (lambda (x) (not (eq x 'tabs)))
                                    whitespace-style)
                           tab-width 4))))
    (add-hook 'go-ts-mode-hook my/go-hook-fun)
    (add-hook 'go-ts-dot-mod-mode-hook my/go-hook-fun))
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-mode
  :straight t
  :config
  (setq godoc-at-point-function 'godoc-gogetdoc))

;;
;; rustic
;;
(use-package rustic
  :straight t)

;;
;; Tree sitter
;;
(use-package tree-sitter
  :no-require t
  :config
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (rust-mode . rust-ts-mode)
          (go-mode . go-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode))
        )
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (c "https://github.com/tree-sitter/tree-sitter-c"))))

;;
;; combobulate
;;
(use-package combobulate
  :straight t)

(use-package terraform-mode
  :straight t

  :custom
  (terraform-indent-level 2)
  (terraform-format-on-save t)

  :config
  (defun my-terraform-mode-init ()
    (outline-minor-mode 1))

  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

;;
;; kubernetes-helm
;;
(use-package kubernetes-helm
  :straight t)

;;
;; bpftrace-mode
;;
(use-package bpftrace-mode
  :straight t
  :mode "\\.bpf\\'")

(provide 'lang-mode-config)
