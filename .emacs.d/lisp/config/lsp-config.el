;; lsp-config.el -*- lexical-binding:t -*-

;;
;; LSP configuration. Both lsp-mode and language servers
;;

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :after yasnippet
  :hook ((ansible-mode       . lsp-deferred)
         (bitbake-mode       . lsp-deferred)
         (c++-ts-mode        . lsp-deferred)
         (c-ts-mode          . lsp-deferred)
         (ess-r-mode         . lsp-deferred)
         (go-ts-mode         . lsp-deferred)
         (haskell-mode       . lsp-deferred)
         (js2-ts-mode        . lsp-deferred)
         (json-ts-mode       . lsp-deferred)
         (lsp-mode           . lsp-enable-which-key-integration)
         (lsp-mode           . lsp-lens-mode)
         (lsp-mode           . yas-minor-mode-on)
         (markdown-mode      . lsp-deferred)
         (python-ts-mode     . lsp-deferred)
         (scala-mode         . lsp-deferred)
         (sh-mode            . lsp-deferred)
         (terraform-mode     . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (yaml-mode          . lsp-deferred)
         (yaml-ts-mode       . lsp-deferred))

  :config
  ;; Should be fixed in https://github.com/emacs-lsp/lsp-mode/issues/641
  ;; (setq lsp-restart 'ignore)
  (setq lsp-ui-doc-enable t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-file-watch-threshold 40000)
  ;; Recommended by lsp performance guidelines
  (setq read-process-output-max (* 1024 1024))

  ;; Make clangd query the driver for system environment variables
  (setq lsp-clients-clangd-args '("--query-driver=/usr/bin/*"))

  ;; Hack to reduce lag when moving cursor, see:
  ;; https://github.com/emacs-lsp/lsp-ui/issues/613
  (setq pgtk-wait-for-event-timeout 0.01)

  ;; Look and feel configuration
  (setq lsp-enable-on-type-formatting nil)

  (add-hook 'lsp-mode-hook
            (lambda ()
              (if (lsp-feature? "textDocument/formatting")
                  (define-key lsp-mode-map (kbd "M-S-q")
                              #'lsp-format-buffer))
              ;; (if (lsp-feature? "textDocument/rangeFormatting")
              ;;     (define-key lsp-mode-map (kbd "M-q")
              ;;                 (lambda () (if (inside-comment-p) lsp-format-region)))
              ;;   )
              ))

  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          ;; This gives annoying err variable
                          ;; shadowing warnings
                          ;; (shadow . t)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t)
                          (unusedvariable . t)))
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-sideline-show-code-actions nil
        ;; Pending https://github.com/emacs-lsp/lsp-ui/issues/613
        ;; lsp-ui-doc-enable nil)
        )
  (setq lsp-ui-doc-position 'top)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;:hook lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package origami
  :straight t)

(use-package lsp-origami
  :straight t
  :after origami
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package ccls
  :straight t
  :config
  :after lsp-mode)

;; Python config
(use-package python-pytest
  :straight t)

(use-package python-black
  :straight t)

(use-package python-isort
  :straight t)

(use-package lsp-haskell
  :straight t)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for
  ;; dap-mode
  :straight t)

(use-package dap-mode
  :straight t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))


(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () '("uv" "run" "pyrefly" "lsp")))
                  :activation-fn (lsp-activate-on "python")
                  :priority -1
                  :add-on? t
                  :server-id 'py-refly))

(lsp-consistency-check lsp-python-refly)

(provide 'lsp-config)
