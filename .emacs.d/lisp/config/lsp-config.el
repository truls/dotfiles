;; lsp-config.el -*- lexical-binding:t -*-

;;
;; LSP configuration. Both lsp-mode and language servers
;;

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :after yasnippet
  :hook ((lsp-mode        . lsp-enable-which-key-integration)
         (lsp-mode        . lsp-lens-mode)
         (lsp-mode        . yas-minor-mode-on)
         (LaTeX-mode      . lsp-deferred)
         (js2-mode        . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (haskell-mode    . lsp-deferred)
         (c-mode-common   . lsp-deferred)
         (python-mode     . lsp-deferred)
         (scala-mode      . lsp-deferred)
         (go-mode         . lsp-deferred)
         (ess-r-mode      . lsp-deferred)
         (yaml-mode       . lsp-deferred)
         (json-mode       . lsp-deferred)
         (markdown-mode   . lsp-deferred))

  :config
  ;; Should be fixed in https://github.com/emacs-lsp/lsp-mode/issues/641
  ;; (setq lsp-restart 'ignore)
  (setq lsp-ui-doc-enable t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-file-watch-threshold 40000)
  ;; Recommended by lsp performance guidelines
  (setq read-process-output-max (* 1024 1024))

  ;; Hack to reduce lag when moving cursor, see:
  ;; https://github.com/emacs-lsp/lsp-ui/issues/613
  (setq pgtk-wait-for-event-timeout 0.01)

  ;; Look and feel configuration
  (setq lsp-enable-on-type-formatting nil)

  (add-hook 'lsp-mode-hook
            (lambda ()
              (if (member major-mode '("c++-mode" "c-mode"))
                  (define-key lsp-mode-map (kbd "M-q")
                    (lambda () (if (inside-comment-p) lsp-format-region)))
                (define-key lsp-mode-map (kbd "M-S-q")
                  #'lsp-format-buffer)))))

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

(use-package lsp-pyright
  :load-path "/home/truls/foo/lsp-pyright"
  ;;:straight t
  :config
  (setq lsp-pyright-diagnostic-mode "workspace")
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package lsp-haskell
  :straight t)

(use-package lsp-latex
  :straight t
  :init
  (setq lsp-latex-build-on-save t
        lsp-latex-build-output-directory "output"
        lsp-latex-lint-on-save t
        lsp-latex-build-args '("-pdf" "-interaction=nonstopmode"
                               "-synctex=1" "-outdir=output"
                               "-shell-escape" "%f")))

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


;; (use-package lsp-latex
;;   :straight t
;;   :config
;;   (add-hook 'latex-mode-hook 'lsp-deferred)
;;   (add-hook 'bibtex-mode-hook 'lsp-deferred)
;;   ;; sideline conflicts with line wrapping mode since the diagnostics
;;   ;; cannot use the outer margins.
;;   (add-hook 'latex-mode-hook
;;             (lambda ()
;;               (setq-local lsp-ui-sideline-enable nil))))

(provide 'lsp-config)
