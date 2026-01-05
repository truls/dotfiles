(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :config
  (setq lsp-bridge-python-multi-lsp-server "pyrefly_ruff")
  (setq lsp-bridge-user-langserver-dir "~/.emacs.d/lsp-bridge-defs/langserver")
  (setq lsp-bridge-user-multiserver-dir "~/.emacs.d/lsp-bridge-defs/multiserver")

  (defun my/get-current-file-path-if-python (filepath)
    (if (or (eq major-mode 'python-mode)
          (eq major-mode 'python-ts-mode))
        (expand-file-name (locate-dominating-file filepath "pyproject.toml"))
      filepath))
  (setq lsp-bridge-get-project-path-by-filepath #'my/get-current-file-path-if-python)

  :bind
  (("C-c l d" . lsp-bridge-diagnostic-list)
   ("C-c l n" . lsp-bridge-diagnostic-jump-next)
   ("C-c l p" . lsp-bridge-diagnostic-jump-prev)
   ("C-c l a" . lsp-bridge-code-action)
   ("C-c l f" . lsp-bridge-code-format)
   ("M-." . lsp-bridge-peek)
   ("M-," . lsp-bridge-peek-jump-back)
   ("C-\\" . lsp-bridge-popup-documentation)))

(provide 'lsp-bridge-config)

