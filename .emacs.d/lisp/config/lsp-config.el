;; lsp-config.el -*- lexical-binding:t -*-

;;
;; LSP configuration. Both lsp-mode and language servers
;;

;; TODO: Do we still need these?
(use-package dash :pin melpa)
(use-package dash-functional :pin melpa)

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :pin melpa
  :commands (lsp lsp-deferred)
  :after yasnippet
  :hook ((lsp-mode        . lsp-enable-which-key-integration)
         (LaTeX-mode      . lsp-deferred)
         (js2-mode        . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (haskell-mode    . lsp-deferred)
         (c-mode-common   . lsp-deferred)
         (python-mode     . lsp-deferred))

  :config
  ;; Should be fixed in https://github.com/emacs-lsp/lsp-mode/issues/641
  ;; (setq lsp-restart 'ignore)
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-file-watch-threshold 40000)
  ;; Recommended by lsp performance guidelines
  (setq read-process-output-max (* 1024 1024))
  (add-hook 'lsp-mode-hook
            (lambda ()
              (if (member major-mode '("c++-mode" "c-mode"))
                  (define-key lsp-mode-map (kbd "M-q")
                    (lambda () (if (inside-comment-p) lsp-format-region)))
                (define-key lsp-mode-map (kbd "M-S-q")
                  #'lsp-format-buffer)))))

(use-package lsp-ui
  :ensure t
  :pin melpa
  :config
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-doc-position 'top)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;:hook lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :pin melpa
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package origami
  :ensure t
  :pin melpa)

(use-package lsp-origami
  :ensure t
  :pin melpa
  :after origami
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package ccls
  :ensure t
  :config
  :after lsp-mode)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))


(use-package lsp-haskell
  :ensure t)

;; (use-package lsp-latex
;;   :pin melpa
;;   :ensure t
;;   :config
;;   (add-hook 'LaTeX-mode-hook 'lsp-deferred)
;;   (add-hook 'bibtex-mode-hook 'lsp-deferred)
;;   ;; Sideline conflicts with line wrapping mode since the diagnostics
;;   ;; cannot use the outer margins.
;;   (add-hook 'LaTeX-mode-hook
;;             (lambda ()
;;               (setq-local lsp-ui-sideline-enable nil))))


;; (use-package lsp-python-ms
;;   :ensure t
;;   :init
;;   (setq lsp-python-ms-executable "/home/truls/.emacs.d/language-servers/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"
;;         lsp-python-executable-cmd "python3")
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))  ; or lsp-deferred

(provide 'lsp-config)
