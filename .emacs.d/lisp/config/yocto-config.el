;; BitBake-mode
(use-package bitbake-ts-mode
  :init
  (add-to-list
   'treesit-language-source-alist
   '(bitbake "https://github.com/tree-sitter-grammars/tree-sitter-bitbake"))
  :straight t)

(flycheck-define-checker oelint-adv
  "A linter for BitBake recipes using oelint-adv."
  :command ("oelint-adv" "--quiet" "--noinfo --release scarthgap" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":"
            "warning:" (message) line-end)
   (error line-start (file-name) ":" line ":"
          "error:" (message) line-end))
  :modes (bitbake-mode)
  :predicate (lambda () (and buffer-file-name
                        (string-match-p "\\.bb\\|\\.bbappend|$" buffer-file-name))))

(add-to-list 'flycheck-checkers 'oelint-adv)

(defun my/set-flycheck-checker-based-on-mode ()
  "Set Flycheck checker based on the current major mode in Polymode."
  (when (bound-and-true-p polymode-mode)
    (pcase major-mode
      ('python-mode (flycheck-select-checker 'python-pylint))
      ('sh-mode (flycheck-select-checker 'sh-shellcheck))
      ('bitbake-mode (flycheck-select-checker 'oelint-adv))
      (_ (flycheck-mode -1))))) ;; Disable Flycheck if no checker is found

(defun my-bitbake-python-ts-mode-wrapper ()
  "A wrapper around `python-ts-mode` that disables certain hooks when used inside poly-bitbake-mode."
  (message "In python wrapper function")
  (setq-local python-ts-mode-hook
         (remove 'ruff-format-on-save-mode
                 (remove 'lsp-deferred python-ts-mode-hook)))
  (setq-local python-mode-hook
         (remove 'ruff-format-on-save-mode
                 (remove 'lsp-deferred python-mode-hook)))
        ;; Prevent after-change-major-mode-hook triggers from enabling LSP globally
  (setq-local after-change-major-mode-hook
         (cl-remove-if (lambda (fn)
                         (or (eq fn #'lsp)
                             (eq fn #'lsp-deferred)))
                       after-change-major-mode-hook))
  (python-ts-mode))

(defun my-bitbake-sh-mode-wrapper ()
  "A wrapper around `python-ts-mode` that disables certain hooks when used inside poly-bitbake-mode."
  (setq-local sh-mode-hook (remove 'lsp-deferred sh-mode-hook))
  ;; Prevent after-change-major-mode-hook triggers from enabling LSP globally
  (setq-local after-change-major-mode-hook
              (cl-remove-if (lambda (fn)
                              (or (eq fn #'lsp)
                                  (eq fn #'lsp-deferred)))
                            after-change-major-mode-hook))
  (sh-mode))


(define-derived-mode bash-script-mode sh-mode "Bash"
  "A major mode for editing Bash scripts, ensuring `sh-mode` uses Bash."
  (sh-set-shell "bash"))  ;; Force Bash mode

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'sh-shellcheck 'bash-script-mode))

(use-package polymode
  :straight t
  :config

  ;; Statement matchers copied from bitbake-mode
  (defconst bitbake-mode-file-regex "\\.\\(bb\\(append\\|class\\)?\\|inc\\)\\'")
  (defconst bitbake-shell-regex "^\\(fakeroot[[:space:]]*\\)?\\([a-zA-Z0-9\-_+.${}/~:]+\\)[[:space:]]*([[:space:]]*)[[:space:]]*{")
  (defconst bitbake-python-regex "^\\(fakeroot[[:space:]]*\\)?python[[:space:]]*\\([a-zA-Z0-9\-_+.${}/~:]+\\)?[[:space:]]*([[:space:]]*)[[:space:]]*{")
  (defconst bitbake-python-def-regex "^def +[a-zA-Z0-9_]+[[:space:]]*([[:space:]a-zA-Z0-9_,=]*)[[:space:]]*:")

  (define-hostmode poly-bitbake-hostmode
    :mode 'bitbake-ts-mode
    :protect-font-lock t)

  (define-innermode poly-bitbake-shell-innermode
    :mode #'my-bitbake-sh-mode-wrapper
    :head-matcher bitbake-shell-regex
    :tail-matcher "^}"
    :head-mode 'host
    :tail-mode 'host
    :protect-font-lock t)

  (define-innermode poly-bitbake-python-innermode
    :mode #'my-bitbake-python-ts-mode-wrapper
    :head-matcher bitbake-python-regex
    :tail-matcher "^}"
    :head-mode 'host
    :tail-mode 'host
    :protect-font-lock t)

  (define-innermode poly-bitbake-python-def-innermode
    :mode #'my-bitbake-python-ts-mode-wrapper
    :head-matcher bitbake-python-def-regex
    :tail-matcher "^[^ \t\n#].*"
    :head-mode 'host
    :tail-mode 'host
    :protect-font-lock t)


  (define-polymode poly-bitbake-mode
    :hostmode 'poly-bitbake-hostmode
    :innermodes '(poly-bitbake-python-innermode
                  poly-bitbake-python-def-innermode
                  poly-bitbake-shell-innermode))

  (add-to-list 'auto-mode-alist `(,bitbake-mode-file-regex . poly-bitbake-mode)))

(provide 'yocto-config)
