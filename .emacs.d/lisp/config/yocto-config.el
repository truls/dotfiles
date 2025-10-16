;; BitBake-mode
(use-package bitbake
  :straight t
  :config
  (setq auto-mode-alist
        (delete `(,bitbake-mode-file-regex . bitbake-mode) auto-mode-alist)))

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

(add-hook 'polymode-init-host-hook #'flycheck-mode)
(add-hook 'polymode-init-inner-hook #'flycheck-mode)
(add-hook 'after-change-major-mode-hook #'my/set-flycheck-checker-based-on-mode)
(add-hook 'poly-mode-hook #'my/set-flycheck-checker-based-on-mode)
(add-hook 'after-change-major-mode-hook #'my/set-flycheck-checker-based-on-mode)

;; TODO: Try to make this work in order to restrict tle flycheck
;; checker to the innermost code region
;;
;;
;; (defun my/flycheck-limit-to-inner-region ()
;;   "Limit Flycheck to the current inner mode region in Polymode."
;;   (when (bound-and-true-p polymode-mode)
;;     (let ((span (pm-innermost-span)))
;;       (when span
;;         (setq-local flycheck-region-function
;;                     (lambda (_start _end)
;;                       (list (nth 1 span) (nth 2 span))))))))

;; (add-hook 'polymode-switch-buffer-hook #'my/flycheck-limit-to-inner-region)

(define-derived-mode bash-script-mode sh-mode "Bash"
  "A major mode for editing Bash scripts, ensuring `sh-mode` uses Bash."
  (sh-set-shell "bash"))  ;; Force Bash mode

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'sh-shellcheck 'bash-script-mode))

(use-package polymode
  :straight t
  :config

    (define-hostmode poly-bitbake-hostmode
      :mode 'bitbake-mode)

    (define-innermode poly-bitbake-shell-innermode
      :mode 'bash-script-mode
      :head-matcher bitbake-shell-regex
      :tail-matcher "^}"
      :head-mode 'host
      :tail-mode 'host)

    (define-innermode poly-bitbake-python-innermode
      :mode 'python-mode
      :head-matcher bitbake-python-regex
      :tail-matcher "^}"
      :head-mode 'host
      :tail-mode 'host)

    (define-innermode poly-bitbake-python-def-innermode
      :mode 'python-mode
      :head-matcher bitbake-python-def-regex
      :tail-matcher "\\(^[^[:space:]\n]\\)"
      :head-mode 'host
      :tail-mode 'host)

    (define-innermode poly-bitbake-python-inline-innermode
      :mode 'python-mode
      :head-matcher "${@"
      :tail-matcher "}"
      :head-mode 'host
      :tail-mode 'host)

    (define-polymode poly-bitbake-mode
      :hostmode 'poly-bitbake-hostmode
      :innermodes '(poly-bitbake-python-innermode
                    poly-bitbake-python-inline-innermode
                    poly-bitbake-python-def-innermode
                    poly-bitbake-shell-innermode))

    (setq auto-mode-alist
          (delete `(,bitbake-mode-file-regex . bitbake-mode) auto-mode-alist))
    (add-to-list 'auto-mode-alist `(,bitbake-mode-file-regex . poly-bitbake-mode)))

(provide 'yocto-config)
