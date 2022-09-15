;; package -- latex-config.el -*- lexical-binding:t -*-

;;
;; LaTeX configuration
;;

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :bind (:map TeX-mode-map
              ("M-s M-s" . git-fast-sync))
  :config
  ;; Enter correlate mode automatically
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

  ;; AucTeX query for master file
  (setq-default TeX-master nil)

  ;; Make AUCTeX build bibtex
  (setq TeX-parse-self t ; Enable parse on load.
        TeX-auto-save t ; Enable parse on save.
        TeX-global-PDF-mode t
        )

  ;; Turn on RefTeX
  ;; (setq reftex-default-bibliography '("~/Nextcloud/bibliography/references.bib"))
  ;; (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  ;; (defvar reftex-plug-into-AUCTeX t)

  ;; Preview mode settings
  (setq preview-auto-cache-preamble t
        preview-scale-function 1.2)

  ;; Use yasnippet
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode-on)
  (add-hook 'LaTeX-mode-hook #'toggle-font-turn-on))


(use-package magic-latex-buffer
  :straight t
  :commands magic-latex-buffer
  ;; :init
  ;; (setq magic-latex-enable-block-highlight nil
  ;;       magic-latex-enable-suscript        t
  ;;       magic-latex-enable-pretty-symbols  t
  ;;       magic-latex-enable-block-align     nil
  ;;       magic-latex-enable-inline-image    nil
  ;;       magic-latex-enable-minibuffer-echo nil)
  :config
  (add-hook 'LaTeX-mode-hook #'magic-latex-buffer))

;; From
;; https://github.com/tom-tan/auctex-latexmk/issues/39#issuecomment-1104743060
;; TODO: remove when auctex-latexmk supports new auctex version
(use-package auctex-latexmk
  :straight t
  :after latex
  :functions auctex-latexmk-setup
  :preface
  (defun my-auctex-latexmk-advice (req feature &rest args)
    "Call REQ with FEATURE and ARGS, unless FEATURE is `tex-buf'."
    (unless (eq feature 'tex-buf)
      (apply req feature args)))
  :init
  (unwind-protect
      (progn (advice-add 'require :around #'my-auctex-latexmk-advice)
             (auctex-latexmk-setup))
    (advice-remove 'require #'my-auctex-latexmk-advice))
  (setq TeX-command-default "LatexMk"
        auctex-latexmk-inherit-TeX-PDF-mode t)
  )

(provide 'latex-config)
;;; latex-config ends here
