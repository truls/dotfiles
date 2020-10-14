;; package -- katex-config.el -*- lexical-binding:t -*-

;;
;; LaTeX configuration
;;

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  ;; Enter correlate mode automatically
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

  ;; AucTeX query for master file
  (setq-default TeX-master nil)

  ;; Make AUCTeX build bibtex
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.)

  ;; Turn on RefTeX
  ;; (setq reftex-default-bibliography '("~/Nextcloud/bibliography/references.bib"))
  ;; (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  ;; (defvar reftex-plug-into-AUCTeX t)

  (add-hook 'LaTeX-mode-hook #'text-auto-fill)

  ;; Preview mode settings
  (setq preview-auto-cache-preamble t
        preview-scale-function 1.2)

  ;; Use yasnippet
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode-on))

(use-package magic-latex-buffer
  :ensure t
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

(use-package auctex-latexmk
  :ensure t
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t
        TeX-command-default "LatexMk")
  (auctex-latexmk-setup)
  :after tex)

(provide 'latex-config)
;;; latex-config ends here
