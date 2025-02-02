;; package -- latex-config.el -*- lexical-binding:t -*-

;;
;; LaTeX configuration
;;

(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
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

  ;; From https://github.com/tom-tan/auctex-latexmk/issues/47

  ;; Turn on RefTeX
  ;; (setq reftex-default-bibliography '("~/Nextcloud/bibliography/references.bib"))
  ;; (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  ;; (defvar reftex-plug-into-AUCTeX t)

  ;; Preview mode settings
  (setq preview-auto-cache-preamble t
        preview-scale-function 1.2)

  ;; Use yasnippet
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode-on)
  (add-hook 'LaTeX-mode-hook #'toggle-font-turn-on)


  (add-to-list 'TeX-expand-list
               '("%(-PDF)"
                 (lambda ()
                   (if TeX-PDF-mode
                       (cond
                        ((eq TeX-engine 'default) "-pdf")
                        ((eq TeX-engine 'xetex) "-pdfxe")
                        ((eq TeX-engine 'luatex) "-pdflua")) ""))))
  (add-to-list 'TeX-command-list
               '("LaTeXmk" "latexmk %(-PDF) -%(PDF)%(latex)='%`%l%(mode)%'' %(output-dir) %t"
                 TeX-run-format nil (latex-mode doctex-mode) :help "Run Latexmk"))
  (with-eval-after-load 'latex
    (setq LaTeX-clean-intermediate-suffixes
          (append LaTeX-clean-intermediate-suffixes '("\\.fdb_latexmk" "\\.fls"))))

  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXmk"))))


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

(provide 'latex-config)
;;; latex-config ends here
