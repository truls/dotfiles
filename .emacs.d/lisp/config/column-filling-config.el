;;
;; Fill mode config
;;

(defun col-fill--comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(use-package auto-fill-mode
  :no-require t
  :delight auto-fill-mode
  :hook ((prog-mode yaml-mode) . col-fill--comment-auto-fill))

(use-package emacs
  :delight auto-fill-function)

(use-package visual-fill-column
  :ensure t
  :hook (TeX-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-width 80)
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package visual-line-mode
  :no-require t
  :delight
  :hook TeX-mode)


(provide 'column-filling-config)
