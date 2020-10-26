;; flyspell-config.el -*- lexical-binding: t -*-
;;
;; Flyspell configuration
;;

(use-package flyspell
  :diminish
  :init
  (setq ispell-program-name "hunspell")
  :config
  (setq flyspell-use-meta-tab nil)
  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save)
  ;; Work around for Hunspell 1.7.0
  ;; From https://emacs.stackexchange.com/questions/47344/ispell-not-working-with-hunspell
  (defun manage-hunspell-1.7 (old-function-ispell &rest arguments)
    "Add null-device when calling \"hunspell -D\"."
    (if  (equal "-D"  (nth 4 arguments))
        (funcall old-function-ispell "hunspell" null-device t nil "-D" null-device)
      (apply old-function-ispell  arguments)))
  (advice-add 'ispell-call-process :around #'manage-hunspell-1.7)
  (defun my/run-flyspell-hook ()
    (flyspell-mode 1))
  (defun my/run-flyspell-prog-hook ()
    (flyspell-prog-mode))
  :commands (flyspell-mode flyspell-prog-mode)
  :hook
  ;; Turn on flyspell(-prog)-mode for all modes
  ((text-mode . my/run-flyspell-hook)
   (prog-mode . my/run-flyspell-prog-hook)
   (erlang-mode . my/flyspell-prog-hook)))

(use-package flyspell-correct-popup
  :ensure t
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper))
  :commands (flyspell-correct-wrapper flyspell-correct-popup)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-popup)
  :after flyspell)


(provide 'flyspell-config)
