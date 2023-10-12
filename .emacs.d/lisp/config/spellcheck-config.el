;; spellcheck-config.el -*- lexical-binding: t -*-
;;
;; Spellchecking configuration
;;

(use-package wucuo
  :straight t
  :diminish
  :init
  (setq ispell-program-name "hunspell")
  :config
  (setq flyspell-use-meta-tab nil)
  :commands (wucuo-mode wucuo-prog-mode)
  :hook
  ;; Turn on flyspell(-prog)-mode for all modes
  ((text-mode . wucuo-start)
   (prog-mode . wucuo-start)))

(use-package flyspell-correct-popup
  :straight t
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper))
  :commands (flyspell-correct-wrapper flyspell-correct-popup)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-popup)
  :after wucuo)



(provide 'spellcheck-config)
