;; lsp-config.el -*- lexical-binding:t -*-

;;
;; Scala config
;;
;; Mostly based on https://scalameta.org/metals/docs/editors/emacs.html
;;

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :pin melpa
  :interpreter
  ("scala" . scala-mode))


;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :pin melpa
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :ensure t
  :pin melpa
  :config
  (setq lsp-metals-treeview-show-when-views-received t))



(provide 'scala-config)
