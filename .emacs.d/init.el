;;; pacakge --- init.el -*- lexical-binding: t -*-
;;;
;;; Author:
;;; Truls Asheim <truls@asheim.dk>
;;;
;;; Commentary:
;;; Emacs initialization file
;;;
;;; Code:
;;
;; Misc global settings
;;
;(setq gc-cons-threshold 20000000)

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
(garbage-collect)) t)

;; Initialize packages (add to load-path) before we evaluate the
;; init-file such that elpa-packages becomes available
;; http://emacs.stackexchange.com/questions/5828/why-do-i-have-to-add-each-package-to-load-path-or-problem-with-require-packag
(setq load-prefer-newer t)

;; Turn on lazy native compilation of packages
(setq comp-deferred-compilation t)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; Set package.el reposotories
(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"          .  5)
        ("melpa"        .  0)))

(let ((lisp-dir "/home/truls/.emacs.d/lisp")
      (normal-top-level-add-subdirs-inode-list nil))
  (add-to-list 'load-path lisp-dir)
  (let ((default-directory lisp-dir))
    (normal-top-level-add-subdirs-to-load-path)))

;; Bootstrap `use-package'
(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;(add-to-list 'load-path "~/.emacs.d/lisp/use-package")
  (require 'use-package))

;;
;; General emacs config
;;

(show-paren-mode t)
;; Save desktop (current state)
(desktop-save-mode t)

;; Enable commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Disable lockfile creation as this works poorly with frequently
;; changing DHCP-derived hostnames
(setq create-lockfiles nil)

;; Switch between buffers using M-<arrow key>
(windmove-default-keybindings 'meta)

;; Used by, among-others, SyncTeX
(server-start)

;; Disable indentation with tabs
(setq-default indent-tabs-mode nil)

;; Go straight to the fun stuff
(setq inhibit-startup-screen t)

;; Disable toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Show column number
(column-number-mode t)

;; Delete selection when typing
(pending-delete-mode 1)

;; Set default font
(unless (file-exists-p "~/.fonts/FiraEmacs-Regular.otf")
 (user-error "Modified Fira fonts not found. Run make in the fira-code folder"))
(use-package fira-code
  :load-path "~/.emacs.d/lisp/fira-code"
  :config
  (add-hook 'prog-mode-hook #'fira-code-mode))

(setq default-frame-alist '((font-backend . "xft")
                            (font . "Fira code-10")
                            (cursor-color . "white")
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)))

;; Keep auto-save files and backup files out of the way
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))


;;
;; Misc keybindings
;;
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c ;") 'resize-window)
;; An alternative is the command fixup-whitepsace
(global-set-key (kbd "M-<backspace>") 'delete-indentation)
;; Dont minimize!
(when window-system (global-unset-key "\C-z"))


;;
;; Personal info
;;
(setq user-full-name "Truls Asheim"
      user-mail-address "truls@asheim.dk")

;;
;; Store custom settings in separate file
;;
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;
;; Fill mode config
;;
(defun disable-comment-only-fill ()
  (setq-local comment-auto-fill-only-comments nil))

(defun text-auto-fill ()
  (setq-local comment-auto-fill-only-comments nil)
  (auto-fill-mode 1))

(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(add-hook 'text-mode-hook 'text-auto-fill)
(add-hook 'prog-mode-hook 'comment-auto-fill)

(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package diminish :ensure t)
(use-package use-package-hydra :ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Enable auto-compile package to automatically compile elisp files
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;
;; Dark/light theme toggeling
;;
(use-package toggle-theme
  :config
  (setq toggle-theme-light-theme 'immaterial-light
        toggle-theme-dark-theme 'immaterial-dark))

;;
;; Use immaterial theme
;;
(use-package immaterial-theme
  :ensure t
  :after
  toggle-theme
  :config
  (toggle-theme-restore-theme))

(use-package auto-fill-mode
  :no-require t
  :diminish)

(use-package google-this
  :ensure t
  :diminish
  :init
  (setq google-this-keybind (kbd "C-x \\"))
  :config
  (google-this-mode t))

(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)
         :map esc-map
         ("C-r" . vr/isearch-backward)
         ("C-s" . vr/isearch-forward)))


(use-package ess
  :disabled t
  :config
  (ess-toggle-underscore nil))

(use-package hydra
  :ensure t
  :commands defhydra)

;;
;; ivy-mode
;;
(use-package ivy
  :ensure t
  :pin melpa
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :pin melpa
  :defer t
  :config
  (defun my/swiper-symbol-at-point ()
    (interactive)
    (let ((bounds (find-tag-default-bounds)))
      (cond
       (bounds
        (swiper (buffer-substring-no-properties
                 (car bounds) (cdr bounds))))
       (t (user-error "No symbol at point"))))))

;; Install smex to show the most used commands first in M-x list
(use-package smex
  :ensure t
  :init
  (setq smex-save-file (expand-file-name "smex-items"
                                         user-emacs-directory)))

(use-package counsel
  :ensure t
  :pin melpa
  :bind (("C-s" . swiper)
         ("C-S-s" . my/swiper-symbol-at-point)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         :map minibuffer-local-map
         ("C-s" . counsel-minibuffer-local-map))
  :after (ivy smex))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :after ivy)

;;
;; solidity-mode
;;
(use-package solidity-mode
  :ensure t
  :mode "\\.sol\\'"
  :init
  :config
  (add-hook 'solidity-mode-hook
            (lambda ()
              (c-set-offset 'arglist-intro '+)
              (c-set-offset 'arglist-close 0))))

(use-package solidity-flycheck
  :ensure t
  :init
  (setq solidity-flycheck-solium-checker-active t)
  (setq solidity-flycheck-solc-checker-active t)
  :after solidity-mode)

(use-package company-solidity
  :ensure t
  :after solidity-mode)

;;
;; sass-mode
;;
(use-package ssass-mode
  ;; https://github.com/AdamNiederer/ssass-mode/pull/4 was
  ;; accepted. Waiting for a new release to be tagged.
  :pin melpa
  :ensure t
  :mode ("\\.scss\\'" "\\.sass\\'"))

;;
;; web-mode
;;
(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook #'lsp-deferred)
  :mode ("\\.tsx\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"))

;;
;; flycheck-mode
;;
(use-package flycheck
  :ensure t
  :config
  (add-hook 'flycheck-error-list-mode-hook
            (lambda () (toggle-truncate-lines +1))))

(use-package company
  :ensure t
  ;;:defer t
  :config
  (global-company-mode)
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map))

  ;; (setq company-tooltip-align-annotations t
  ;;       ;; Easy navigation to candidates with M-<n>
  ;;       company-show-numbers t
  ;;       company-dabbrev-downcase nil
  ;;       company-idle-delay 0.1))
  :diminish company-mode)

;;
;; yasnippet
;;
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :pin melpa
    :ensure t)
  (yas-reload-all)
  (when yas-minor-mode
    (ivy-yasnippet))
  :hook (elisp-mode . yas-minor-mode)
  :commands (yas-minior-mode yas-global-mode yas-expand-snippet)
  :diminish yas-minor-mode)

(use-package ivy-yasnippet
  :ensure t
  :commands ivy-yasnippet)

;;
;; lsp-mode
;;
(use-package dash :pin melpa)
(use-package dash-functional :pin melpa)

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :pin melpa
  :commands (lsp lsp-deferred)
  :after yasnippet
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-restart 'ignore)
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


(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; `javascript' mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js2-strict-trailing-comma-warning nil)
  (add-hook 'js2-mode-hook #'lsp-deferred)
  (add-hook 'js2-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (setq typescript-indent-level 2))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init
;;   (setq lsp-python-ms-executable "/home/truls/.emacs.d/language-servers/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"
;;         lsp-python-executable-cmd "python3")
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))  ; or lsp-deferred

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

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.mrk\\'")
  :config
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'text-auto-fill))

(use-package magit
  :ensure t
  ;; Stable melpa exhibits highlighting issues with Emacs 27
  ;; https://github.com/magit/magit/issues/3986
  ;; Unstable version of git-commit and with-editor was also installed
  :pin melpa
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-diff-refine-hunk 'all)
  (add-hook 'magit-diff-mode-hook (lambda ()
                                    (toggle-truncate-lines -1)
                                    (visual-line-mode 1)))
  (add-hook 'magit-status-mode-hook (lambda ()
                                      (toggle-truncate-lines -1)
                                      (visual-line-mode 1))))

;; TODO: Figure out why these are broken
;; (use-package forge
;;   :pin melpa
;;   :ensure t
;;   :after magit)

;; (use-package magithub
;;   :ensure t
;;   :after magit
;;   :pin melpa
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/github"))

(use-package display-line-numbers
  :no-require t
  :config
  (global-display-line-numbers-mode))

(use-package seq :ensure t)
(use-package whitespace
  :ensure t
  :diminish
  :after seq
  :preface
  ;;  (Inspired by
  ;; https://emacs.stackexchange.com/questions/38771/magit-status-does-not-open-when-using-global-whitespace-mode-1)
  ;; Whitespace mode blacklist
  (defconst my/whitespace-mode-blacklist '(magit-mode org-mode))

  (defun my/enable-whitespace-for-mode ()
    "Check if whitespace mode is disabled for current major mode."
    (not (seq-reduce (lambda (a b) (or a (derived-mode-p b)))
                     my/whitespace-mode-blacklist nil)))

  :config
  (add-function :before-while whitespace-enable-predicate
                'my/enable-whitespace-for-mode)
  (setq whitespace-action nil
        whitespace-global-modes (quote
                                 (not term-mode erc-mode Custom-mode completion-list-mode MagitPopup Magit csharp-mode))
        whitespace-style (quote
                          (trailing space-before-tab newline empty tabs face newline)))
  (global-whitespace-mode))



;; (use-package company-bibtex
;;   :ensure t
;;   :after org-ref
;;   :config
;;   (add-to-list 'company-backends 'company-bibtex)
;;   (setq company-bibtex-bibliography org-ref-default-bibliography))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :diminish
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-use-git-grep t)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :pin melpa
  :config
  (counsel-projectile-mode))

(use-package helm
  :ensure t
  :pin melpa)

(use-package helm-projectile
  :ensure t)

(use-package helm-rg
  :ensure t
  :pin melpa)

(use-package helm-ag
  :ensure t)

(use-package pdf-tools
  :ensure t
  :preface
  (defun my-inhibit-global-nlinum-mode ()
    "Prevent nlinum mode from being enabled."
    (add-hook 'after-change-major-mode-hook
              (lambda () (display-line-numbers-mode 0))
              :append :local))
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  ;; Disable nlinum-mode when PDF-view mode is enabled
  (add-hook 'pdf-view-mode-hook #'my-inhibit-global-nlinum-mode)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward))



(use-package c-mode
  :no-require t
  :config
  ;; C-mode hook to avoid indenting after extern "C"
  (add-hook 'c-mode-common-hook
            (lambda()
              (c-set-offset 'inextern-lang 0))))

(use-package ccls
  :ensure t
  :config
  (add-hook 'c-mode-common-hook #'lsp-deferred)
  :after lsp-mode)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package highlight-indentation
  :ensure t
  :hook ((yaml-mode . highlight-indentation-mode))
  :config
  (setq highlight-indentation-blank-lines t)
  (set-face-background 'highlight-indentation-face "grey24")
  (set-face-background 'highlight-indentation-current-column-face "grey30"))

(use-package python
  :config
  (add-hook 'python-mode-hook #'lsp-deferred))

(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :ensure t)

(use-package lsp-haskell
  :ensure t)

(use-package haskell-mode
  :ensure t
  :bind (:map haskell-mode-map
              ("C-c h" . haskell-hoogle))
  :config
  (add-hook 'haskell-mode-hook #'lsp-deferred)
  :after lsp-haskell)

(use-package smart-shift
  :ensure t
  :config
  (global-smart-shift-mode 1))

(use-package langtool
  :ensure t
  :config
  ;; Run my-install.sh script in language-servers/languagetool to
  ;; install languagetool
  (setq langtool-language-tool-jar (expand-file-name "language-servers/languagetool/LanguageTool-4.4-stable/languagetool-commandline.jar" user-emacs-directory)))

(use-package ess
  :ensure t
  :init (require 'ess-site))

(use-package visual-fill-column
  :ensure t)

(use-package mutt-mode
  :ensure t
  :mode ("\\.muttrc\\'"
         "\\.neomuttrc\\'"
         "\\neomuttrc\\'")
  :commands mutt-mode)

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile"))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\; ?w ?e ?r ?u ?i ?o ?q ?t ?y ?p)))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)))

(use-package julia-mode
  :ensure t)

(use-package package-lint
  :ensure t
  :commands (package-lint-buffer
             package-lint-current-buffer
             pacakge-lint-batch-and-exit
             package-lint-looks-like-a-package-p))

(use-package json-navigator
  :ensure t)

(use-package sh-script
  :no-require t
  :config
  (add-hook 'sh-mode-hook #'flycheck-mode))

(use-package slurm-mode
  :load-path "~/misc/projects/slurm.el"
  :config
  (setq slurm-remote-username "trulsas")
  (setq slurm-remote-host "stallo.uit.no"))

(use-package slurm-script-mode
  :load-path "~/misc/projects/slurm.el")

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :commands er/expand-region)

(use-package scihub
  :commands (scihub-get-from-publisher-url
             scihub-get-from-doi
             scihub-get-from-scihub-url))

(use-package citations
  :load-path "~/misc/projects/acm-citations/"
  :commands (citations-bibtex-entry-pdf
             citations-crossref-lookup
             citations-get-citation
             citations-download
             citations-dwim)
  :config
  (setq citations-download-fallback #'scihub-get-from-publisher-url)
  (setq citations-bibtex-entry-formatter #'org-ref-clean-bibtex-entry))

(use-package easy-escape
  :ensure t
  :commands easy-escape-minor-mode
  :config
  (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode))

(use-package windmove
  :no-require t
  :config
  (setq windmove-wrap-around t))

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

(use-package cc-mode
  :no-require t
  :config
  (add-hook 'c++-mode-hook (lambda ()
                            (c-set-offset 'innamespace 0))))

;; Prevent frequent garbage collections from interfering with Emacs
;; performance by increasing the GC threshold high threshold when Emacs
;; is in use and decrease it after Emacs has been idel for a while.
(use-package gcmh
  :ensure t
  :diminish
  :config
  (setq gcmh-high-cons-threshold #x60000000
        gcmh-verbose t)
  (gcmh-mode +1))

;;
;; Use undo-tree
;;
(use-package undo-tree
  :ensure t
  :diminish
  :init
  (global-undo-tree-mode)
  :config
  (define-key undo-tree-map (kbd "C-+") #'undo-tree-redo))

;;
;; gitignore-mode
;;
(use-package gitignore-mode
  :ensure t
  :pin melpa
  :mode "\\.gitignore\\'")

;;
;; gitignore-templates
;;
(use-package gitignore-templates
  :ensure t
  :pin melpa
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

;;
;; Misc functions
;;
(defun my/recompile-all-packages ()
  "Recompile all packages"
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))


;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(use-package line-manip
  :config
  (global-set-key (kbd "C-S-c") #'comment-duplicate-line)
  (global-set-key (kbd "C-S-d") #'duplicate-line)
  (global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)
  (global-set-key (kbd "M-S-<up>") 'move-line-up)
  (global-set-key (kbd "M-S-<down>") 'move-line-down))

;;
;; Delete word on C-backspace
;;

;; From: https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key (read-kbd-macro "<C-Backspace>") 'backward-delete-word)

;;
;; Check if we are currently inside a comment (probably a hack)
;; FIXME: This isn't perfect and fails if on first char of a
;;
(defun inside-comment-p ()
  (let ((p (point)))
    (save-excursion
      (comment-only-p (comment-search-backward) p))))

;;
;; Pull and push in sequence
;;
(defun git-pull-push ()
  (interactive)
  (magit-pull-from-pushremote)
  (magit-push-current-to-pushremote))

;;
;; Config includes
;;
(require 'org-config)
(require 'org-ref-config)
(require 'erc-config)
(require 'latex-config)

(provide 'init)
;;; init.el ends here
