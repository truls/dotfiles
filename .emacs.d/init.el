;;; pacakge --- init.el -*- lexical-binding: t -*-
;;;
;;; Author:
;;; Truls Asheim <truls@asheim.dk>
;;;
;;; Commentary:
;;; Emacs initialization file

;;; Code:
;;
;; Misc global settings
;;
;(setq gc-cons-threshold 20000000)

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
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

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; Set package.el reposotories
(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"     . 5)
        ("melpa"        . 0)))

(package-initialize)

(let ((lisp-dir "/home/truls/.emacs.d/lisp")
      (normal-top-level-add-subdirs-inode-list nil))
  (add-to-list 'load-path lisp-dir)
  (let ((default-directory lisp-dir))
    (normal-top-level-add-subdirs-to-load-path)))

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
(use-package fira-code
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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;(add-to-list 'load-path "~/.emacs.d/lisp/use-package")
  (require 'use-package))

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

;; Setup color themes with the replace-colortheme package
(use-package color-theme-modern
  :ensure t
  :no-require t
  :init
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory "~/.emacs.d/lisp/replace-colorthemes"))
  :config
  (load-theme 'kingsajz t t)
  (enable-theme 'kingsajz))

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
  (add-hook 'web-mode-hook #'lsp)
  :mode ("\\.tsx\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"))

;;
;; flycheck-mode
;;
(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :pin melpa-stable
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
(use-package lsp-mode
  :ensure t
  :pin melpa
  :commands lsp
  :after yasnippet
  :init
  ;(setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "\"/home/truls//tslog\"" "--tsserver-log-verbosity" "verbose"))
  (require 'lsp-clients)
  :config
  (setq lsp-restart 'ignore)
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-file-watch-threshold 40000))

(use-package lsp-ui
  :ensure t
  :pin melpa
  :config
  (setq lsp-ui-sideline-show-code-actions nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;:hook lsp-mode
  :commands lsp-ui-mode)


(use-package company-lsp
  :ensure t
  :pin melpa
  ;;:hook lsp-mode
  :commands company-lsp)


;; `javascript' mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js2-strict-trailing-comma-warning nil)
  (add-hook 'js2-mode-hook #'lsp)
  (add-hook 'js2-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook #'lsp)
  (setq typescript-indent-level 2)
  )

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
  :commands (flyspell-mode flyspell-prog-mode)
  :hook
  ;; Turn on flyspell(-prog)-mode for all modes
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)
   (erlang-mode . flyspell-prog-mode)))

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
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-diff-refine-hunk 'all)
  (add-hook 'magit-status-mode-hook (lambda ()
                                      (toggle-truncate-lines -1)
                                      (visual-line-mode 1)))
)

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

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode))

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

;;
;; IRC configuration
;;

(use-package erc
  :preface
  (defun ks-timestamp (string)
    (erc-insert-timestamp-left string)
    (let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
      (unless (string= datestamp erc-last-datestamp)
        (erc-insert-timestamp-left datestamp)
        (setq erc-last-datestamp datestamp))))
  :init
  (setq erc-max-buffer-size 300000
        erc-modules (quote
                     (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp spelling track truncate notifications))
        erc-notifications-mode t
        erc-track-exclude-types (quote ("JOIN" "NICK" "QUIT" "333" "353")))
  :config
  (setq erc-timestamp-only-if-changed-flag t
        erc-timestamp-format "[%H:%M] "
        erc-datestamp-format "[%Y-%m-%d %a]\n" ; mandatory ascii art
        erc-fill-prefix "      "
        erc-insert-timestamp-function 'ks-timestamp)
  (add-hook 'erc-mode-hook (lambda ()
                             (auto-fill-mode 0)
                             (visual-line-mode 1)
                             ))
  ;; Set ERC timestamps on every line
  (make-variable-buffer-local
   (defvar erc-last-datestamp nil))
  :commands (erc erc-tls))

(use-package erc-fill
  :after erc)
;;(erc-fill-mode 1)

(use-package erc-spelling
  :after erc
  :config
  (setq erc-spelling-dictionaries '(("bastion.terrifyinglytrue.eu" "english")
                                    ("#diku" "dansk")
                                    ("#eggsml" "dansk")
                                    ("#gallopsled" "dansk")
                                    ("#zomg_pwnies" "dansk")
                                    ("#dikuhost" "dansk")
                                    ("irc.gitter.im" "english")))
  (erc-spelling-mode 1))

(use-package erc-pcomplete
  :after erc
  :config
  (add-hook 'erc-mode-hook
          '(lambda ()
             (pcomplete-erc-setup)
             (erc-completion-mode 1))))

(use-package znc
  :ensure t
  :after erc
  :commands (znc-all znc-erc)
  :init
  ;; TODO: Byte compile warning is issued from znc.el:119. Figure out
  ;; why this is
  (setq byte-compile-warnings nil)
  :config
  (require 'my-secrets)
  (setq znc-servers `(("bastion.terrifyinglytrue.eu" 6667 t
                       ((irc\.freenode\.org ,znc-username ,znc-password)))
                      ;; ("irc.gitter.im" 6667 t
                      ;;  ((irc\.gitter\.im ,gitter-username ,gitter-password)))
                      )))


;;
;; Org configuration
;;
(use-package org
  :ensure t
  :defer t
  :preface
  (defun my-org-prefix (file)
    "Prepend local org prefix to FILE."
    (concat "~/Nextcloud/org/" file))
  :mode ("\\.org\\'" . org-mode)
  :config
  (defvar org-todo-keywords
    '((sequence "TODO" "WAITING" "STARTED" "DONE")))
  (setq org-default-notes-file (my-org-prefix "todo.org")
        ;;set priority range from A to C with default A
        org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?A
        org-babel-load-languages (quote ((emacs-lisp . t) (latex . t)))
        org-startup-with-inline-images t
        org-support-shift-select t)

  ;; Default to opening PDFs in evince
  ;; We do this by removing the entry in org-file-apps mapping pdf
  ;; files to the `default' handler. That handler, will eventually
  ;; result in a lookup through the `mailcap' machinery which returns
  ;; `pdf-view-mode' as the handler for PDF files.
  (let* ((key "\\.pdf\\'")
         (apps (assoc-delete-all key org-file-apps))
         (newapps `((,key . "evince %s"))))
    (setq org-file-apps (append newapps apps)))

  :bind (("\C-coc" . org-capture)
         ("\C-ca" . org-agenda))
  :custom-face
  (org-document-info ((t (:foreground "pale turquoise" :height 1.5))))
  (org-document-title ((t (:foreground "pale turquoise" :weight bold :height 2.0))))
  (org-level-1 ((t (:inherit outline-1 :height 1.6))))
  (org-level-2 ((t (:inherit outline-2 :height 1.4))))
  (org-level-3 ((t (:inherit outline-3 :height 1.3))))
  (org-level-4 ((t (:inherit outline-4 :height 1.2))))
  (org-level-5 ((t (:inherit outline-5 :height 1.1))))
  (org-todo ((t (:foreground "Pink" :weight bold)))))

(use-package org-agenda
  :config
  (setq org-agenda-window-setup (quote current-window))
  (setq org-agenda-files (mapcar #'my-org-prefix (list "todo.org" "gcal.org")))
  :after org)

(use-package org-capture
  :commands org-capture
  :config
  ;; from https://github.com/sprig/org-capture-extension
  (defun my/square-to-round-brackets(string-to-transform)
    "Transforms STRING-TO-TRANSFORM by turning [ into ( and ] into ).  Other cars are unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
    )
  ;; We use ` instead of ' here as it enables evaluation of expressions by
  ;; prefixing them with ,
  (setq org-capture-templates
        `(("a" "Appointment" entry (file ,(my-org-prefix "gcal.org"))
           "* %?\n\n%^T\n\n")
          ("t" "todo" entry (file+headline ,(my-org-prefix "todo.org") "Tasks")
           "* TODO [#A] %?")
          ("m" "Mail" entry (file+headline ,(my-org-prefix "todo.org") "Incoming")
           "* TODO %^{Title}\n\n  Source: %u, %:link\n\n  %i"
           :empty-lines 1)
          ("p" "Protocol" entry (file+headline ,(my-org-prefix "todo.org") "Incoming")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file+headline ,(my-org-prefix "todo.org") "Incoming")
           "* %? [[%:link][%(my/square-to-round-brackets \"%:description\")]] \nCaptured On: %U")))
  :bind ("C-c c" . org-capture)
  :after org)

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode)
  :after org)

(use-package org-gcal
  :ensure t
  :commands (org-gcal-sync org-gcal-fetch)
  :config
  (require 'my-secrets)
  (setq org-gcal-client-id my/org-gcal-client-id
        org-gcal-client-secret my/org-gcal-client-secret
        org-gcal-file-alist `(("trulsa@gmail.com" .  ,(my-org-prefix "gcal.org"))))
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
  :after org)

;; Mutt <-> org integration
;; Configuration and scripts based on https://upsilon.cc/~zack/blog/posts/2010/02/integrating_Mutt_with_Org-mode/
;; standard org <-> remember stuff, RTFM

(use-package org-protocol
  :config
  ;; ensure that emacsclient will show just the note to be edited when invoked
  ;; from Mutt, and that it will shut down emacsclient once finished;
  ;; fallback to legacy behavior when not invoked via org-protocol.
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (defvar my-org-protocol-flag nil)
  (defadvice org-capture-finalize (after delete-frame-at-end activate)
    "Delete frame at remember finalization."
    (progn (if my-org-protocol-flag (delete-frame))
           (setq my-org-protocol-flag nil)))
  (defadvice org-capture-kill (after delete-frame-at-end activate)
    "Delete frame at remember abort."
    (progn (if my-org-protocol-flag (delete-frame))
           (setq my-org-protocol-flag nil)))
  (defadvice org-protocol-capture (before set-org-protocol-flag activate)
    (setq my-org-protocol-flag t))

  (defun open-mail-in-mutt (message)
    "Open a mail MESSAGE in Mutt, using an external terminal.

Message can be specified either by a path pointing inside a
Maildir, or by Message-ID."
    (interactive "MPath or Message-ID: ")
    (start-process "mutt-client" nil "gnome-terminal" "--"
                   (substitute-in-file-name "$HOME/.bin/mutt-open") message))

  ;; add support for "mutt:ID" links
  (org-link-set-parameters "mutt" :follow 'open-mail-in-mutt))

;;
;; Configuration for org clock
;;
(use-package org-clock
  :bind (("C-c w" . hydra-org-clock/body)
         :map org-agenda-mode-map
         ("C-c w" . hydra-org-agenda-clock/body))
  :after org-agenda
  :config
  (setq org-log-done 'time
        org-clock-idle-time nil
        org-clock-continuously nil
        org-clock-persist t
        org-clock-in-switch-to-state "STARTED"
        org-clock-in-resume nil
        org-clock-report-include-clocking-task t
        org-clock-out-remove-zero-time-clocks t
        ;; Too many clock entries clutter up a heading
        org-log-into-drawer t
        org-clock-into-drawer 1)

  (defun bh/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at (point))))

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

  :hydra ((hydra-org-clock (:color blue :hint nil)
  "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))

          (hydra-org-agenda-clock (:color blue :hint nil)
  "
Clock   In/out^
-----------------------------------------
        _i_n
        _g_oto entry
        _o_ut
        _q_uit
      "
  ("i" org-agenda-clock-in)
  ("o" org-agenda-clock-out)
  ("q" org-agenda-clock-cancel)
  ("g" org-agenda-clock-goto))))

(use-package org-ref
  :ensure t
  :pin melpa
  :init
  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Nextcloud/bibliography/notes.org"
        org-ref-default-bibliography '("~/Nextcloud/bibliography/bibliography.bib")
        org-ref-pdf-directory "~/Nextcloud/bibliography/bibtex-pdfs/"
        bibtex-dialect 'biblatex
        org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f")
        bibtex-completion-bibliography
        '("~/Nextcloud/bibliography/bibliography.bib")
        bibtex-completion-pdf-open-function 'org-open-file-with-system)

  :config
  ;; If bibtex-note-storage is not set, then set it to
  ;; org-ref-bibliography-notes as adding the note entry will
  ;; otherwise fail
  (setq doi-utils-make-notes-function
        (lambda ()
          (let ((key (cdr (assoc "=key=" (bibtex-parse-entry)))))
            (bibtex-find-entry key)
            (org-ref-open-bibtex-notes))))
  ;; (setq doi-utils-make-notes-function
  ;;       (lambda ()
  ;;         (bibtex-beginning-of-entry)
  ;;         (let ((bibtex-completion-notes-path
  ;;                (or bibtex-completion-notes-path
  ;;                    org-ref-bibliography-notes)))
  ;;           (if bibtex-completion-notes-path
  ;;               (bibtex-completion-edit-notes (list (cdr (assoc "=key=" (bibtex-parse-entry)))))
  ;;             (message "Neither `bibtex-completion-notes-path` or `org-ref-bibliography-notes` are set. Not adding bibliography note entry")))))


  ;; Insert the citation into the notes PDF as a \fullcite inside a blockquote
  (setq org-ref-create-notes-hook
        '((lambda ()
            (org-narrow-to-subtree)
            (insert (format "\n#+BEGIN_QUOTE\nfullcite:%s\n#+END_QUOTE\n"
                            (org-entry-get (point) "Custom_ID")))
            (goto-char (point-max)))))

  ;; Overrides of org-ref functions. Uses a better regex for matching DOIs
  (setq org-ref-pdf-doi-regex
                                        ;"10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+\\|10.1002/[^[:space:]]+")
                "10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+")

  (defun org-ref-extract-doi-from-pdf (pdf)
    "Try to extract a doi from a PDF file.
There may be more than one doi in the file. This function returns
all the ones it finds based on two patterns: doi: up to a quote,
bracket, space or end of line. dx.doi.org/up to a quote, bracket,
space or end of line.

If there is a trailing . we chomp it off. Returns a list of doi
strings, or nil.

"
  (with-temp-buffer
    (insert (shell-command-to-string (format "%s %s -"
                                             pdftotext-executable
                                             (shell-quote-argument (dnd-unescape-uri pdf)))))
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward org-ref-pdf-doi-regex nil t)
        ;; I don't know how to avoid a trailing . on some dois with the
        ;; expression above, so if it is there, I chomp it off here.
        (let ((doi (match-string 0)))
          (when (s-ends-with? "." doi)
            (setq doi (substring doi 0 (- (length doi) 1))))
          (cl-pushnew doi matches :test #'equal)))
      matches)))

  (defun org-ref-get-pdf-title (uri)
    (let ((title (shell-command-to-string
                  (format "pdftitle"
                          (shell-quote-argument (dnd-unescape-uri pdf))))))))

(defun org-ref-pdf-dnd-protocol (uri action)
  "Drag-n-drop protocol.
PDF will be a string like file:path.
ACTION is what to do. It is required for `dnd-protocol-alist'.
This function should only apply when in a bibtex file."
  (if (and (buffer-file-name)
           (f-ext? (buffer-file-name) "bib"))
      (let* ((path (substring uri 5))
             dois)
        (cond
         ((f-ext? path "pdf")
          (setq dois (org-ref-extract-doi-from-pdf
                      path))
          (cond
           ((null dois)
            (message "No doi found in %s" path)
            nil)
           ((= 1 (length dois))
            ;; we do not need to get the pdf, since we have one.
            (let ((doi-utils-download-pdf nil))
              ;; doi-utils-add-biblatex-entry-from-doi returns nil on
              ;; success and a string on failure
              (unless (doi-utils-add-bibtex-entry-from-doi
                       (car dois)
                       (buffer-file-name))
                ;; we should copy the pdf to the pdf directory though
                ;; TODO: Add custom variable here for deciding if file
                ;; should be moved or copied
                (let ((key (cdr (assoc "=key=" (bibtex-parse-entry)))))
                  (copy-file (dnd-unescape-uri path)(expand-file-name (format "%s.pdf" key) org-ref-pdf-directory)))))
            action)
           ;; Multiple DOIs found
           (t
            (helm :sources `((name . "Select a DOI")
                             (candidates . ,(org-ref-pdf-doi-candidates dois))
                             (action . org-ref-pdf-add-dois)))
            action)))
         ;; drag a bib file on and add contents to the end of the file.
         ((f-ext? path "bib")
          (goto-char (point-max))
          (insert "\n")
          (insert-file-contents path))))
    ;; ignoring. pass back to dnd. Copied from `org-download-dnd'. Apparently
    ;; returning nil does not do this.
    (let ((dnd-protocol-alist
           (rassq-delete-all
            'org-ref-pdf-dnd-protocol
            (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri)))))

(use-package company-bibtex
  :ensure t
  :after org-ref
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography org-ref-default-bibliography))

(use-package projectile
  :ensure t
  :pin melpa-stable
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

(use-package helm-projectile
  :ensure t)

(use-package helm-rg
  :ensure t
  :pin melpa)

(use-package pdf-tools
  :ensure t
  :preface
  (defun my-inhibit-global-nlinum-mode ()
    "Prevent nlinum mode from being enabled."
    (add-hook 'after-change-major-mode-hook
              (lambda () (nlinum-mode 0))
              :append :local))
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  ;; Disable nlinum-mode when PDF-view mode is enabled
  (add-hook 'pdf-view-mode-hook #'my-inhibit-global-nlinum-mode)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  ;; Enter correlate mode automatically
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

  ;; AucTeX query for master file
  (setq-default TeX-master nil)

  ;; Make AUCTeX build bibtex
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.)

  ;; Turn on RefTeX
  (setq reftex-default-bibliography '("~/Nextcloud/bibliography/references.bib"))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (defvar reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'text-auto-fill)

  ;; Preview mode settings
  (setq preview-auto-cache-preamble t
        preview-scale-function 1.2))

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
  (add-hook 'c-mode-common-hook #'lsp)
  :after lsp-mode)


(use-package highlight-indentation
  :ensure t
  :hook ((yaml-mode . highlight-indentation-mode))
  :config
  (setq highlight-indentation-blank-lines t)
  (set-face-background 'highlight-indentation-face "grey24")
  (set-face-background 'highlight-indentation-current-column-face "grey30"))

(use-package python
  :config
  (add-hook 'python-mode-hook #'lsp))

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
  (add-hook 'haskell-mode-hook #'lsp)
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
  :mode ("\\.r\\'" "\\.R\\'"))

(use-package visual-fill-column
  :ensure t)

(use-package mutt-mode
  :ensure t)

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
  :load-path "~/.emacs.d/lisp/slurm.el"
  :config
  (setq slurm-filter-user-at-start "trulsas")

(use-package slurm-script-mode
  :load-path "~/.emacs.d/lisp/slurm.el")

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :commands er/expand-region)

;;
;; Misc functions
;;
(defun my/recompile-all-packages ()
  "Recompile all packages"
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

;; MOVE TO BEGINNING OF LINE/TEXT
;;
;; rebind C-a to move to the beginning of text instead of beginning of line
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;;
(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)


;; Move lines up or down
(defun move-line-up ()
  "Move move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


(global-set-key (kbd "M-S-<up>") 'move-line-up)
(global-set-key (kbd "M-S-<down>") 'move-line-down)

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

;; Add command for duplicating a line and duplicating and commenting a
;; line

(defun comment-duplicate-line ()
  (interactive)
  (duplicate-line t))

(defun line-commented-p ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at-p comment-start-skip)))

(defun duplicate-line (&optional comment)
  (interactive)
  (let ((begin)
        (end))
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (end-of-line)
      (setq end (point))
      ;; Set last-command to nil as we don't want it to append to
      ;; previous kill ring entry if previous command was kill-region
      (let ((last-command nil))
        (copy-region-as-kill begin end))
      (open-line 1)
      (if (and comment
               (not (line-commented-p)))
          (comment-line 1)
        (next-line))
      (yank))
    (next-line)))

(global-set-key (kbd "C-S-c") #'comment-duplicate-line)
(global-set-key (kbd "C-S-d") #'duplicate-line)

(provide 'init)
;;; init.el ends here
