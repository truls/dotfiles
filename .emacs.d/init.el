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
      '(("melpa-stable" .  0)
        ("gnu"          .  5)
        ("melpa"        .  10)))

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
                            (font . "Fira emacs-10")
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

(global-set-key (kbd "M-DEL") 'delete-indentation)

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

(global-set-key (read-kbd-macro "C-<Backspace>") 'backward-delete-word)

;;
;; Check if we are currently inside a comment (probably a hack)
;; FIXME: This isn't perfect and fails if on first char of a
;;
(defun inside-comment-p ()
  (let ((p (point)))
    (save-excursion
      (comment-only-p (comment-search-backward) p))))


(defun my/call-with-symbol-at-point (f)
  "Call F with symbol at point as an argument."
  (unless (functionp f)
    (error "Argument must be a function"))
  ;; Maybe cleaner to just use thing-at-point here
  (let ((bounds (find-tag-default-bounds)))
    (cond
     (bounds
      (apply f `(,(buffer-substring-no-properties
                   (car bounds) (cdr bounds)))))
     (t (user-error "No symbol at point")))))


(defun my/helm-projectile-ag-thing-at-point ()
  "Call `helm-projectile-ag' using symbol at point as default
input."
  (interactive)
  (let ((helm-ag-insert-at-point 'symbol))
    (helm-projectile-ag)))

;;
;; Config includes
;;
(require 'misc-pkgs-config)
(require 'lsp-config)
(require 'org-config)
(require 'org-ref-config)
(require 'erc-config)
(require 'latex-config)
(require 'magit-config)
(require 'flyspell-config)
(require 'lang-mode-config)
(require 'scala-config)

(provide 'init)
;;; init.el ends here
