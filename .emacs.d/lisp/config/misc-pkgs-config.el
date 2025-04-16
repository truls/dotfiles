(use-package gnu-elpa-keyring-update
  :straight t)

(use-package diminish :straight t)
(use-package use-package-hydra :straight t)

;; Enable auto-compile package to automatically compile elisp files
(use-package auto-compile
  :straight t
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
  :straight t
  :after
  toggle-theme
  :config
  (toggle-theme-restore-theme))

(use-package google-this
  :straight t
  :diminish
  :init
  (setq google-this-keybind (kbd "C-x \\"))
  :config
  (google-this-mode t))

(use-package visual-regexp-steroids
  :straight t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)
         :map esc-map
         ("C-r" . vr/isearch-backward)
         ("C-s" . vr/isearch-forward)))


(use-package hydra
  :straight t
  :commands defhydra)

;;
;; ivy-mode
;;
(use-package ivy
  :straight t
  :delight
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package swiper
  :straight t
  :defer t
  :config
  (defun my/swiper-symbol-at-point ()
    (interactive)
    (my/call-with-symbol-at-point #'swiper)))

;; Install smex to show the most used commands first in M-x list
(use-package smex
  :straight t
  :init
  (setq smex-save-file (expand-file-name "smex-items"
                                         user-emacs-directory)))

(use-package counsel
  :straight t
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
  :straight t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :after ivy)


(use-package solidity-flycheck
  :straight t
  :init
  (setq solidity-flycheck-solium-checker-active t)
  (setq solidity-flycheck-solc-checker-active t)
  :after solidity-mode)

(use-package company-solidity
  :straight t
  :after solidity-mode)


;;
;; flycheck-mode
;;
(use-package flycheck
  :straight t
  :config
  (add-hook 'flycheck-error-list-mode-hook
            (lambda () (toggle-truncate-lines +1))))

(use-package company
  :straight t
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
  :straight t
  :config
  (use-package yasnippet-snippets
    :straight t)
  (yas-reload-all)
  (when yas-minor-mode
    (ivy-yasnippet))
  :hook ((elisp-mode . yas-minor-mode)
         (R-mode     . yas-minor-mode))
  :commands (yas-minior-mode yas-global-mode yas-expand-snippet)
  :diminish yas-minor-mode)

(use-package ivy-yasnippet
  :straight t
  :commands ivy-yasnippet)

(use-package which-key
  :straight t
  :config
  (which-key-mode))


(use-package display-line-numbers
  :no-require t
  :config
  (global-display-line-numbers-mode))

(use-package seq :straight t)
(use-package whitespace
  :straight t
  :delight global-whitespace-mode
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
;;   :straight t
;;   :after org-ref
;;   :config
;;   (add-to-list 'company-backends 'company-bibtex)
;;   (setq company-bibtex-bibliography org-ref-default-bibliography))

(use-package projectile
  :straight t
  :diminish
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-use-git-grep t)
  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

(use-package helm
  :straight t)

(use-package helm-projectile
  :bind (("C-'" . helm-projectile-rg)
         ("C-\"" . my/helm-projectile-ag-thing-at-point))
  :config
  (setq helm-projectile-ignore-strategy 'search-tool)
  :straight t)

(use-package helm-rg
  :straight t)

(use-package helm-ag
  :straight t)

(use-package fzf
  :straight t
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package pdf-tools
  :straight t
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



(use-package highlight-indentation
  :straight t
  :hook ((yaml-mode . highlight-indentation-mode))
  :config
  (setq highlight-indentation-blank-lines t)
  (set-face-background 'highlight-indentation-face "grey24")
  (set-face-background 'highlight-indentation-current-column-face "grey30"))

(use-package smart-shift
  :straight t
  :config
  (global-smart-shift-mode 1))

(use-package langtool
  :straight t
  :config
  ;; Run my-install.sh script in language-servers/languagetool to
  ;; install languagetool
  (setq langtool-language-tool-jar (expand-file-name "language-servers/languagetool/LanguageTool-4.4-stable/languagetool-commandline.jar" user-emacs-directory)))


(use-package ace-window
  :straight t
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\; ?w ?e ?r ?u ?i ?o ?q ?t ?y ?p)))

(use-package avy
  :straight t
  :bind (("C-:" . avy-goto-char)))

(use-package package-lint
  :straight t
  :commands (package-lint-buffer
             package-lint-current-buffer
             pacakge-lint-batch-and-exit
             package-lint-looks-like-a-package-p))

;; Fails to compile
;; (use-package json-navigator
;;   :straight t)

;; (use-package slurm-mode
;;   :load-path "~/misc/projects/slurm.el"
;;   :config
;;   (setq slurm-remote-username "trulsas")
;;   (setq slurm-remote-host "stallo.uit.no"))

(use-package expand-region
  :straight t
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
  :straight t
  :commands easy-escape-minor-mode
  :config
  (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode))

(use-package windmove
  :no-require t
  :config
  (setq windmove-wrap-around t))


;; Prevent frequent garbage collections from interfering with Emacs
;; performance by increasing the GC threshold high threshold when Emacs
;; is in use and decrease it after Emacs has been idel for a while.
(use-package gcmh
  :straight t
  :diminish
  :config
  (setq gcmh-high-cons-threshold #x60000000
        gcmh-verbose t)
  (gcmh-mode +1))

;;
;; gitignore-templates
;;
(use-package gitignore-templates
  :straight t
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

;;
;; editorconfig
;;
(use-package editorconfig
  :straight t
  :diminish
  :config
  (editorconfig-mode 1))

;;
;; dash-docs
;;
(use-package dash-docs
  :straight t
  :init
  (setq dash-docs-docsets-path (expand-file-name "~/.config/docsets")))

;;
;; counsel-dash
;;
(use-package counsel-dash
  :straight t)

;;
;; writeroom-mode
;;
(use-package writeroom-mode
  :straight t
  :commands
  writeroom-mode)

;;
;; vagrant-tramp
;;
(use-package vagrant-tramp
  :straight t)

;;
;; toggle-font
;;
(use-package toggle-font
  :load-path "~/.emacs.d/lisp/toggle-font"
  :init
  (setq toggle-font-alternative-family "DejaVu Sans"))

;;
;; writegood-mode
;;
(use-package writegood-mode
  :straight t
  :commands
  writegood-mode)

;;
;; Github copilot
;;
(use-package copilot
  :straight (copilot :type git
                     :host github
                     :repo "zerolfx/copilot.el"
                     :files ("*.el" "dist")))

(provide 'misc-pkgs-config)
