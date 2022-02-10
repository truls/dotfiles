;; package -- org-config.el -*- lexical-binding:t -*-

;;
;; Org configuration
;;
(use-package org
  :straight t
  :defer t
  :preface
  (defun my-org-prefix (file)
    "Prepend local org prefix to FILE."
    (concat "~/Nextcloud/gtd/" file))

  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)"))
        org-default-notes-file (my-org-prefix "todo.org")
        ;;set priority range from A to C with default A
        org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?A
        org-babel-load-languages (quote ((emacs-lisp . t) (latex . t)))
        org-startup-with-inline-images t
        org-support-shift-select t
        org-log-done 'time)

  (setq org-refile-targets
        `((,(my-org-prefix "gtd.org") :maxlevel . 3)
          (,(my-org-prefix "someday.org") :maxlevel . 1)
          (,(my-org-prefix "tickler.org") :maxlevel . 2)))

  ;; Default to opening PDFs in evince
  ;; We do this by removing the entry in org-file-apps mapping pdf
  ;; files to the `default' handler. That handler, will eventually
  ;; result in a lookup through the `mailcap' machinery which returns
  ;; `pdf-view-mode' as the handler for PDF files.
  (let* ((key "\\.pdf\\'")
         (apps (assoc-delete-all key org-file-apps))
         (newapps `((,key . "exec nohup evince %s"))))
    (setq org-file-apps (append newapps apps)))

  (with-eval-after-load 'ox-latex
    (setq org-latex-classes
          (let ((entry '("ta-article"
                         "\\documentclass[]{ta-article}"
                         ("\\section{%s}" . "\\section*{%s}")
                         ("\\subsection{%s}" . "\\subsection*{%s}")
                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                         ("\\paragraph{%s}" . "\\paragraph*{%s}")
                         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
            (if (--any? (string= (car it) "ta-article") org-latex-classes)
                (-map-first (lambda (x) (string= (car x) "ta-article"))
                            (lambda (_) entry) org-latex-classes)
              (add-to-list 'org-latex-classes entry)))))

  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t
           ("pdflatex"))
          ("T1" "fontenc" t
           ("pdflatex"))
          (#1="" "graphicx" t)
          (#1# "grffile" t)
         (#1# "longtable" nil)
          (#1# "wrapfig" nil)
          (#1# "rotating" nil)
          ("normalem" "ulem" t)
          (#1# "amsmath" t)
          (#1# "textcomp" t)
          (#1# "capt-of" nil)
          (#1# "hyperref" nil)))


  :bind (("\C-coc" . org-capture)
         ("\C-ca" . org-agenda)
         :map org-mode-map
         ;; Allow M-<arrow keys> to be used for changing windows
         ("M-s-<down>" . org-metadown)
         ("M-s-<up>" . org-metaup)
         ("M-s-<right>" . org-metaright)
         ("M-s-<left>" . org-metaleft)
         ("M-<down>" . nil)
         ("M-<up>" . nil)
         ("M-<right>" . nil)
         ("M-<left>" . nil))

  :custom-face
  (org-document-info  ((t (:foreground "pale turquoise" :height 1.5))))
  (org-document-title ((t (:foreground "pale turquoise" :weight bold :height 2.0))))
  (org-level-1        ((t (:inherit outline-1 :height 1.6))))
  (org-level-2        ((t (:inherit outline-2 :height 1.4))))
  (org-level-3        ((t (:inherit outline-3 :height 1.3))))
  (org-level-4        ((t (:inherit outline-4 :height 1.2))))
  (org-level-5        ((t (:inherit outline-5 :height 1.1))))
  (org-todo           ((t (:foreground "Pink" :weight bold))))
  (org-tag            ((t (:weight bold :height 0.5)))))
  ;;(org-default        ((t (:family ")

(use-package org-agenda
  :config
  (setq org-agenda-window-setup (quote current-window))
  (setq org-agenda-files (mapcar #'my-org-prefix (list "tickler.org" "gtd.org" "inbox.org" "roam")))

  (setq org-agenda-custom-commands
        '(("f" "All first TODOs" todo "TODO|STARTED|WAITING"
           ((org-agenda-overriding-header "All first items")
            (org-agenda-skip-function #'org-config-agenda-skip-all-siblings-but-first)))
          ("w" "Weekly review"
           agenda ""
           ((org-agenda-start-day "-14d")
            (org-agenda-span 14)
            (org-agenda-start-on-weekday 1)
            (org-agenda-start-with-log-mode '(closed))
            (org-agenda-archives-mode t)
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE "))))))
  :after org)

(use-package org-capture
  :commands org-capture
  :config
  ;; from https://github.com/sprig/org-capture-extension
  (defun my/square-to-round-brackets (string-to-transform)
    "Transforms STRING-TO-TRANSFORM by turning [ into ( and ] into ).  Other cars are unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
  ;; We use ` instead of ' here as it enables evaluation of expressions by
  ;; prefixing them with ,
  (setq org-capture-templates
        ;; `(("a" "Appointment" entry (file ,(my-org-prefix "gcal.org"))
        ;;    "* %?\n\n%^T\n\n")
        `(("t" "todo" entry (file+headline ,(my-org-prefix "inbox.org") "Tasks")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline ,(my-org-prefix "tickler.org") "Tickler")
           "* %i%? \n %U")
          ("m" "Mail" entry (file+headline ,(my-org-prefix "inbox.org") "Emails")
           "* TODO %^{Title}\n\n  Source: %u, %:link\n\n  %i"
           :empty-lines 1)
          ("p" "Protocol" entry (file+headline ,(my-org-prefix "inbox.org") "Websites")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file+headline ,(my-org-prefix "inbox.org") "Websites")
           "* %? [[%:link][%(my/square-to-round-brackets \"%:description\")]] \nCaptured On: %U")))
  :bind ("C-c c" . org-capture)
  :after org)

(use-package org-bullets
  :straight t
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode)
  :after org)

(use-package org-gcal
  :straight t
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

(use-package org-pomodoro
  :straight t
  :commands org-pomodoro)

(defun org-config-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-config-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-config-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-config-current-is-todo ()
  (or
   (string= "TODO" (org-get-todo-state))
   (string= "STARTED" (org-get-todo-state))
   (string= "WAITING" (org-get-todo-state))))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Nextcloud/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(provide 'org-config)
;;; org-config ends here
