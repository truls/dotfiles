;; erc-config.el -*- lexical-binding: t -*-
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
                     (autojoin button completion fill irccontrols
                      list match menu move-to-prompt netsplit networks
                      noncommands notifications readonly ring stamp spelling
                      track truncate notifications))
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
  :straight t
  :after erc
  :commands (znc-all znc-erc)
  :init
  ;; TODO: Byte compile warning is issued from znc.el:119. Figure out
  ;; why this is
  (setq byte-compile-warnings nil)
  :config
  (require 'my-secrets)
  (setq znc-servers `(("bastion.terrifyinglytrue.eu" 6667 t
                       ((irc\.freenode\.org "trulsa"
                                            ,(shell-command-to-string "secret-tool lookup znc password"))))
                      ;; ("irc.gitter.im" 6667 t
                      ;;  ((irc\.gitter\.im ,gitter-username ,gitter-password)))
                      )))

(provide 'erc-config)
