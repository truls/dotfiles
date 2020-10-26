;;
;; Magit config
;;

(use-package magit
  :ensure t
  ;; Stable melpa exhibits highlighting issues with Emacs 27
  ;; https://github.com/magit/magit/issues/3986
  ;; Unstable version of git-commit and with-editor was also installed
  :pin melpa
  ;; magit-toplevel is listed here because it is the first function
  ;; invoked by the git-fast-sync function below
  :commands (magit-status magit-toplevel)
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

;;
;; Pull and push in sequence
;;

(defun git-fast-sync--has-conflicts-p ()
  "Returns t if current git repository has conflicts."
  (> (length (vc-git-conflicted-files (magit-toplevel))) 0))

(defvar git-fast-sync--state 'pull
  "State of automated git pull push. States: 'pull 'commit 'push")

(defun git-fast-sync--pull ()
  "Pulls from current git repository from remote. Returns t if
pull resulted in no conflicts and nil otherwise."
  (interactive)
  (pcase-let ((`(,branch ,remote)
               (magit--select-push-remote "pull from there")))
    (run-hooks 'magit-credential-hook)
    (magit-run-git "pull" remote branch))
  (not (git-fast-sync--has-conflicts-p)))

(defun git-fast-sync--push ()
  "Pushes current git repository to remote. Always returns t."
  ;; TODO: Detect failures
  (interactive)
  (pcase-let ((`(,branch ,remote)
               (magit--select-push-remote "push there")))
    (run-hooks 'magit-credential-hook)
    (magit-run-git "push" "-v" remote
                   (format "refs/heads/%s:refs/heads/%s"
                           branch branch)))
  t)

(defun git-fast-sync ()
  "Performs a quick sync with a remote git repository by performing the following actions:
   - pull
   - commit all tracked files with a default commit message
   - push
   If the pull action results in conflicts, the function will skip
   the pull step and proceed directly to commit. The function will only run
   if there are no files with conflicts in the repository.

  A function like this is an EXTREMELY BAD IDEA in almost all cases. It is,
  however, quite useful fro interacting with overleaf documents via git."

  (interactive)
  (if (git-fast-sync--has-conflicts-p)
      (user-error "Please resolve repository conflicts before calling this function"))

  ;; The following abuses the -if-let* structure of dash.el to emulate
  ;; switch-case with fallthrough. This is used to implement a state
  ;; machine going through steps in a strict sequence and starting
  ;; from where the previous step left off in case it failed to
  ;; complete.
  ;;
  ;; -if-let* will only bind the subsequent value if the previous
  ;; binding was non-nil. Here, we let each binding return non-nil
  ;; only if a) the operation was successful or b) the current value
  ;; of git-fast-sync-state did not match what is handled by the
  ;; current case
  (-if-let* ((_ (if (eq git-fast-sync--state 'pull)
                    (progn
                      (message "Running pull")
                      (prog1
                          (git-fast-sync--pull)
                        (setq git-fast-sync--state 'commit)))
                  t))
             (_ (if (eq git-fast-sync--state 'commit)
                    (progn
                      (message "Running commit")
                      (prog1
                          (progn
                            (magit-run-git "add" "-u" ".")
                            (if (magit-anything-staged-p)
                                (magit-run-git "commit" "-m" "changed"))
                            t)
                        (setq git-fast-sync--state 'push)))
                  t))
             (_ (if (eq git-fast-sync--state 'push)
                    (progn
                      (message "Running push")
                      (prog1
                          (git-fast-sync--push)
                        (setq git-fast-sync--state 'pull)))
                  t)))
      (progn
        (message "")
        t)
    nil))

(provide 'magit-config)
