;; org-ref-config.el -*- lexical-binding:t -*-

(use-package helm-bibtex
  :straight t)

(use-package bibtex-completion
  :straight t)

(use-package org-ref
  :straight t
  :init
  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Nextcloud/bibliography/notes.org"
        org-ref-default-bibliography '("~/Nextcloud/bibliography/bibliography.bib")
        org-ref-pdf-directory "~/Nextcloud/bibliography/bibtex-pdfs/"
        bibtex-dialect 'biblatex
        org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f")
        bibtex-completion-bibliography '("~/Nextcloud/bibliography/bibliography.bib")
        bibtex-completion-pdf-open-function 'org-open-file-with-system)

   :config
   ;; Insert the citation into the notes PDF as a \fullcite inside a blockquote
   (setq org-ref-create-notes-hook
         '((lambda ()
             (org-narrow-to-subtree)
             (insert (format "\n#+BEGIN_QUOTE\nfullcite:%s\n#+END_QUOTE\n"
                             (org-entry-get (point) "Custom_ID")))
             (goto-char (point-max)))))

   ;; Overrides of org-ref functions. Uses a better regex for matching DOIs
   (setq org-ref-pdf-doi-regex
         ;;"10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+\\|10.1002/[^[:space:]]+")
         "10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+"))


;;   ;; Journal definitions
;;   ;; (concat org-ref-bibtex-journal-abbreviations
;;   ;;         '(("sigarch" "acm sigarch computer architecture news" "acm sigarch comp. arch. new.")
;;   ;;           ("sigplan" "acm sigplan notices" "acm sigarch not")))
;;   ;; If bibtex-note-storage is not set, then set it to
;;   ;; org-ref-bibliography-notes as adding the note entry will
;;   ;; otherwise fail
;;   (setq doi-utils-make-notes-function
;;         (lambda ()
;;           (let ((key (cdr (assoc "=key=" (bibtex-parse-entry)))))
;;             (bibtex-find-entry key)
;;             (org-ref-open-bibtex-notes))))
;;   (defun my/bibtex-get-entry-key ()
;;         (save-excursion
;;           (bibtex-beginning-of-entry)
;;           (let ((bibtex-expand-strings t)
;;                  (entry (bibtex-parse-entry t)))
;;             (reftex-get-bib-field "=key=" entry))))
;;   (defun my/gen-bibtex-key ()
;;     "Generate bibtex entry autokey. Copied from org-ref `orcb-key' function."
;;     (let ((key (funcall org-ref-clean-bibtex-key-function
;;                         (bibtex-generate-autokey))))
;;       (replace-regexp-in-string "\\\\" "" key)))
;;   (defun my/rename-bibtex-pdf ()
;;     "Rename PDF associated with entry or print message if no PDF is found."
;;     (interactive)
;;     (let* ((key (my/bibtex-get-entry-key))
;;            (newkey (my/gen-bibtex-key))
;;            (pdf (funcall org-ref-get-pdf-filename-function key))
;;            (newpdf (funcall org-ref-get-pdf-filename-function newkey)))
;;       (if (file-exists-p pdf)
;;           (unless (string= pdf newpdf)
;;               (rename-file pdf newpdf))
;;             (message (format "Entry %s is missing associated PDF file" key)))))

;;   (defun my/rename-org-notes-key ()
;;     "Rename strings in note file matching FROM to TO."
;;     (let ((from (my/bibtex-get-entry-key))
;;           (to (my/gen-bibtex-key)))
;;       (unless (string= from to)
;;         (save-restriction
;;           (save-window-excursion
;;             (when org-ref-bibliography-notes
;;               (find-file-other-window org-ref-bibliography-notes)
;;               (widen)
;;               (goto-char (point-min))
;;               (org-save-outline-visibility t
;;                 (ignore-errors
;;                   ;; FIXME: There must be a better way to reveal the
;;                   ;; whole file
;;                   (while (outline-next-heading)
;;                     (org-show-subtree))
;;                   (goto-char (point-min))
;;                   (while (search-forward from)
;;                     (replace-match to t))))))))))

;;         ;; FIXME: There must be a better way of 0

;;   (defun my/orcb-& ()
;;   "Replace naked & with \& in a bibtex entry."
;;   (save-restriction
;;     (bibtex-narrow-to-entry)
;;     (bibtex-beginning-of-entry)
;;     (while (re-search-forward " & " nil t)
;;       (save-excursion
;;         (goto-char (- (point) 2))
;;         (unless (looking-at-p "\\\\")
;;           (replace-match " \\\\& "))))))

;;   (defun my/orcb-% ()
;;     "Replace naked % with \% in a bibtex entry."
;;     (save-restriction
;;       (bibtex-narrow-to-entry)
;;       (bibtex-beginning-of-entry)
;;       (while (re-search-forward "%" nil t)
;;         (save-excursion
;;           (goto-char (- (point) 2))
;;           (unless (looking-at-p "\\\\")
;;             (replace-match " \\\\%"))))))

;;   (setq my/bibtex-field-replace-list '(("journal" . "journaltitle")))

;;   (defun my/bibtex-entry-article-to-inproceedings ()
;;     "Change miscategorized @article bibtex entry to @inproceedings."
;;     (interactive)
;;     (bibtex-beginning-of-entry)
;;     (save-restriction
;;       (bibtex-narrow-to-entry)
;;       (search-forward "@article")
;;       (replace-match "@inproceedings")
;;       (let ((replacemap
;;              '(("journal" . "booktitle")
;;                ("journaltitle" . "booktitle"))))
;;         (my/org-ref-fixup-downcase-fields replacemap)))
;;     )

;; (defun my/org-ref-fixup-downcase-fields (&optional replacemap)
;;   "Downcase the entry type and fields.
;; Optionally override `my/bibtex-field-replace-list' with REPLACEMAP."
;;   (unless replacemap
;;     (setq replacemap my/bibtex-field-replace-list))
;;   (bibtex-beginning-of-entry)
;;   (let* ((entry (bibtex-parse-entry))
;;          (entry-fields)
;;          (type (cdr (assoc "=type=" entry)))
;;          (key (cdr (assoc "=key=" entry))))

;;     (setq entry-fields (mapcar (lambda (x) (car x)) entry))
;;     ;; we do not want to reenter these fields
;;     (setq entry-fields (remove "=key=" entry-fields))
;;     (setq entry-fields (remove "=type=" entry-fields))
;;     (unless (and
;;              (let ((replacables (-map #'car replacemap)))
;;                     (-all? (lambda (x) (not (-contains? replacables (downcase x)))) entry-fields))
;;              (-all? (lambda (x) (string= x (downcase x))) (cons type entry-fields)))
;;       (bibtex-kill-entry)
;;       (insert
;;        (concat "@" (downcase type) "{" key ",\n"
;;                (mapconcat
;;                 (lambda (field)
;;                   (format "%s = %s,"
;;                           (let ((field- (downcase field)))
;;                             (alist-get field- replacemap field- nil #'string=))
;;                           (cdr (assoc field entry))))
;;                 entry-fields "\n")
;;                "\n}"))
;;       (bibtex-find-entry key))
;;     (bibtex-fill-entry)
;;     (unwind-protect
;;         (bibtex-clean-entry)
;;       (message "bibtex-clean-entry failed for entry %s" key))))

;; (defun my/orcb-check-journal ()
;;   "Check entry at point to see if journal exists in `org-ref-bibtex-journal-abbreviations'.
;; If not, issue a warning."
;;   (interactive)
;;   (when
;;       (string= "article"
;;                (downcase
;;                 (cdr (assoc "=type=" (bibtex-parse-entry)))))
;;     (save-excursion
;;       (bibtex-beginning-of-entry)
;;       (let* ((entry (bibtex-parse-entry t))
;;              (journal (reftex-get-bib-field "journaltitle" entry)))
;;         (when (null journal)
;;           (error "Unable to get journal for this entry."))
;;         (unless (member journal (-flatten org-ref-bibtex-journal-abbreviations))
;;           (message "Journal \"%s\" not found in org-ref-bibtex-journal-abbreviations." journal))))))


;;   ;;(add-hook #'my/rename-bibtex-pdf org-ref-clean-bibtex-entry-hook)
;;   ;; TODO: Why doesn't add-hook work?
;;   (setq org-ref-clean-bibtex-entry-hook
;;         '(my/org-ref-fixup-downcase-fields
;;           org-ref-bibtex-format-url-if-doi
;;           orcb-key-comma
;;           org-ref-replace-nonascii
;;           my/orcb-&
;;           my/orcb-%
;;           org-ref-title-case-article
;;           orcb-clean-year
;;           my/rename-bibtex-pdf
;;           my/rename-org-notes-key
;;           orcb-key
;;           orcb-clean-doi
;;           orcb-clean-pages
;;           my/orcb-check-journal
;;           org-ref-sort-bibtex-entry
;;           orcb-fix-spacing))

;;   ;; (setq doi-utils-make-notes-function
;;   ;;       (lambda ()
;;   ;;         (bibtex-beginning-of-entry)
;;   ;;         (let ((bibtex-completion-notes-path
;;   ;;                (or bibtex-completion-notes-path
;;   ;;                    org-ref-bibliography-notes)))
;;   ;;           (if bibtex-completion-notes-path
;;   ;;               (bibtex-completion-edit-notes (list (cdr (assoc "=key=" (bibtex-parse-entry)))))
;;   ;;             (message "Neither `bibtex-completion-notes-path` or `org-ref-bibliography-notes` are set. Not adding bibliography note entry")))))


;;   (defun org-ref-extract-doi-from-pdf (pdf)
;;     "Try to extract a doi from a PDF file.
;; There may be more than one doi in the file. This function returns
;; all the ones it finds based on two patterns: doi: up to a quote,
;; bracket, space or end of line. dx.doi.org/up to a quote, bracket,
;; space or end of line.

;; If there is a trailing . we chomp it off. Returns a list of doi
;; strings, or nil.

;; "
;;   (with-temp-buffer
;;     (insert (shell-command-to-string (format "%s %s -"
;;                                              pdftotext-executable
;;                                              (shell-quote-argument (dnd-unescape-uri pdf)))))
;;     (goto-char (point-min))
;;     (let ((matches '()))
;;       (while (re-search-forward org-ref-pdf-doi-regex nil t)
;;         ;; I don't know how to avoid a trailing . on some dois with the
;;         ;; expression above, so if it is there, I chomp it off here.
;;         (let ((doi (match-string 0)))
;;           (when (s-ends-with? "." doi)
;;             (setq doi (substring doi 0 (- (length doi) 1))))
;;           (cl-pushnew doi matches :test #'equal)))
;;       matches)))

;;   (defun org-ref-get-pdf-title (uri)
;;     (let ((title (shell-command-to-string
;;                   (format "pdftitle"
;;                           (shell-quote-argument (dnd-unescape-uri pdf))))))))

;; (defun org-ref-pdf-dnd-protocol (uri action)
;;   "Drag-n-drop protocol.
;; PDF will be a string like file:path.
;; ACTION is what to do. It is required for `dnd-protocol-alist'.
;; This function should only apply when in a bibtex file."
;;   (if (and (buffer-file-name)
;;            (f-ext? (buffer-file-name) "bib"))
;;       (let* ((path (substring uri 5))
;;              dois)
;;         (cond
;;          ((f-ext? path "pdf")
;;           (setq dois (org-ref-extract-doi-from-pdf
;;                       path))
;;           (cond
;;            ((null dois)
;;             (message "No doi found in %s" path)
;;             nil)
;;            ((= 1 (length dois))
;;             ;; we do not need to get the pdf, since we have one.
;;             (let ((doi-utils-download-pdf nil))
;;               ;; doi-utils-add-biblatex-entry-from-doi returns nil on
;;               ;; success and a string on failure
;;               (unless (doi-utils-add-bibtex-entry-from-doi
;;                        (car dois)
;;                        (buffer-file-name))
;;                 ;; we should copy the pdf to the pdf directory though
;;                 ;; TODO: Add custom variable here for deciding if file
;;                 ;; should be moved or copied
;;                 (let ((key (cdr (assoc "=key=" (bibtex-parse-entry)))))
;;                   (copy-file (dnd-unescape-uri path)(expand-file-name (format "%s.pdf" key) org-ref-pdf-directory)))))
;;             action)
;;            ;; Multiple DOIs found
;;            (t
;;             (helm :sources `((name . "Select a DOI")
;;                              (candidates . ,(org-ref-pdf-doi-candidates dois))
;;                              (action . org-ref-pdf-add-dois)))
;;             action)))
;;          ;; drag a bib file on and add contents to the end of the file.
;;          ((f-ext? path "bib")
;;           (goto-char (point-max))
;;           (insert "\n")
;;           (insert-file-contents path))))
;;     ;; ignoring. pass back to dnd. Copied from `org-download-dnd'. Apparently
;;     ;; returning nil does not do this.
;;     (let ((dnd-protocol-alist
;;            (rassq-delete-all
;;             'org-ref-pdf-dnd-protocol
;;             (copy-alist dnd-protocol-alist))))
;;       (dnd-handle-one-url nil action uri)))))

(provide 'org-ref-config)
;;; org-ref-config.el ends here
