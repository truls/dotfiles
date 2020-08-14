;;; Misc line manipulation functions --- line-manip.el -*- lexical-binding: t -*-



;; Add command for duplicating a line and duplicating and commenting a
;; line
(defun comment-duplicate-line ()
  (interactive)
  (duplicate-line t))

(defun line-commented-p ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (when comment-start-skip
      (looking-at-p comment-start-skip))))

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


(provide 'line-manip)
