;;; org-tasklist.el --- dr Wozniak’s tasklists in Org-mode

;; Copyright (C) 2017 Michal Rus

;; Author: Michal Rus <m@michalrus.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.2"))
;; Keywords: outlines
;; URL: https://github.com/michalrus/dotfiles/tree/master/.emacs.d/package-sketches.symlink

;;; Commentary:

;; org-tasklist lets you prioritize your top-level TODO items using
;; values (e.g. dollars) and investments (e.g. hours). They are then
;; sorted descending by the value/investment quotient.
;;
;; If you learn to assign correct values and investments and keep
;; doing tasks from the top of your list, you will be spending your
;; time the most effectively.
;;
;; Add `:tasklist t' to your org-capture template(s), e.g.:
;;
;;    (setq org-capture-templates
;;          '(("l" "Music → Listening" entry (file+olp "~/Org/Tasklist.org" "Music" "Listening")
;;             "* TODO %?\n%U\n%x"
;;             :tasklist t :kill-buffer t)))
;;
;; Also, splice `org-tasklist-agenda-options' into agenda view
;; configurations in `org-agenda-custom-commands', e.g.:
;;
;;    (setq org-agenda-custom-commands
;;          `(("t" "Tasklist" alltodo ""
;;             ((org-agenda-files '("~/Org/Tasklist.org"))
;;              ,@org-tasklist-agenda-options))))
;;
;; For more information, original idea, and motivation, see
;; https://www.supermemo.com/articles/tasklists.htm

;;; Code:

(require 'org)
(require 'org-agenda)

(defconst org-tasklist-agenda-options
  '((org-agenda-todo-list-sublevels nil)
    (org-agenda-prefix-format
     (let ((cpy (copy-alist org-agenda-prefix-format)))
       (progn
         (dolist (key '(todo tags search))
           (setcdr (assq key cpy)
                   (concat "%(org-tasklist--agenda-prefix-format) "
                           (alist-get key org-agenda-prefix-format))))
         cpy)))
    (org-agenda-sorting-strategy '(user-defined-down))
    (org-agenda-cmp-user-defined 'org-tasklist--agenda-sorting-cmp)))

(add-hook 'org-capture-prepare-finalize-hook 'org-tasklist--capture-prepare-finalize)
(defun org-tasklist--capture-prepare-finalize ()
  (unless org-note-abort
    (when (org-capture-get :tasklist t)
      (call-interactively 'org-tasklist-set-priority))))

(defun org-tasklist--is-1st-level-task-p (pom)
  "Will return `t' iff the task at `pom' is a TODO/DONE/… at the first level, i.e. not a subtask of another TODO/DONE/…, `nil' otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pom)
      (and (or (org-entry-is-todo-p)
               (org-entry-is-done-p))
           (or (eq (org-current-level) 1)
               (let* ((check-parent (lambda ()
                                      (save-excursion
                                        (let ((point-was-at (point)))
                                          (condition-case nil
                                              (outline-up-heading 1 t)
                                            (error nil))
                                          (cond ((or (org-entry-is-done-p) (org-entry-is-todo-p)) nil)
                                                ((eq (point) point-was-at) t)
                                                (t (funcall check-parent))))))))
                 (funcall check-parent)))))))

(defun org-tasklist--get-numeric-property (pom name)
  (let ((value      (org-entry-get (point) name)))
    (cond (value (float (string-to-number value)))
          (t nil))))

(defun org-tasklist--get-priority (pom)
  (let ((value      (org-tasklist--get-numeric-property (point) "TASKLIST_VALUE"))
        (investment (org-tasklist--get-numeric-property (point) "TASKLIST_INVESTMENT")))
    (cond ((and value investment) (/ value investment))
          (t nil))))

(defun org-tasklist--agenda-prefix-format ()
  (let ((prio (org-tasklist--get-priority (point))))
    (cond (prio
           (let (;; TODO: read units & labels from :PROPERTIES:
                 (unit-value "$")
                 (unit-investment "hr"))
             (format "%6.02f %s/%s"
                     prio
                     (substring unit-value 0 1)
                     (substring unit-investment 0 1))))
          (t (format "%6s %3s" "——" "")))))

(defun org-tasklist--agenda-sorting-cmp (a b)
  "Sorts agenda entries according to their tasklist priorities. Hacky—relies on entry formatting (priority being the first number) for performance."
  (let ((pa (string-to-number (or (car (split-string a)) "0")))
        (pb (string-to-number (or (car (split-string b)) "0"))))
    (cond ((eq pa pb) nil)
          ((eq 0 pa) 1)
          ((eq 0 pb) -1)
          ((< pa pb) -1)
          (t 1))))

(defun org-tasklist-set-priority (pom)
  "Set tasklist priority for the current headline."
  (interactive (list (point)))
  (unless (org-tasklist--is-1st-level-task-p pom)
    (error (format "Entry at %d is not a 1st-level task, aborting" pom)))
  (let* ((read-num (lambda (label unit default)
                     (let* ((str (read-string (format "%s [%s] (%f): " label unit default)
                                              nil nil (number-to-string default)))
                            (num (string-to-number str)))
                       (if (eq num 0) (error (format "%s [%s] must be a non-zero number, got ‘%s’" label unit str))) num)))
         ;; TODO: read units & labels from :PROPERTIES:
         (value-default      (or (org-tasklist--get-numeric-property (point) "TASKLIST_VALUE") 25.0))
         (investment-default (or (org-tasklist--get-numeric-property (point) "TASKLIST_INVESTMENT") 0.5))
         (value      (funcall read-num "value" "$" value-default))
         (investment (funcall read-num "time"  "hr" investment-default)))
    (org-entry-put pom "TASKLIST_VALUE" (number-to-string value))
    (org-entry-put pom "TASKLIST_INVESTMENT" (number-to-string investment))))

(defun org-tasklist-agenda-set-priority ()
  "Set tasklist priority for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (save-excursion
          (org-show-context 'agenda))
        (save-excursion
          (and (outline-next-heading)
               (org-flag-heading nil)))   ; show the next heading
        (goto-char pos)
        (call-interactively 'org-tasklist-set-priority))))
  (org-agenda-redo t))

(defun org-tasklist-archive-all-done ()
  "Archive all done to-do’s reachable from top level in the current Org buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (cond ((org-entry-is-todo-p)
            ;; skip TODO subtree altogether
            (setq org-map-continue-from
                  (save-excursion
                    (outline-end-of-subtree)
                    (forward-char)
                    (point))))
           ((org-entry-is-done-p)
            (let ((point-was-at (point)))
              (message "Siema! @ %d" point-was-at)
              (org-archive-subtree)
              (setq org-map-continue-from point-was-at)))))
   t 'file))

(provide 'org-tasklist)
