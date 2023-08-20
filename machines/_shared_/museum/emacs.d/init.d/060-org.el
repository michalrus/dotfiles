;; -*- mode: emacs-lisp -*-

(use-package org
  :demand t ;; to have org-agenda commands defined without opening any org files

  :mode ("\\.org\\'" . org-mode)

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))

  :config
  (setq org-directory "~/Org")

  (defun michalrus/org-files ()
    (seq-filter 'file-exists-p
                (directory-files-recursively org-directory "\\.org\\'")))

  (setq-default
     org-refile-targets '((michalrus/org-files . (:level . 0)))
     org-refile-use-outline-path 'file
     org-todo-keywords '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "WAITING(w@)" "|" "DONE(d!)" "DELEGATED(g@)" "CANCELED(c@)"))
     org-log-states-order-reversed nil
     org-reverse-note-order nil
     org-log-into-drawer t
     org-startup-indented t
     org-log-done nil
     org-support-shift-select t
     org-catch-invisible-edits 'error
     org-ctrl-k-protect-subtree 'error)

  (use-package org-id
    :init
    (setq org-id-link-to-org-use-id t))

  ;; Unbind S-left/right, because I’m logging every state change!
  (dolist (dir '("left" "right"))
    (define-key org-mode-map (kbd (format "S-<%s>" dir)) nil))

  (use-package org-tasklist
    :demand t ;; to have org-agenda commands defined without opening any org files
    :bind (:map org-mode-map
           ("C-c C-a" . org-tasklist-archive-all-done)
           ("C-c C-v" . org-tasklist-set-priority)
           :map org-agenda-mode-map
           ("C-c C-v" . org-tasklist-agenda-set-priority))

    :config

    (defun michalrus/existing-wildcards (&rest ws)
      (seq-filter 'file-exists-p
                  (seq-mapcat 'file-expand-wildcards ws)))

    (setq org-agenda-custom-commands
          `(
            ("g" "General"  alltodo "" ((org-agenda-files (michalrus/existing-wildcards "~/Org/Tooling/*.org"
                                                                                        "~/Org/Life/*.org"
                                                                                        "~/Org/Homes/*.org"   )) ,@org-tasklist-agenda-options))
            ("p" "Projects" alltodo "" ((org-agenda-files (michalrus/existing-wildcards "~/Org/Projects/*.org")) ,@org-tasklist-agenda-options))
            ))

    (setq org-capture-templates
          (let* ((subdir (lambda (dir template)
                           `((,(downcase (substring dir 0 1)) ,dir)
                             ,@(seq-map (lambda (file)
                                          (let ((filename (file-name-base file)))
                                          `(,(concat (downcase (substring dir 0 1))
                                                     (downcase (substring filename 0 1)))
                                            ,filename
                                            entry
                                            (file ,file)
                                            ,template
                                            :tasklist t :killbuffer t)))
                                        (michalrus/existing-wildcards (concat "~/Org/" dir "/*.org")))))))
            `(("l" "Music → Listening" entry (file+olp "~/Org/Tasklist.org" "Music" "Listening")
               "* TODO %?\n%U\n%x"
               :tasklist t :kill-buffer t)

              ,@(funcall subdir "Tooling"  "* TODO %?\n%U\n%a")
              ,@(funcall subdir "Life"     "* TODO %?\n%U")
              ,@(funcall subdir "Homes"    "* TODO %?\n%U")
              ,@(funcall subdir "Projects" "* TODO %?\n%U\n%a")
              )))
    ))
