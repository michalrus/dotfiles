;; -*- mode: emacs-lisp -*-

;; don’t clutter file trees with backups~ and #autosaves#
(let ((tmp (expand-file-name "./tmp/" user-emacs-directory)))
  (setq backup-directory-alist `((".*" . ,tmp)))
  (setq auto-save-file-name-transforms `((".*" ,tmp t))))

;; sane whitespace behavior
(setq-default require-final-newline t)
(setq-default delete-trailing-lines t)

(use-package focus-autosave-mode
  :config
  (focus-autosave-mode 1))

(use-package super-save
  :config
  (setq auto-save-default nil
        super-save-auto-save-when-idle t
        super-save-idle-duration (* 5 60))
  (super-save-mode +1))

(use-package ws-butler
  :config
  (setq ws-butler-global-exempt-modes nil
        ws-butler-keep-whitespace-before-point t)
  (ws-butler-global-mode 1))
