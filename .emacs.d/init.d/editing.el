;; don’t clutter file trees with backups~ and #autosaves#
(defconst user-emacs-temporary-directory (expand-file-name "./tmp/" user-emacs-directory))
(setq backup-directory-alist `((".*" . ,user-emacs-temporary-directory)))
(setq auto-save-file-name-transforms `((".*" ,user-emacs-temporary-directory t)))

;; taaabs
(setq tab-width 2)
(setq indent-tabs-mode nil)

;; always hilight the corresponding parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; automatically pair braces, quotes, etc.
(use-package autopair :config (autopair-global-mode 1) :ensure t)

;; auto complete
(use-package auto-complete :config (ac-config-default) :ensure t)
