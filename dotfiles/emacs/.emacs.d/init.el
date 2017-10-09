;; -*- mode: emacs-lisp -*-

;; to keep package.el from adding (package-initialize) at the beginning of init.el
;(package-initialize)

;; to keep Custom from writing directly to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load-file custom-file))

;; load *.el files in init.d/ alphabetically
(dolist (part (directory-files (expand-file-name "init.d" user-emacs-directory) t "\\.el$"))
  (load-file part))
