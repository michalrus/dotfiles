;; -*- mode: emacs-lisp -*-

;; scroll the compilation output by default
(setq compilation-scroll-output t)

(defun michalrus/setup-compilation-with-make ()
  "Sets current buffer to compile (C-c C-c) using the nearest Makefile up in the directory hierarchy."
  (interactive)
  (let ((makefile-directory (locate-dominating-file default-directory "Makefile")))
    (when makefile-directory
      (set (make-local-variable 'compile-command) (format "make -k -C %s" makefile-directory))
      (set (make-local-variable 'compilation-read-command) nil)
      (local-set-key (kbd "C-c C-c") 'compile))))

;; set Makefile-mode to run `make' on C-c C-c
(add-hook 'makefile-mode-hook 'michalrus/setup-compilation-with-make)

;; woman
(use-package woman
  :bind (("C-c w" . woman)))

(use-package flycheck
  :config
  (global-flycheck-mode t))

(use-package lsp-mode
  :commands lsp-mode
  :config
  (use-package lsp-flycheck
    :demand t)

  ;; why does lsp-ui behave so weirdly?
  ;;(use-package lsp-ui
  ;;  :demand t
  ;;  :hook (lsp-mode . lsp-ui-mode))
  )
