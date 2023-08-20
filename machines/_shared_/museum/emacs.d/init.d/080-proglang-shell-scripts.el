;; -*- mode: emacs-lisp -*-

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function 'nix-indent-line))

(use-package shell
  :config
  (setq sh-indentation 2
        sh-basic-offset 2))
