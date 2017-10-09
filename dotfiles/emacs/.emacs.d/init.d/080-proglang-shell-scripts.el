;; -*- mode: emacs-lisp -*-

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package shell
  :config
  (setq sh-indentation 2
        sh-basic-offset 2))
