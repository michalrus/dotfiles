;; -*- mode: emacs-lisp -*-

(use-package rust-mode
  :hook ((rust-mode . lsp)
         (rust-mode . yas-minor-mode))
  :init (setq lsp-rust-server 'rust-analyzer))
