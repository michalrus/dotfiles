;; -*- mode: emacs-lisp -*-

(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package python-mode
  :mode "\\.py\\'" :interpreter "python")

(use-package go-mode
  :mode "\\.go\\'")

(use-package po-mode
  :mode "\\.po\\'\\|\\.po\\.")

(use-package js-mode
  :config
  (setq js-indent-level 2))
