;; -*- mode: emacs-lisp -*-

(use-package dhall-mode
  :mode "\\.dhall\\'"
  :config
  (setq dhall-command nil
        dhall-format-at-save nil))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package python-mode
  :mode "\\.py\\'" :interpreter "python")

(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode))

(use-package go-mode
  :mode "\\.go\\'")

(use-package po-mode
  :mode "\\.po\\'\\|\\.po\\.")

(use-package js
  :config
  (setq js-indent-level 2))
