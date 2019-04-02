;; -*- mode: emacs-lisp -*-

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-u" . michalrus/haskell-mode-insert-undefined))
  :config
  (add-hook 'haskell-mode-hook 'michalrus/setup-compilation-with-make)
  (add-hook 'haskell-mode-hook (lambda ()
    (setq-local company-backends '((company-dabbrev-code company-gtags company-etags company-keywords)))))
  (add-hook 'haskell-interactive-mode-hook 'company-mode)

  (defun michalrus/haskell-mode-insert-undefined ()
    (interactive)
    (insert "undefined"))

  (use-package hayoo
    :demand t
    :bind (:map haskell-mode-map
           ("C-c h" . hayoo-query))))
