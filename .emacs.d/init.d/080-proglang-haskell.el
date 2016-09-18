;; -*- mode: emacs-lisp -*-

(use-package haskell-mode
  :config
  (setq haskell-tags-on-save t
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t)
  (define-key haskell-mode-map (kbd "C-c C-n l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-n s") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n i") 'haskell-process-do-info)
  (setq hindent-style "chris-done"))

(when (require 'ghc nil 'noerror)
  (add-hook 'haskell-mode-hook 'ghc-init)
  (use-package company-ghc
    :config
    (add-to-list 'company-backends 'company-ghc)
    (setq company-ghc-show-info t)))

(use-package hindent
  :config (add-hook 'haskell-mode-hook #'hindent-mode))
