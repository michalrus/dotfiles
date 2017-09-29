;; -*- mode: emacs-lisp -*-

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-u" . michalrus/haskell-mode-insert-undefined))
  :config
  (add-hook 'haskell-mode-hook 'michalrus/setup-compilation-with-make)
  (add-hook 'haskell-interactive-mode-hook 'company-mode)

  (defun michalrus/haskell-mode-insert-undefined ()
    (interactive)
    (insert "undefined"))

  (defun michalrus/haskell-mode-align-servant-types ()
    (interactive)
    (save-excursion
      (let ((symb ":<|>"))
        (goto-char 0)
        ;; repeat
        (while (search-forward ":<|>" nil t)
          (goto-char (match-beginning 0))
          (haskell-indentation-newline-and-indent)
          (forward-char (length symb))))))

  (use-package intero
    :config
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
    (add-hook 'haskell-mode-hook
              (lambda ()
                (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
                (let ((wants-intero
                       (or (locate-dominating-file default-directory "stack.yaml")
                           (locate-dominating-file default-directory "default.nix")))) ;; TODO: think of something better here
                  (cond (wants-intero (intero-mode t))
                        (t (add-to-list 'flycheck-disabled-checkers 'intero)))))))

  (use-package hayoo
    :demand t
    :bind (:map haskell-mode-map
           ("C-c h" . hayoo-query))))
