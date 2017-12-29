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

  (use-package dante
    :ensure t
    :commands 'dante-mode
    :init
    (add-hook 'haskell-mode-hook 'dante-mode)
    (add-hook 'dante-mode-hook
       '(lambda () (flycheck-add-next-checker 'haskell-dante
                    '(warning . haskell-hlint)))))

  (use-package hayoo
    :demand t
    :bind (:map haskell-mode-map
           ("C-c h" . hayoo-query))))
