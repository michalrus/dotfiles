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

  (use-package lsp-haskell
    :demand t
    :config
    (add-hook 'haskell-mode-hook
              (lambda ()
                ;; If there’s a 'hie.sh' defined locally by a project
                ;; (e.g. to run HIE in a nix-shell), use it…
                (let ((hie-directory (locate-dominating-file default-directory "hie.sh")))
                  (when hie-directory
                    (setq-local lsp-haskell-process-path-hie (expand-file-name "hie.sh" hie-directory))))
                ;; … and only then setup the LSP.
                (lsp-haskell-enable))))

  (use-package hayoo
    :demand t
    :bind (:map haskell-mode-map
           ("C-c h" . hayoo-query))))
