;; -*- mode: emacs-lisp -*-

(use-package scala-mode
  :mode ("\\.scala\\'" "\\.sbt\\'")
  :interpreter ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook
            (lambda ()
              (company-mode)
              (setq comment-start "/* "
                    comment-end " */"
                    comment-style 'multi-line
                    comment-empty-lines t)
              (scala-mode:goto-start-of-code)))

  (defun scala-mode-newline-comments ()
    "Custom newline appropriate for `scala-mode'."
    ;; shouldn't this be in a post-insert hook?
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)

  (use-package sbt-mode
    :config
    ;; What a weird decision they made. o_O’
    (add-hook 'sbt-mode-hook (lambda () (setq comint-scroll-to-bottom-on-output nil)) t))

  (use-package ensime
    :config
    (add-hook 'scala-mode-hook
              (lambda ()
                (ensime-mode)
                (setq company-backends '(ensime-company (company-keywords company-dabbrev-code company-etags company-yasnippet)))))
    (use-package ensime-expand-region)
    (plist-put ensime-goto-test-config-defaults :test-class-suffixes
               '("Spec" "Test" "Specification" "Check"))
    (plist-put ensime-goto-test-config-defaults :test-template-fn
               'ensime-goto-test--test-template-scalatest-wordspec)))
