;; -*- mode: emacs-lisp -*-

;; LilyPond mode
(use-package lilypond-mode
  :mode ("\\.ly$" . LilyPond-mode)
  :init
  (setq-default LilyPond-pdf-command "evince")
  :config
  (add-hook 'LilyPond-mode-hook 'michalrus/setup-compilation-with-make))

;; typesetting Gregorian chant
(use-package gregorio-mode
  :mode "\\.gabc\\'")
