;; -*- mode: emacs-lisp -*-

;; Circumvent a lie in original def: “This requires you to be running
;; either Gnome, KDE, Xfce4 or LXDE.”
(use-package browse-url
  :config
  (defun browse-url-can-use-xdg-open ()
    (and (getenv "DISPLAY") (executable-find "xdg-open") (executable-find "nohup"))))

(global-set-key (kbd "C-c C-o") 'browse-url-at-point)
