;; sane default appearance/bahavior (w/o packages)
(add-to-list 'default-frame-alist '(alpha 85 85))
(load-theme 'tango-dark)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(desktop-save-mode 1)
(set-background-color "black")
(toggle-frame-maximized)
(setq custom-safe-themes t)
(setq inhibit-startup-echo-area-message "m")
(setq inhibit-startup-message t)

;; turn off C-z (never used it)
(global-unset-key "\C-z")

;; turn off mouse
; TODO: how?!?!/1/1

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)

;; disable the default theme and load monokai (if possible) with a black background
(defun disable-all-themes () "disable all enabled themes"
  (interactive) (dolist (i custom-enabled-themes) (disable-theme i)))
(use-package monokai-theme
  :config (disable-all-themes) (load-theme 'monokai) (set-background-color "black")
  :ensure t)

;; make frames transparent, when run in terminal
(defun set-term-frame-transparent (frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'set-term-frame-transparent)
(add-hook 'window-setup-hook (lambda () (set-term-frame-transparent (selected-frame))))

;; load mu4e if installed (no error if not)
(require 'mu4e nil 'noerror)
