;; sane default appearance (w/o packages)
(add-to-list 'default-frame-alist '(alpha 85 85))
(load-theme 'tango-dark)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(desktop-save-mode 1)
(set-background-color "black")
(toggle-frame-maximized)
(setq inhibit-startup-echo-area-message (user-real-login-name))
(setq inhibit-startup-message t)
(unless indicate-empty-lines (toggle-indicate-empty-lines))

;; turn off C-z (never used it)
(global-unset-key "\C-z")

;; turn off mouse
; TODO: how?!?!/1/1

;; show keystrokes almost instantly in the echo area
(setq echo-keystrokes 0.01)

;; no dialog boxes
(setq use-dialog-box nil)

;; increase/decrease text size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; disable the default theme and load monokai (if possible) with a black background
(defun disable-all-themes () "disable all active themes"
  (interactive) (dolist (i custom-enabled-themes) (disable-theme i)))
(setq custom-safe-themes t)
(use-package monokai-theme
  :config (disable-all-themes) (load-theme 'monokai) (set-background-color "black")
  :ensure t)

;; make frames transparent, when run in terminal
(defun set-term-frame-transparent (frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'set-term-frame-transparent)
(add-hook 'window-setup-hook (lambda () (set-term-frame-transparent (selected-frame))))
