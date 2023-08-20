;; -*- mode: emacs-lisp -*-

;; sane default appearance (w/o packages)
(add-to-list 'default-frame-alist '(alpha 97 97))
;(load-theme 'tango-dark)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(column-number-mode 1)
(toggle-frame-maximized)
(unless indicate-empty-lines (toggle-indicate-empty-lines))

(setq ring-bell-function 'ignore)

;; A little more visibily for the active window.
(set-face-attribute 'mode-line nil :overline "#117E99")

(setq inhibit-startup-echo-area-message (user-real-login-name)
      inhibit-startup-message t
      initial-scratch-message nil
      initial-buffer-choice (lambda () (get-buffer "*Messages*")))
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (let ((b (get-buffer "*scratch*")))
              (when b (kill-buffer b)))))

(setq frame-title-format "%b")

(set-face-attribute 'variable-pitch nil :family "FreeSans" :height 1.0)

(setq-default cursor-type '(bar . 1))
(blink-cursor-mode t)

;; turn off C-z (never used it)
(global-unset-key "\C-z")

;; yes-or-no questions are boring, use y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

(setq help-window-select t)

(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; show keystrokes almost instantly in the echo area
(setq echo-keystrokes 0.01)

;; no dialog boxes
(setq use-dialog-box nil)

;; increase/decrease text size
(defun michalrus/text-scale-reset ()
  (interactive)
  (text-scale-increase 0))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'michalrus/text-scale-reset)

;; disable the default theme(s) and load solarized
(use-package solarized-theme
  :config
  (dolist (i custom-enabled-themes) (disable-theme i))
  (setq custom-safe-themes t)
  (load-theme 'solarized-dark t))

;; make frames transparent, when run in terminal, and override theme bg with "black" when in GUI
;(defun fix-new-frame-background (frame)
;  (set-face-background 'default (if (display-graphic-p frame) "black" "unspecified-bg") frame))
;(add-hook 'after-make-frame-functions 'fix-new-frame-background)
;(add-hook 'window-setup-hook (lambda () (fix-new-frame-background (selected-frame))))

(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (setq rm-blacklist '(" wb" " super-save" " focus-save" " counsel" " ivy"))
  (sml/setup))
