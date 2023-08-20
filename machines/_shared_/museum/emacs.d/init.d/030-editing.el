;; -*- mode: emacs-lisp -*-

;; tabs
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)

;; always highlight the corresponding parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; turn-off (overwrite-mode) on <insert>
(global-unset-key (kbd "<insert>"))

;; overwrite selected text
(delete-selection-mode t)

;; never visit symlinks when their targets are under version control
;; (visit the target instead)
(setq vc-follow-symlinks t)

;; always automatically reload changed files from disk
(global-auto-revert-mode 1)

;; automatically pair braces, quotes, etc.
(electric-pair-mode 1)

;; auto complete
(use-package company
  :hook (prog-mode . company-mode)
  :init
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t))

;; visual line mode
(visual-line-mode 1)

;; recent files
(use-package recentf
  :init
  (setq recentf-max-menu-items 1000
        recentf-max-saved-items 1000)
  :config
  (recentf-mode 1)
  (add-to-list 'delete-frame-functions
               (lambda (frm)
                 (recentf-save-list)))
  (run-at-time nil 37 (lambda ()
                        (let ((inhibit-message t))
                          (recentf-save-list)))))

(use-package sort-words)

(use-package expand-region
  :bind ("M-=" . er/expand-region))

;; Swiper & Ivy
(use-package swiper
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-c C-r" . ivy-resume)
         ("C-r" . counsel-recentf))
  :init
  (global-unset-key (kbd "C-x C-r")) ;; temporarily, to unlearn old binding for ivy-recentf
  (setq projectile-completion-system 'ivy)
  :config
  (ivy-mode 1))

(use-package counsel
  :init
  (setq counsel-ag-base-command "ag --hidden --nocolor --nogroup %s -- .")
  :config
  (counsel-mode 1)
  (defun counsel-git-grep-or-ag ()
    (interactive)
    (cond ((projectile-project-p) (counsel-git-grep))
          (t (counsel-ag))))
  (global-set-key (kbd "C-c p s") 'counsel-git-grep-or-ag)
  (global-set-key (kbd "C-c u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c l") 'counsel-locate))

(use-package google-translate
  :bind ("C-c t" . google-translate-smooth-translate))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :init
  (setq dired-listing-switches "-alh"))

(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode))

(use-package move-text
  :config
  (move-text-default-bindings))
