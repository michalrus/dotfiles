;;; Code:

(setq initial-scratch-message nil)

;; Useless:
(global-unset-key "\C-z")

;; Don’t enter overwrite-mode on <insert>:
(global-unset-key (kbd "<insert>"))

;; Theme, but adjust a few faces to match the original ‘solarized-theme’:
(setq doom-theme 'doom-solarized-dark
      doom-font (font-spec :family "Iosevka Nerd Font Propo" :size (if doom--system-macos-p 14.0 12.0) :weight 'light)
      doom-serif-font (font-spec :family (cond ((eq system-type 'gnu/linux) "Noto Serif")
                                               (t "Georgia"))))
(add-to-list 'default-frame-alist '(alpha-background . 95))
(add-to-list 'default-frame-alist '(background-color . "#002b36"))  ; prevent initial flicker
(add-hook! doom-load-theme
  (setq frame-title-format "%b – Emacs")
  (after! font-lock
    (set-face-attribute 'font-lock-constant-face      nil :foreground (doom-color 'blue))
    (set-face-attribute 'font-lock-builtin-face       nil :foreground (doom-color 'white) :weight 'bold)
    (set-face-attribute 'font-lock-variable-name-face nil :foreground (doom-color 'blue)))
  (after! magit
    (set-face-attribute 'magit-section-heading        nil :foreground (doom-color 'yellow)))
  (after! org
    (set-face-attribute 'org-level-1 nil :weight 'normal :foreground (doom-color 'orange))
    (set-face-attribute 'org-level-2 nil :weight 'normal :foreground (doom-color 'green))
    (set-face-attribute 'org-level-3 nil :weight 'normal :foreground (doom-color 'blue))
    (set-face-attribute 'org-level-4 nil :weight 'normal :foreground (doom-color 'yellow))
    (set-face-attribute 'org-level-5 nil :weight 'normal :foreground (doom-color 'cyan))
    (set-face-attribute 'org-level-6 nil :weight 'normal :foreground (doom-color 'green))
    (set-face-attribute 'org-level-7 nil :weight 'normal :foreground (doom-color 'red))
    (set-face-attribute 'org-level-8 nil :weight 'normal :foreground (doom-color 'blue)))
  (set-face-attribute 'cursor nil :background (doom-color 'fg))
  (setq-default cursor-type '(bar . 2))
  (blink-cursor-mode t))

(add-hook! after-init
  (setq dired-mode-hook (remove 'dired-omit-mode dired-mode-hook)))  ; I want `..'

;; Otherwise, adding another frame is not instantaneous:
(add-hook! server-after-make-frame :append (setq server-after-make-frame-hook nil))

(after! org
  (add-hook! org-mode
    (display-line-numbers-mode -1)
    (company-mode -1))
  (setq org-directory "~/Org"
        org-support-shift-select t
        org-ctrl-k-protect-subtree 'error
        org-todo-keywords '((sequence "TODO(t)" "DOIN(i)" "BLCK(b)" "|" "DONE(d)" "DELE(g)" "KILL(k)"))
        org-todo-keyword-faces '(("TODO" .  org-todo)
                                 ("DOIN" . +org-todo-active)
                                 ("BLCK" . +org-todo-onhold)
                                 ("DONE" .  org-done)
                                 ("DELE" .  org-done)
                                 ("KILL" . +org-todo-cancel))
        org-insert-heading-respect-content nil  ; insert new headings under point
        org-startup-folded t)
  (map! :map org-mode-map
        "S-<left>"  nil "C-<left>"  #'org-shiftleft
        "S-<right>" nil "C-<right>" #'org-shiftright
        "S-<up>"    nil "C-<up>"    #'org-shiftup
        "S-<down>"  nil "C-<down>"  #'org-shiftdown))

(map! "C-M-<left>"  #'windmove-left
      "C-M-<right>" #'windmove-right
      "C-M-<up>"    #'windmove-up
      "C-M-<down>"  #'windmove-down)

(after! ivy
  (setq ivy-wrap nil)
  (map! "C-s" #'+default/search-buffer)
  ; TODO: Restore sorting of (+ivy/project-search) outputs (`ag' was stable, `ripgrep' is not):
  ;(add-to-list 'ivy-sort-matches-functions-alist '(counsel-projectile-rg . ???))
  )

(after! projectile
  (setq projectile-enable-caching nil))

(use-package! recentf)
(after! recentf
  (recentf-load-list) ; why do I need to call this, or else the list is empty on load?
  (map! "C-r" #'recentf-open-files)
  (setq recentf-auto-cleanup 'never  ; requires SSH-ing to check remote entries
        recentf-max-saved-items 1000)
  (run-at-time t 300 'recentf-save-list)
  (run-with-idle-timer 60 t #'recentf-save-list)
  ;(add-hook 'focus-out-hook #'recentf-save-list)
  (add-hook 'kill-emacs-hook #'recentf-save-list))

(after! magit
  (setq magit-save-repository-buffers 'dontask
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        vc-follow-symlinks t)
  (global-auto-revert-mode t))

(add-hook! after-init
  (setq tab-always-indent t))

(after! yasnippet
  ;; Conflicts with company and `tab-always-indent'
  (define-key yas-minor-mode-map (kbd "TAB")   nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

(after! company
  (map! :map company-active-map
        "<tab>" #'company-complete-selection  ;; By default, it’s `company-complete-common'.
        ))

(after! counsel
  (setq counsel-find-file-ignore-regexp nil
        counsel-projectile-sort-files t
        counsel-projectile-sort-directories t
        counsel-projectile-sort-buffers t
        counsel-projectile-sort-projects t))

(map! :map global-map
      "M-<right>" #'forward-word
      "M-<left>"  #'backward-word)

(after! cc-mode
  ; Useful for editing GNU code:
  ;(add-hook 'c-mode-hook (lambda () (setq tab-width 8)))
  )

(after! lsp-mode
  (setq lsp-disabled-clients '(rls)
        lsp-rust-server 'rust-analyzer)
  (map! :map lsp-mode-map
        "C-c d" #'lsp-describe-thing-at-point))

;; Auto-saving:
(add-hook! focus-out (save-some-buffers t))
(add-hook! doom-switch-buffer (save-some-buffers t))
(setq auto-save-default nil)          ; don’t autosave to /tmp/,
(setq auto-save-visited-interval 10)  ; but instead, after this many seconds idle,
(auto-save-visited-mode t)            ; save to the actual file

(use-package! git-link
  :config
  (setq git-link-use-commit t
        git-link-open-in-browser t)
  (map! :leader "g l" #'git-link))

(map! "M-=" #'er/expand-region)

(map! "C-c C-o" #'browse-url-at-point)

;; Swap, I'm used to:
(map! "C-x k" #'doom/kill-this-buffer-in-all-windows
      "C-x K" #'kill-buffer)

;; Turn off this "intelligent" behavior.
(after! smartparens
  (setq sp-autodelete-pair nil
        sp-autodelete-closing-pair nil
        sp-autodelete-opening-pair nil
        sp-autodelete-wrap nil))

;; Automatic point history (a better ‘point-undo’):
(use-package! gumshoe
  :config
  (global-gumshoe-mode 1)
  (setq gumshoe-show-footprints-p nil
        gumshoe-prefer-same-window t
        gumshoe-follow-distance 20
        gumshoe-idle-time 5
        gumshoe-display-buffer-action '((display-buffer-same-window))
        gumshoe-auto-cancel-backtracking-p t)
  (map! "M-s-<left>"  #'gumshoe-backtrack
        "M-s-<right>" #'gumshoe-backtrack)
  (map! :map global-gumshoe-backtracking-mode-map
        "M-s-<left>"  #'global-gumshoe-backtracking-mode-back
        "M-s-<right>" #'global-gumshoe-backtracking-mode-forward))

(after! nix-mode
  (set-company-backend! 'nix-mode 'company-files 'company-dabbrev)
  (setq-hook! 'nix-mode-hook company-idle-delay 0.2)
  (setq nix-indent-function 'nix-indent-line))

(use-package! jsonnet-mode
  :mode "\\.jsonnet\\'")

;
