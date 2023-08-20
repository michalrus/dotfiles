;; -*- mode: emacs-lisp -*-

(use-package magit
  :bind (("C-x g" . magit-status) ;; mostly for dired buffers
         )
  :config
  (setq magit-log-arguments '("--graph" "--color" "--decorate" "--show-signature" "-n256" "++order=date")
        magit-merge-arguments '("--ff-only")
        magit-save-repository-buffers 'dontask
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        ))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package git-link
  :bind (("C-c g l" . git-link))
  :demand t ;; for .dir-locals.el
  :config
  (setq git-link-use-commit t
        git-link-open-in-browser t))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh) ;; <https://github.com/dgutov/diff-hl#magit>
  )

(use-package projectile
  :demand t
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p s") nil)
  )

(use-package neotree
  :bind (("M-<f2>" . michalrus/neotree-show)
         ("M-<f3>" . neotree-hide))
  :init
  (setq neo-smart-open t
        neo-window-width 30
        neo-theme 'arrow
        neo-show-hidden-files nil
        neo-hidden-regexp-list '("^\\.#")
        neo-vc-integration '(char face))
  :config
  (defun michalrus/neotree-show ()
    (interactive)
    (let ((current-window (get-buffer-window))
          (root-dir (cond ((projectile-project-p) (projectile-project-root))
                         ((buffer-file-name) (file-name-directory (directory-file-name (buffer-file-name)))))))
      (when root-dir
        (message root-dir)
        (neotree-dir root-dir)
        (select-window current-window)
        (neotree-find) ; jump to current file in the tree
        (select-window current-window)))))
