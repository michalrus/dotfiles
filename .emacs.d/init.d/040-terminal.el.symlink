;; -*- mode: emacs-lisp -*-

;; set $PATH, $MANPATH and exec-path using values from ~/.profile
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(defun random-uuid ()
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string "uuidgen -r")))

(defun insert-random-uuid ()
  (interactive)
  (insert (random-uuid)))

(use-package eshell
  :demand t
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point))))

(use-package comint
  :demand t
  :config
  ;; Make processes’ outputs read-only. The prompt is easy.
  (setq comint-prompt-read-only t)
  ;; Outputs. Now you cannot delete them, but they still can be interleaved with some other text.
  (add-hook 'comint-preoutput-filter-functions
            (lambda (text)
              (propertize text 'read-only t)))
  ;; So we need tackle rear-nonsticky:
  (defadvice comint-output-filter (after michalrus/comint-output-read-only activate)
    (let* ((start-marker comint-last-output-start)
           (proc (get-buffer-process (current-buffer)))
           (end-marker (if proc (process-mark proc) (point-max-marker))))
      (when (and start-marker
                 end-marker
                 (< start-marker end-marker)) ;; Account for some of the IELM’s wilderness.
        (let ((inhibit-read-only t))
          ;; Disallow interleaving.
          (remove-text-properties start-marker (1- end-marker) '(rear-nonsticky))
          ;; Make sure that at `max-point' you can always append.
          ;; Important for bad REPLs that keep writing after giving us prompt (e.g. sbt).
          (add-text-properties (1- end-marker) end-marker '(rear-nonsticky t))
          ;; Protect fence (newline of input, just before output).
          (when (eq (char-before start-marker) ?\n)
            (remove-text-properties (1- start-marker) start-marker '(rear-nonsticky))
            (add-text-properties    (1- start-marker) start-marker '(read-only t))))))
    ;; What’s left is some initial welcome `insert's and process exit messages, but whatever.
    ))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook 'company-mode))

(use-package term
  :demand t
  :bind (("C-x t" . michalrus/term-dwim))
  :config
  (defadvice term-handle-exit
      (after michalrus/term-kill-buffer-on-exit activate)
    (kill-buffer))

  (defun michalrus/term-dwim ()
    (interactive)
    (let ((wanted-dir default-directory))
      (when (derived-mode-p 'term-mode)
        (bury-buffer))
      (let ((matching-buf (catch 'loop
                            (dolist (buf (buffer-list))
                              (with-current-buffer buf
                                (when (and (string= default-directory wanted-dir)
                                           (derived-mode-p 'term-mode))
                                  (throw 'loop buf)))))))
        (cond (matching-buf (switch-to-buffer matching-buf))
              (t (with-temp-buffer
                   (cd wanted-dir)
                   (ansi-term (getenv "SHELL"))))))))

  (defun michalrus/term-mode-hook ()
    "C-x is the prefix command, rather than C-c."
    (term-set-escape-char ?\C-x)
    (push '(projectile-mode . nil) minor-mode-overriding-map-alist) ; why do I have to do this? And only for projectile?
    )
  (add-hook 'term-mode-hook 'michalrus/term-mode-hook)

  ;; Expose some more global bindings in term-char-mode.
  (let ((exposed-bindings
         '("M-:" "M-[" "M-]"
           "C-s" "M-w" "C-h"
           )))
    (dolist (binding exposed-bindings)
      (let ((internal (kbd binding)))
        (define-key term-raw-map internal
          (lookup-key (current-global-map) internal)))))

  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "M-<left>") 'left-char)
  (define-key term-raw-map (kbd "M-<right>") 'right-char)
  (define-key term-raw-map (kbd "M-<up>") 'previous-line)
  (define-key term-raw-map (kbd "M-<down>") 'next-line)

  (defmacro michalrus/defterm (name cmd &optional send-raw &rest args)
    `(defun ,(intern name) ()
       ,(concat "Runs `" cmd "' in `ansi-term'. Defined by `michalrus/defterm'.")
       (interactive)
       (let* ((buf-name (format "*%s*" ,name))
              (buf (get-buffer buf-name))
               (exec (executable-find ,cmd)))
         (cond (buf (switch-to-buffer buf))
               (exec (with-temp-buffer
                       (cd "/") ; to not interfere with C-x t run in ~
                       (let ((buf (term-ansi-make-term buf-name exec nil ,@args)))
                         (set-buffer buf)
                         (term-mode)
                         (term-char-mode)
                         (switch-to-buffer buf)
                         (when ,send-raw
                           (term-send-raw-string ,send-raw)))))
               (t (message (format "%s (or %s) not found." ,cmd "screen")))))))
  (michalrus/defterm "mtr" "mtr" "d" "8.8.8.8")
  (michalrus/defterm "htop" "htop"))
