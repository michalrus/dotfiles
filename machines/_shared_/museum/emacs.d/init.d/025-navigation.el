;; -*- mode: emacs-lisp -*-

(global-set-key (kbd "C-M-n") 'next-buffer)
(global-set-key (kbd "C-M-p") 'previous-buffer)

;; (global-set-key (kbd "C-M-k") 'kill-this-buffer)  (global-unset-key (kbd "C-x k"))
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; (global-set-key (kbd "C-M-f") 'find-file)         (global-unset-key (kbd "C-x f"))
;; (global-set-key (kbd "C-M-b") 'switch-to-buffer)  (global-unset-key (kbd "C-x b"))
;; (global-set-key (kbd "C-M-s") 'save-buffer)       (global-unset-key (kbd "C-x C-s"))
;; (global-set-key (kbd "C-M-a") 'mark-whole-buffer) (global-unset-key (kbd "C-x h"))

;; (global-set-key (kbd "C-M-3") 'split-window-right)   (global-unset-key (kbd "C-x 3"))
;; (global-set-key (kbd "C-M-2") 'split-window-below)   (global-unset-key (kbd "C-x 2"))
;; (global-set-key (kbd "C-M-1") 'delete-other-windows) (global-unset-key (kbd "C-x 1"))
;; (global-set-key (kbd "C-M-0") 'delete-window)        (global-unset-key (kbd "C-x 0"))

;; (global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-M-<left>")  'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>")    'windmove-up)
(global-set-key (kbd "C-M-<down>")  'windmove-down)

(global-set-key (kbd "C-M-s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-s-<down>")  'shrink-window)
(global-set-key (kbd "C-M-s-<up>")    'enlarge-window)
