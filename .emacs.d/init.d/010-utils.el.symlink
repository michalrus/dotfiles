;; -*- mode: emacs-lisp -*-

;; Emacs defaults are a bit too conservative for the 21st century.
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 5000)

(defun michalrus/add-list-to-list (dst src)
  "Like `add-to-list' but takes a list of elements to add. Beware, O(n·m)!"
  (dolist (i src) (add-to-list dst i)))
