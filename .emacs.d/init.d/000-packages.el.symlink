;; -*- mode: emacs-lisp -*-

(require 'package)

(cond
 ;; on NixOS, use only /nix/store/
 ((executable-find "nixos-version")
  (setq package-archives nil))

 ;; on any other host, install used packages from (M)ELPA
 (t
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "http://elpa.gnu.org/packages/")))
  (setq use-package-always-ensure t)))

;; add local package sketches to load-path
(add-to-list 'load-path (expand-file-name "package-sketches" user-emacs-directory))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
