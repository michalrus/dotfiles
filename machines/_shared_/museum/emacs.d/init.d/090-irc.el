;; -*- mode: emacs-lisp -*-

(use-package weechat
  :commands weechat-connect
  :config
  (setq ;weechat-auto-monitor-buffers t
        ;weechat-auto-monitor-new-buffers t
        weechat-auto-close-buffers t
        ;weechat-auto-recenter nil
        weechat-auto-reconnect-retries 99999
        weechat-notification-mode t
        weechat-complete-order-nickname nil
        weechat-buffer-kill-buffers-on-disconnect t
        weechat-relay-ping-idle-seconds 10
        weechat-initial-lines 256)
  (require 'gnutls)
  (add-to-list 'gnutls-trustfiles (expand-file-name "weechat.cert" user-emacs-directory))
  (require 'weechat-notifications)
  (require 'weechat-image)
  (require 'weechat-tracking)
  (add-hook 'kill-emacs-hook (lambda () (when weechat--connected (weechat-disconnect)))))
