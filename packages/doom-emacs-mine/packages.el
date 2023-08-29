;;; Code:

(package! git-link)
(package! gumshoe)

;; Prevent this double-pin Magit bug:
;;   • <https://github.com/doomemacs/doomemacs/issues/7363#issuecomment-1696530746>
;;   • <https://github.com/magit/magit/issues/4994>
(unpin! git-commit)

;
