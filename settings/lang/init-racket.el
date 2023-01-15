;;; init-racket.el --- for SICP.

;;; Commentary:
;;

;;; Code:

(use-package racket-mode
  :ensure t
  :hook
  (racket-mode . racket-xp-mode))

(provide 'init-racket)

;;; init-racket.el ends here
