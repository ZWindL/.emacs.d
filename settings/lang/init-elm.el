;;; init-elm.el --- elm mode

;;; Commentary:

;;; Code:
(use-package elm-mode
  :ensure t
  :hook
  (elm-mode . elm-indent-simple-mode))

(provide 'init-elm)
;;; init-elm.el ends here
