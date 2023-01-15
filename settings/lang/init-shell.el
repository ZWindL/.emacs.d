;;; init-shell.el --- Shell development mode. -*- lexical-binding: t -*-

;;; Commentary:

;; Shell development mode

;;; Code:

(use-package sh-mode
  :ensure nil
  :hook
  (sh-mode . eglot-ensure)
  :mode
  ("\\.sh\\'" . sh-mode)
  ("\\.zsh\\'" . sh-mode)
  ("\\.*shrc\\'" . sh-mode))

(provide 'init-shell)

;;; init-shell.el ends here
