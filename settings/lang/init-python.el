;;; init-python --- Summary

;;; Commentary:

;;; Code:

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Auto recognizes the virtualenv
(use-package pyvenv
  :ensure t)

(provide 'init-python)

;;; init-python.el ends here
