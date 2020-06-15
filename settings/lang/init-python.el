;;; init-python --- Summary

;;; Commentary:

;;; Code:

;; Microsoft python-language-server support
(use-package lsp-python-ms
  :ensure t
  :defines (lsp-python-ms-python-executable-cmd)
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))

(provide 'init-python)

;;; init-python.el ends here
