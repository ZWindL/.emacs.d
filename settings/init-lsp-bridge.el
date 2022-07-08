;;; init-lsp-bridge.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(add-to-list 'load-path "/home/zwindl/.emacs.d/elpa/lsp-bridge")

(use-package lsp-bridge
  :hook
  (after-init . global-lsp-bridge-mode)
  :custom
  (lsp-bridge-enable-signature-help t)
  (lsp-bridge-org-babel-lang-list '(c rust))
  (acm-enable-doc t)
  (acm-enable-icon t))

(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
