;;; init-eglot.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(use-package eglot
  :hook ((python-mode cc++-mode c-mode) . eglot-ensure)
  :custom
  (eglot-extend-to-xref t)
  :custom-face
  (eglot-highlight-symbol-face ((t (
                                    :inherit bold
                                    :underline t)))))

(provide 'init-eglot)

;;; init-eglot.el ends here
