;;; init-haskell.el --- haskell -*- lexical-binding: t -*-
;;; Commentary:
;; copied from https://github.com/condy0919/.emacs.d/blob/master/lisp/lang/init-haskell.el

;;; Code:

(use-package lsp-haskell :ensure t)

(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . haskell-doc-mode)
  (haskell-mode . lsp-mode)
  (haskell-literate-mode . lsp-mode))
  ;; :custom
  ;; (haskell-completing-read-function 'completing-read)
  ;; (haskell-process-check-cabal-config-on-load nil)
  ;; (haskell-process-suggest-add-package nil)
  ;; (haskell-process-suggest-hoogle-imports nil)
  ;; (haskell-process-suggest-language-pragmas nil)
  ;; (haskell-process-suggest-overloaded-strings nil)
  ;; (haskell-process-suggest-restart nil))


(provide 'init-haskell)

;;; init-haskell.el ends here
