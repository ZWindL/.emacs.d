;;; init-haskell.el --- haskell -*- lexical-binding: t -*-
;;; Commentary:
;; copied from https://github.com/condy0919/.emacs.d/blob/master/lisp/lang/init-haskell.el

;;; Code:

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends)))))
  :custom
  (haskell-completing-read-function 'completing-read)
  (haskell-process-check-cabal-config-on-load nil)
  (haskell-process-suggest-add-package nil)
  (haskell-process-suggest-hoogle-imports nil)
  (haskell-process-suggest-language-pragmas nil)
  (haskell-process-suggest-overloaded-strings nil)
  (haskell-process-suggest-restart nil))

(use-package lsp-haskell
  :ensure t
  :hook
  ((haskell-mode . lsp-mode)
   (haskell-literate-mode . lsp-mode)))

(use-package dante
  :ensure t
  :hook (haskell-mode . dante-mode)
  :bind (:map haskell-mode-map
         ;; Compatible with lsp-mode keybindings
         ("C-c d" . dante-info)
         ("C-c C-c" . dante-eval-block))
  )

(provide 'init-haskell)

;;; init-haskell.el ends here
