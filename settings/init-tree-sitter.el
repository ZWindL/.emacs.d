;; init-tree-sitter.el --- Initialize company configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree-sitter configurations.
;;
;;; Code:

;; Tree-sitter
;; (when (not (treesit-available-p))
;;   (use-package tree-sitter :ensure t))

(use-package treesit
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :custom
  (global-treesit-auto-mode t)
  (treesit-auto-install t))

;; (use-package tree-sitter-langs :ensure t)

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install t)
  ;; (treesit-auto-install 'prompt)
  :config
  ;; (add-to-list 'treesit-auto-fallback-alist '(bash-ts-mode . sh-mode))
  (global-treesit-auto-mode))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
