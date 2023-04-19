;; init-tree-sitter.el --- Initialize tree-sitter configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree-sitter configurations.
;;
;;; Code:

;; Tree-sitter
;; TODO: tree-sitter is built-in on Emacs 30
;; (when (not (treesit-available-p))
;;   (use-package tree-sitter :ensure t))

;; Tree-sitter
(use-package tree-sitter
  :ensure t
  :hook
  (prog-mode            . global-tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs :ensure t)

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; (add-to-list 'treesit-auto-fallback-alist '(bash-ts-mode . sh-mode))
  (global-treesit-auto-mode))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
