;; init-tree-sitter.el --- Initialize company configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree-sitter configurations.
;;
;;; Code:

;; Tree-sitter
(use-package tree-sitter
  :ensure t
  :hook
  (prog-mode . global-tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (use-package tree-sitter-langs
    :ensure t))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
