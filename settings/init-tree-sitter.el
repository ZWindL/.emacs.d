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

;; TODO: ⛔ Error (use-package): treesit-auto/:config: Symbol’s function definition is void: global-treesit-auto-mode
;; (use-package treesit-auto
;;   :ensure t
;;   :demand t
;;   :config
;;   (add-to-list 'treesit-auto-fallback-alist '(bash-ts-mode . sh-mode))
;;   (setq treesit-auto-install 'prompt)
;;   (global-treesit-auto-mode))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
