;;; init-rust.el --- Rust -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package rustic
  :ensure t
  :after (flycheck)
  :config
  (setq rustic-format-trigger nil)
  (push 'rustic-clippy flycheck-checkers))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(provide 'init-rust)

;;; init-rust.el ends here
