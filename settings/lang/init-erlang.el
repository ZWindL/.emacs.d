;;; init-erlang.el --- erlang metals -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Erlang mode
(use-package erlang
  :ensure t
  :hook (erlang-mode . eglot-ensure))

(provide 'init-erlang)

;;; init-erlang.el ends here
