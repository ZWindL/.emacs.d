;;; init-ocaml.el --- ocaml -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Ocaml mode
(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode))

;; Indentation tool for OCaml
;; (use-package ocp-indent
;;   :ensure t
;;   :when (executable-find "ocp-indent")
;;   :hook (tuareg-mode . ocp-setup-indent)
;;   :commands (ocp-indent-region ocp-indent-buffer))

;; The dune build system
(use-package dune
  :ensure t
  :mode ("dune\\(?:\\.inc\\)?\\'" . dune-mode))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
