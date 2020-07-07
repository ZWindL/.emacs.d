;;; init-go.el --- init golang.

;; Cpoied from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-go.el

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
         ("C-c R" . go-remove-unused-imports)
         ("<f1>" . godoc-at-point))
  :hook (go-mode . lsp-deferred)
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
   (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  (setq lsp-gopls-staticcheck t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-gopls-complete-unimported t))
  ;; ;; Misc
  ;; (use-package go-dlv :ensure t)
  ;; (use-package go-fill-struct :ensure t)
  ;; (use-package go-impl :ensure t)

  ;; (use-package go-tag
  ;;   :ensure t
  ;;   :bind (:map go-mode-map
  ;;          ("C-c t t" . go-tag-add)
  ;;          ("C-c t T" . go-tag-remove))
  ;;   :init (setq go-tag-args (list "-transform" "camelcase")))

  ;; (use-package go-gen-test
  ;;   :ensure t
  ;;   :bind (:map go-mode-map
  ;;          ("C-c t g" . go-gen-test-dwim)))

  ;; (use-package gotest
  ;;   :ensure t
  ;;   :bind (:map go-mode-map
  ;;          ("C-c t a" . go-test-current-project)
  ;;          ("C-c t m" . go-test-current-file)
  ;;          ("C-c t ." . go-test-current-test)
  ;;          ("C-c t x" . go-run))))

;; Local Golang playground for short snippets
(use-package go-playground
  :ensure t
  :diminish)

(provide 'init-go)

;;; init-go.el ends here
