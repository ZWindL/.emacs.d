;;; init-go.el --- init golang.

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :bind
  (:map go-mode-map
        ("C-c e g" . godoc-at-point))
  :hook ((go-mode . lsp-mode)
         (go-mode . smartparens-mode))
  :config
  (with-eval-after-load 'lsp-mode
    (lsp-register-custom-settings
     '(("gopls.usePlaceholders" t t)
       ("codelens.gc_details" t t))))
  :init
  (setq go-fontify-function-calls t))


(use-package go-guru
  :ensure t
  :after go-mode)

;; go-dlv conficts with emacs 28q
;; (use-package go-dlv :ensure t)
(use-package go-fill-struct :ensure t)
(use-package go-impl :ensure t)

(use-package go-tag
  :ensure t
  :bind (:map go-mode-map
              ("C-c t t" . go-tag-add)
              ("C-c t T" . go-tag-remove))
  :init (setq go-tag-args (list "-transform" "camelcase")))

(use-package go-gen-test
  :ensure t
  :bind (:map go-mode-map
              ("C-c t g" . go-gen-test-dwim)))

(use-package gotest
  :ensure t
  :bind (:map go-mode-map
              ("C-c t a" . go-test-current-project)
              ("C-c t m" . go-test-current-file)
              ("C-c t ." . go-test-current-test)
              ("C-c t x" . go-run)))

;; Local Golang playground for short snippets
(use-package go-playground
  :ensure t
  :diminish)

(provide 'init-go)

;;; init-go.el ends here
