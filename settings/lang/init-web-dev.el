;;; init-web-dev.el --- Description.

;;; Commentary:

;;; Code:

(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  :init
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))
        )
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package emmet-mode
  :ensure t
  :hook
  (web-mode css-mode))

(use-package mmm-mode
  :ensure t)

(use-package vue-mode
  :ensure t
  :after (mmm-mode)
  :mode "\\.vue\\'"
  :hook (mmm-mode . (lambda ()
                      (set-face-background 'mmm-default-submode-face nil)))
  :init
  (defun fix-mmm-syntax ()
  (save-restriction
    (setq-local syntax-ppss-table typescript-mode-syntax-table)
    ))
  (add-hook 'mmm-typescript-mode-enter-hook 'fix-mmm-syntax)
  :config
  (add-to-list 'mmm-save-local-variables '(syntax-ppss-table buffer))
  (add-hook 'vue-mode-hook #'lsp))

(provide 'init-web-dev)

;;; init-web-dev.el ends here
