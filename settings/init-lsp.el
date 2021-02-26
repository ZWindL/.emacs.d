;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :after (company-mode)
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp-enable-which-key-integration lsp-format-buffer lsp-organize-imports lsp)
  ;;:diminish
  ;; :bind (:map lsp-mode-map
  ;;           ([remap xref-find-definitions] . lsp-find-definition)
  ;;           ([remap xref-find-references] . lsp-find-references))
  ;; (lsp-face-highlight-* textDocument/documentSymbol)
  :custom
  (lsp-idle-delay 0.5)                 ;; lazy refresh
  (lsp-log-io t)                       ;; enable log only for debug
  (lsp-enable-folding t)
  ;; (lsp-auto-configure t)
  (lsp-enable-links t)
  (lsp-eldoc-render-all t)
  (lsp-enable-symbol-highlighting t)
  (lsp-diagnostic-package :flycheck)   ;; prefer flycheck
  (lsp-lens-auto-enable t)             ;; enable lens
  (lsp-flycheck-live-reporting nil)    ;; obey `flycheck-check-syntax-automatically'
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-enable-file-watchers nil)       ;; turn off for better performance
  (lsp-enable-text-document-color t)
  (lsp-enable-symbol-highlighting t)
  (lsp-signature-render-documentation t)
  (lsp-semantic-highlighting t)
  (lsp-enable-indentation nil)         ;; indent by ourself
  (lsp-enable-on-type-formatting nil)  ;; disable formatting on the fly
  (lsp-auto-guess-root t)              ;; auto guess root
  (lsp-keep-workspace-alive nil)       ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)         ;; disable eldoc hover
  (lsp-signature-auto-activate t)      ;; show function signature
  (lsp-signature-doc-lines 2)          ;; but dont take up more lines
  (lsp-completion-provider :none)
  (company-lsp-async t)
  (lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename)))

(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom-face
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  :custom
  ;; lsp-ui-sideline
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
;  (lsp-ui-sideline-update-mode t)
  (lsp-ui-sideline-delay 2)
  ;; lsp-ui-peekpp
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
;  (lsp-ui-doc-position)
  (lsp-ui-doc-delay 2)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-window-width 30)
  ;; (lsp-ui-imenu--custom-mode-line-format)
  )

;; lsp-treemacs
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :defines (lsp-metals-treeview-show-when-views-received lsp-metals-treeview-enable)
  :config
  (setq lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t)
  :bind (:map lsp-mode-map
          ("C-<f8>" . lsp-treemacs-errors-list)
          ("M-<f8>" . lsp-treemacs-symbols)
          ("s-<f8>" . lsp-treemacs-java-deps-list)))

(provide 'init-lsp)

;;; init-lsp.el ends here
