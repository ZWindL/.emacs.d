;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode  . lsp-enable-which-key-integration))
  :init
  ;; this line makes things in `:general' work properly
  (add-hook 'lsp-mode-hook #'evil-normalize-keymaps)
  (setq lsp-keymap-prefix "C-c C-l")
  :commands
  (lsp lsp-deferred lsp-format-buffer lsp-organize-imports lsp-enable-which-key-integration)
  :config
  (keymap-set lsp-mode-map "C-c C-l" lsp-command-map)
  :custom
  (lsp-idle-delay 0.5)                 ;; lazy refresh
  (lsp-log-io nil)                     ;; enable log only for debug
  (lsp-enable-folding t)
  (lsp-auto-configure t)               ;; turn this off for more control
  (lsp-enable-links t)
  (lsp-eldoc-render-all t)
  (lsp-enable-symbol-highlighting t)
  (lsp-diagnostic-package :flycheck)   ;; prefer flycheck
  (lsp-modeline-code-actions-segments '(count icon name))
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
  (company-lsp-async t)
  (lsp-warn-no-matched-clients nil)
  (company-files-exclusions '(".git/" ".DS_Store"))
  :bind (:map lsp-mode-map
              ("C-c f" . lsp-format-region)
              ("C-c F" . lsp-format-buffer)
              ("C-c H" . lsp-describe-thing-at-point)   ;; `h' stands for `hover'
              ("C-c a" . lsp-execute-code-action)
              ("C-c R" . lsp-rename))
  :general
  (:definer 'minor-mode :states 'normal :keymaps 'lsp-mode
            "gR" 'lsp-rename
            "ga" 'lsp-execute-code-action))

(use-package lsp-ivy
  :ensure t
  :init
  ;; this line makes things in `:general' work properly
  (add-hook 'lsp-mode-hook #'evil-normalize-keymaps)
  :commands lsp-ivy-workspace-symbol
  :bind (:map lsp-mode-map
              ("C-c C-l s" . lsp-ivy-workspace-symbol))
  :general
  (:definer 'minor-mode :states 'normal :keymaps 'lsp-mode
            "gy" 'lsp-ivy-workspace-symbol))

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :init
  ;; this line makes things in `:general' work properly
  (add-hook 'lsp-ui-mode-hook #'evil-normalize-keymaps)
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-ui-imenu-mode . (lambda () (display-line-numbers-mode -1)))
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references)
              ("C-c i"                       . lsp-ui-imenu)
              ("C-c C-l T i"                 . lsp-ui-imenu)
              ("C-c h"                       . lsp-ui-doc-show) ;; `h' stands for `hover'
              ("C-c o"                       . lsp-ui-doc-focus-frame)
         :map lsp-ui-doc-mode-map
              ([remap keyboard-quit] . lsp-ui-doc-hide)
         :map lsp-ui-doc-frame-mode-map
              ("M-o" . lsp-ui-doc-unfocus-frame)
              ("q"   . my/unfocus-and-hide-doc))
  :general
  (:definer 'minor-mode :states 'normal :keymaps 'lsp-ui-mode
           "gh" 'lsp-ui-doc-show)
  :config
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  (defun my/unfocus-and-hide-doc ()
      "Close lsp-ui-doc frame"
      (interactive)
      (lsp-ui-doc-unfocus-frame)
      (lsp-ui-doc-hide))
  :custom
  ;; lsp-ui-sideline
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode t)
  (lsp-ui-sideline-delay 2)
  ;; lsp-ui-peekpp
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 2)
  (lsp-ui-doc-show-with-mouse t)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-window-width 50)
  (lsp-ui-imenu-auto-refresh t))

;; lsp-treemacs
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :commands lsp-treemacs-errors-list
  :defines (lsp-metals-treeview-show-when-views-received lsp-metals-treeview-enable)
  :custom
  (lsp-metals-treeview-enable t)
  (lsp-metals-treeview-show-when-views-received t)
  :config
  (lsp-treemacs-sync-mode 1)
  :bind (:map lsp-mode-map
              ("C-c t c" . lsp-treemacs-call-hierarchy)
              ("C-c t d" . lsp-treemacs-java-deps-list)
              ("C-c t e" . lsp-treemacs-errors-list)
              ("C-c t s" . lsp-treemacs-symbols)
              ("C-<f8>"  . lsp-treemacs-errors-list)
              ("M-<f8>"  . lsp-treemacs-symbols)
              ("s-<f8>"  . lsp-treemacs-java-deps-list)))

(provide 'init-lsp)

;;; init-lsp.el ends here
