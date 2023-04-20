;; init-corfu.el --- Initialize corfu completion.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Auto-completion configurations.
;;
;;; Code:

;; yasnippet support
(use-package yasnippet
  :ensure t
  ;; :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :custom
  (yas-inhibit-overlay-modification-protection t)
  :config
  ;; pre-wrote snippets
  (use-package yasnippet-snippets :ensure t))

(use-package corfu
  :ensure t
  :hook
  (after-init        . global-corfu-mode)
  (global-corfu-mode . corfu-popupinfo-mode)
  (eshell-mode       . (lambda ()
                         (setq-local corfu-auto nil)
                         (corfu-mode)))
  :custom
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary t)     ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-popupinfo-delay (cons nil 1.0))
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  ;; Always show all candidates in popup menu
  (completion-cycle-threshold nil)
  :bind
  (:map corfu-map
        ("TAB"      . corfu-next)
        ([tab]      . corfu-next)
        ("S-TAB"    . corfu-previous)
        ([backtab]  . corfu-previous)
        ("C-n"      . corfu-next)
        ("C-p"      . corfu-previous)
        ([escape]   . corfu-quit)
        ("<return>" . corfu-insert)
        ("M-d"      . corfu-show-documentation)
        ("M-l"      . corfu-show-location))
  :config
  ;; this line makes evil-define-key works properly
  (add-hook 'lsp-mode-hook #'evil-normalize-keymaps)
  ;; (advice-add '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
  ;;    (evil-make-overriding-map corfu-map)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; Use corfu in terminal
;; (quelpa '(corfu-terminal
;;           :fetcher git
;;           :url "https://codeberg.org/akib/emacs-corfu-terminal.git"))

;; (use-package corfu-terminal
;;   :config
;;   (unless (display-graphic-p)
;;     (corfu-terminal-mode +1)))

;; Display icons before candidates
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (insert (let ((kind-icon-default-style
               '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1.0)))
          (kind-icon-reset-cache)
          (kind-icon-formatted 'variable)))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package svg-lib
  :ensure t
  :custom
  (svg-lib-style-default
   '(:background "#282c34" :foreground "#bbc2cf" :padding -1 :margin 0 :stroke 2 :radius 3 :alignment 0.5 :width 10 :height 0.5 :scale 0.75 :ascent center :crop-left nil :crop-right nil :collection "material" :font-family "Monaco" :font-size 9 :font-weight regular)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(provide 'init-corfu)

;;; init-corfu.el ends here
