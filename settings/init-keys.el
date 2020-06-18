;;; init-keys.el --- Define emacs keybindings

;;; Commentary:

;;; Code:

;; rg
(use-package rg
  :ensure t
  :hook (after-init . rg-enable-default-bindings)
  :bind ("C-c s" . rg))

;; avy
(use-package avy
  :ensure t
  :bind
  ("C-'" . avy-goto-char)
  ("C-\"" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0))

;; ivy
(use-package ivy
  :ensure t
  :bind (
         ("C-c C-r" . ivy-resume)
         ("\C-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h o" . counsel-describe-symbol)
         ("C-h l" . counsel-find-library)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         :map ivy-minibuffer-map
         ("C-c C-e" . my/ivy-woccur)
         :map ivy-occur-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-occur-grep-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode))
    :preface
    ;; Copy from
    ;; https://github.com/honmaple/maple-emacs/blob/master/lisp/init-ivy.el
    (defun my/ivy-woccur ()
      "ivy-occur with wgrep-mode enabled."
      (interactive)
      (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
      (ivy-occur)))

;; Fuzzy match
(use-package counsel
  :ensure t
  :bind (([remap recentf-open-files] . counsel-recentf)
         ([remap swiper]             . counsel-grep-or-swiper)))

;; Use swiper less, it takes up `ivy-height' lines.
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ;; consistent with ivy-occur
         ("C-c C-o" . isearch-occur)
         ;; Edit the search string instead of jumping back
         ([remap isearch-delete-char] . isearch-del-char)))

;; VTERM
(use-package shell-pop
  :ensure t
  :bind ("M-=" . shell-pop))

;; Write documentation comment in an easy way
(use-package separedit
  :ensure t
  :bind (:map prog-mode-map
              ("C-c '" . separedit)))

;; The completion engine
(use-package company
  :ensure t
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         :map company-active-map
         ([escape] . company-abort)
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-s" . company-filter-candidates)
         ("<tab>" . company-complete-common-or-cycle)
         :map company-search-map
         ([escape] . company-search-abort)
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point)
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename)))

;; treemacs
(use-package treemacs
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; lsp-treemacs
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :bind (:map lsp-mode-map
          ("C-<f8>" . lsp-treemacs-errors-list)
          ("M-<f8>" . lsp-treemacs-symbols)
          ("s-<f8>" . lsp-treemacs-java-deps-list)))

;; Highlight TODO
(use-package hl-todo
  :ensure t
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur)))

;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :ensure t
  :bind (("C-c x" . quickrun)))

;; Fold selected area
;; (use-package fold-this
;;   :ensure t
;;   :bind (("C-c C-f" . fold-this-all)
;;          ("C-c C-f" . fold-this)
;;          ("C-c M-f" . fold-this-unfold-all)))

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-selector 'ivy)
  (dump-jump-prefer-searcher 'rg))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map prog-mode-map
         ("C-c TAB" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))
  (defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn))

;; Org-mode key shortcuts
(use-package org
  :ensure nil
  ;; :hook (org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(provide 'init-keys)

;;; init-keys.el ends here
