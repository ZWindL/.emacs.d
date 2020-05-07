;(defun keys-init ())

;; rg
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind ("C-c s" . rg))

;; avy
(use-package avy
  :bind
  ("C-'" . avy-goto-char)
  ("C-\"" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0))

;; ivy
(use-package ivy
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
  :bind (([remap recentf-open-files] . counsel-recentf)
         ([remap swiper]             . counsel-grep-or-swiper)))

;; Use swiper less, it takes up `ivy-height' lines.
(use-package isearch
  :bind (:map isearch-mode-map
         ;; consistent with ivy-occur
         ("C-c C-o" . isearch-occur)
         ;; Edit the search string instead of jumping back
         ([remap isearch-delete-char] . isearch-del-char)))

;; VTERM
(use-package shell-pop
  :bind ("M-=" . shell-pop))

;; Write documentation comment in an easy way
(use-package separedit
  :bind (:map prog-mode-map
              ("C-c '" . separedit)))

;; The completion engine
(use-package company
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
  :custom
  (lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename)))

(provide 'init-keys)
