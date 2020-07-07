;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Tips for next keystroke
(use-package which-key
  :ensure t
  ;; :init
  ;; (setq which-key-popup-type 'frame)
  ;; (setq which-key-frame-max-width 60)
  ;; (setq which-key-frame-max-height 20)
  :hook (after-init . which-key-mode)
  :custom (which-key-idle-delay 0.5))

;; The blazing grep tool
;; Press C-c s to search
(use-package rg
  :ensure t
  :defer t
  :when (executable-find "rg")
  :hook (after-init . rg-enable-default-bindings))

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  ;; integrate with isearch and others
  ;; C-' to select isearch-candidate with avy
  :hook (after-init . avy-setup-default)
  :custom
  (avy-timeout-seconds 0.2)
  (avy-all-windows nil)
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))
  :config
  ;; Force to use pre `avy-style'
  (define-advice avy-isearch (:around (func &rest args))
    (let ((avy-style 'pre))
      (apply func args)))
  :bind (
         ("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

;; ivy core
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :custom
  (ivy-display-style 'fancy)          ;; fancy style
  (ivy-count-format "%d/%d ")         ;; better counts
  (ivy-use-virtual-buffers t)         ;; show recent files
  (ivy-height 10)
  (ivy-fixed-height-minibuffer t)     ;; fixed height
  (ivy-on-del-error-function 'ignore) ;; dont quit minibuffer when del-error
  :config
  ;; Default keybinding: C-'
  (use-package ivy-avy
    :ensure t)
  :bind (
         ("C-c C-r" . ivy-resume)
         ("C-c k" . counsel-ag)
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

;; Fuzzy matcher
(use-package counsel
  :ensure t
  :hook (ivy-mode . counsel-mode)
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("d" my/delete-file "delete")
     ("r" my/rename-file "rename")
     ("l" vlf            "view large file")
     ("b" hexl-find-file "open file in binary mode")
     ("x" counsel-find-file-as-root "open as root")))
  :bind (([remap recentf-open-files] . counsel-recentf)
         ([remap swiper]             . counsel-grep-or-swiper))
  :custom
  (counsel-preselect-current-file t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n-----------\n")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

;; Use swiper less, it takes up `ivy-height' lines.
(use-package isearch
  :ensure nil
  :custom
  ;; One space can represent a sequence of whitespaces
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace t)
  (search-whitespace-regexp "[ \t\r\n]+")
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-count-suffix-format nil)
  (lazy-highlight-cleanup nil)
  :bind (:map isearch-mode-map
              ;; consistent with ivy-occur
              ("C-c C-o" . isearch-occur)
              ;; Edit the search string instead of jumping back
              ([remap isearch-delete-char] . isearch-del-char)
              ([remap isearch-query-replace] . anzu-isearch-query-replace)
              ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

;; isearch alternative
(use-package swiper
  :ensure t
  :defer t
  :custom
  (swiper-action-recenter t))

;; Preview search/replace results
(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-threshold 50)
   '(anzu-replace-to-string-separator " => ")))

;; Writable grep buffer. company well with ivy-occur
(use-package wgrep
  :ensure t
  :defer 1
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; View/Edit reStructuredText file
(use-package rst
  :ensure nil
  :mode (("\\.rst\\'"  . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;; Write documentation comment in an easy way
(use-package separedit
  :ensure t
  :custom
  (separedit-default-mode 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-buffer-creation-hook #'auto-fill-mode)
  :bind (:map prog-mode-map
              ("C-c '" . separedit)))

;; Pixel alignment for org/markdown tables
(use-package valign
  :ensure t
  :straight (:host github :repo "casouri/valign")
  :hook ((markdown-mode org-mode) . valign-mode)
  :config
  ;; compatible with outline mode
  (define-advice outline-show-entry (:override nil)
    "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
    (interactive)
    (save-excursion
      (outline-back-to-heading t)
      (outline-flag-region (max (point-min) (1- (point)))
                           (progn
                             (outline-next-preface)
                             (if (= 1 (- (point-max) (point)))
                                 (point-max)
                               (point)))
                           nil)))
  )

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . auto-fill-mode)
  :init
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore)
  :custom
  (markdown-header-scaling t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t))

;; Generate table of contents for markdown-mode
(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map
         ("r" . markdown-toc-generate-or-refresh-toc)))

;; Jump forward-backward between CamelCasesWords
(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))

;; Free hands
(use-package auto-package-update
  :ensure t
  :defer t
  :custom
  (auto-package-update-delete-old-versions t))

;; GC optimization
(use-package gcmh
  :ensure t
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000) ;; 100 MB
  :hook (after-init . gcmh-mode))

;; winum
(use-package winum
  :ensure t)

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
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
        ("C-x t M-t" . treemacs-find-tag))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; GC optimization
(use-package gcmh
  :ensure t
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000) ;; 100 MB
  :hook (after-init . gcmh-mode))

;; Pastebin service
(use-package webpaste
  :ensure t
  :defer 1
  :custom
  (webpaste-open-in-browser t)
  (webpaste-paste-confirmation t)
  (webpaste-add-to-killring nil)
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "ix.io")))

;; Open very large files
(use-package vlf-setup
  :ensure vlf)

;; ;; Note taking app
;; ;; Replaced by org-roam
;; (use-package zetteldeft
;;   :ensure t
;;   :after deft
;;   :config
;;  (zetteldeft-set-classic-keybindings))

;; Visual bookmarks
(use-package bm
  :ensure t
  :hook ((after-init   . bm-repository-load)
         (find-file    . bm-buffer-restore)
         (after-revert . bm-buffer-restore)
         (kill-buffer  . bm-buffer-save)
         (kill-emacs   . (lambda ()
                           (bm-buffer-save-all)
                           (bm-repository-save))))
  :custom
  (bm-annotate-on-create t)
  (bm-buffer-persistence t)
  (bm-cycle-all-buffers t)
  (bm-goto-position nil)
  (bm-in-lifo-order t)
  (bm-recenter t))

;; Customize popwin behavior
(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((magit-status-mode    :select t :inhibit-window-quit t :same t)
                   (magit-log-mode       :select t :inhibit-window-quit t :same t)
                   (profiler-report-mode :select t)
                   (help-mode            :select t :align t :size 0.4)
                   (comint-mode          :select t :align t :size 0.4)
                   (Man-mode             :select t :other t)
                   (woman-mode           :select t :other t)
                   (grep-mode            :select t :align t)
                   (rg-mode              :select t :align t)
                   ("*bm-bookmarks*"           :select t   :align t)
                   ("*Flycheck errors*"        :select t   :align t :size 10)
                   ("*quickrun*"               :select nil :align t :size 15)
                   ("*Backtrace*"              :select t   :align t :size 15)
                   ("*Shell Command Output*"   :select nil :align t :size 0.4)
                   ("*Async Shell Command*"    :ignore t)
                   ("*package update results*" :select nil :align t :size 10)
                   ("\\*ivy-occur .*\\*"       :regexp t :select t :align t)))
  )

;; smart parens
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :init
  (require 'smartparens-config))

;; Leetcode!!!
(use-package leetcode
  :ensure t
  :config
  (setq leetcode-prefer-language "c++")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/project/leetcode"))

(provide 'init-tools)

;;; init-tools.el ends here
