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
(use-package rg
  :ensure t
  :defer t)

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  :custom
  (avy-timeout-seconds 0.2)
  (avy-all-windows nil)
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p)))

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
  )

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
  :custom
  (counsel-preselect-current-file t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n-----------\n")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

;; Use swiper less, it takes up `ivy-height' lines.
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ;; consistent with ivy-occur
         ("C-c C-o" . isearch-occur)
         ;; Edit the search string instead of jumping back
         ([remap isearch-delete-char] . isearch-del-char))
  :custom
  ;; One space can represent a sequence of whitespaces
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace t)
  (search-whitespace-regexp "[ \t\r\n]+")
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-count-suffix-format nil)
  (lazy-highlight-cleanup nil))

;; isearch alternative
(use-package swiper
  :ensure t
  :defer t
  :custom
  (swiper-action-recenter t))

;; Writable grep buffer. company well with ivy-occur
(use-package wgrep
  :ensure t
  :defer 1
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . auto-fill-mode)
  :custom
  (markdown-command "pandoc")
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Free hands
(use-package auto-package-update
  :ensure t
  :defer t
  :custom
  (auto-package-update-delete-old-versions t))

;; Beautiful term mode & friends
(use-package vterm
  :ensure t
;  :defines (evil-insert-state-cursor)
;  :commands (evil-insert-state vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback-when-clearing t)
  :hook (vterm-mode . (lambda ()
;                        (setq-local evil-insert-state-cursor 'box)
                        (setq-local global-hl-line-mode nil)
                        ;; Dont prompt about processes when killing vterm
                        (setq confirm-kill-processes nil)
                                        ;                       (evil-insert-state)))
                        ))
  )

(use-package shell-pop
  :ensure t
  :custom
  (shell-pop-full-span t)
  (shell-pop-shell-type '("vterm" "*vterm*" #'vterm)))

;; git/svn diff
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (diff-hl-mode)
                        (diff-hl-flydiff-mode)
                        (diff-hl-margin-mode)))
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode-unless-remote)))

;; winum
(use-package winum
  :ensure t)

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
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

;(use-package treemacs-evil
;  :after treemacs evil
;  :ensure t)

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

;; Pastebin service
(use-package webpaste
  :ensure t
  :defer 1
  :custom
  (webpaste-open-in-browser t)
  (webpaste-paste-confirmation t)
  (webpaste-add-to-killring nil)
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "ix.io")))

;; TODO: vlf (very large file)

;; Notes manager
(use-package deft
  :ensure t
  :defer t
  :defines (org-directory)
  :init (setq deft-default-extension "org")
  :custom
  (deft-directory org-directory)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase))))

;; Note taking app
(use-package zetteldeft
  :ensure t
  :after deft
  :config
  (zetteldeft-set-classic-keybindings))

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

(provide 'init-tools)
