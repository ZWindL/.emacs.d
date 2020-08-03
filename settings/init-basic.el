;;; init-basic --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Supress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Full screen
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; No splash screen
(setq inhibit-splash-screen 1)

;; Line number settings
(global-display-line-numbers-mode)

;; Linux specific
(setq x-gtk-use-system-tooltips nil
      x-underline-at-descent-line t)

;; MacOS specific
(setq ns-use-thing-smoothing t
      ns-pop-up-frames nil)

;; Optimize for very long lines
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Recent file settings
(use-package recentf
  :ensure nil
  :after no-littering
  :hook ((after-init . recentf-mode)
         (focus-out-hook . (recentf-save-list recentf-cleanup)))
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude `(,(expand-file-name package-user-dir)
                     ,no-littering-var-directory
                     ,no-littering-etc-directory
                     ".cache"
                     "cache"
                     "^/tmp/"
                     "/ssh:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "COMMIT_EDITMSG\\'")))

;; Always load the newest file
(setq load-prefer-newer t)

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; Supress annoying features
(setq ring-bell-function 'ignore
      blink-cursor-mode nil)

;; Smooth scroll & friends
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

;; Dont scroll without our permission
(setq auto-window-vscroll nil)

;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

;; Dont move points out of eyes
(setq mouse-yank-at-point t)

(setq-default fill-column 80)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Fonts
;; If running Emacs under Macos, set :height to 140
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Fantasque Sans Mono" :height 140)
    (set-face-attribute 'default nil :family "Fantasque Sans Mono" :height 110))

(if (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family "Noto Sans CJK SC" :height 100))))


;; Prefer shorter names
(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)

;; Use TeX as default IM
(setq default-input-method "TeX")

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; Keep clean
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; The selected region of text can be deleted
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Show line/column number
(use-package simple
  :ensure nil
  :custom
  ;; column starts from 1
  (column-number-indicator-zero-based nil)
  ;; save current clipboard text
  (save-interprogram-paste-before-kill t)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n'
  (kill-whole-line t)
  :hook (after-init . (lambda ()
                        (line-number-mode)
                        (column-number-mode)
                        (size-indication-mode))))

;; Type text
(use-package text-mode
  :ensure nil
  :custom
  ;; fill
  (adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
  (adaptive-fill-first-line-regexp "^* *$")
  ;; paragraphs
  (sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (sentence-end-double-space nil))

;; Back to the previous position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Update buffer whenever file changes
;; Also revert dired buffer.
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (global-auto-revert-non-file-buffers t)
  :hook (after-init . global-auto-revert-mode))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Switch window
(use-package window
  :ensure nil
  :bind ("M-o" . other-window))

;; Server mode.
;; Use emacsclient to connect
(use-package server
  :ensure nil
  :when (display-graphic-p)
  :defer 1
  :commands (server-running-p)
  :config
  (unless (server-running-p)
    (server-start)))

;; Workaround with minified source files
(use-package so-long
  :ensure nil
  :when (>= emacs-major-version 27)
  :hook (after-init . global-so-long-mode))

;; Make escape more nature
(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-local-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-ns-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-completion-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-must-match-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-isearch-map
         ([escape] . abort-recursive-edit)))

;; What day is it today?
(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  (calendar-holidays (append holiday-general-holidays
                             holiday-oriental-holidays
                             holiday-solar-holidays))
  (calendar-chinese-all-holidays-flag t)
  (calendar-mark-holidays-flag t)
  ;; start from Monday
  (calendar-week-start-day 1)
  ;; year/month/day
  (calendar-date-string 'iso))

;; Appointment
(use-package appt
  :ensure nil
  :hook (after-init . appt-activate)
  :custom
  (appt-display-mode-line t)
  (appt-display-interval 3)
  (appt-message-warning-time 15))

;; lifelog
(use-package diary-lib
  :ensure nil
  :defer t
  :custom
  (diary-number-of-entries 7)
  (diary-comment-start "#"))

;; quick access to files/tags
(use-package speedbar
  :ensure nil
  :bind ("<f8>" . speedbar-get-focus)
  :custom-face
  (speedbar-file-face ((t (:foreground "cyan4"))))
  (speedbar-selected-face ((t (:foreground "red3" :underline t))))
  :custom
  (speedbar-indentation-width 2))

;; transparent remote access
(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method "ssh"))

;; htop like monitor
(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-auto-update-flag t))

;; mouse wheel optimization
(use-package mwheel
  :ensure nil
  :defer t
  :custom
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control)))))

;; Better abbrev expansion
(use-package hippie-exp
  :ensure nil
  :bind ("M-/" . hippie-expand))

;; Make align be a simple thing
(use-package align
  :ensure nil
  :bind (("C-c [" . align-regexp)
         ("C-c ]" . align-regexp)))

;; Needed by `webpaste'
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-generic-program "firefox-nightly"))

;; Notifications
(use-package notifications
  :ensure nil
  :commands (notifications-notify))

;; Try out emacs package without installing
(use-package try
  :ensure t
  :defer t)

;; Keep ~/.emacs.d clean
(use-package no-littering
  :ensure t
  :demand t)

;; Undo tree, do what vim does
(use-package undo-tree
  :ensure t
  :hook (after-init . global-undo-tree-mode))

(provide 'init-basic)

;;; init-basic.el ends here
