;;; init-basic --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; MacOS specific
;; (setq ns-use-thing-smoothing t
;;       ns-pop-up-frames nil)

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
                     "node_modules"
                     ".git"
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

;; Nano-like h-scroll for long lines
(setq auto-hscroll-mode 'current-line)

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

;; Prefer shorter names
(fset 'yes-or-no-p 'y-or-n-p)

(fset 'list-buffers 'ibuffer)

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

;; Supress emacs-native-comp warnings
(setq-default native-comp-async-report-warnings-errors nil)

;; Supress warnings in *Warnings* buffer
(setq-default warning-minimum-level :error)

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
  ;; show line/column/filesize in modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  (copy-region-blink-delay 0.2)
  ;; Relative line numbers
  (display-line-numbers-type 'relative)
  ;; column starts from 1
  (column-number-indicator-zero-based nil)
  ;; save current clipboard text
  (save-interprogram-paste-before-kill t)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n'
  (kill-whole-line t)
  ;; show the name of character in `what-cursor-position'
  (what-cursor-show-names t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (global-display-line-numbers-mode))

;; Type text
(use-package text-mode
  :ensure nil
  :custom
  (word-wrap-by-category t)
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
  :config
  (defun appt-display-with-notification (min-to-app new-time appt-msg)
    (notify-send :title (format "Appointment in %s minutes" min-to-app)
                 :body appt-msg
                 :urgency 'critical)
    (appt-disp-window min-to-app new-time appt-msg))
  :custom
  (appt-display-mode-line t)
  (appt-display-interval 3)
  (appt-message-warning-time 15)
  (appt-disp-window-function #'appt-display-with-notification))

;; Build regexp with visual feedback
(use-package re-builder
  :ensure nil
  :commands re-builder
  :bind (:map reb-mode-map
         ("C-c C-k" . reb-quit)
         ("C-c C-p" . reb-prev-match)
         ("C-c C-n" . reb-next-match))
  :custom
  (reb-re-syntax 'string))

;; transparent remote access
(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method "ssh"))

;; Better abbrev expansion
(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (defun try-expand-tempo (_old)
    (require 'tempo)
    (tempo-expand-if-complete))
  :custom
  (hippie-expand-try-functions-list '(try-expand-tempo
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

;; Make align be a simple thing
(use-package align
  :ensure nil
  :bind (("C-c ]" . align-regexp)))

;; Needed by `webpaste'
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-generic-program (or (executable-find "firefox-nightly")
                                  (executable-find "chromium")
                                  (when (eq system-type 'darwin) "open")
                                  (when (eq system-type 'gnu/linux) "xdg-open")))
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

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

;; Copy environment variables from shell
(use-package exec-path-from-shell
  :ensure t
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("GOPATH" "GO111MODULE" "GOPROXY" "PATH")))

;; Repeat mode, only for Emacs version >= 28
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :custom
  (repeat-exit-key (kbd "RET")))

;; Quelpa, the substitution for straight.el
(use-package quelpa
  :ensure t
  :hook (after-init . quelpa-upgrade-all-maybe)
  :custom
  (quelpa-update-melpa-p nil)
  (quelpa-upgrade-interval 7))

(provide 'init-basic)

;;; init-basic.el ends here
