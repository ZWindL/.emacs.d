;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package transient
  :ensure t
  :commands (transient-setup transient-prefix)
  :bind (:map transient-map
         ;; Close transient with ESC
         ([escape] . transient-quit-one)))

;; The awesome git client
(use-package magit
  :ensure t
  :bind (("C-x g g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  ;; Supress message
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-process-popup-time 30)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-hunk t))

;; Todo integration
(use-package magit-todos
  :ensure t
  :hook (magit-status-mode . magit-todos-mode))

;; NB `diff-hl' depends on `vc'
(use-package vc
  :ensure nil
  :custom
  ;; Disable vc for remote files, and `diff-hl' won't work as expected.
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

;; Highlight uncommitted changes using git
(use-package diff-hl
  :ensure t
  :hook ((after-init         . (lambda ()
                                 (global-diff-hl-mode)
                                 (diff-hl-flydiff-mode)))
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode         . diff-hl-dired-mode-unless-remote)))

;; Open current file in browser
(use-package browse-at-remote
  :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :bind (:map vc-prefix-map
         ("b" . bar-browse)
         ("c" . bar-to-clipboard)))

;; Pop up last commit information of current line
(use-package git-messenger
  :ensure t
  :custom
  (git-messenger:show-detail t)
  (git-messenger:use-magit-popup t)
  :bind
  ("C-x g m" . git-messenger:popup-message)
  (:map vc-prefix-map
        ("p" . git-messenger:popup-message)
        :map git-messenger-map
        ("m" . git-messenger:copy-message)))

;; Setup gitignore mode
(use-package conf-mode
  :ensure nil
  :mode (("\\.gitignore\\'" . conf-unix-mode)))

(use-package blamer
  :ensure t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-author-formatter " ✎ %s ")
  (setq blamer-commit-formatter "● %s")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 90
                    :italic t)))
  :bind
  ("C-x g b" . blamer-mode)
  ("C-x g B" . blamer-show-posframe-commit-info)
  :config
  (blamer-mode nil))

(provide 'init-git)

;;; init-git.el ends here
