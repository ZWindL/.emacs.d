;;; init-evil.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package goto-chg
  :ensure t)

;; Be kind to your little finger
(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :bind (:map evil-normal-state-map
         ("gs" . evil-avy-goto-char-timer)
         ("go" . evil-avy-goto-word-or-subword-1)
         ("gl" . evil-avy-goto-line))
  :config
  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
  :custom
  (evil-cross-lines t)
  (evil-disable-insert-state-bindings t)
  (evil-respect-visual-line-mode t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-symbol-word-search t)
  (evil-undo-system (if (>= emacs-major-version 28) 'undo-redo 'undo-tree))
  (evil-want-C-g-bindings t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil) ; for evil-collection
  (evil-want-fine-undo t))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init)
  ;; Disable `evil-collection' in certain modes
  (dolist (ig-mode '())
    (setq evil-collection-mode-list (remove ig-mode evil-collection-mode-list)))
  ;; Keybindings tweaks
  (evil-collection-define-key 'normal 'occur-mode-map
    ;; consistent with ivy
                              (kbd "C-c C-e") 'occur-edit-mode)
  :custom
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-company-use-tng nil)
  (evil-collection-outline-bind-tab-p nil)
  (evil-collection-setup-minibuffer nil)
  (evil-collection-setup-debugger-keys nil))

(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary
  :ensure t)

(use-package evil-numbers
  :ensure t
  :after (evil)
  :bind (:map evil-normal-state-map
              ("C-c +" . evil-numbers/inc-at-pt)
              ("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; SPC keybindings
;; copied from https://github.com/condy0919/.emacs.d/blob/master/lisp/init-evil.el
(use-package general
  :ensure t
  :after evil
  :config
  (general-create-definer leader-def
    :states 'normal
    :prefix "SPC"
    :keymaps 'override)
  (leader-def
    ;; file
    "f" '(:ignore t :which-key "file")
    "ff" 'find-file
    "f." 'find-file
    "fF" 'find-file-other-window
    "f/" 'find-file-other-window
    "fg" 'rgrep
    "fj" 'counsel-file-jump
    "fo" 'counsel-find-file-extern
    "fC" 'my/copy-current-file
    "fD" 'my/delete-current-file
    "fy" 'my/copy-current-filename
    "fR" 'my/rename-current-file
    "fr" 'recentf-open-files
    "fl" 'find-file-literally
    "fz" 'counsel-fzf

    ;; code
    "c" '(:ignore t :which-key "code")
    "ca" 'add-change-log-entry-other-window
    "cd" 'rmsbolt-compile
    "cc" 'compile
    "cC" 'recompile
    "ck" 'kill-compilation
    "cx" 'quickrun
    "cX" 'quickrun-shell

    ;; window
    "w" '(:keymap evil-window-map :which-key "window")
    "wx" 'kill-buffer-and-window
    "wu" 'my/transient-winner-undo
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally

    ;; text
    "x" '(:ignore t :which-key "text")
    "xj" 'set-justification
    "xw" 'delete-trailing-whitespace
    "x TAB" 'indent-rigidly

    ;; search
    "s" '(:ignore t :which-key "search")
    "ss" 'swiper-isearch
    "sS" 'swiper-isearch-thing-at-point
    "sb" 'swiper-all
    "sB" 'swiper-all-thing-at-point
    "sj" 'evil-show-jumps
    "sm" 'evil-show-marks
    "sr" 'evil-show-registers
    "si" 'imenu
    "sl" 'ivy-resume
    "sg" 'counsel-rg

    ;; insert
    "i" '(:ignore t :which-key "insert")
    "iq" 'quickurl-prefix-map
    "it" 'insert-date-time
    "iu" 'counsel-unicode-char
    "iy" 'clipboard-yank

    ;; git
    "g" '(:ignore t :which-key "git")
    "g." 'magit-file-dispatch
    "gb" 'magit-branch-checkout
    "gB" 'magit-blame-addition
    "gc" 'magit-branch-and-checkout
    "gC" 'magit-commit-create
    "gd" 'magit-diff
    "gf" 'magit-find-file
    "gg" 'magit-status
    "gG" 'magit-status-here
    "gi" 'magit-init
    "gr" 'magit-rebase-interactive

    ;; project
    "p" '(:package projectile :keymap projectile-command-map :which-key "project"))

  (general-create-definer local-leader-def
    :states 'normal
    :prefix "SPC m")
  (local-leader-def
    :keymaps 'org-mode-map
    "." 'counsel-org-goto
    "/" 'counsel-org-goto-all
    "a" 'org-archive-subtree
    "d" 'org-deadline
    "e" 'org-set-effort
    "f" 'org-footnote-new
    "l" 'org-lint
    "o" 'org-toggle-ordered-property
    "p" 'org-set-property
    "q" 'org-set-tags-command
    "r" 'org-refile
    "s" 'org-schedule
    "t" 'org-todo
    "T" 'org-todo-list
    "P" 'org-preview-latex-fragment

    "b" '(:ignore t :which-key "babel")
    "bp" 'org-babel-previous-src-block
    "bn" 'org-babel-next-src-block
    "be" 'org-babel-expand-src-block
    "bg" 'org-babel-goto-named-src-block
    "bs" 'org-babel-execute-subtree
    "bb" 'org-babel-execute-buffer
    "bt" 'org-babel-tangle
    "bf" 'org-babel-tangle-file
    "bc" 'org-babel-check-src-block
    "bi" 'org-babel-insert-header-arg
    "bI" 'org-babel-view-src-block-info
    "bk" 'org-babel-remove-result-one-or-many

    "c" '(:ignore t :which-key "clock")
    "cc" 'org-clock-in
    "cC" 'org-clock-out
    "cd" 'org-clock-mark-default-task
    "ce" 'org-clock-modify-effort-estimate
    "cg" 'org-clock-goto
    "cl" 'org-clock-in-last
    "cr" 'org-clock-report
    "cs" 'org-clock-display
    "cx" 'org-clock-cancel
    "c=" 'org-clock-timestamps-up
    "c-" 'org-clock-timestamps-down

    "i" '(:ignore t :which-key "insert")
    "id" 'org-insert-drawer
    "in" 'org-add-note
    "it" 'org-time-stamp-inactive
    "iT" 'org-time-stamp)
  :custom
  (general-implicit-kbd t)
  (general-override-auto-enable t))

(provide 'init-evil)

;;; init-evil.el ends here
