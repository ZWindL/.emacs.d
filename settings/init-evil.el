;;; init-evil.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; go to the last edited point
;; default keybindings: `g ;' `g .'
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
  :init
  ;; Emacs default keybindings do matter!
  ;; Looks like this variable can only be set here
  ;; or in the custome.el
  (setq evil-disable-insert-state-bindings t)
  :custom
  (evil-cross-lines t)
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
  :hook (evil-mode . evil-collection-init)
  :custom
  ;; (evil-collection-calendar-want-org-bindings t)
  (evil-collection-outline-bind-tab-p nil)
  (evil-collection-want-unimpaired-p t)
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-find-usages-bindings t)
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
(use-package general
  :ensure t
  :after evil
  :config
  (general-auto-unbind-keys)
  (general-create-definer leader-def
    :states 'normal
    :prefix "SPC"
    :keymaps 'override)
  (leader-def
    ;; file
    "f" '(:ignore t :which-key "file")
    "fF" 'find-file-other-window
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

    ;; text
    "x" '(:ignore t :which-key "text")
    "xj" 'set-justification
    "xw" 'delete-trailing-whitespace
    "x TAB" 'indent-rigidly

    ;; search
    "s" '(:ignore t :which-key "search")
    "ss" 'swiper-isearch
    "sS" 'swiper-isearch-thing-at-point
    "sa" 'swiper-all
    "sA" 'swiper-all-thing-at-point
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
  :custom
  (general-implicit-kbd t)
  (general-override-auto-enable t))

(provide 'init-evil)

;;; init-evil.el ends here
