;;; init-evil.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; go to the last edited point
;; default keybindings: `g ;' `g ,'
(use-package goto-chg
  :ensure t)

;; Be kind to your little finger
(use-package evil
  :ensure t
  :hook ((after-init . evil-mode)
         (after-init . visual-line-mode))
  :bind (:map evil-normal-state-map
              ("gs" . evil-avy-goto-char-timer)
              ("go" . evil-avy-goto-word-or-subword-1)
              ("gl" . evil-avy-goto-line))
  :init
  ;; Emacs default keybindings do matter!
  ;; Looks like this variable can only be set here
  ;; or in the custome.el
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
  :custom
  (evil-cross-lines t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-symbol-word-search t)
  (evil-respect-visual-line-mode t)
  (evil-undo-system (if (>= emacs-major-version 28) 'undo-redo 'undo-tree))
  (evil-want-C-g-bindings t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil) ; for evil-collection
  (evil-want-fine-undo t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-symbol-word-searh t))

(use-package evil-collection
  :ensure t
  :after evil
  :custom
  ;; (evil-collection-calendar-want-org-bindings t)
  (evil-collection-outline-bind-tab-p nil)
  (evil-collection-want-unimpaired-p t)
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-find-usages-bindings t)
  (evil-collection-setup-debugger-keys nil)
  :config
  (evil-collection-init))

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
(use-package evil
  :ensure nil
  :init
  (defun define-leader-key (state map localleader &rest bindings)
    "Define leader key in MAP when STATE, a wrapper for
    `evil-define-key*'. All BINDINGS are prefixed with \"<leader>\"
    if LOCALLEADER is nil, otherwise \"<localleader>\"."
    (cl-assert (cl-evenp (length bindings)))
    (let ((prefix (if localleader "<localleader>" "<leader>")))
      (while bindings
        (let ((key (pop bindings))
              (def (pop bindings)))
          (evil-define-key* state map (kbd (concat prefix key)) def)))))
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal (kbd "C-SPC") t) ;local leader
  ;; switch buffer, consistent with my nvim config
  (evil-define-key 'normal 'global
    "[b" 'switch-to-prev-buffer
    "]b" 'switch-to-next-buffer)
  (define-leader-key 'normal 'global nil
                     ;; files
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

                     ;; dired
                     "dj" 'dired-jump
                     "dJ" 'dired-jump-other-window

                     ;; code
                     "ca" 'add-change-log-entry-other-window
                     "cd" 'rmsbolt-compile
                     "cc" 'compile
                     "cC" 'recompile
                     "ck" 'kill-compilation
                     "cx" 'quickrun
                     "cX" 'quickrun-shell

                     ;; buffer & bookmark
                     "bb" 'switch-to-buffer
                     "bB" 'switch-to-buffer-other-window
                     "bc" 'clone-indirect-buffer
                     "bC" 'clone-indirect-buffer-other-window
                     "by" '+copy-current-buffer-name
                     "bv" 'revert-buffer
                     "bx" 'scratch-buffer
                     "bz" 'bury-buffer
                     ;; --------------
                     "bm" 'bookmark-set
                     "bM" 'bookmark-set-no-overwrite
                     "bi" 'bookmark-insert
                     "br" 'bookmark-rename
                     "bd" 'bookmark-delete
                     "bw" 'bookmark-write
                     "bj" 'bookmark-jump
                     "bJ" 'bookmark-jump-other-window
                     "bl" 'bookmark-bmenu-list
                     ;; text
                     "xj" 'set-justification
                     "xw" 'delete-trailing-whitespace
                     "x TAB" 'indent-rigidly

                     ;; search
                     "sa" 'swiper-all
                     "sA" 'swiper-all-thing-at-point
                     "sj" 'evil-show-jumps
                     "sm" 'evil-show-marks
                     "sr" 'evil-show-registers
                     "si" 'imenu
                     "sl" 'ivy-resume
                     "sg" 'counsel-rg

                     ;; git
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
                     "p" 'projectile-command-map

                     ;; app
                     "aa" 'org-agenda))


(provide 'init-evil)

;;; init-evil.el ends here
