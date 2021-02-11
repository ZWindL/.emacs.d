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

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
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
   "fz" 'counsel-fzf)
  (general-create-definer local-leader-def
                          :states 'normal
                          :prefix "SPC m")
  )

(provide 'init-evil)

;;; init-evil.el ends here
