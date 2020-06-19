;;; init-ui --- custmize UI components

;;; Commentary:
;; consider using ivy-postframe, it's good looking but overlap a part of the main frame

;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
;  (load-theme 'doom-one-light t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-github nil)
  (doom-modeline-unicode-fallback t)
  :hook (after-init . doom-modeline-mode))

;; Restore windows layout
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

;; Nyan cat mode
(use-package nyan-mode
  :ensure t
  :custom
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t)
  :hook
  (after-init . nyan-mode))

;; Parrot mode
(use-package parrot
  :ensure t
  :config
  (parrot-mode)
  :custom
  (parrot-set-parrot-type 'confused))
;; default, confused, emacs, nyan, rotating, science, thumbsup
;; (add-hook 'mu4e-index-updated-hook #'parrot-start-animation))

;; Blink the current line with fancy animation
(use-package beacon
  :ensure t
  :preface
  (defun my/recenter-and-blink (&rest _)
    "Recenter and blink the current line."
    (recenter)
    (beacon-blink))
  :config
  (setq beacon-blink-when-window-scrolls nil)
  (setq beacon-blink-when-window-changes t)
  (setq beacon-blink-when-point-moves t)
  :hook ((consel-grep-post-action
         dumb-jump-after-jump
         bookmark-after-jump
         imenu-after-jump) . my/recenter-and-blink))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-ui)

;;; init-ui.el ends here
