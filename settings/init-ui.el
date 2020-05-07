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

(provide 'init-ui)
