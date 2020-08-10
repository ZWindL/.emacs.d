;;; init-ui --- custmize UI components

;;; Commentary:
;; consider using ivy-postframe, it's good looking but overlap a part of the main frame

;;; Code:

(use-package doom-themes
  :ensure t
  :config
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-one-light t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven-dark t)
  (setq org-fontify-whole-heading-line t)
  (setq leuven-scale-org-agenda-structure t))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-github nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-env-version t)
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
  :init (beacon-mode 1)
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

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode org-medo conf-mode) . whitespace-mode)
  :config
  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7")))
  (setq
   whitespace-line-column nil
   whitespace-style
   '(face             ; visualize things below:
     empty            ; empty lines at beginning/end of buffer
     lines-tail       ; lines go beyond `fill-column'
     space-before-tab ; spaces before tab
     trailing         ; trailing blanks
     tabs             ; tabs (show by face)
     tab-mark         ; tabs (show by symbol)
     )))



;; Modern tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "slant"
        ;; (setq centaur-tabs-style "rounded")
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        ;; centaur-tabs-close-button "x"
        centaur-tabs-set-modified-marker t
        ;; centaur-tabs-modified-marker "*"
        centaur-tabs-show-navigation-buttons t)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; native Emacs tab bar
;; (use-package tab-bar
;;   :ensure nil
;;   :when (>= emacs-major-version 27)
;;   :hook (after-init . (lambda ()
;;                         (tab-bar-mode)
;;                         (tab-bar-history-mode))))

(provide 'init-ui)

;;; init-ui.el ends here
