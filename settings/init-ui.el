;;; init-ui --- custmize UI components

;;; Commentary:
;; consider using ivy-postframe, it's good looking but overlap a part of the main frame

;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-one-light t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  :custom
  (doom-themes-treemacs-theme "doom-colors"))

(use-package leuven-theme
  :ensure t
  :config
  ;; (load-theme 'leuven-dark t)
  :custom
  (org-fontify-whole-heading-line t)
  (leuven-scale-org-agenda-structure t))

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
  :hook ((consel-grep-post-action
         dumb-jump-after-jump
         bookmark-after-jump
         imenu-after-jump) . my/recenter-and-blink)
  :custom
  (beacon-blink-when-window-scrolls nil)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-point-moves t))

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
  :custom
  (whitespace-line-column nil)
  (whitespace-style
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
  :after evil
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-headline-match)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
	  ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
	  ;; "Remote")
	  ((or (string-equal "*" (substring (buffer-name) 0 1))
	       (memq major-mode '(magit-process-mode
				              magit-status-mode
				              magit-diff-mode
				              magit-log-mode
				              magit-file-mode
				              magit-blob-mode
				              magit-blame-mode
				              )))
	   "Emacs")
	  ((derived-mode-p 'prog-mode)
	   "Editing")
	  ((derived-mode-p 'dired-mode)
	   "Dired")
	  ((memq major-mode '(helpful-mode
			              help-mode))
	   "Help")
	  ((memq major-mode '(org-mode
			              org-agenda-clockreport-mode
			              org-src-mode
			              org-agenda-mode
			              org-beamer-mode
			              org-indent-mode
			              org-bullets-mode
			              org-cdlatex-mode
			              org-agenda-log-mode
			              diary-mode))
	   "OrgMode")
	  (t
	   (centaur-tabs-get-group-name (current-buffer))))))
  :custom
  (centaur-tabs-mode t)
  ;;(centaur-tabs-style "slant")
  ;;(centaur-tabs-style "wave")
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'left)
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  ;;(centaur-tabs-close-button "x")
  (centaur-tabs-set-modified-marker t)
  ;;(centaur-tabs-modified-marker "*")
  (centaur-tabs-show-navigation-buttons t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  (:map evil-normal-state-map
	     ("g t" . centaur-tabs-forward)
	     ("g T" . centaur-tabs-backward)))

;; native Emacs tab bar
;; (use-package tab-bar
;;   :ensure nil
;;   :when (>= emacs-major-version 27)
;;   :hook (after-init . (lambda ()
;;                         (tab-bar-mode)
;;                         (tab-bar-history-mode))))

(use-package all-the-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode))

(use-package recentf
  :ensure nil
  :hook ((after-init . recentf-mode)
         (focus-out-hook . (recentf-save-list recentf-cleanup)))
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     no-littering-var-directory
                     no-littering-etc-directory
                     ".cache"
                     "cache"
                     "recentf"
                     "^/tmp/"
                     "/ssh:"
                     "^/usr/include/"
                     "bookmarks"
                     "COMMIT_EDITMSG\\'")))

(use-package dashboard
  :ensure t
  :hook
  (after-init . dashboard-setup-startup-hook)
  (dashboard-mode . (lambda ()
                             (setq-local global-hl-line-mode nil)))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :custom
  (dashboard-banner-logo-title "Welcome back! ZWindL")
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents   . 10)
                     (projects  . 5)
                     (bookmarks . 5))))

(provide 'init-ui)

;;; init-ui.el ends here
