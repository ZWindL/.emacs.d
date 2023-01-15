;;; init-ui --- custmize UI components

;;; Commentary:
;; consider using ivy-postframe, it's good looking but overlap a part of the main frame

;;; Code:

;; Supress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Full screen
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; Pixelwise resize
;; TODO: this line can cause emacs double number overflow
;;(setq window-resize-pixelwise t
;;      frame-resize-pixelwise t)

(setq x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-underline-at-descent-line t)

;; If running Emacs under Macos, set :height to 140
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Fantasque Sans Mono" :height 140)
  (set-face-attribute 'default nil :family "Fantasque Sans Mono" :height 105))

(if (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family "Noto Sans CJK SC" :height 105))))

;; Fonts
;; Display Color Emoji
(set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)

(use-package posframe :ensure t)

;; Linux specific
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-outrun-electric t)
  ;; (load-theme 'doom-one-moonligt t)
  ;; (load-theme 'doom-one-light t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config))

;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   ;; (load-theme 'leuven-dark t)
;;   ;; (load-theme 'leuven t)
;;   :custom
;;   (org-fontify-whole-heading-line t)
;;   (leuven-scale-org-agenda-structure t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-github nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-env-version t))

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
  :defer t
  :hook (after-init . parrot-mode)
  :bind (("C-x M-n" . parrot-rotate-next-word-at-point)
         ("C-x M-p" . parrot-rotate-prev-word-at-point))
  ;; :general
  ;; (:states 'normal
  ;;          "]r" #'parrot-rotate-next-word-at-point
  ;;          "[r" #'parrot-rotate-prev-word-at-point)
  :config
  (parrot-set-parrot-type 'confused)
  (evil-define-key 'normal 'global
    "]r" #'parrot-rotate-next-word-at-point
    "[r" #'parrot-rotate-prev-word-at-point)
  (dolist (entry '((:rot ("emacs" "vim" "vscode") :caps t :upcase t)
                   (:rot ("t" "nil"))
                   (:rot ("is" "is not"))))
    (add-to-list 'parrot-rotate-dict entry)))
;; default, confused, emacs, nyan, rotating, science, thumbsup
;; (add-hook 'mu4e-index-updated-hook #'parrot-start-animation))

;; Blink the current line with fancy animation
(use-package beacon
  :ensure t
  :hook
  (after-init . beacon-mode)
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

;; Highlight changes and yanked area
(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

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
                     (bookmarks . 5)
                     (agenda    . 5)))
  :config
  (dashboard-setup-startup-hook)
  (use-package page-break-lines :ensure t))

(provide 'init-ui)

;;; init-ui.el ends here
