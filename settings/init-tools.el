;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Tips for next keystroke
(use-package which-key
  :ensure t
  ;; :init
  ;; (setq which-key-popup-type 'frame)
  ;; (setq which-key-frame-max-width 60)
  ;; (setq which-key-frame-max-height 20)
  :hook (after-init . which-key-mode)
  :custom (which-key-idle-delay 0.5))

;; The blazing grep tool
;; Press C-c s to search
(use-package rg
  :ensure t
  :defer t
  :when (executable-find "rg")
  :hook (after-init . rg-enable-default-bindings))

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  ;; integrate with isearch and others
  ;; C-' to select isearch-candidate with avy
  :hook (after-init . avy-setup-default)
  :custom
  (avy-timeout-seconds 0.2)
  (avy-all-windows nil)
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))
  :config
  ;; Force to use pre `avy-style'
  (define-advice avy-isearch (:around (func &rest args))
    (let ((avy-style 'pre))
      (apply func args)))
  :bind (
         ("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

;; ivy core
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :custom
  (ivy-display-style 'fancy)          ;; fancy style
  (ivy-count-format "%d/%d ")         ;; better counts
  (ivy-use-virtual-buffers t)         ;; show recent files
  (ivy-height 10)
  (ivy-fixed-height-minibuffer t)     ;; fixed height
  (ivy-on-del-error-function 'ignore) ;; dont quit minibuffer when del-error
  :config
  ;; Default keybinding: C-'
  (use-package ivy-avy :ensure t)
  :bind (
         ("C-c C-r" . ivy-resume)
         ("C-c k" . counsel-ag)
         :map ivy-minibuffer-map
         ("C-c C-e" . my/ivy-woccur)
         :map ivy-occur-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-occur-grep-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode))
  :preface
  ;; Copy from
  ;; https://github.com/honmaple/maple-emacs/blob/master/lisp/init-ivy.el
  (defun my/ivy-woccur ()
    "ivy-occur with wgrep-mode enabled."
    (interactive)
    (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur)))

(use-package ivy-rich :ensure t
  :hook (ivy-mode . ivy-rich-mode)
  :custom (ivy-rich-path-style 'abbrev))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; Fuzzy matcher
(use-package counsel
  :ensure t
  :hook (ivy-mode . counsel-mode)
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("d" my/delete-file "delete")
     ("r" my/rename-file "rename")
     ("l" vlf            "view large file")
     ("b" hexl-find-file "open file in binary mode")
     ("x" counsel-find-file-as-root "open as root")))
  :bind (([remap recentf-open-files] . counsel-recentf)
         ([remap swiper]             . counsel-grep-or-swiper))
  :custom
  (counsel-preselect-current-file t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n-----------\n")
  (counsel-find-file-at-point nil)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

;; Use swiper less, it takes up `ivy-height' lines.
(use-package isearch
  :ensure nil
  :custom
  ;; One space can represent a sequence of whitespaces
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace t)
  (search-whitespace-regexp "[ \t\r\n]+")
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-count-suffix-format nil)
  (lazy-highlight-cleanup nil)
  :bind (:map isearch-mode-map
              ;; consistent with ivy-occur
              ("C-c C-o" . isearch-occur)
              ;; Edit the search string instead of jumping back
              ([remap isearch-delete-char] . isearch-del-char)
              ([remap isearch-query-replace] . anzu-isearch-query-replace)
              ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

;; isearch alternative
(use-package swiper
  :ensure t
  :defer t
  :custom
  (swiper-action-recenter t))

;; Preview search/replace results
(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-threshold 50)
   '(anzu-replace-to-string-separator " => ")))

;; Writable grep buffer. company well with ivy-occur
(use-package wgrep
  :ensure t
  :defer 1
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; View/Edit reStructuredText file
(use-package rst
  :ensure nil
  :mode (("\\.rst\\'"  . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;; Write documentation comment in an easy way
(use-package separedit
  :ensure t
  :custom
  (separedit-default-mode 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-buffer-creation-hook #'auto-fill-mode)
  :bind (:map prog-mode-map
              ("C-c '" . separedit)))

;; Pixel alignment for org/markdown tables
(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))
  ;; :config
  ;; compatible with outline mode
  ;; (define-advice outline-show-entry (:override nil))
  ;;:custom
  ;;(valign-fancy-bar t))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . auto-fill-mode)
  :init
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore)
  :custom
  (markdown-header-scaling t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t))

;; Generate table of contents for markdown-mode
(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map
         ("r" . markdown-toc-generate-or-refresh-toc)))

;; Jump forward-backward between CamelCasesWords
(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))

;; Free hands
(use-package auto-package-update
  :ensure t
  :defer t
  :custom
  (auto-package-update-delete-old-versions t))

;; GC optimization
(use-package gcmh
  :ensure t
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000) ;; 100 MB
  :hook (after-init . gcmh-mode))

;; winum
(use-package winum
  :ensure t)

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; Pastebin service
(use-package webpaste
  :ensure t
  :defer 1
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
  :custom
  (webpaste-open-in-browser t)
  (webpaste-paste-confirmation t)
  (webpaste-add-to-killring nil)
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "ix.io")))

;; Open very large files
(use-package vlf-setup
  :ensure vlf)

;; Customize popwin behavior
(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((magit-status-mode    :select t :inhibit-window-quit t :same t)
                   (magit-log-mode       :select t :inhibit-window-quit t :same t)
                   (profiler-report-mode :select t)
                   (help-mode            :select t :align t :size 0.4)
                   (comint-mode          :select t :align t :size 0.4)
                   (Man-mode             :select t :other t)
                   (woman-mode           :select t :other t)
                   (grep-mode            :select t :align t)
                   (rg-mode              :select t :align t)
                   ("*bm-bookmarks*"           :select t   :align t)
                   ("*Flycheck errors*"        :select t   :align t :size 10)
                   ("*quickrun*"               :select nil :align t :size 15)
                   ("*Backtrace*"              :select t   :align t :size 15)
                   ("*Shell Command Output*"   :select nil :align t :size 0.4)
                   ("*Async Shell Command*"    :ignore t)
                   ("*package update results*" :select nil :align t :size 10)
                   ("\\*ivy-occur .*\\*"       :regexp t :select t :align t))))

;; smart parens
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :init
  (require 'smartparens-config)
  :config
  (progn (show-smartparens-global-mode t))
  ;; Auto indent after hit return
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
  ;; This snippet comes from https://ebzzry.io/en/emacs-pairs/
  ;;   (defmacro def-pairs (pairs)
  ;;     "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
  ;; conses, where NAME is the function name that will be created and
  ;; STRING is a single-character string that marks the opening character.

  ;;   (def-pairs ((paren . \"(\")
  ;;               (bracket . \"[\"))

  ;; defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
  ;; respectively."
  ;;     `(progn
  ;;        ,@(loop for (key . val) in pairs
  ;;                collect
  ;;                `(defun ,(read (concat
  ;;                                "wrap-with-"
  ;;                                (prin1-to-string key)
  ;;                                "s"))
  ;;                     (&optional arg)
  ;;                   (interactive "p")
  ;;                   (sp-wrap-with-pair ,val)))))

  ;;   (def-pairs ((paren . "(")
  ;;               (bracket . "[")
  ;;               (brace . "{")
  ;;               (single-quote . "'")
  ;;               (double-quote . "\"")
  ;;               (back-quote . "`")))
  :bind
  (:map smartparens-mode-map
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-e" . sp-end-of-sexp)

        ("C-<down>" . sp-down-sexp)
        ("C-<up>"   . sp-up-sexp)
        ("M-<down>" . sp-backward-down-sexp)
        ("M-<up>"   . sp-backward-up-sexp)

        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)

        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)

        ("C-S-f" . sp-forward-symbol)
        ("C-S-b" . sp-backward-symbol)

        ("C-<right>" . sp-forward-slurp-sexp)
        ("M-<right>" . sp-forward-barf-sexp)
        ("C-<left>"  . sp-backward-slurp-sexp)
        ("M-<left>"  . sp-backward-barf-sexp)

        ("C-M-t" . sp-transpose-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-k"   . sp-kill-hybrid-sexp)
        ("M-k"   . sp-backward-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-d" . delete-sexp)

        ("M-<backspace>" . backward-kill-word)
        ("C-<backspace>" . sp-backward-kill-word)
        ([remap sp-backward-kill-word] . backward-kill-word)

        ("M-[" . sp-backward-unwrap-sexp)
        ("M-]" . sp-unwrap-sexp)

        ("C-x C-t" . sp-transpose-hybrid-sexp)

        ;; ("C-c ("  . wrap-with-parens)
        ;; ("C-c ["  . wrap-with-brackets)
        ;; ("C-c {"  . wrap-with-braces)
        ;; ("C-c '"  . wrap-with-single-quotes)
        ;; ("C-c \"" . wrap-with-double-quotes)
        ;; ("C-c _"  . wrap-with-underscores)
        ;; ("C-c `"  . wrap-with-back-quotes))
        ))

;; Leetcode!!!
(use-package leetcode
  :ensure t
  :config
  (setq leetcode-prefer-language "golang")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/Projects/leetcode"))

;; 内置中文输入法 + 中文分词
(use-package pyim
  :ensure t
  :demand t
  :after evil
  :config
  ;; 激活 basedict 拼音词库
  ;; (use-package pyim-basedict
  ;;   :ensure nil
  ;;   :config (pyim-basedict-enable))
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'xiaohe-shuangpin)
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)
  (setq pyim-page-tooltip 'popup)
  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)
  ;; (global-set-key (kbd "M-f") 'pyim-forward-word)
  ;; (global-set-key (kbd "M-b") 'pyim-backward-word)
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  :bind
  ("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合测试
  ("C-;" . pyim-delete-word-from-personal-buffer))
  ;; (:map evil-normal-state-map
  ;;       ("w" . pyim-forward-word)
  ;;       ("b" . pyim-backward-word)))

(use-package remember
  :ensure nil
  :custom
  (remember-handler-functions '(remember-store-in-files))
  (remember-data-directory (concat org-directory "remember/")))

(use-package wakatime-mode
  :hook (after-init . global-wakatime-mode)
  :custom
  (wakatime-cli-path "/usr/bin/wakatime")
  :ensure t)

;; For editing plantuml language
(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :custom
  (plantuml-executable-path "/usr/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(provide 'init-tools)

;;; init-tools.el ends here
