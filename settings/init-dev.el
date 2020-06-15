;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Compilation Mode
(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (compilation-ask-about-save nil) ;; save all buffers on `compile'
  :hook (compilation-filter . my/colorize-compilation-buffer)
  :config
  (add-to-list 'compilation-finish-functions 'my/notify-compilation-result)
  :preface
  (defun my/colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (defun my/notify-compilation-result (_comp-buffer exit-string)
    "Notify after the compilation is done."
    (if (string-match "^finished" exit-string)
        (notifications-notify :title "Compilation"
                              :body "Compilation successful :-)"
                              :timeout 5000
                              :urgency 'normal)
      (notifications-notify :title "Compilation"
                            :body "Compilation failed :-("
                            :timeout 5000
                            :urgency 'critical)))
  )

;; Debugger
(use-package gdb-mi
  :ensure nil
  :hook (gud-mode . gud-tooltip-mode)
  :custom
  (gdb-show-main t)
  (gdb-display-io-nopopup t)
  (gdb-show-changed-values t)
  (gdb-delete-out-of-scope t)
  (gdb-use-colon-colon-notation t)
  (gdb-restore-window-configuration-after-quit t)
  :config
  ;; Add color to the current GUD line
  ;; From https://kousik.blogspot.com/2005/10/highlight-current-line-in-gdbemacs.html
  (defconst gud-highlight-face 'secondary-selection)

  (defvar gud-overlay
    (let ((overlay (make-overlay (point) (point))))
      (overlay-put overlay 'face gud-highlight-face)
      overlay)
    "Overlay variable for GUD highlighting.")

  (define-advice gud-display-line (:after (true-file _line))
    "Highlight gud current line."
    (when-let* ((buffer (gud-find-file true-file)))
      (with-current-buffer buffer
        (move-overlay gud-overlay (line-beginning-position) (line-end-position)
                      (current-buffer)))))

  (define-advice gud-kill-buffer-hook (:after nil)
    "Remove highlight overlay."
    (delete-overlay gud-overlay))

  (define-advice gud-sentinel (:after (_1 _2))
    "Remove highlight overlay when user quit gud."
    (delete-overlay gud-overlay)))

;; Highlight TODO
(use-package hl-todo
  :ensure t
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur))
  :hook (prog-mode . hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "ISSUE" "FIXME" "XXX" "NOTE" "NB"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

;; Visual diff interface
(use-package ediff
  :ensure nil
  :hook (ediff-quit . winner-undo) ;; restore windows layout
  :custom
  (ediff-diff-options "-w") ;; turn off whitespace checking
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :ensure t
  :defer 1
  :custom (quickrun-focus-p nil)
  :bind (("C-c x" . quickrun)))

;; Superb compiler explorer implementation
(use-package rmsbolt
  :ensure t
  :defer 1
  :custom
  (rmsbolt-asm-format nil)
  (rmsbolt-default-directory "/tmp"))

;; Project management
(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :hook (prog-mode . projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'hybrid)
  (projectile-read-command nil) ;; no prompt in projectile-compile-project
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so"))
  (projectile-ignored-projects '("/tmp/"))
  :config
  ;; cmake project build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake -Bbuild"
                                    :compile "cmake --build build"
                                    :test "cd build && ctest --output-on-failure")

  ;; bazel project builds
  (projectile-register-project-type 'bazel '("WORKSPACE")
                                    :compile "bazel build //..."
                                    :test "bazel test //...")

  (dolist (dir '(".ccls-cache"
                 ".clangd"
                 "bazel-bin"
                 "bazel-out"
                 "node_modules"
                 "package.json"
                 "bazel-testlogs"))
    (push dir projectile-globally-ignored-directories))
  )

;; Comprehensive ivy integration for projectile
(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :hook (prog-mode . counsel-projectile-mode))

;; Lint tool
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  ;; clang/gcc/cppcheck flycheckers never know the include path
  (flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :config
  ;; consistent with help-mode
  (with-eval-after-load 'evil
    (evil-set-initial-state 'flycheck-error-list-mode 'normal)
    (evil-define-key 'normal flycheck-error-list-mode-map
      "q" 'quit-window))
  )

(use-package flymake
  :disabled
  :custom
  (flymake-fringe-indicator-position 'right-fringe))

;; Spell checker
(use-package flyspell
  :ensure nil
  :if (executable-find "hunspell")
  :hook (git-commit-mode . flyspell-mode)
  :custom
  (ispell-dictionary "en_US")
  (ispell-program-name "hunspell")
  (ispell-personal-dictionary
   (expand-file-name "hunspell_dict.txt" user-emacs-directory))
  ;; "C-;" is captured by fcitx
  ;; M-TAB also can correct
  (flyspell-auto-correct-binding (kbd "C-M-;"))
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil))

;; xref
;; (use-package ivy-xref
;;   :ensure t
;;   :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  ;; (when (>= emacs-major-version 27)
  ;;   (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  ;; (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
;;        )

;; Antlr mode
(use-package antlr-mode
  :ensure nil
  :mode ("\\.g4\\'" . antlr-mode))

;; Config files mode
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'"  . yaml-mode)))
(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

(require 'init-cpp)
(require 'init-elisp)
(require 'init-rust)
(require 'init-html)
(require 'init-python)
(require 'init-ocaml)

(provide 'init-dev)

;;; init-dev.el ends here
