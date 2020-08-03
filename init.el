;;; init --- Emacs init file

;;; Commentary:
;; Most of the content in these config files are inspired by or
;; directly copy from https://github.com/condy0919/.emacs.d
;; Also thanks to the help of Condy Chen https://github.com/condy0919/.emacs.d/

;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)))

;; Increase the amount of data from the process
;; `lsp-mode' gains
(setq read-process-output-max (* 1024 1024))

;; elpa mirrors
(setq package-archives
      '(("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("gnu"   . "https://elpa.emacs-china.org/gnu/")
        ("org"   . "https://elpa.emacs-china.org/org/")
        ))

(package-initialize)

;; init packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "settings/lang" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-core)
(require 'init-basic)
(require 'init-startup)
(require 'init-org)
(require 'init-ui)
(require 'init-tools)
(require 'init-shell)
(require 'init-dev)
(require 'init-lsp)
(require 'init-company)
(require 'init-git)
(require 'init-templates)

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
