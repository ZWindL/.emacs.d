;;; init --- Emacs init file

;;; Commentary:
;; Most of the content in these config files are inspired by or
;; directly copy from https://github.com/condy0919/.emacs.d
;; Also thanks to the help of Condy Chen https://github.com/condy0919/.emacs.d/

;;; Code:

;; Increase the amount of data from the process
;; `lsp-mode' gains
(setq read-process-output-max (* 1024 1024))

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(unless (or (daemonp) noninteractive)
  ;; Keep a ref to the actual file-name-handler
  (defvar default-file-name-handler-alist file-name-handler-alist)
  ;; Set the file-name-handler to nil (because regexing is cpu intensive)
  (setq file-name-handler-alist nil)
  ;; Reset file-name-handler-alist after initialization
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist default-file-name-handler-alist))))

;; Proxy settings
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "127.0.0.1:8123")
     ("https" . "127.0.0.1:8123")))

;; elpa mirrors
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
;; (setq package-archives
;;       '(("melpa" . "http://elpa.emacs-china.org/melpa/")
;;         ("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;         ("org"   . "http://elpa.emacs-china.org/org/")
;;         ))
;; (setq package-archives
;;       '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;         ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;; Emacs 27 introduces a quickstart mechanism which concatenate autoloads of all
;; packages to reduce the IO time.
;;
;; Don't forget to M-x package-quickstart-refresh if a new package is installed.
;; (setq package-quickstart t)

;; TODO: Does it conflict with stright.el?
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
(setq package-enable-at-startup nil)
(setq straight-repository-branch "develop")

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

(defvar straight-check-for-modifications)
(setq straight-check-for-modifications
      '(check-on-save find-when-checking))

;; Ref: https://emacs-china.org/t/package/19959/3
;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.
;; Make `package-autoremove' work with `use-package'
(defvar use-package-selected-packages '(use-package)
  "Packages pulled in by use-package.")

(eval-and-compile
  (define-advice use-package-handler/:ensure (:around (fn name-symbol keyword args rest state) select)
    (let ((items (funcall fn name-symbol keyword args rest state)))
      (dolist (ensure args items)
        (let ((package
               (or (and (eq ensure t) (use-package-as-symbol name-symbol))
                   ensure)))
          (when package
            (when (consp package)
              (setq package (car package)))
            (push `(add-to-list 'use-package-selected-packages ',package) items)))))))


(setq debug-on-error nil)
(defconst global-leader-key "SPC")

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "settings/lang" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq org-roam-v2-ack t)

(require 'init-core)
(require 'init-basic)
(require 'init-org)
(require 'init-ui)
(require 'init-tools)
(require 'init-shell)
(require 'init-dev)
(require 'init-lsp)
(require 'init-company)
(require 'init-git)
(require 'init-templates)
(require 'init-reader)
(require 'init-evil)
(require 'init-tex)

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
