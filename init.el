;;; init --- Emacs init file

;;; Commentary:
;; Most of the content in these config files are inspired by or
;; directly copy from https://github.com/condy0919/.emacs.d
;; Also thanks to the help of Condy Chen https://github.com/condy0919/.emacs.d/

;;; Code:

;; Increase the amount of data from the process
;; `lsp-mode' gains
(setq read-process-output-max (* 1024 1024))

;; Proxy settings
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;         ("http" . "127.0.0.1:8123")
;;         ("https" . "127.0.0.1:8123")))

;; elpa mirrors
;; (setq package-archives
;;     '(("melpa" . "https://melpa.org/packages/")
;;       ("gnu" . "https://elpa.gnu.org/packages/")))
;; (setq package-archives
;;       '(("melpa" . "http://elpa.emacs-china.org/melpa/")
;;         ("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;         ("org"   . "http://elpa.emacs-china.org/org/")
;;         ))
(setq package-archives
      '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
        ;; ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;; Emacs 27 introduces a quickstart mechanism which concatenate autoloads of all
;; packages to reduce the IO time.
;;
;; Don't forget to M-x package-quickstart-refresh if a new package is installed.
;; (setq package-quickstart t)

(package-initialize)

;; init packages
(use-package use-package
  :custom
  (use-package-always-ensure nil)
  (use-package-always-defer nil)
  (use-package-always-demand nil)
  (use-package-expand-minimally nil)
  (use-package-enable-imenu-support t))

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

(when (fboundp 'package--save-selected-packages)
  (add-hook 'after-init-hook
            (lambda ()
              (package--save-selected-packages
               (seq-uniq (append use-package-selected-packages package-selected-packages))))))

(setq debug-on-error nil)

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "settings/lang" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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
