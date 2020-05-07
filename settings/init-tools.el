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
(use-package rg
  :ensure t
  :defer t)

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  :custom
  (avy-timeout-seconds 0.2)
  (avy-all-windows nil)
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p)))

;; ivy core
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-c C-e" . my/ivy-woccur)
         :map ivy-occur-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-occur-grep-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode))
  :custom
  (ivy-display-style 'fancy)          ;; fancy style
  (ivy-count-format "%d/%d ")         ;; better counts
  (ivy-use-virtual-buffers t)         ;; show recent files
  (ivy-height 10)
  (ivy-fixed-height-minibuffer t)     ;; fixed height
  (ivy-on-del-error-function 'ignore) ;; dont quit minibuffer when del-error
  :preface
  ;; Copy from
  ;; https://github.com/honmaple/maple-emacs/blob/master/lisp/init-ivy.el
  (defun my/ivy-woccur ()
    "ivy-occur with wgrep-mode enabled."
    (interactive)
    (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur))
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
    (evil-make-overriding-map ivy-occur-mode-map 'normal)))

(provide 'init-tools)
