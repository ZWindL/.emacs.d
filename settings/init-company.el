;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Auto-completion configurations.
;;
;;; Code:

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and (display-graphic-p)
       (require 'all-the-icons nil t)))

;; yasnippet support
(use-package yasnippet
  :ensure t
  ;; :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :custom
  (yas-inhibit-overlay-modification-protection t)
  :config
  ;; pre-wrote snippets
  (use-package yasnippet-snippets :ensure t))


;; The completion engine
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-show-numbers t) ;; Easy navigation to candidates with M-<n>
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  ;; complete `abbrev' only in current buffer
  (company-dabbrev-other-buffers nil)
  ;; make dabbrev case-sensitive
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-tempo-expand t)
  (company-transformers '(company-sort-prefer-same-case-prefix))
  (company-backends '((company-capf company-yasnippet)
                      (company-files company-dabbrev-code company-etags company-keywords company-dabbrev)))
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete)
              :map company-active-map
              ([escape] . company-abort)
              ("C-p"    . company-select-previous)
              ("C-n"    . company-select-next)
              ("C-s"    . company-filter-candidates)
              ("<tab>"  . company-complete-common-or-cycle)
              :map company-search-map
              ([escape] . company-search-abort)
              ("C-p"    . company-select-previous)
              ("C-n"    . company-select-next)))

(use-package company-try-hard
  :ensure t
  :bind (("C-<tab>" . company-try-hard)
         :map company-active-map
         ("C-<tab>" . company-try-hard)))

(use-package company-box
  :ensure t
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :init (setq company-box-enable-icon t
              company-box-backends-colors nil
              company-box-show-single-candidate t
              company-box-doc-delay 0.5)
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

    (when (icons-displayable-p)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 1.0 :v-adjust -0.2))
              (Text . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
              (Method . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Function . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Constructor . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Field . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
              (Variable . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
              (Class . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Interface . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Module . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Property . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
              (Value . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Enum . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
              (Snippet . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
              (Color . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
              (File . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
              (Folder . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
              (Constant . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
              (Struct . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Event . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
              (TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
              (Template . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
            company-box-icons-alist 'company-box-icons-all-the-icons))))


(use-package company-prescient
  :ensure t
  :hook
  (after-init . company-prescient-mode))

(provide 'init-company)

;;; init-company.el ends here
