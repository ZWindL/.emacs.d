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
  ;; :hook (yas-global-mode . global-company-mode)
  ;; :hook (prog-mode . company-mode)
  :custom
  (company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-show-numbers t) ;; Easy navigation to candidates with M-<n>
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  ;; complete `abbrev' only in current buffer
  (company-dabbrev-other-buffers nil)
  ;; make dabbrev case-sensitive
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-transformers '(company-sort-prefer-same-case-prefix))
  (company-backends
   '(company-semantic company-cmake company-capf company-clang company-files
              (company-dabbrev-code company-gtags company-etags company-keywords)
              company-oddmuse company-dabbrev))
  ;; (company-frontends '(company-pseudo-tooltip-frontend
                       ;; company-echo-metadata-frontend))
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete)
              ("<backtab>" . company-yasnippet)
              :map company-active-map
              ([escape] . company-abort)
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-s" . company-filter-candidates)
              ("<tab>" . company-complete-common-or-cycle)
              ("<backtab>" . company-yasnippet)
              :map company-search-map
              ([escape] . company-search-abort)
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next)))

;; Copied from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el#L115
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
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
          company-box-icons-alist 'company-box-icons-all-the-icons))))

(use-package company-prescient
  :ensure t
  :init
  (company-prescient-mode 1))

;; Let the AI take my job!
(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine)
  ;; workaround for company-transformers
  (setq company-tabnine--disable-next-transform nil)
  (defun my-company--transform-candidates (func &rest args)
    (if (not company-tabnine--disable-next-transform)
        (apply func args)
      (setq company-tabnine--disable-next-transform nil)
      (car args)))

  (defun my-company-tabnine (func &rest args)
    (when (eq (car args) 'candidates)
      (setq company-tabnine--disable-next-transform t))
    (apply func args))

  (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
  (advice-add #'company-tabnine :around #'my-company-tabnine))

(provide 'init-company)

;;; init-company.el ends here
