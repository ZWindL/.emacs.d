;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package ccls
  :ensure t
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (ccls-use-default-rainbow-sem-highlight)
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json" ".ccls")
                  projectile-project-root-files-top-down-recurring)))
  (progn
    (eval-when-compile
      (lsp-interface
       (CclsLR (:L :R) nil)
       (CclsSemanticHighlightSymbol (:id :parentKind :kind :storage :ranges) nil)
       (CclsSemanticHighlight (:uri :symbols) nil)
       (CclsSkippedRanges (:uri :skippedRanges) nil))))
  :init
  (if (eq system-type 'darwin)
      (setq ccls-executable "/usr/local/bin/ccls")
    (setq ccls-executable "/usr/bin/ccls"))
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (setq ccls-sem-highlight-method 'font-lock)
  (defun ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
  (defun ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))
  (defun ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

  ;; References w/ Role::Role
  (defun ccls/references-read () (interactive)
         (lsp-ui-peek-find-custom "textDocument/references"
                                  (plist-put (lsp--text-document-position-params) :role 8)))

  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16)))

  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro () (interactive)
         (lsp-ui-peek-find-custom "textDocument/references"
                                  (plist-put (lsp--text-document-position-params) :role 64)))

  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call () (interactive)
         (lsp-ui-peek-find-custom "textDocument/references"
                                  (plist-put (lsp--text-document-position-params) :excludeRole 32)))

  ;; ccls/vars ccls/base ccls/derived ccls/members have a parameter while others are interactive.
  ;; (ccls/base 1) direct bases
  ;; (ccls/derived 1) direct derived
  ;; (ccls/member 2) => 2 (Type) => nested classes / types in a namespace
  ;; (ccls/member 3) => 3 (Func) => member functions / functions in a namespace
  ;; (ccls/member 0) => member variables / variables in a namespace
  ;; (ccls/vars 1) => field
  ;; (ccls/vars 2) => local variable
  ;; (ccls/vars 3) => field or local variable. 3 = 1 | 2
  ;; (ccls/vars 4) => parameter

  ;; References whose filenames are under this project
  ;;(lsp-ui-peek-find-references nil (list :folders (vector (projectile-project-root)))))
)

;; (use-package cc-mode
;;   :ensure nil
;;   :mode ("\\.cxx\\'" . cc-mode)
;;   :hook (c-mode . (lambda ()
;;                     (setq comment-start "// "
;;                           comment-end "")))
;;   :defines (lsp-clients-clangd-args)
;;   :custom
;;   (c-offsets-alist '((inline-open           . 0)
;;                      (brace-list-open       . 0)
;;                      (inextern-lang         . 0)
;;                      (statement-case-open   . 4)
;;                      (access-label          . -)
;;                      (case-label            . 0)
;;                      (member-init-intro     . +)
;;                      (topmost-intro         . 0)
;;                      (inlambda              . 0) ;; better indentation for lambda
;;                      (innamespace           . -) ;; no indentation after namespace
;;                      (arglist-cont-nonempty . +)))
;;   :config
;;   (setq c-basic-offset 4)
;;   (with-eval-after-load 'lsp-mode
;;     (setq lsp-clients-clangd-args
;;           '("-j=2"
;;             "--background-index"
;;             "--clang-tidy"
;;             "--completion-style=bundled"
;;             "--pch-storage=memory"
;;             "--suggest-missing-includes"
;;             "--header-insertion-decorators=0")))
;;   )

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :ensure t
  :defines (company-backends)
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends 'company-cmake)))

(provide 'init-cpp)
;;; init-cpp.el ends here
