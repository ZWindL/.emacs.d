;;; init-org-roam.el --- Description.

;;; Commentary:

;;; Code:

;; Dependency of org-roam
(use-package emacsql-sqlite-builtin :ensure t)

;; Make better connection in your notes
(use-package org-roam
  :ensure t
  :requires emacsql-sqlite-builtin
  :hook
  ;; (after-init . org-roam-mode)
  (after-init . org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (expand-file-name (concat org-directory "roam/")))
  (org-roam-graph-viewer "/usr/bin/firefox-nightly")
  (org-roam-protocol-store-links t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-completion-everywhere t)
  (org-roam-database-connector 'sqlite-builtin)
  ;; (org-roam-node-display-template "${tags:10} ${title:100} ${backlinkscount:6}")
  (org-roam-node-display-template
    (concat (propertize "${tags:20}  " 'face 'org-tag)
            "${title:*} ${backlinkscount:*}"))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n\n#+filetags: \n\n - tags :: \n\n* What is it?\n")
      :unnarrowed t) ;; Tells the org-roam to show the contents of the whole file
     ))
  (org-roam-mode-section-functions
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section))
  :bind (("C-c n b"   . org-roam-buffer-toggle)
         ("C-c n B"   . org-roam-buffer-display-dedicated)
         ("C-c n c"   . org-roam-capture)
         ("C-c n f"   . org-roam-node-find)
         ("C-c n g"   . org-roam-graph)
         ("C-c n i"   . org-roam-node-insert)
         ("C-c n l"   . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-enable)
  (require 'org-roam-protocol)
  ;; for org-roam-buffer-toggle
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count))))

;; Visulize org-roam files
(use-package org-roam-ui
  :ensure t
  :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
  :bind
  ("C-c n u" . org-roam-ui-mode)
  ("C-c n w" . org-roam-ui-open)
  ("C-c n o" . org-roam-ui-follow-mode)
  :config
  (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; Notes manager
;; NOTE: Currently only manage org-roam files
(use-package deft
  :ensure t
  :after org
  :defer t
  :defines (org-roam-directory)
  :init (setq deft-default-extension "org")
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-directory org-roam-directory)
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase))))

;; org download
(use-package org-download
  :ensure t
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
