;;; init-org-roam.el --- Description.

;;; Commentary:

;;; Code:
;; Make better connection in your notes
(use-package org-roam
  :ensure t
  :hook
  ;; (after-init . org-roam-mode)
  (after-init . org-roam-db-autosync-mode)
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (expand-file-name (concat org-directory "roam/")))
  (org-roam-index-file (expand-file-name "index.org" org-roam-directory))
  (org-roam-graph-viewer "/usr/bin/firefox-nightly")
  (org-roam-completion-system 'ivy)
  (org-roam-protocol-store-links t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template "${directories:10} ${tags:10} ${title:100} ${backlinkscount:6}")
  (org-roam-dailies-directory  "roam-daily/")
  (org-roam-dailies-capture-templates
   '(("s" "study" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "daily/%<%Y-%m-%d>.org"
      :head "#+title: %<%Y-%m-%d>\n"
      :olp ("Study notes"))

     ("j" "journal" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "daily/%<%Y-%m-%d>.org"
      :head "#+title: %<%Y-%m-%d>\n"
      :olp ("Journal"))))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?" ;; The template inserted on each call to `capture'
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n\n#+filetags: \n\n - tags :: \n\n* What is it?\n")
      :unnarrowed t) ;; Tells the org-roam to show the contents of the whole file
     ))
  (org-roam-mode-section-functions
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         ;; #'org-roam-unlinked-references-section
         ))
  :bind (("C-c n b"   . org-roam-buffer-toggle)
         ("C-c n B"   . org-roam-buffer-display-dedicated)
         ("C-c n c t" . org-roam-dailies-capture-today)
         ("C-c n c c" . org-roam-capture)
         ("C-c n f"   . org-roam-node-find)
         ("C-c n t"   . org-roam-dailies-goto-today)
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
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (f-split dirs)))
      ""))
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
