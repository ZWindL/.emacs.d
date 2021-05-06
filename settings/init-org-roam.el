;;; init-org-roam.el --- Description.

;;; Commentary:

;;; Code:
;; Make better connection in your notes
(use-package org-roam
  :ensure t
  :hook ((org-load . org-roam-mode)
         (org-roam-backlinks-mode . visual-line-mode))
  :custom
  (org-roam-directory (expand-file-name (concat org-directory "roam/")))
  (org-roam-buffer-no-delete-other-windows t)
  (org-roam-completion-system 'ivy)
  (org-roam-tag-sources '(prop vanilla all-directories))
  (org-roam-title-sources '(title alias))
  (org-roam-index-file (expand-file-name "index.org" org-roam-directory))
  (org-roam-graph-viewer "/usr/bin/firefox-nightly")
  (org-roam-dailies-directory "roam-daily/")
  (org-roam-completion-system 'ivy)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-link-title-format "R:%s")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
      '(("s" "study" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n"
         :olp ("Study notes"))

        ("j" "journal" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n"
         :olp ("Journal"))))
  (org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           ;; shortcut / full-name / plain | entry (for headers) | dedicated function
           "* What is it? %?" ;; The template inserted on each call to `capture'
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_alias: \n\n#+roam_tags: \n\n - links :: \n\n"
           :unnarrowed t) ;; Tells the org-roam to show the contents of the whole file
          ))
  :bind (:map org-roam-mode-map
         ("C-c n b" . org-roam-switch-to-buffer)
         ("C-c n c d" . org-roam-dailies-capture-today)
         ("C-c n c c" . org-roam-capture)
         ("C-c n f f" . org-roam-find-file)
         ("C-c n f d" . org-roam-dailies-find-today)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-insert)
         ("C-c n j" . org-roam-jump-to-index)
         ("C-c n l" . org-roam)
         ("C-c n d d" . org-roam-doctor))
  :config
  (use-package org-roam-protocol))

;; Visulize org-roam files
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

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
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
