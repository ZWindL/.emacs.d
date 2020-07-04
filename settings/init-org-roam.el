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
  (org-roam-tag-sources '(prop all-directories))
  (org-roam-index-file (expand-file-name "index.org" org-roam-directory))
  (org-roam-graph-viewer "/usr/bin/firefox-nightly")
  (org-roam-completion-system 'ivy)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           ;; shortcut / full-name / plain | entry (for headers) | dedicated function
           "%?" ;; The template inserted on each call to `capture'
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t) ;; Tells the org-roam to show the contents of the whole file
          ))
  :bind (:map org-roam-mode-map
         ("C-c n b" . org-roam-switch-to-buffer)
         ("C-c n c" . org-roam-capture)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-insert)
         ("C-c n j" . org-roam-jump-to-index)
         ("C-c n l" . org-roam))
  :config
  (use-package org-roam-protocol))

;; Complete roam files
(use-package company-org-roam
  :ensure t
  :after (company org-roam)
  :config
  (push 'company-org-roam company-backends))

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

(provide 'init-org-roam)
;;; init-org-roam.el ends here
