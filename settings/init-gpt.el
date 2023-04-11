;; init-gpt.el --- Initialize ChatGPT configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; ChatGPT configurations.
;;
;;; Code:

(defun get-gpt-api-key ()
    "Read the contents of the `gpt.api.key' file in the Emacs user config root."
    (let ((key-file (expand-file-name "gpt.api.key" user-emacs-directory)))
      (when (file-exists-p key-file)
        (with-temp-buffer
          (insert-file-contents key-file)
          (string-trim-right (buffer-string))))))

(use-package gptel
  :ensure t
  :custom
  (gptel-api-key (get-gpt-api-key)))

(use-package org-ai
  :ensure
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token (get-gpt-api-key))
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  ;; if you are on the gpt-4 beta:
  ;; (setq org-ai-default-chat-model "gpt-4")
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))

(provide 'init-gpt)
;;; init-gpt.el ends here
