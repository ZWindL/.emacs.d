;; init-gpt.el --- Initialize ChatGPT configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; ChatGPT configurations.
;;
;;; Code:

(use-package gptel
  :ensure t
  :custom
  (gptel-api-key (get-gpt-api-key))
  :init
  (defun get-gpt-api-key ()
    "Read the contents of the `gpt.api.key' file in the Emacs user config root directory and return its contents as a string."
    (let ((key-file (expand-file-name "gpt.api.key" user-emacs-directory)))
      (when (file-exists-p key-file)
        (with-temp-buffer
          (insert-file-contents key-file)
          (buffer-substring-no-properties (point-min) (point-max)))))))

(provide 'init-gpt)
;;; init-gpt.el ends here
