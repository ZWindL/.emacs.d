;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
(setq gc-cons-threshold most-positive-fixnum)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here