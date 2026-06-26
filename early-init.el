;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; Back up the GC configuration so we can restore it after initialization. The
;; function kjz-restore-gc-values gets called from the hook at the end of
;; init.el. This significantly speeds up Emacs startup.

(defvar kjz-backup-gc-cons-threshold gc-cons-threshold)
(defvar kjz-backup-gc-cons-percentage gc-cons-percentage)

(if noninteractive
    (setq gc-cons-threshold 268435456) ; 256 Mb, for batch processing
  (setq gc-cons-threshold most-positive-fixnum))
(setq gc-cons-percentage 1.0)

(defun kjz-restore-gc-values ()
  "Restore garbage collection values to the defaults detected before initialization."
  (setq gc-cons-threshold kjz-backup-gc-cons-threshold)
  (setq gc-cons-percentage kjz-backup-gc-cons-percentage))
