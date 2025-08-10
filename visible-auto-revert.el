;;; visible-auto-revert.el --- Auto-revert only visible file buffers -*- lexical-binding: t; -*-

;; Author: Nobuyuki Kamimoto
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: files, convenience
;; URL: https://github.com/kn66/visible-auto-revert,el

;;; Commentary:
;;
;; This global minor mode enables `auto-revert-mode` only for buffers that:
;;   - are currently visible in any window
;;   - are visiting a file (`buffer-file-name` is non-nil)
;;
;; Optimized implementation with state tracking and differential updates
;; to minimize CPU usage and file system monitoring overhead.
;;
;; Usage:
;;   (require 'visible-auto-revert)
;;   (visible-auto-revert-mode 1)

;;; Code:

(defvar visible-auto-revert--monitored-buffers (make-hash-table :test 'eq)
  "Hash table of buffers currently being monitored.")

(defvar visible-auto-revert--update-timer nil
  "Timer for delayed updates to reduce frequent processing.")

(defcustom visible-auto-revert-delay 0.1
  "Delay in seconds before processing buffer visibility changes.
Lower values provide faster response but may increase CPU usage.
Higher values reduce CPU usage but may delay buffer monitoring updates."
  :type 'number
  :group 'auto-revert)

(defun visible-auto-revert--get-visible-file-buffers ()
  "Return hash table of visible file-visiting buffers."
  (let ((visible-hash (make-hash-table :test 'eq)))
    (dolist (window (window-list))
      (let ((buf (window-buffer window)))
        (when (buffer-file-name buf)
          (puthash buf t visible-hash))))
    visible-hash))

(defun visible-auto-revert--update-delayed ()
  "Perform differential update of auto-revert mode for visible buffers."
  (when visible-auto-revert--update-timer
    (cancel-timer visible-auto-revert--update-timer)
    (setq visible-auto-revert--update-timer nil))

  (let ((visible-hash (visible-auto-revert--get-visible-file-buffers))
        (to-enable '())
        (to-disable '()))

    ;; Find buffers that should be enabled
    (maphash (lambda (buf _)
               (unless (gethash buf visible-auto-revert--monitored-buffers)
                 (when (buffer-live-p buf)
                   (push buf to-enable))))
             visible-hash)

    ;; Find buffers that should be disabled
    (maphash (lambda (buf _)
               (unless (gethash buf visible-hash)
                 (when (buffer-live-p buf)
                   (push buf to-disable))))
             visible-auto-revert--monitored-buffers)

    ;; Apply changes
    (dolist (buf to-enable)
      (with-current-buffer buf
        (unless auto-revert-mode
          (auto-revert-mode 1))
        (puthash buf t visible-auto-revert--monitored-buffers)))

    (dolist (buf to-disable)
      (with-current-buffer buf
        (when auto-revert-mode
          (auto-revert-mode 0))
        (remhash buf visible-auto-revert--monitored-buffers)))

    ;; Clean up dead buffers
    (maphash (lambda (buf _)
               (unless (buffer-live-p buf)
                 (remhash buf visible-auto-revert--monitored-buffers)))
             visible-auto-revert--monitored-buffers)))

(defun visible-auto-revert--schedule-update (&rest _)
  "Schedule a delayed update to avoid excessive processing."
  (when visible-auto-revert--update-timer
    (cancel-timer visible-auto-revert--update-timer))
  (setq visible-auto-revert--update-timer
        (run-with-timer visible-auto-revert-delay nil
                        #'visible-auto-revert--update-delayed)))

;;;###autoload
(define-minor-mode visible-auto-revert-mode
  "Auto-revert only visible file-visiting buffers.
Optimized version that uses state tracking and differential updates."
  :global t
  :group 'auto-revert
  (if visible-auto-revert-mode
      (progn
        ;; Use delayed scheduling hooks
        (add-hook 'window-buffer-change-functions #'visible-auto-revert--schedule-update)
        (add-hook 'window-configuration-change-hook #'visible-auto-revert--schedule-update)
        (add-hook 'buffer-list-update-hook #'visible-auto-revert--schedule-update)
        ;; Initial update
        (visible-auto-revert--update-delayed))
    ;; Cleanup
    (remove-hook 'window-buffer-change-functions #'visible-auto-revert--schedule-update)
    (remove-hook 'window-configuration-change-hook #'visible-auto-revert--schedule-update)
    (remove-hook 'buffer-list-update-hook #'visible-auto-revert--schedule-update)
    ;; Cancel pending timer
    (when visible-auto-revert--update-timer
      (cancel-timer visible-auto-revert--update-timer)
      (setq visible-auto-revert--update-timer nil))
    ;; Disable auto-revert only in monitored buffers
    (maphash (lambda (buf _)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (auto-revert-mode 0))))
             visible-auto-revert--monitored-buffers)
    ;; Clear state
    (clrhash visible-auto-revert--monitored-buffers)))

(provide 'visible-auto-revert)

;;; visible-auto-revert.el ends here
