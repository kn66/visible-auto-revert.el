;;; visible-auto-revert.el --- Auto-revert only visible file buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 kn66

;; Author: Nobuyuki Kamimoto
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: files, convenience
;; URL: https://github.com/kn66/visible-auto-revert.el

;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This global minor mode enables `auto-revert-mode` only for buffers that:
;;   - are currently visible in any window (across all frames)
;;   - are visiting a file (`buffer-file-name' is non-nil)
;;
;; Optimized implementation with state tracking and differential updates
;; to minimize CPU usage and file system monitoring overhead.
;;
;; Usage:
;;   (require 'visible-auto-revert)
;;   (visible-auto-revert-mode 1)
;;
;; Diagnostic commands:
;;   M-x visible-auto-revert-status  - Show current monitoring status

;;; Code:

(require 'autorevert)

(defgroup visible-auto-revert nil
  "Auto-revert only visible file-visiting buffers."
  :group 'auto-revert
  :group 'files
  :prefix "visible-auto-revert-"
  :link '(url-link :tag "GitHub" "https://github.com/kn66/visible-auto-revert.el"))

(defvar visible-auto-revert--monitored-buffers (make-hash-table :test 'eq)
  "Hash table of monitored buffers.
Values are symbols describing ownership:
- `owned' means this package enabled `auto-revert-mode' for the buffer.
- `external' means Auto Revert was already active for the buffer.")

(defvar visible-auto-revert--update-timer nil
  "Timer for delayed updates to reduce frequent processing.")

(defcustom visible-auto-revert-delay 0.1
  "Delay in seconds before processing buffer visibility changes.
Lower values provide faster response but may increase CPU usage.
Higher values reduce CPU usage but may delay buffer monitoring updates."
  :type 'number
  :group 'visible-auto-revert)

(defun visible-auto-revert--auto-revert-active-p ()
  "Return non-nil if Auto Revert already handles the current buffer."
  (if (fboundp 'auto-revert-active-p)
      (auto-revert-active-p)
    (or auto-revert-mode
        (bound-and-true-p auto-revert-tail-mode)
        (bound-and-true-p auto-revert--global-mode))))

(defun visible-auto-revert--get-visible-file-buffers ()
  "Return hash table of visible file-visiting buffers across all frames."
  (let ((visible-hash (make-hash-table :test 'eq)))
    (walk-windows
     (lambda (window)
       ;; `frame-visible-p' may return `icon' for iconified frames; only `t'
       ;; means the frame is actually visible on screen.
       (when (eq (frame-visible-p (window-frame window)) t)
         (let ((buf (window-buffer window)))
           (when (and (buffer-live-p buf)
                      (buffer-file-name buf))
             (puthash buf t visible-hash)))))
     'no-minibuf t)
    visible-hash))

(defun visible-auto-revert--update-delayed ()
  "Perform differential update of auto-revert mode for visible buffers.
This function is safe to call from hooks as it catches and reports errors."
  (when visible-auto-revert--update-timer
    (cancel-timer visible-auto-revert--update-timer)
    (setq visible-auto-revert--update-timer nil))

  (condition-case err
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
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (if (visible-auto-revert--auto-revert-active-p)
                  (puthash buf 'external visible-auto-revert--monitored-buffers)
                (auto-revert-mode 1)
                (puthash buf 'owned visible-auto-revert--monitored-buffers)))))

        (dolist (buf to-disable)
          (when (buffer-live-p buf)
            (let ((ownership (gethash buf visible-auto-revert--monitored-buffers)))
              (with-current-buffer buf
                (when (and (eq ownership 'owned)
                           auto-revert-mode)
                  (auto-revert-mode -1)))
              (remhash buf visible-auto-revert--monitored-buffers))))

        ;; Clean up dead buffers from hash table
        (let ((dead-buffers '()))
          (maphash (lambda (buf _)
                     (unless (buffer-live-p buf)
                       (push buf dead-buffers)))
                   visible-auto-revert--monitored-buffers)
          (dolist (buf dead-buffers)
            (remhash buf visible-auto-revert--monitored-buffers))))
    (error
     (message "visible-auto-revert: Error during update: %S" err))))

(defun visible-auto-revert--schedule-update (&rest _)
  "Schedule a delayed update to avoid excessive processing."
  (let ((old-timer visible-auto-revert--update-timer))
    (setq visible-auto-revert--update-timer
          (run-with-timer visible-auto-revert-delay nil
                          #'visible-auto-revert--update-delayed))
    (when old-timer
      (cancel-timer old-timer))))

(defun visible-auto-revert--cleanup-killed-buffer ()
  "Remove current buffer from monitoring when killed."
  (remhash (current-buffer) visible-auto-revert--monitored-buffers))

(defun visible-auto-revert--add-window-hooks ()
  "Install hooks for visible window state tracking."
  (if (boundp 'window-state-change-functions)
      (add-hook 'window-state-change-functions
                #'visible-auto-revert--schedule-update)
    (add-hook 'window-buffer-change-functions
              #'visible-auto-revert--schedule-update)
    (add-hook 'window-configuration-change-hook
              #'visible-auto-revert--schedule-update)))

(defun visible-auto-revert--remove-window-hooks ()
  "Remove hooks installed by `visible-auto-revert--add-window-hooks'."
  (if (boundp 'window-state-change-functions)
      (remove-hook 'window-state-change-functions
                   #'visible-auto-revert--schedule-update)
    (remove-hook 'window-buffer-change-functions
                 #'visible-auto-revert--schedule-update)
    (remove-hook 'window-configuration-change-hook
                 #'visible-auto-revert--schedule-update)))

;;;###autoload
(define-minor-mode visible-auto-revert-mode
  "Auto-revert only visible file-visiting buffers.
Optimized version that uses state tracking and differential updates.
Works across all frames, only monitoring buffers in visible windows."
  :global t
  :group 'visible-auto-revert
  :lighter nil
  (if visible-auto-revert-mode
      (progn
        (visible-auto-revert--add-window-hooks)
        ;; Frame hooks for multi-frame support
        (add-hook 'after-make-frame-functions #'visible-auto-revert--schedule-update)
        (add-hook 'delete-frame-functions #'visible-auto-revert--schedule-update)
        ;; Buffer cleanup hook
        (add-hook 'kill-buffer-hook #'visible-auto-revert--cleanup-killed-buffer)
        ;; Initial update
        (visible-auto-revert--update-delayed))
    ;; Cleanup hooks
    (visible-auto-revert--remove-window-hooks)
    (remove-hook 'after-make-frame-functions #'visible-auto-revert--schedule-update)
    (remove-hook 'delete-frame-functions #'visible-auto-revert--schedule-update)
    (remove-hook 'kill-buffer-hook #'visible-auto-revert--cleanup-killed-buffer)
    ;; Cancel pending timer
    (when visible-auto-revert--update-timer
      (cancel-timer visible-auto-revert--update-timer)
      (setq visible-auto-revert--update-timer nil))
    ;; Disable auto-revert only in monitored buffers
    (maphash (lambda (buf ownership)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (and (eq ownership 'owned)
                              auto-revert-mode)
                     (auto-revert-mode -1)))))
             visible-auto-revert--monitored-buffers)
    ;; Clear state
    (clrhash visible-auto-revert--monitored-buffers)))

;;;###autoload
(defun visible-auto-revert-status ()
  "Display current visible-auto-revert monitoring status."
  (interactive)
  (let ((monitored-count (hash-table-count visible-auto-revert--monitored-buffers))
        (monitored-names '()))
    (maphash (lambda (buf _)
               (when (buffer-live-p buf)
                 (push (buffer-name buf) monitored-names)))
             visible-auto-revert--monitored-buffers)
    (message "visible-auto-revert: %s, monitoring %d buffer(s)%s"
             (if visible-auto-revert-mode "ON" "OFF")
             monitored-count
             (if monitored-names
                 (format ": %s" (string-join (sort monitored-names #'string<) ", "))
               ""))))

(provide 'visible-auto-revert)

;;; visible-auto-revert.el ends here
