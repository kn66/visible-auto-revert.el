;;; visible-auto-revert-tests.el --- Tests for visible-auto-revert -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for ownership tracking and cleanup behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'visible-auto-revert)

(defun visible-auto-revert-test--visible-hash (&rest buffers)
  "Return a visibility hash table containing BUFFERS."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (buf buffers)
      (puthash buf t table))
    table))

(defun visible-auto-revert-test--make-file-buffer (name)
  "Create a live file-visiting buffer named NAME for testing."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (setq-local buffer-file-name (format "/tmp/%s" (buffer-name buf))))
    buf))

(defmacro visible-auto-revert-test--with-mocked-auto-revert (&rest body)
  "Run BODY with `auto-revert-mode' mocked to only toggle the mode variable."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'auto-revert-mode)
              (lambda (&optional arg)
                (setq auto-revert-mode
                      (cond
                       ((null arg) (not auto-revert-mode))
                       ((and (numberp arg) (> arg 0)) t)
                       (t nil)))
                auto-revert-mode)))
     ,@body))

(ert-deftest visible-auto-revert-preserves-external-auto-revert ()
  (let ((visible-auto-revert--monitored-buffers (make-hash-table :test 'eq))
        (visible-auto-revert--update-timer nil)
        (buf (visible-auto-revert-test--make-file-buffer " *var-external*")))
    (unwind-protect
        (visible-auto-revert-test--with-mocked-auto-revert
          (with-current-buffer buf
            (setq-local auto-revert-mode t))
          (cl-letf (((symbol-function 'visible-auto-revert--get-visible-file-buffers)
                     (lambda ()
                       (visible-auto-revert-test--visible-hash buf))))
            (visible-auto-revert--update-delayed))
          (should (eq (gethash buf visible-auto-revert--monitored-buffers) 'external))
          (cl-letf (((symbol-function 'visible-auto-revert--get-visible-file-buffers)
                     (lambda ()
                       (visible-auto-revert-test--visible-hash))))
            (visible-auto-revert--update-delayed))
          (should-not (gethash buf visible-auto-revert--monitored-buffers))
          (with-current-buffer buf
            (should auto-revert-mode)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest visible-auto-revert-preserves-auto-revert-tail-mode ()
  (let ((visible-auto-revert--monitored-buffers (make-hash-table :test 'eq))
        (visible-auto-revert--update-timer nil)
        (buf (visible-auto-revert-test--make-file-buffer " *var-tail*"))
        (auto-revert-calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'auto-revert-mode)
                   (lambda (&optional _arg)
                     (cl-incf auto-revert-calls)
                     (setq auto-revert-mode t))))
          (with-current-buffer buf
            (setq-local auto-revert-mode nil)
            (setq-local auto-revert-tail-mode t))
          (cl-letf (((symbol-function 'visible-auto-revert--get-visible-file-buffers)
                     (lambda ()
                       (visible-auto-revert-test--visible-hash buf))))
            (visible-auto-revert--update-delayed))
          (should (eq (gethash buf visible-auto-revert--monitored-buffers) 'external))
          (should (= auto-revert-calls 0))
          (cl-letf (((symbol-function 'visible-auto-revert--get-visible-file-buffers)
                     (lambda ()
                       (visible-auto-revert-test--visible-hash))))
            (visible-auto-revert--update-delayed))
          (should-not (gethash buf visible-auto-revert--monitored-buffers))
          (should (= auto-revert-calls 0))
          (with-current-buffer buf
            (should auto-revert-tail-mode)
            (should-not auto-revert-mode)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest visible-auto-revert-preserves-global-auto-revert-buffer ()
  (let ((visible-auto-revert--monitored-buffers (make-hash-table :test 'eq))
        (visible-auto-revert--update-timer nil)
        (buf (visible-auto-revert-test--make-file-buffer " *var-global*"))
        (auto-revert-calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'auto-revert-mode)
                   (lambda (&optional _arg)
                     (cl-incf auto-revert-calls)
                     (setq auto-revert-mode t))))
          (with-current-buffer buf
            (setq-local auto-revert-mode nil)
            (setq-local auto-revert--global-mode t))
          (cl-letf (((symbol-function 'visible-auto-revert--get-visible-file-buffers)
                     (lambda ()
                       (visible-auto-revert-test--visible-hash buf))))
            (visible-auto-revert--update-delayed))
          (should (eq (gethash buf visible-auto-revert--monitored-buffers) 'external))
          (should (= auto-revert-calls 0)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest visible-auto-revert-owned-buffer-toggles-with-visibility ()
  (let ((visible-auto-revert--monitored-buffers (make-hash-table :test 'eq))
        (visible-auto-revert--update-timer nil)
        (buf (visible-auto-revert-test--make-file-buffer " *var-owned*")))
    (unwind-protect
        (visible-auto-revert-test--with-mocked-auto-revert
          (with-current-buffer buf
            (setq-local auto-revert-mode nil))
          (cl-letf (((symbol-function 'visible-auto-revert--get-visible-file-buffers)
                     (lambda ()
                       (visible-auto-revert-test--visible-hash buf))))
            (visible-auto-revert--update-delayed))
          (should (eq (gethash buf visible-auto-revert--monitored-buffers) 'owned))
          (with-current-buffer buf
            (should auto-revert-mode))
          (cl-letf (((symbol-function 'visible-auto-revert--get-visible-file-buffers)
                     (lambda ()
                       (visible-auto-revert-test--visible-hash))))
            (visible-auto-revert--update-delayed))
          (should-not (gethash buf visible-auto-revert--monitored-buffers))
          (with-current-buffer buf
            (should-not auto-revert-mode))
          (cl-letf (((symbol-function 'visible-auto-revert--get-visible-file-buffers)
                     (lambda ()
                       (visible-auto-revert-test--visible-hash buf))))
            (visible-auto-revert--update-delayed))
          (should (eq (gethash buf visible-auto-revert--monitored-buffers) 'owned))
          (with-current-buffer buf
            (should auto-revert-mode)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest visible-auto-revert-mode-disable-only-stops-owned-buffers ()
  (let ((visible-auto-revert--monitored-buffers (make-hash-table :test 'eq))
        (visible-auto-revert--update-timer nil)
        (owned-buf (visible-auto-revert-test--make-file-buffer " *var-owned-cleanup*"))
        (external-buf (visible-auto-revert-test--make-file-buffer " *var-external-cleanup*")))
    (unwind-protect
        (visible-auto-revert-test--with-mocked-auto-revert
          (with-current-buffer owned-buf
            (setq-local auto-revert-mode t))
          (with-current-buffer external-buf
            (setq-local auto-revert-mode t))
          (puthash owned-buf 'owned visible-auto-revert--monitored-buffers)
          (puthash external-buf 'external visible-auto-revert--monitored-buffers)
          (visible-auto-revert-mode -1)
          (with-current-buffer owned-buf
            (should-not auto-revert-mode))
          (with-current-buffer external-buf
            (should auto-revert-mode))
          (should (= 0 (hash-table-count visible-auto-revert--monitored-buffers))))
      (when (buffer-live-p owned-buf)
        (kill-buffer owned-buf))
      (when (buffer-live-p external-buf)
        (kill-buffer external-buf)))))

(provide 'visible-auto-revert-tests)

;;; visible-auto-revert-tests.el ends here
