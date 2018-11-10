#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_xfer.el --- test xfer utilities
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 30, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-11-10 11:35:30 dharms>
;; Modified by: Dan Harms
;; Keywords: tools
;; URL: https://github.com/articuluxe/xfer.git
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Test xfer file transfer utilities.
;;

;;; Code:
(load-file "test/xfer-test-common.el")
(require 'xfer)

(ert-deftest xfer-test-is-compressed-p ()
  (should (xfer-file-compressed-p "test.zip"))
  (should (xfer-file-compressed-p "test.gz"))
  (should (xfer-file-compressed-p "test.rar"))
  (should (not (xfer-file-compressed-p "test")))
  )

(ert-deftest xfer-test-compression ()
  (let* ((base (file-name-directory load-file-name))
         (stage (concat base "stage/"))
         (file "test_xfer.el")
         (src (concat base file))
         result)
    (delete-directory stage t)
    (make-directory stage t)
    (copy-file src stage)
    (setq result
          (xfer-compress-file (concat stage file) nil 'zip))
    (should result)
    (should (file-exists-p result))
    (should (string= (file-name-extension result) "zip"))
    (should-error
     (xfer-compress-file result nil 'zip)
     :type 'user-error)
    (setq result (xfer-uncompress-file result))
    (should (file-exists-p result))
    (should (string= (file-name-nondirectory result)
                     file))
    (delete-directory stage t)
    )
  (let* ((base (file-name-directory load-file-name))
         (stage (concat base "stage/"))
         (file "test_xfer.el")
         (src (concat base file))
         result)
    (delete-directory stage t)
    (make-directory stage t)
    (copy-file src stage)
    (setq result
          (xfer-compress-file (concat stage file) nil 'gzip))
    (should-error
     (xfer-compress-file result nil 'gzip)
     :type 'user-error)
    (should result)
    (should (file-exists-p result))
    (should (string= (file-name-extension result) "gz"))
    (setq result (xfer-uncompress-file result))
    (should (file-exists-p result))
    (should (string= (file-name-nondirectory result)
                     file))
    (delete-directory stage t)
    )
  )

(ert-deftest xfer-test-transfer-no-compression ()
  (let ((base (file-name-directory load-file-name)))
    (cl-letf (((symbol-function 'xfer--should-compress)
               (lambda (_1 _2 _3 _4)
                 nil)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst 'standard)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst '(standard))
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst '(standard scp))
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst 'scp)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst '(scp))
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst '(scp standard))
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst))
        (should (file-regular-p (concat dst "test_xfer.el"))))
      ;nonexistent source
      (let ((src (concat base "nonexistent"))
            (dst (concat base "stage/")))
        (should-error
         (xfer-transfer-file src dst)
         :type 'user-error)))
    (delete-directory (concat base "stage") t)))


(ert-deftest xfer-test-transfer-with-compression ()
  (let ((base (file-name-directory load-file-name)))
    (cl-letf (((symbol-function 'xfer--should-compress)
               (lambda (_1 _2 _3 _4)
                 t)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst 'standard)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst 'standard 'zip)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst 'standard 'gzip)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst 'scp 'zip)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/copy.el")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst 'scp 'gzip)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst)))
      (let ((src (concat base "test_xfer.el"))
            (dst (concat base "stage/")))
        (delete-directory (concat base "stage") t)
        (xfer-transfer-file src dst)
        (should (file-directory-p (concat base "stage")))
        (should (file-exists-p dst))
        (should (file-regular-p (concat dst "test_xfer.el"))))
      (let ((src (concat base "nonexistent"))
            (dst (concat base "stage/")))
        (should-error
         (xfer-transfer-file src dst)
         :type 'user-error)))
    (delete-directory (concat base "stage") t)))

(ert-run-tests-batch-and-exit (car argv))
;;; test_xfer.el ends here
