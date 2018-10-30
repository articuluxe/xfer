#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_xfer.el --- test xfer utilities
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 30, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-10-30 09:01:45 dharms>
;; Modified by: Dan Harms
;; Keywords: tools

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

(ert-deftest xfer-test-is-compressed-p ()
  (should (xfer-file-compressed-p "test.zip"))
  (should (xfer-file-compressed-p "test.gz"))
  (should (xfer-file-compressed-p "test.rar"))
  (should (not (xfer-file-compressed-p "test")))
  )

(ert-run-tests-batch-and-exit (car argv))
;;; test_xfer.el ends here
