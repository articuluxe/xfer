;;; xfer-util.el --- Utilities for xfer
;; Copyright (C) 2019  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Monday, September  9, 2019
;; Version: 1.0
;; Modified Time-stamp: <2019-09-09 16:00:04 dan.harms>
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
;; Utilities useful to transfer files.
;;

;;; Code:
(defun xfer-util-remote-homedir-find (file)
  "Return `$HOME' on remote host of FILE, a full tramp path.
Note that `getenv' always operates on the local host."
  (let ((default-directory (file-name-directory file))
        (shell-file-name "sh"))
    (string-trim (shell-command-to-string "echo $HOME"))))

(defun xfer-util-remote-executable-find (exe)
  "Try to find the binary associated with EXE on a remote host.
Note that `executable-find' always operates on the local host."
  (string-trim (shell-command-to-string (format "which %s" exe))))

(defun xfer-util-find-executable (exe &optional path)
  "Search for executable EXE given directory PATH.
If PATH is not supplied, `default-directory' is used."
  (let* ((default-directory (if path
                                (file-name-directory path)
                              default-directory))
         (func (if (file-remote-p default-directory)
                   #'xfer-util-remote-executable-find
                 #'executable-find)))
    (funcall func exe)))

(defun xfer-util-abbreviate-file-name (filename)
  "Return a shortened version of FILENAME for remote hosts."
  (let ((abbreviated-home-dir
         (format "\\`%s\\(/\\|\\'\\)"
                 (xfer-util-remote-homedir-find filename))))
    (abbreviate-file-name filename)))

(defun xfer-util-exe-version (exe regex)
  "Find version of EXE given REGEX.
EXE is a full command, including version parameter.
The first capture group should be the executable's version number."
  (let* ((str (string-trim (shell-command-to-string exe))))
    (when (string-match regex str)
      (match-string-no-properties 1 str))))

(defun xfer-util-test-exe-version (exe regex version &optional path)
  "Test EXE is at least VERSION according to REGEX at PATH."
  (let* ((default-directory (if path
                                (file-name-directory path)
                              default-directory))
         (curr (xfer-util-exe-version exe regex)))
    (and curr
         (cond ((stringp version)
                (not (version< curr version)))
               ((eq version 't) t)))))

(provide 'xfer-util)
;;; xfer-util.el ends here
