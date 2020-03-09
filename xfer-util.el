;;; xfer-util.el --- Utilities for xfer
;; Copyright (C) 2019-2020  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Monday, September  9, 2019
;; Version: 1.0
;; Modified Time-stamp: <2020-03-09 09:41:45 Dan.Harms>
;; Modified by: Dan.Harms
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
(defun xfer-util-echo-env (var &optional path)
  "Return the value of the environment variable VAR at PATH.
PATH may be a remote path."
  (interactive "")
  (let ((default-directory (or path default-directory))
        (shell-file-name "sh"))
    (string-trim
     (shell-command-to-string
      (format "echo $%s" var)))))

(defun xfer-util-remote-homedir-find (file)
  "Return `$HOME' on remote host of FILE, a full tramp path.
Note that `getenv' always operates on the local host."
  (xfer-util-echo-env "HOME" (file-name-as-directory
                                 (file-name-directory file))))

(defun xfer-util-remote-executable-find (exe)
  "Try to find the binary associated with EXE on a remote host.
Note that `executable-find' always operates on the local host."
  (let ((result
         (string-trim
          (shell-command-to-string (format "which %s" exe)))))
    (and
     (not (string-match-p (format "which: no %s" exe) result))
     result)))

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

(defun xfer-util-test-exe-versions (scheme &optional path)
  "Test an executable according to SCHEME.
PATH is an optional path, otherwise `default-directory' is used.
SCHEME is an alist of the form:
'(version-cmd . ((regex . ver) (regex . ver)))
where `version-cmd' is a string used to obtain a version,
regex is a regular expression used to match the output,
in which the first capture group should be the version number,
and ver is a minimum version number to test for, or the atom t
in order to match all versions."
  (seq-find (lambda (version)
              (xfer-util-test-exe-version
               (car scheme)
               (car version)
               (cdr version)
               path))
            (cdr scheme)))

(defun xfer-util--normalize-hostname (str)
  "Attempt to normalize hostname STR for further comparisons."
  (let ((i (string-match "^\\([^.]+\\)" str)))
    (if i
        (match-string-no-properties 1 str)
      str)))

(defun xfer-util-same-hostname-p (host1 host2)
  "Test whether HOST1 and HOST2 are the same host."
  (or (eq t (compare-strings host1 nil nil host2 nil nil t))
      (eq t (compare-strings
             (xfer-util--normalize-hostname host1) nil nil
             (xfer-util--normalize-hostname host2) nil nil
             t))))

(provide 'xfer-util)
;;; xfer-util.el ends here
