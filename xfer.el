;;; xfer.el --- emacs file transfer utilities
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 30, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-10-30 21:39:56 dharms>
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
;; File transfer utilities.
;;

;;; Code:
(require 'subr-x)
(require 'seq)
(require 'format-spec)

(defvar xfer-compression-schemes
  '((zip
     :compress-exe "zip"
     :uncompress-exe "unzip"
     :transform (lambda (name)
                  (concat name ".zip"))
     :compress-cmd "zip %o -r --filesync %i"
     :uncompress-cmd "unzip -p %i > %o"
     )
    (gzip
     :compress-exe "gzip"
     :uncompress-exe "gunzip"
     :transform (lambda (name)
                  (concat name ".gz"))
     :compress-cmd "gzip -c9 %i > %o"
     :uncompress-cmd "gunzip -c9 %i > %o"
     ))
  "Compression scheme definitions.")

(defvar xfer-compression-extensions
  '("zip" "gz" "gzip" "rar")
  "Extensions for already-compressed files.")

(defun xfer-remote-executable-find (exe)
  "Try to find the binary associated with EXE on a remote host.
Note that `executable-find' operates on the local host."
  (string-trim (shell-command-to-string (format "which %s" exe))))

(defun xfer-find-executable (exe &optional path)
  "Search for executable EXE given directory PATH.
If PATH is not supplied, `default-directory' is used."
  (let* ((default-directory (or path default-directory))
         (func (if (file-remote-p default-directory)
                   #'xfer-remote-executable-find
                 #'executable-find)))
    (funcall func exe)))

(defun xfer--test-compression-method (src-path dst-path scheme
                                               &optional force)
  "Test SRC-PATH and DST-PATH for compression method SCHEME.
SCHEME is a plist, see each element of `xfer-compression-schemes'.
Optional FORCE specifies a compression method."
  (let ((method (car scheme))
        (compress (plist-get (cdr scheme) :compress-exe))
        (uncompress (plist-get (cdr scheme) :uncompress-exe)))
    (and (xfer-find-executable compress src-path)
         (xfer-find-executable uncompress dst-path)
         (or (not force) (eq force method)))))

(defun xfer--find-compression-method (src dest rules
                                          &optional force)
  "Return a valid compression method among RULES to use for SRC and DEST.
Optional FORCE specifies a compression method."
  (let ((method (seq-find (lambda (element)
                            (xfer--test-compression-method
                             src dest element force))
                          rules)))
    (when (and force (not method))      ;didn't find the override
      (setq method (seq-find (lambda (element)
                               (xfer--test-compression-method
                                src dest element))
                             rules)))
    method))

(defun xfer-transfer-compress-file (path src dst method)
  "At PATH, compress SRC into DST using METHOD.
METHOD's format is a plist according to `xfer-compression-schemes'."
  (let* ((default-directory path)
         (output (funcall (plist-get method :transform) dst))
         (cmd (format-spec (plist-get method :compress-cmd)
                           `((?\i . ,src)
                             (?\o . ,output))))
         return)
    (setq return (shell-command cmd))
    ;; (when proviso-transfer-debug (message "proviso-transfer: %s (result:%d)"
    ;;                                       cmd return))
    output))

(defun xfer-uncompress-file (path src dst method)
  "At PATH, uncompress SRC to DST using METHOD.
METHOD's format is a plist according to `xfer-compression-schemes'."
  (let ((default-directory path)
        (cmd (format-spec (plist-get method :uncompress-cmd)
                          `((?\i . ,src)
                            (?\o . ,dst))))
        return)
    (setq return (shell-command cmd))
    ;; (when proviso-transfer-debug (message "proviso-transfer: %s (result:%d)"
    ;;                                       cmd return))
    (delete-file src)))

(defun xfer-file-compressed-p (file)
  "Return non-nil if FILE is compressed."
  (let ((ext (file-name-extension file)))
    (member ext xfer-compression-extensions)))

(provide 'xfer)
;;; xfer.el ends here
