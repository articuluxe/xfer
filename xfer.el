;;; xfer.el --- emacs file transfer utilities
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 30, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-11-09 16:38:36 dharms>
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
(require 'tramp)
(require 'format-spec)

;; compression
(defvar xfer-compression-schemes
  '((zip
     :compress-exe "zip"
     :uncompress-exe "unzip"
     :extensions ("zip")
     :compress-cmd "zip %o -r --filesync %i"
     :uncompress-cmd "unzip -p %i > %o"
     )
    (gzip
     :compress-exe "gzip"
     :uncompress-exe "gunzip"
     :extensions ("gz")
     :compress-cmd "gzip -c9 %i > %o"
     :uncompress-cmd "gunzip -c9 %i > %o"
     ))
  "Compression scheme definitions.")

(defvar xfer-compression-extensions
  '("zip" "gz" "gzip" "rar")
  "Extensions for already-compressed files.")

;; transfer
(defvar xfer-transfer-scheme-alist
  '((scp
     :local-exe "scp"
     :remote-exe "scp"
     :cmd xfer--scp
     )
    (pscp
     :local-exe "pscp"
     :cmd xfer--pscp
     )
    (standard))
  "Transfer scheme definitions.")

(defvar xfer-transfer-schemes '(scp standard)
  "List of transfer schemes to try in order.")

(if (eq (window-system) 'w32)
    (add-to-list 'xfer-transfer-schemes 'pscp))

(defun xfer--scp (src-fullname src-host src-dir src-file
                               dst-fullname dst-host dst-dir
                               dst-file)
  "Return an scp command to copy SRC-FILE in SRC-DIR on SRC-HOST.
The destination will be DST-FILE in DST-DIR on DST-HOST.
SRC-FULLNAME and DST-FULLNAME contain the full tramp paths, if any."
  (let ((source (if src-host
                    (format "%s:%s" src-host
                            (expand-file-name src-file src-dir))
                  (expand-file-name src-file src-dir)))
        (destination (if dst-host
                         (format "%s:%s" dst-host
                                 (expand-file-name dst-file dst-dir))
                       (expand-file-name dst-file dst-dir)))
        (spec "scp %s %d"))
    (format-spec spec `((?s . ,source)
                        (?d . ,destination)))))


(defun xfer--pscp (src-fullname src-host src-dir src-file
                                dst-fullname dst-host dst-dir
                                dst-file)
  "Return a pscp command to copy SRC-FILE in SRC-DIR on SRC-HOST.
The destination will be DST-FILE in DST-DIR on DST-HOST.
SRC-FULLNAME and DST-FULLNAME contain the full tramp paths, if any.
If local, host strings should be nil."
  (let ((source (if src-host
                    (format "%s:%s" src-host
                            (expand-file-name src-file src-dir))
                  (expand-file-name src-file src-dir)))
        (source-home (if src-host
                         (let ((default-directory
                                 (file-name-directory src-fullname)))
                           (xfer-homedir-find))
                       (xfer-homedir-find)))
        (destination (if dst-host
                         (format "%s:%s" dst-host
                                 (expand-file-name dst-file dst-dir))
                       (expand-file-name dst-file dst-dir)))
        (dest-home (if dst-host
                       (let ((default-directory
                               (file-name-directory dst-fullname)))
                         (xfer-homedir-find))
                     (xfer-homedir-find)))
        (tilde "~/")
        (spec "pscp -batch -p -q %s %d"))
    ;; unless paths are absolute, pscp assumes they are in the home dir
    (setq source (replace-regexp-in-string tilde "" source))
    (setq source (replace-regexp-in-string (concat source-home "/")
                                           "" source))
    (setq destination (replace-regexp-in-string tilde "" destination))
    (setq destination (replace-regexp-in-string (concat dest-home "/")
                                                "" destination))
    (format-spec spec `((?s . ,source)
                        (?d . ,destination)))))

(defun xfer-homedir-find ()
  "Find the value of the $HOME environment variable.
Note that `getenv' always operates on the local host."
  (or (string-trim
       (shell-command-to-string
        (format "echo $HOME")))
      (getenv "HOME")))

(defun xfer-remote-executable-find (exe)
  "Try to find the binary associated with EXE on a remote host.
Note that `executable-find' always operates on the local host."
  (string-trim (shell-command-to-string (format "which %s" exe))))

(defun xfer-find-executable (exe &optional path)
  "Search for executable EXE given directory PATH.
If PATH is not supplied, `default-directory' is used."
  (let* ((default-directory (or (file-name-directory path)
                                default-directory))
         (func (if (file-remote-p default-directory)
                   #'xfer-remote-executable-find
                 #'executable-find)))
    (funcall func exe)))

(defun xfer--test-compression-methods (src-path dst-path scheme
                                                &optional type force)
  "Test SRC-PATH and DST-PATH for compression method SCHEME.
SRC-PATH is minimally the directory of the file in question, but
may also be a compressed filename in case that type is
'uncompress.  TYPE is a symbol in (compress, uncompress, both)
telling which methods to search for.  SCHEME is a plist, see
`xfer-compression-schemes'.  Optional FORCE is a symbol that
specifies a compression method by name."
  (let ((method (car scheme))
        (compress (plist-get (cdr scheme) :compress-exe))
        (uncompress (plist-get (cdr scheme) :uncompress-exe)))
    (cond ((or (not type) (eq type 'both))
           (and
            (or (not force) (eq force method))
            (xfer-find-executable compress src-path)
            (xfer-find-executable uncompress dst-path)))
          ((eq type 'compress)
           (and
            (or (not force) (eq force method))
            (xfer-find-executable compress src-path)))
          ((eq type 'uncompress)
           (and
            (or (not force) (eq force method))
            (xfer-find-executable uncompress dst-path)
            (member (file-name-extension src-path)
                    (plist-get (cdr scheme) :extensions)))))))

(defun xfer--find-compression-method (rules src &optional dest type force)
  "Return a valid compression method among RULES to use for SRC and DEST.
TYPE is a symbol in (compress, uncompress, both) telling which
methods to search for.  If DEST is not supplied, it is assumed to
be the same as SRC.  Optional FORCE is a symbol that specifies a
compression method by name.  If FORCE is not found, a fallback is
searched for."
  (let* ((dest (or dest src))
         (type (or type 'both))
         (method (seq-find (lambda (element)
                             (xfer--test-compression-methods
                              src dest element type force))
                           rules)))
    (when (and force (not method))      ;didn't find the override
      (setq method (seq-find (lambda (element)
                               (xfer--test-compression-methods
                                src dest element type))
                             rules)))
    method))

(defun xfer--compress-file (path src dst method)
  "At PATH, compress SRC into DST using METHOD.
METHOD's format is a plist according to `xfer-compression-schemes'.
If successful, returns the resultant compressed file name."
  (let* ((default-directory path)
         (output (concat dst "." (car (plist-get
                                       (cdr method) :extensions))))
         (cmd (format-spec (plist-get (cdr method) :compress-cmd)
                           `((?i . ,src)
                             (?o . ,output))))
         code)
    (setq code (shell-command cmd))
    (message "xfer %s: %s (result:%d)" (car method) cmd code)
    (and (eq code 0)
         (file-exists-p output)
         output)))

(defun xfer--uncompress-file (path src method &optional dst)
  "At PATH, uncompress SRC to DST using METHOD.
DST, if not supplied, defaults to SRC sans extension.
METHOD's format is a plist according to `xfer-compression-schemes'."
  (let* ((default-directory path)
         (dst (or dst
                  (file-name-sans-extension src)))
         (cmd (format-spec (plist-get (cdr method) :uncompress-cmd)
                           `((?i . ,src)
                             (?o . ,dst))))
         code)
    (setq code (shell-command cmd))
    (message "xfer %s: %s (result:%d)" (car method) cmd code)
    (expand-file-name dst path)))

(defun xfer-file-compressed-p (file)
  "Return non-nil if FILE is compressed."
  (let ((ext (file-name-extension file)))
    (member ext xfer-compression-extensions)))

(defun xfer--test-scheme (src dst scheme &optional force)
  "Test paths SRC and DST for transfer method SCHEME.
SCHEME's format is according to `xfer-transfer-scheme-alist'.
Optional FORCE is a symbol that specifies a preferred scheme by
name."
  (or (eq (car scheme) 'standard)       ;standard primitives always work
      (let* ((method (cdr scheme))
             (local-exe (plist-get method :local-exe))
             (remote-exe (plist-get method :remote-exe)))
        (and (or (not force) (eq force (car scheme)))
             (xfer-find-executable local-exe src)
             ;; we don't require remote executable be present
             (or (not remote-exe)
                 (xfer-find-executable remote-exe dst))))))

(defun xfer--find-scheme (src dst schemes &optional force)
  "Return a valid transfer method for paths SRC to DST.
SCHEMES is an alist of transfer schemes, see `xfer-transfer-scheme-alist'.
Optional FORCE is a symbol that forces a scheme by name."
  (let ((method (seq-find (lambda (elt)
                            (xfer--test-scheme src dst elt force))
                          schemes)))
    ;; unlike compression schemes (which fallback if needed), for transfer
    ;; schemes if an override is provided but not found, we return nil
    method))

(defun xfer--should-compress (file src dst scheme)
  "Return non-nil if FILE at SRC should be compressed before copying to DST.
SRC and DST are the remote prefixes, or nil if paths aren't
remote.  SCHEME is the transfer scheme, see
`xfer-transfer-scheme-alist', which may have an opinion."
  (and (not (xfer-file-compressed-p file))
       ;; never compress on same host
       (if src (if dst (not (string= src dst)) t) dst)))

(defun xfer--copy-file (src-fullname src-host src-dir src-file
                                     dst-fullname dst-host
                                     dst-dir dst-file scheme)
  "Copy SRC-FILE in SRC-DIR on SRC-HOST to DST-FILE in DST-DIR on DST-HOST.
SRC-FULLNAME and DST-FULLNAME contain the full tramp path, if any.
SCHEME is the method to employ, see `xfer-transfer-scheme-alist'.
Returns non-nil if successful."
  (let* ((method (cdr scheme))
         (func (plist-get method :cmd))
         (cmd (funcall func src-fullname src-host src-dir src-file
                       dst-fullname dst-host dst-dir dst-file))
         (code (shell-command cmd)))
    (message "xfer: %s (result:%d)" cmd code)
    (eq code 0)))

(defun xfer-compress-file (file &optional dest force)
  "Compress FILE.
DEST, if supplied, specifies the intended destination path; this
function makes a best effort to see that the compression scheme
used has a corresponding uncompression scheme at that path.  If
not supplied, this defaults to the same path as FILE.  Optional
FORCE is a symbol that forces a compression scheme by name, see
`xfer-transfer-schemes'."
  (interactive "fFile: \nsMethod: ")
  (let* ((src-dir (file-name-directory file))
         (src-file (file-name-nondirectory file))
         (scheme (xfer--find-compression-method
                  xfer-compression-schemes
                  src-dir
                  (or dest src-dir)
                  (if dest 'both 'compress)
                  force))
         result zipped)
    (when (xfer-file-compressed-p file)
      (user-error "File '%s' already compressed" file))
    (if scheme
        (if (and (setq result (xfer--compress-file
                               src-dir src-file src-file scheme))
                 (setq zipped (expand-file-name result src-dir))
                 (file-exists-p zipped)
                 (prog1 t
                   (message "xfer compressed %s to %s via %s"
                            file zipped (car scheme))))
            zipped
          (user-error "Xfer unable to compress %s" file))
      (user-error "Xfer unable to find compression method for %s" file))))

(defun xfer-uncompress-file (file)
  "Uncompress FILE.
The uncompression scheme will be chosen based on extension."
  (interactive "fFile: \nsMethod: ")
  (let* ((path (file-name-directory file))
         (name (file-name-nondirectory file))
         (scheme (xfer--find-compression-method
                  xfer-compression-schemes
                  file                  ;use file here for extension
                  path
                  'uncompress))
         result)
    (unless (xfer-file-compressed-p file)
      (user-error "File '%s' not compressed" file))
    (if scheme
        (if (setq result (xfer--uncompress-file path name scheme))
            (progn
              (message "xfer uncompressed %s to %s via %s"
                       file result (car scheme))
              result)
          (user-error "Xfer unable to uncompress %s" file))
      (user-error "Xfer unable to find uncompression method for %s" file))))

(defun xfer-transfer-file (src dst &optional force force-compress)
  "Transfer SRC to DST.
Optional FORCE is an atom, or a list of atoms that are tried in
order, specifying the transfer method by name, see
`xfer-transfer-schemes'.  Optional FORCE-COMPRESS is a symbol
that forces a compression method by name, see
`xfer-compression-schemes'."
  (interactive "fSource file: \nGDestination: \nsMethod: \nsCompress: ")
  (let* ((src-path (file-name-directory src))
         (src-file (file-name-nondirectory src))
         (src-remote (file-remote-p src))
         (dst-path (file-name-directory dst))
         (dst-file (file-name-nondirectory dst))
         (dst-remote (file-remote-p dst))
         (start (current-time))
         (methods (cond ((not force)
                         xfer-transfer-schemes)
                        ((listp force)
                         force)
                        (t (list force))))
         scheme done)
    (unless (memq 'standard methods)
      (setq methods (append methods (list 'standard))))
    (unless (file-exists-p src)
      (user-error "File '%s' does not exist" src))
    (when (string-empty-p dst-file)
      (setq dst-file src-file))
    (make-directory dst-path t)
    (setq done
          (catch 'done
            (dolist (method methods)
              (when (setq scheme (xfer--find-scheme src-path dst-path
                                                    xfer-transfer-scheme-alist
                                                    method))
                (let ((compress (and (xfer--should-compress src-file src-path
                                                            dst-path scheme)
                                     (xfer--find-compression-method
                                      xfer-compression-schemes src-path dst-path
                                      'both force-compress)))
                      (source (expand-file-name src-file src-path))
                      (destination (expand-file-name dst-file dst-path))
                      source-host source-dir source-file
                      dest-host dest-dir dest-file
                      cmp-file)
                  (when compress
                    (if (setq cmp-file (xfer--compress-file src-path src-file
                                                            dst-file compress))
                        (progn
                          (setq source (expand-file-name cmp-file src-path))
                          (setq destination (expand-file-name cmp-file dst-path)))
                      (message "xfer: %s compression failed for %s"
                               (car compress) source)
                      ;; but carry on
                      (setq compress nil)))
                  (if (eq method 'standard)
                      (copy-file source destination t t t t)
                    (if src-remote
                        (with-parsed-tramp-file-name source var
                          (setq source-host var-host)
                          (setq source-dir (file-name-directory var-localname))
                          (setq source-file (file-name-nondirectory var-localname)))
                      (setq source-dir (file-name-directory source))
                      (setq source-file (file-name-nondirectory source)))
                    (if dst-remote
                        (with-parsed-tramp-file-name destination var
                          (setq dest-host var-host)
                          (setq dest-dir (file-name-directory var-localname))
                          (setq dest-file (file-name-nondirectory var-localname)))
                      (setq dest-dir (file-name-directory destination))
                      (setq dest-file (file-name-nondirectory destination)))
                    (xfer--copy-file source source-host source-dir source-file
                                     destination dest-host dest-dir dest-file scheme))
                  (when compress
                    (xfer--uncompress-file dst-path cmp-file compress dst-file)
                    (unless (string= source src)
                      (delete-file source))
                    (unless (string= destination dst)
                      (delete-file destination)))
                  (if (file-exists-p dst)
                      (throw 'done (car scheme))))))
            (throw 'done nil)))
    (if done
        (message "xfer transferred %s to %s (%s) in %.3f sec." src dst done
                 (float-time (time-subtract (current-time) start)))
      (user-error "Unable to transfer %s to %s" src dst))))

(provide 'xfer)
;;; xfer.el ends here
