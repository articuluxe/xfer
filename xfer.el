;;; xfer.el --- emacs file transfer utilities
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 30, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-11-17 06:52:45 dharms>
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
(require 'async)

(defgroup xfer nil "Transfer files using emacs."
  :group 'tools
  :prefix "xfer")

;; verbose logging
(defcustom xfer-debug t
  "If non-nil, increases debug logging."
  :type 'boolean)

;; compression
(defvar xfer-compression-schemes
  '(
    (gzip
     :compress-exe "gzip"
     :uncompress-exe "gunzip"
     :extensions ("gz")
     :compress-cmd "gzip -fk9 %i"
     :uncompress-cmd "gunzip -fk9 %i"
     )
    (zip
     :compress-exe "zip"
     :uncompress-exe "unzip"
     :extensions ("zip")
     :compress-cmd "zip %o -r --filesync %i"
     :uncompress-cmd "unzip -jobq %i"
     )
    )
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
     :predicate xfer--scp-enabled
     )
    (pscp
     :local-exe "pscp"
     :cmd xfer--pscp
     :predicate xfer--scp-enabled
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
        (spec "scp -p -q %s %d"))
    (format-spec spec `((?s . ,source)
                        (?d . ,destination)))))

(defun xfer--scp-enabled (src dst)
  "Return non-nil if scp should be enabled from SRC to DST."
  (let ((src-host (file-remote-p src))
        (dst-host (file-remote-p dst)))
    (if src-host
        (if dst-host
            (not (string= src-host dst-host))
          t)
      dst-host)))

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

(defun xfer-abbreviate-file-name (filename)
  "Return a shortened version of FILENAME for remote hosts."
  (let ((abbreviated-home-dir
         (format "\\`%s\\(/\\|\\'\\)"
                 (xfer-homedir-find))))
    (abbreviate-file-name filename)))

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
If successful, returns a cons cell (FILE . MSG), where FILE is the
compressed file name, and MSG is an informative message.
If not successful, returns a cons cell (nil . MSG), where MSG
is an error message."
  (let* ((default-directory path)
         (output (concat src "." (car (plist-get
                                       (cdr method) :extensions))))
         (spec (format-spec (plist-get (cdr method) :compress-cmd)
                            `((?i . ,src)
                              (?o . ,output))))
         (cmd (split-string spec))
         code msg)
    (with-temp-buffer
      (setq code (apply #'process-file (car cmd) nil t nil
                        (cdr cmd)))
      (if (eq code 0)
          (setq msg (format "xfer: %s (result:%d)" spec code))
        (setq msg (format "xfer: %s (result:%d) %s"
                          spec code (buffer-string)))))
    (if (and (eq code 0)
             (file-exists-p output))
        (cons output msg)
      (cons nil msg))))

(defun xfer--uncompress-file (path src method &optional dst)
  "At PATH, uncompress SRC to DST using METHOD.
DST, if not supplied, defaults to SRC sans extension.
METHOD's format is a plist according to `xfer-compression-schemes'.
If successful, returns a cons cell (FILE . MSG), where FILE is the
uncompressed file name, and MSG is an informative message.
If not successful, returns a cons cell (nil . MSG), where MSG
is an error message."
  (let* ((default-directory path)
         (src-base (file-name-sans-extension src))
         (final (or dst src-base))
         (final-abs (expand-file-name final path))
         (spec (format-spec (plist-get (cdr method) :uncompress-cmd)
                            `((?i . ,src)
                              (?o . ,final))))
         (cmd (split-string spec))
         code msg tmp)
    (with-temp-buffer
      (setq code (apply #'process-file (car cmd) nil t nil
                        (cdr cmd)))
      (if (eq code 0)
          (setq msg (format "xfer: %s (result:%d)" spec code))
        (setq msg (format "xfer: %s (result:%d) %s"
                          spec code (buffer-string)))))
    (and dst
         (not (string= src-base dst))
         (eq code 0)
         (setq tmp (expand-file-name src-base path))
         (file-exists-p tmp)
         (rename-file tmp final-abs t)
         (when xfer-debug
           (message "xfer renamed %s to %s" tmp final-abs)))
    (if (and (eq code 0)
             (file-exists-p final-abs))
        (cons final-abs msg)
      (cons nil msg))))

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
             (remote-exe (plist-get method :remote-exe))
             (pred (plist-get method :predicate)))
        (and (or (not force) (eq force (car scheme)))
             (xfer-find-executable local-exe src)
             ;; we don't require remote executable be present
             (or (not remote-exe)
                 (xfer-find-executable remote-exe dst))
             (if (and pred (functionp pred))
                 (funcall pred src dst) t)))))

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
Returns a cons cell (RET . MSG) where RET is non-nil on success,
or nil on failure, and MSG is either an informative message,
or an error message, respectively."
  (let* ((method (cdr scheme))
         (func (plist-get method :cmd))
         (spec (funcall func src-fullname src-host src-dir src-file
                        dst-fullname dst-host dst-dir dst-file))
         (cmd (split-string spec))
         code msg)
    (with-temp-buffer
      (setq code (apply #'process-file (car cmd) nil t nil
                        (cdr cmd)))
      (if (eq code 0)
          (setq msg (format "xfer: %s (result:%d)" spec code))
        (setq msg  (format "xfer: %s (result:%d) %s"
                           spec code (buffer-string)))))
    (cons (eq code 0) msg)))

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
        (progn
          (setq result (xfer--compress-file
                        src-dir src-file src-file scheme))
          (if (and (car result)
                   (setq zipped (expand-file-name
                                 (car result) src-dir))
                   (file-exists-p zipped))
              (progn
                (message "xfer compressed %s to %s via %s"
                         file zipped (car scheme))
                zipped)
            (user-error "Xfer unable to compress %s" file)))
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
        (progn
          (setq result (xfer--uncompress-file path name scheme))
          (if (car result)
              (progn
                (message "xfer uncompressed %s to %s via %s"
                         file (car result) (car scheme))
                (car result))
            (user-error "Xfer unable to uncompress %s" file)))
      (user-error "Xfer unable to find uncompression method for %s" file))))

(defun xfer-transfer-file-async (src dst &optional force force-compress)
  "Transfer SRC to DST asynchronously.
Optional FORCE is an atom, or a list of atoms that are tried in
order, specifying the transfer method by name, see
`xfer-transfer-schemes'.  Optional FORCE-COMPRESS is a symbol
that forces a compression method by name, see
`xfer-compression-schemes'."
  (interactive "fSource file: \nGDestination: \nsMethod: \nsCompress: ")
  (let ((start (current-time))
        msg)
    (async-start
     `(lambda ()
        (setq inhibit-message t)
        ,(async-inject-variables "load-path")
        (require 'xfer)
        (xfer--transfer-file ,src ,dst ,force ,force-compress))
     `(lambda (result)
        (if (car result)
            (prog1 t
              (message (cdr result)))
          (user-error "%s" (cdr result)))))))

(defun xfer-transfer-file (src dst &optional force force-compress)
  "Transfer SRC to DST.
Optional FORCE is an atom, or a list of atoms that are tried in
order, specifying the transfer method by name, see
`xfer-transfer-schemes'.  Optional FORCE-COMPRESS is a symbol
that forces a compression method by name, see
`xfer-compression-schemes', or 'none to inhibit compression."
  (interactive "fSource file: \nGDestination: \nsMethod: \nsCompress: ")
  (let ((result (xfer--transfer-file src dst force force-compress)))
    (if (car result)
        (prog1 t
          (message (cdr result)))
      (user-error "%s" (cdr result)))))

(defun xfer--transfer-file (src dst &optional force force-compress)
  "Transfer SRC to DST.
Optional FORCE is an atom, or a list of atoms that are tried in
order, specifying the transfer method by name, see
`xfer-transfer-schemes'.  Optional FORCE-COMPRESS is a symbol
that forces a compression method by name, see
`xfer-compression-schemes', or 'none to inhibit compression."
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
                (let ((compress (and (not (eq force-compress 'none))
                                     (xfer--should-compress src-file src-remote
                                                            dst-remote scheme)
                                     (xfer--find-compression-method
                                      xfer-compression-schemes src-path dst-path
                                      'both force-compress)))
                      (source (expand-file-name src-file src-path))
                      (destination (expand-file-name dst-file dst-path))
                      source-host source-dir source-file
                      dest-host dest-dir dest-file
                      result cmp-file)
                  (when compress
                    (setq result (xfer--compress-file src-path src-file
                                                      dst-file compress))
                    (if (setq cmp-file (car result))
                        (progn
                          (setq source (expand-file-name cmp-file src-path))
                          (setq destination (expand-file-name cmp-file dst-path)))
                      (when xfer-debug
                        (message "xfer: %s compression failed for %s: %s"
                                 (car compress) source (cdr result)))
                      ;; but carry on
                      (setq compress nil)))
                  (if (eq (car scheme) 'standard)
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
                    (setq result (xfer--uncompress-file dst-path cmp-file compress
                                                        dst-file))
                    (if (car result)
                        (progn
                          (unless (string= source src)
                            (delete-file source))
                          (unless (string= destination dst)
                            (delete-file destination)))
                      (when xfer-debug
                        (message "xfer %s error: %s" (car compress) (cdr result)))))
                  (if (file-exists-p dst)
                      (throw 'done (car scheme))))))
            (throw 'done nil)))
    (if done
        (cons t (format "xfer transferred %s to %s (%s) in %.3f sec." src dst done
                        (float-time (time-subtract (current-time) start))))
      (cons nil (format "Unable to transfer %s to %s" src dst)))))

(provide 'xfer)
;;; xfer.el ends here
