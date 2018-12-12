;;; xfer.el --- emacs file transfer utilities
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 30, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-12-12 15:57:11 dan.harms>
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
  '((zip
     :compress-exe "zip"
     :uncompress-exe "unzip"
     :extensions ("zip")
     :compress-cmd "zip %o -r --filesync %i"
     :uncompress-cmd "unzip -jobq %i"
     :compress-versions ("zip --version" .
                         (("This is Zip \\([[:digit:].]+\\)" . t)))
     :uncompress-versions ("unzip -v" .
                           (("UnZip \\([[:digit:].]+\\) of" . t))))
    (gzip
     :compress-exe "gzip"
     :uncompress-exe "gunzip"
     :extensions ("gz")
     :compress-cmd "gzip -fk9 %i"
     :uncompress-cmd "gunzip -fk9 %i"
     :compress-versions ("gzip --version" .
                         (("Apple gzip \\([[:digit:].]+\\)" . t)
                          ("gzip \\([[:digit:].]+\\)" . "1.8")
                          ))
     :uncompress-versions ("gunzip --version" .
                           (("Apple gzip \\([[:digit:].]+\\)" . t)
                            ("gunzip (gzip) \\([[:digit:].]+\\)" . "1.8")
                            ))))
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

(defun xfer--scp (src-fullname src-host src-user src-dir src-file
                               dst-fullname dst-host dst-user dst-dir
                               dst-file)
  "Return an scp command to copy SRC-FILE in SRC-DIR on SRC-HOST as SRC-USER.
The destination will be DST-FILE in DST-DIR on DST-HOST as DST-USER.
SRC-FULLNAME and DST-FULLNAME contain the full tramp paths, if any."
  (let ((source (if src-host
                    (format "%s:%s"
                            (if (and src-user (not (string-empty-p src-user)))
                                (format "%s@%s" src-user src-host)
                              src-host)
                            (expand-file-name src-file src-dir))
                  (expand-file-name src-file src-dir)))
        (destination (if dst-host
                         (format "%s:%s"
                                 (if (and dst-user (not (string-empty-p dst-user)))
                                     (format "%s@%s" dst-user dst-host)
                                   dst-host)
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

(defun xfer--pscp (src-fullname src-host src-user src-dir src-file
                                dst-fullname dst-host dst-user
                                dst-dir dst-file)
  "Return a pscp command to copy SRC-FILE in SRC-DIR on SRC-HOST as SRC-USER.
The destination will be DST-FILE in DST-DIR on DST-HOST as DST-USER.
SRC-FULLNAME and DST-FULLNAME contain the full tramp paths, if any.
If local, host strings should be nil."
  (let ((source (if src-host
                    (format "%s:%s"
                            (if (and src-user (not (string-empty-p src-user)))
                                (format "%s@%s" src-user src-host)
                              src-host)
                            (concat src-dir src-file))
                  (expand-file-name src-file src-dir)))
        (source-home (if src-host
                         (xfer--remote-homedir-find src-fullname)
                       (getenv "HOME")))
        (destination (if dst-host
                         (format "%s:%s"
                                 (if (and dst-user (not (string-empty-p dst-user)))
                                     (format "%s@%s" dst-user dst-host)
                                   dst-host)
                                 (concat dst-dir dst-file))
                       (expand-file-name dst-file dst-dir)))
        (dest-home (if dst-host
                       (xfer--remote-homedir-find dst-fullname)
                     (getenv "HOME")))
        (tilde "~/")
        (spec "pscp -batch -p -q %s %d"))
    ;; unless paths are absolute, pscp assumes they are in the home dir
    (setq source (replace-regexp-in-string (concat source-home "/")
                                           "" source))
    (setq source (replace-regexp-in-string tilde "" source))
    (setq destination (replace-regexp-in-string (concat dest-home "/")
                                                "" destination))
    (setq destination (replace-regexp-in-string tilde "" destination))
    (format-spec spec `((?s . ,source)
                        (?d . ,destination)))))

(defun xfer--remote-homedir-find (file)
  "Return `$HOME' on remote host of FILE, a full tramp path.
Note that `getenv' always operates on the local host."
  (let ((default-directory (file-name-directory file))
        (shell-file-name "sh"))
    (string-trim (shell-command-to-string "echo $HOME"))))

(defun xfer-remote-executable-find (exe)
  "Try to find the binary associated with EXE on a remote host.
Note that `executable-find' always operates on the local host."
  (string-trim (shell-command-to-string (format "which %s" exe))))

(defun xfer-find-executable (exe &optional path)
  "Search for executable EXE given directory PATH.
If PATH is not supplied, `default-directory' is used."
  (let* ((default-directory (if path
                                (file-name-directory path)
                              default-directory))
         (func (if (file-remote-p default-directory)
                   #'xfer-remote-executable-find
                 #'executable-find)))
    (funcall func exe)))

(defun xfer-abbreviate-file-name (filename)
  "Return a shortened version of FILENAME for remote hosts."
  (let ((abbreviated-home-dir
         (format "\\`%s\\(/\\|\\'\\)"
                 (xfer--remote-homedir-find filename))))
    (abbreviate-file-name filename)))

(defun xfer--exe-version (exe regex)
  "Find version of EXE given REGEX.
EXE is a full command, including version parameter.
The first capture group should be the executable's version number."
  (let* ((str (string-trim (shell-command-to-string exe))))
    (when (string-match regex str)
      (match-string-no-properties 1 str))))

(defun xfer--test-exe-version (exe regex version &optional path)
  "Test EXE is at least VERSION according to REGEX at PATH."
  (let* ((default-directory (if path
                                (file-name-directory path)
                              default-directory))
         (curr (xfer--exe-version exe regex)))
    (and curr
         (cond ((stringp version)
                (not (version< curr version)))
               ((eq version 't) t)))))

(defun xfer--test-compression-method (src-path dst-path scheme
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
        (uncompress (plist-get (cdr scheme) :uncompress-exe))
        (version-compress (plist-get (cdr scheme) :compress-versions))
        (version-uncompress (plist-get (cdr scheme) :uncompress-versions)))
    (cond ((or (not type) (eq type 'both))
           (and
            (or (not force) (eq force method))
            (xfer-find-executable compress src-path)
            (or (not version-compress)
                (seq-find (lambda (ver)
                            (xfer--test-exe-version
                             (car version-compress)
                             (car ver) (cdr ver) src-path))
                          (cdr version-compress)))
            (xfer-find-executable uncompress dst-path)
            (or (not version-uncompress)
                (seq-find (lambda (ver)
                            (xfer--test-exe-version
                             (car version-uncompress)
                             (car ver) (cdr ver) dst-path))
                          (cdr version-uncompress)))))
          ((eq type 'compress)
           (and
            (or (not force) (eq force method))
            (xfer-find-executable compress src-path)
            (or (not version-compress)
                (seq-find (lambda (ver)
                            (xfer--test-exe-version
                             (car version-compress)
                             (car ver) (cdr ver) src-path))
                          (cdr version-compress)))))
          ((eq type 'uncompress)
           (and
            (or (not force) (eq force method))
            (member (file-name-extension src-path)
                    (plist-get (cdr scheme) :extensions))
            (xfer-find-executable uncompress dst-path)
            (or (not version-uncompress)
                (seq-find (lambda (ver)
                            (xfer--test-exe-version
                             (car version-uncompress)
                             (car ver) (cdr ver) dst-path))
                          (cdr version-uncompress))))))))

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
                             (xfer--test-compression-method
                              src dest element type force))
                           rules)))
    (when (and force (not method))      ;didn't find the override
      (setq method (seq-find (lambda (element)
                               (xfer--test-compression-method
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
          (setq msg (format "%s (result:%d)" spec code))
        (setq msg (format "%s (result:%d) %s"
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
          (setq msg (format "%s (result:%d)" spec code))
        (setq msg (format "%s (result:%d) %s"
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
;; TODO take into account file size

(defun xfer--copy-file (src-fullname src-host src-user src-dir src-file
                                     dst-fullname dst-host dst-user
                                     dst-dir dst-file scheme)
  "Copy SRC-FILE in SRC-DIR on SRC-HOST to DST-FILE in DST-DIR on DST-HOST.
SRC-FULLNAME and DST-FULLNAME contain the full tramp path, if any.
SCHEME is the method to employ, see `xfer-transfer-scheme-alist'.
Returns a cons cell (RET . MSG) where RET is non-nil on success,
or nil on failure, and MSG is either an informative message,
or an error message, respectively."
  (let* ((method (cdr scheme))
         (func (plist-get method :cmd))
         (spec (funcall func src-fullname src-host src-user src-dir
                        src-file dst-fullname dst-host dst-user
                        dst-dir dst-file))
         (cmd (split-string spec))
         code msg)
    (with-temp-buffer
      (setq code (apply #'process-file (car cmd) nil t nil
                        (cdr cmd)))
      (if (eq code 0)
          (setq msg (format "%s (result:%d)" spec code))
        (setq msg  (format "%s (result:%d) %s"
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
                (message "xfer: %s ==> %s [%s]"
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
                (message "xfer: %s ==> %s [%s]"
                         file (car result) (car scheme))
                (car result))
            (user-error "Xfer unable to uncompress %s" file)))
      (user-error "Xfer unable to find uncompression method for %s" file))))

(defun xfer-transfer-print-msg (result)
  "Print an informative message RESULT.
RESULT is a cons cell (success . message)."
  (if (car result)
      (prog1 t
        (message (cdr result)))
    (user-error "%s" (cdr result))))

;;;###autoload
(defun xfer-transfer-file-async (src dst &optional force
                                     force-compress completion)
  "Transfer SRC to DST asynchronously.
Optional FORCE is an atom, or a list of atoms that are tried in
order, specifying the transfer method by name, see
`xfer-transfer-schemes'.  Optional FORCE-COMPRESS is a symbol
that forces a compression method by name, see
`xfer-compression-schemes'.  Optional COMPLETION is a completion
handler, which defaults to `xfer-transfer-print-msg'.  If
COMPLETION is the symbol 'future then a future is returned, which
is the result of calling `async-start' without a completion
function, and can be accessed using `async-read' and `async-get'."
  (interactive "fSource file: \nGDestination: \nsMethod: \nsCompress: ")
  (let ((finish (cond ((eq completion 'future) nil)
                      ((not completion) #'xfer-transfer-print-msg)
                      (t completion))))
    (async-start
     `(lambda ()
        (setq inhibit-message t)
        ,(async-inject-variables "load-path")
        (require 'xfer)
        (xfer-transfer-file-no-msg ,src ,dst
                                   (quote ,force)
                                   (quote ,force-compress)))
     finish)))

;;;###autoload
(defun xfer-transfer-file (src dst &optional force force-compress)
  "Transfer SRC to DST.
Optional FORCE is an atom, or a list of atoms that are tried in
order, specifying the transfer method by name, see
`xfer-transfer-schemes'.  Optional FORCE-COMPRESS is a symbol
that forces a compression method by name, see
`xfer-compression-schemes', or 'none to inhibit compression."
  (interactive "fSource file: \nGDestination: \nsMethod: \nsCompress: ")
  (let ((result (xfer-transfer-file-no-msg src dst force force-compress)))
    (if (car result)
        (prog1 t
          (message (cdr result)))
      (user-error "%s" (cdr result)))))

;;;###autoload
(defun xfer-transfer-file-no-msg (src dst &optional force force-compress)
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
         source-host source-user source-dir source-file ;for remote hosts
         dest-host dest-user dest-dir dest-file         ;for remote hosts
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
                      (setq compress nil)
                      ;; TODO if compression was destructive, replenish src file
                      ))
                  ;; update tracking variables
                  (if src-remote
                      (with-parsed-tramp-file-name source var
                        (setq source-host var-host)
                        (setq source-user (substring-no-properties var-user))
                        (setq source-dir (file-name-directory var-localname))
                        (setq source-file (file-name-nondirectory var-localname)))
                    (setq source-dir (file-name-directory source))
                    (setq source-file (file-name-nondirectory source)))
                  (if dst-remote
                      (with-parsed-tramp-file-name destination var
                        (setq dest-host var-host)
                        (setq dest-user (substring-no-properties var-user))
                        (setq dest-dir (file-name-directory var-localname))
                        (setq dest-file (file-name-nondirectory var-localname)))
                    (setq dest-dir (file-name-directory destination))
                    (setq dest-file (file-name-nondirectory destination)))
                  ;; perform the transfer
                  (if (eq (car scheme) 'standard)
                      (copy-file source destination t t t t)
                    (xfer--copy-file source source-host source-user source-dir source-file
                                     destination dest-host dest-user dest-dir dest-file
                                     scheme))
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
                  (when (file-exists-p (expand-file-name dst-file dst-path))
                    (throw 'done (cons (car scheme) (car compress)))))))
            (throw 'done nil)))
    (if done
        (let* ((how (if (eq (car done) 'standard) 'std (car done)))
               (src (if src-remote
                        (concat
                         (if (and source-user
                                  (not (string-empty-p source-user))
                                  (not (string= source-user user-login-name)))
                             (concat source-user "@" source-host)
                           source-host)
                         ":"
                         (replace-regexp-in-string
                          (xfer--remote-homedir-find
                           (expand-file-name src-file src-path))
                          "~" source-dir)
                         src-file)
                      (abbreviate-file-name
                       (expand-file-name src-file src-path))))
               (dst (if dst-remote
                        (concat
                         (if (and dest-user
                                  (not (string-empty-p dest-user))
                                  (not (string= dest-user user-login-name)))
                             (concat dest-user "@" dest-host)
                           dest-host)
                         ":"
                         (replace-regexp-in-string
                          (xfer--remote-homedir-find
                           (expand-file-name dst-file dst-path))
                          "~" dest-dir)
                         dst-file)
                      (if (file-in-directory-p dst-path src-path)
                          (concat "./"
                                  (file-relative-name (expand-file-name dst-file dst-path)
                                                      src-path))
                        (abbreviate-file-name (expand-file-name dst-file dst-path))))))
          (cons t (format "xfer: %s ==> %s [%s, %.3f sec.]"
                          src dst
                          (if (cdr done)
                              (format "%s/%s" how (cdr done))
                            how)
                          (float-time (time-subtract (current-time) start)))))
      (cons nil (format "Unable to transfer %s to %s" src dst)))))

(provide 'xfer)
;;; xfer.el ends here
