;;; package-x.el --- Package extras

;; Copyright (C) 2007-2013 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;;         Daniel Hackney <dan@haxney.org>
;; Created: 10 Mar 2007
;; Keywords: tools
;; Package: package

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file currently contains parts of the package system that many
;; won't need, such as package uploading.

;; To upload to an archive, first set `package-x-archive-upload-base' to
;; some desired directory.  For testing purposes, you can specify any
;; directory you want, but if you want the archive to be accessible to
;; others via http, this is typically a directory in the /var/www tree
;; (possibly one on a remote machine, accessed via Tramp).

;; Then call M-x package-upload-file, which prompts for a file to
;; upload. Alternatively, M-x package-upload-buffer uploads the
;; current buffer, if it's visiting a package file.

;; Once a package is uploaded, users can access it via the Package
;; Menu, by adding the archive to `package-archives'.

;;; Code:

(require 'package)
(require 'xml)

(defvar gnus-article-buffer)

(defgroup package-x nil
  "Maintenance of package archives."
  :group 'package
  :version "24.4")

(defcustom package-x-archive-upload-base nil
  "The base location of the archive to which packages are uploaded.
This should be an absolute directory name.  If the archive is on
another machine, you may specify a remote name in the usual way,
e.g. \"/ssh:foo@example.com:/var/www/packages/\".
See Info node `(emacs)Remote Files'.

Unlike `package-archives', you can't specify a HTTP URL."
  :type 'directory
  :group 'package-x
  :version "24.4")

(define-obsolete-variable-alias 'package-archive-upload-base
  'package-x-archive-upload-base
  "24.4")

(defcustom package-x-update-news-on-upload nil
  "Whether uploading a package should also update NEWS and RSS feeds."
  :type 'boolean
  :group 'package-x
  :version "24.4")

(define-obsolete-variable-alias 'package-update-news-on-upload
  'package-x-update-news-on-upload
  "24.4")

(defcustom package-x-public-archive-url nil
  "Public URL of the package archive.
Appears in news and RSS entries."
  :type 'string
  :group 'package-x
  :version "24.4")

(defun package-x--make-rss-entry (title text archive-url)
  (let ((date-string (format-time-string "%a, %d %B %Y %T %z")))
    (concat "<item>\n"
            "<title>" (xml-escape-string title) "</title>\n"
            ;; FIXME: should have a link in the web page.
            "<link>" archive-url "news.html</link>\n"
            "<description>" (xml-escape-string text) "</description>\n"
            "<pubDate>" date-string "</pubDate>\n"
            "</item>\n")))

(defun package-x--make-html-entry (title text archive-url)
  (concat "<li> " (format-time-string "%B %e") " - "
          title " - " (xml-escape-string text)
          " </li>\n"))

(defun package-x--update-file (file tag text)
  "Update the package news file named FILE.
FILE should be relative to `package-x-archive-upload-base'.
TAG is a string that can be found within the file; TEXT is
inserted after its first occurrence in the file."
  (setq file (expand-file-name file package-x-archive-upload-base))
  (save-excursion
    (let ((old-buffer (find-buffer-visiting file)))
      (with-current-buffer (let ((find-file-visit-truename t))
                             (or old-buffer (find-file-noselect file)))
        (goto-char (point-min))
        (search-forward tag)
        (forward-line)
        (insert text)
        (let ((file-precious-flag t))
          (save-buffer))
        (unless old-buffer
          (kill-buffer (current-buffer)))))))

(defun package-x-add-news-item (title description)
  "Add a news item to the webpages associated with the package archive.
TITLE is the title of the news item.
DESCRIPTION is the text of the news item."
  (interactive "sTitle: \nsText: ")
  (package--update-file "elpa.rss"
                        "<description>"
                        (package--make-rss-entry title description))
  (package--update-file "news.html"
                        "New entries go here"
                        (package--make-html-entry title description)))

(define-obsolete-function-alias 'package-maint-add-news-item
  'package-x-add-news-item
  "24.4")

(defun package-x--maybe-create-upload-base ()
  "Asks the user to create `package-x-archive-upload-base' if it does not exist.
Returns whether `package-x-archive-upload-base' exists.

If `package-x-archive-upload-base' is not set, asks the user for a
value."
  (unless (stringp package-x-archive-upload-base)
    (setq package-x-archive-upload-base
          (read-directory-name
           "Base directory for package archive: ")))

  (if (and (not (file-directory-p package-x-archive-upload-base))
           (y-or-n-p (format "%s does not exist; create it? "
                             package-x-archive-upload-base)))
      (make-directory package-x-archive-upload-base t))

  ;; Return whether the directory exists
  (file-directory-p package-x-archive-upload-base))

(defun package-x-upload-buffer ()
  "Upload the current buffer as an Emacs package.
If `package-x-archive-upload-base' does not specify a valid upload
destination, prompt for one."
  (interactive)
  (unless (package-x--maybe-create-upload-base)
    (error "Archive upload directory does not exist"))

  (save-excursion
    (save-restriction
      (let* ((desc (package-desc-from-buffer))
             (commentary (package-desc-commentary desc))
             (contents (package--read-archive-file
                        (expand-file-name package--archive-contents-filename
                                          package-x-archive-upload-base))))

        ;; Update the existing package definition or add a new one
        (let* ((elt (assq (package-desc-name desc) contents))
               old-pkg)
          (if elt
              (progn
                (setq old-pkg (package-desc-from-archive-format elt))
                (if (version-list-<= (package-desc-version desc)
                                     (package-desc-version old-pkg))
                    (error "New package has the same or smaller version: %s"
                           (package-desc-version-string desc))
                  (setcdr elt (package-desc-to-archive-format desc t))))
            (push (package-desc-to-archive-format desc) contents)))

        ;; Now CONTENTS is the updated archive contents.  Upload this
        ;; and the package itself.  For now we assume ELPA is writable
        ;; via file primitives.
        (let ((print-level nil)
              (print-length nil))
          (write-region (pp-to-string
                         (cons package-archive-version contents))
                        nil
                        (expand-file-name package--archive-contents-filename
                                          package-x-archive-upload-base)))

        ;; If there is a commentary section, write it.
        (when commentary
          (write-region commentary nil
                        (expand-file-name
                         (package-desc-readme-file desc)
                         package-x-archive-upload-base)))

        (write-region (point-min) (point-max)
                      (expand-file-name (package-desc-filename desc)
                                        package-x-archive-upload-base)
                      nil nil nil 'excl)

        ;; Write a news entry.
        (if (and package-x-update-news-on-upload
                 package-x-public-archive-url)
            (package-x-add-news-item desc))))))

(defun package-x-upload-file (file)
  "Upload the Emacs Lisp package FILE to the package archive.
Interactively, prompt for FILE.  The package is considered a
single-file package if FILE ends in \".el\", and a multi-file
package if FILE ends in \".tar\".  If
`package-x-archive-upload-base' does not specify a valid upload
destination, prompt for one."
  (interactive "fPackage file name: ")
  (if (not (or (string-match "\\.tar$" file)
               (string-match "\\.el$" file)))
      (error "Unrecognized extension `%s'"
             (file-name-extension file)))

  (with-temp-buffer
    (insert-file-contents-literally file)
    (package-upload-buffer)))

(define-obsolete-function-alias 'package-upload-file 'package-x-upload-file
  "24.4")

(defun package-x-gnus-summary-upload ()
  "Upload a package contained in the current *Article* buffer.
This should be invoked from the gnus *Summary* buffer."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (package-upload-buffer)))

(define-obsolete-function-alias 'package-gnus-summary-upload
  'package-x-gnus-summary-upload
  "24.4")

(provide 'package-x)

;;; package-x.el ends here
