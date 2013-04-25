;;; package-test.el --- Tests for the Emacs package system

;; Author: Daniel Hackney <dan@haxney.org>
;; Version: 1.0

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Run this from a separate Emacs instance from your main one as it
;; messes with the package archive files. In fact, it wouldn't be a
;; bad idea to back up your whole package archive before testing!

;; Run this in a clean Emacs session using:
;;
;;     $ emacs -Q --batch -L . -l package-x-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'package-x)
(require 'ert)
(require 'cl-lib)
(let ((load-path (append (list (file-name-directory load-file-name)))))
 (require 'package-test))

(defvar package-x-test--single-archive-entry-1-3
  '(simple-single .
                 [(1 3)
                  nil "A single-file package with no dependencies" single])
  "Expected contents of the archive entry from the \"simple-single\" package.")

(cl-defmacro with-package-x-test ((&optional &key file basedir build-dir update-news) &rest body)
  "Set up temporary locations and variables for testing."
  (declare (indent 1))
  `(let* ((package-x-test-archive-upload-base (make-temp-file "pkg-archive-base-" t))
          (package-x-archive-upload-base package-x-test-archive-upload-base)
          (old-yes-no-defn (symbol-function 'yes-or-no-p))
          (old-pwd default-directory)
          package--initialized
          ,@(if build-dir (list (list 'build-dir build-dir)
                                (list 'build-tar (concat build-dir ".tar")))
              (list (cl-gensym)))
          ,@(if update-news
                '(package-x-update-news-on-upload t)
              (list (cl-gensym)))) ;; Dummy value so `let' doesn't try to bind `nil'
     (unwind-protect
         (progn
           ,(if basedir (list 'cd basedir))
           (setf (symbol-function 'yes-or-no-p) #'(lambda (&rest r) t))

           (if (boundp 'build-dir)
               (package-test-build-multifile build-dir))
           (with-temp-buffer
             ,(if file
                  (list 'insert-file-contents file))
             ,@body))
       ,(if build-dir
            (list 'package-test-cleanup-built-files build-dir))
       (when (file-directory-p package-x-test-archive-upload-base)
         (delete-directory package-x-test-archive-upload-base t))
       (setf (symbol-function 'yes-or-no-p) old-yes-no-defn)
       (cd old-pwd))))

(ert-deftest package-x-test-upload-buffer ()
  "Test creating an \"archive-contents\" file"
  (with-package-x-test (:basedir "data/package" :file "simple-single-1.3.el")
    (package-x-upload-buffer)
    (should (file-exists-p (expand-file-name package--archive-contents-filename
                                             package-x-archive-upload-base)))
    (should (file-exists-p (expand-file-name "simple-single-1.3.el"
                                             package-x-archive-upload-base)))
    (should (file-exists-p (expand-file-name "simple-single-readme.txt"
                                             package-x-archive-upload-base)))

    (let (archive-contents)
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name package--archive-contents-filename
                           package-x-archive-upload-base))
        (setq archive-contents
              (package-read-from-string
               (buffer-substring (point-min) (point-max)))))
      (should (equal archive-contents
                     (list 1 package-x-test--single-archive-entry-1-3))))))

(provide 'package-x-test)

;;; package-x-test.el ends here
