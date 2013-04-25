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
;; messes with the installed packages. In fact, you should probably
;; back up your `package-user-dir' just in case!

;; Run this in a clean Emacs session using:
;;
;;     $ emacs -Q --batch -L . -l package-test.el -l ert -f ert-run-tests-batch-and-exit

;;; Code:

(require 'package)
(require 'ert)
(require 'cl-lib)

(defvar package-test-user-dir nil
  "Directory to use for installing packages during testing.")

(defvar package-test-file-dir (file-name-directory load-file-name)
  "Directory of the actual \"package-test.el\" file.")

(defvar simple-single-desc
  `[cl-struct-package-desc simple-single (1 3)
                           "A single-file package with no dependencies"
                           nil single nil
                           ,(concat
                             "This package provides a minor mode to frobnicate "
                             "and/or bifurcate\nany flanges you desire. "
                             "To activate it, type \"C-M-r M-3 butterfly\"\n"
                             "and all your dreams will come true.\n")]
  "Expected `package-desc' parsed from simple-single-1.3.el.")

(defvar simple-single-desc-1-4
  `[cl-struct-package-desc simple-single (1 4)
                           "A single-file package with no dependencies"
                           nil single nil
                           ,(concat
                             "This package provides a minor mode to frobnicate "
                             "and/or bifurcate\nany flanges you desire. "
                             "To activate it, type \"C-M-r M-3 butterfly\"\n"
                             "and all your dreams will come true.\n"
                             "\nThis is a new, updated version.\n")]
  "Expected `package-desc' parsed from simple-single-1.4.el.")

(defvar simple-depend-desc
  [cl-struct-package-desc simple-depend (1 0)
                          "A single-file package with a dependency."
                          ((simple-single (1 3))) single nil
                          "Depends on another package.\n"]
  "Expected `package-desc' parsed from simple-depend-1.0.el.")

(defvar multi-file-desc
  [cl-struct-package-desc multi-file (0 2 3)
                          "Example of a multi-file tar package"
                          nil tar nil
                          "This is a bare-bones readme file for the multi-file package.\n"]
  "Expected `package-desc' from \"multi-file-0.2.3.tar\".")

(defvar new-pkg-desc [cl-struct-package-desc new-pkg (1 0)
                                             "A package only seen after \"updating\" archive-contents"
                                             nil single nil]
  "Expected `package-desc' parsed from new-pkg-1.0.el.")

(defvar package-test-data-dir (expand-file-name "data/package" package-test-file-dir)
  "Base directory of package test files.")

(defvar package-test-fake-contents-file
  (expand-file-name package--archive-contents-filename package-test-data-dir)
  "Path to a static copy of \"archive-contents\".")

(defvar package-test-built-file-suffixes '(".tar" "/dir" "/*.info")
  "Remove these files when cleaning up a built package.")

(cl-defmacro with-package-test ((&optional &key file
                                           basedir
                                           build-dir
                                           install
                                           update-news
                                           upload-base)
                                &rest body)
  "Set up temporary locations and variables for testing."
  (declare (indent 1))
  `(let* ((package-test-user-dir (make-temp-file "pkg-test-user-dir-" t))
          (package-user-dir package-test-user-dir)
          (package-archives `(("gnu" . ,package-test-data-dir)))
          (old-yes-no-defn (symbol-function 'yes-or-no-p))
          (old-pwd default-directory)
          package--initialized
          ,@(if update-news
                '(package-x-update-news-on-upload t)
              (list (cl-gensym)))
          ,@(if upload-base
                '((package-x-test-archive-upload-base (make-temp-file "pkg-archive-base-" t))
                  (package-x-archive-upload-base package-x-test-archive-upload-base))
              (list (cl-gensym)))
          ,@(if build-dir `((build-dir ,build-dir)
                            (build-tar (concat build-dir ".tar")))
              (list (cl-gensym)))) ;; Dummy value so `let' doesn't try to bind `nil'
     (unwind-protect
         (progn
           ,(if basedir `(cd ,basedir))
           (setf (symbol-function 'yes-or-no-p) #'(lambda (&rest r) t))
           (unless (file-directory-p package-user-dir)
             (mkdir package-user-dir))
           (if (boundp 'build-dir)
               (package-test-build-multifile build-dir))
           ,@(when install
               `((package-refresh-contents)
                 (mapc 'package-install ,install)))
           (with-temp-buffer
             ,(if file
                  `(insert-file-contents ,file))
             ,@body))
       ,(if build-dir
            `(package-test-cleanup-built-files ,build-dir))

       (when (file-directory-p package-test-user-dir)
         (delete-directory package-test-user-dir t))

       (when (and (boundp 'package-x-test-archive-upload-base)
                  (file-directory-p package-x-test-archive-upload-base))
         (delete-directory package-x-test-archive-upload-base t))
       (setf (symbol-function 'yes-or-no-p) old-yes-no-defn)
       (cd old-pwd))))

(defun package-test-install-texinfo (file)
  "Install from texinfo FILE.

FILE should be a .texinfo file relative to the current
`default-directory'"
  (require 'info)
  (let* ((full-file (expand-file-name file))
         (info-file (replace-regexp-in-string "\\.texi\\'" ".info" full-file))
         (old-info-defn (symbol-function 'Info-revert-find-node)))
    (require 'info)
    (setf (symbol-function 'Info-revert-find-node) #'ignore)
    (with-current-buffer (find-file-literally full-file)
      (unwind-protect
          (progn
            (require 'makeinfo)
            (makeinfo-buffer)
            ;; Give `makeinfo-buffer' a chance to finish
            (while compilation-in-progress
              (sit-for 0.1))
            (call-process "ginstall-info" nil nil nil
                          (format "--info-dir=%s" default-directory)
                          (format "%s" info-file)))
        (kill-buffer)
        (setf (symbol-function 'Info-revert-find-node) old-info-defn)))))

(defun package-test-build-multifile (dir)
  "Build a tar package from a multiple-file directory DIR.

DIR must not have a trailing slash."
  (let* ((pkg-dirname (file-name-nondirectory dir))
         (pkg-name (package-strip-version pkg-dirname))
         (pkg-version (match-string-no-properties 2 pkg-dirname))
         (tar-name (concat pkg-dirname ".tar"))
         (default-directory (expand-file-name dir)))
    (package-test-install-texinfo (concat pkg-name ".texi"))
    (setq default-directory (file-name-directory default-directory))
    (call-process "tar" nil nil nil "caf" tar-name pkg-dirname)))

(defun package-test-suffix-matches (base suffix-list)
  "Return file names matching BASE concatenated with each item in SUFFIX-LIST"
  (cl-mapcan
   '(lambda (item) (file-expand-wildcards (concat base item)))
   suffix-list))

(defun package-test-cleanup-built-files (dir)
  "Remove files which were the result of creating a tar archive.

DIR is the base name of the package directory, without the trailing slash"
  (let* ((pkg-dirname (file-name-nondirectory dir)))
    (dolist (file (package-test-suffix-matches dir package-test-built-file-suffixes))
      (delete-file file))))

(defun package-test-search-tar-file (filename)
  "Search the current buffer's `tar-parse-info' variable for FILENAME.

Must called from within a `tar-mode' buffer."
  (cl-dolist (header tar-parse-info)
    (let ((tar-name (tar-header-name header)))
      (when (string= tar-name filename)
        (cl-return t)))))

(ert-deftest package-test-desc-from-buffer ()
  "Parse an elisp buffer to get a `package-desc' object."
  (with-package-test (:basedir "data/package" :file "simple-single-1.3.el")
    (should (equal (package-desc-from-buffer) simple-single-desc)))
  (with-package-test (:basedir "data/package" :file "simple-depend-1.0.el")
    (should (equal (package-desc-from-buffer) simple-depend-desc)))
  (with-package-test (:basedir "data/package"
                               :build-dir "multi-file-0.2.3"
                               :file "multi-file-0.2.3.tar")
    (should (equal (package-desc-from-buffer) multi-file-desc))))

(ert-deftest package-test-install-single ()
  "Install a single file without using an archive."
  (with-package-test (:basedir "data/package" :file "simple-single-1.3.el")
    (should (package-install-single))
    (let* ((simple-pkg-dir (file-name-as-directory
                            (expand-file-name
                             "simple-single-1.3"
                             package-test-user-dir)))
           (autoloads-file (expand-file-name "simple-single-autoloads.el" simple-pkg-dir)))
      (should (file-directory-p simple-pkg-dir))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "simple-single-pkg.el" simple-pkg-dir))
        (should (string= (buffer-string)
                         "(define-package \"simple-single\" \"1.3\" \"A single-file package with no dependencies\" nil)\n")))
      (should (file-exists-p autoloads-file))
      (should-not (get-file-buffer autoloads-file)))))

(ert-deftest package-test-install-dependency ()
  "Install a package which includes a dependency."
  (with-package-test ()
    (package-refresh-contents)
    (package-install 'simple-depend)
    (should (package-installed-p 'simple-single))
    (should (package-installed-p 'simple-depend))))

(ert-deftest package-test-refresh-contents ()
  "Parse an \"archive-contents\" file."
  (with-package-test ()
    (package-refresh-contents)))

(ert-deftest package-test-install-single-from-archive ()
  "Install a single package from a package archive."
  (with-package-test ()
    (package-refresh-contents)
    (package-install 'simple-single)))

(ert-deftest package-test-build-multifile ()
  "Build a multi-file archive."
  (with-package-test (:basedir "data/package" :build-dir "multi-file-0.2.3")
    (should (file-exists-p build-tar))
    (let ((suffixes
           (remove build-tar (package-test-suffix-matches
                              build-dir
                              package-test-built-file-suffixes))))
      (with-current-buffer (find-file build-tar)
        (dolist (file suffixes)
          (should (package-test-search-tar-file file)))
        (kill-buffer)))))

(ert-deftest package-test-install-multifile ()
  "Check properties of the installed multi-file package."
  (with-package-test (:basedir "data/package" :build-dir "multi-file-0.2.3"
                               :install '(multi-file))
    (let ((autoload-file
           (expand-file-name "multi-file-autoloads.el"
                             (expand-file-name
                              "multi-file-0.2.3"
                              package-test-user-dir)))
          (installed-files '("dir" "multi-file.info" "multi-file-sub.elc"
                             "multi-file-autoloads.el" "multi-file.elc"))
          (autoload-forms '("^(defvar multi-file-custom-var"
                            "^(custom-autoload 'multi-file-custom-var"
                            "^(autoload 'multi-file-mode"
                            "^(provide 'multi-file-autoloads)"))
          (pkg-dir (file-name-as-directory
                    (expand-file-name
                     "multi-file-0.2.3"
                     package-test-user-dir))))
      (package-initialize)
      (should (package-installed-p 'multi-file))
      (with-temp-buffer
        (insert-file-contents-literally autoload-file)
        (dolist (fn installed-files)
          (should (file-exists-p (expand-file-name fn pkg-dir))))
        (dolist (re autoload-forms)
          (goto-char (point-min))
          (should (re-search-forward re nil t)))))))

(ert-deftest package-test-update-listing ()
  "Ensure installed package status is updated."
  (with-package-test ()
    (let ((buf (package-list-packages)))
      (search-forward-regexp "^ +simple-single")
      (package-menu-mark-install)
      (package-menu-execute)
      (should (package-installed-p 'simple-single))
      (switch-to-buffer "*Packages*")
      (goto-char (point-min))
      (should (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+installed" nil t))
      (goto-char (point-min))
      (should-not (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+\\(available\\|new\\)" nil t))
      (kill-buffer buf))))

(ert-deftest package-test-update-archives ()
  "Test updating package archives."
  (with-package-test ()
    (let ((buf (package-list-packages)))
      (package-menu-refresh)
      (search-forward-regexp "^ +simple-single")
      (package-menu-mark-install)
      (package-menu-execute)
      (should (package-installed-p 'simple-single))
      (let ((package-test-data-dir
             (expand-file-name "data/package/newer-versions" package-test-file-dir)))
        (setq package-archives `(("gnu" . ,package-test-data-dir)))
        (package-menu-refresh)

        ;; New version should be available and old version should be installed
        (goto-char (point-min))
        (should (re-search-forward "^\\s-+simple-single\\s-+1.4\\s-+new" nil t))
        (should (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+installed" nil t))

        (goto-char (point-min))
        (should (re-search-forward "^\\s-+new-pkg\\s-+1.0\\s-+\\(available\\|new\\)" nil t))

        (package-menu-mark-upgrades)
        (package-menu-execute)
        (package-menu-refresh)
        (should (package-installed-p 'simple-single '(1 4)))))))

(provide 'package-test)

;;; package-test.el ends here
