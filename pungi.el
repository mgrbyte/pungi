;;; pungi.el --- Integrates jedi with virtualenv and buildout python environments

;; Copyright (C) 2014  Matthew Russell

;; Author: Matthew Russell <matthew.russell@horizon5.org>
;; Version: 0.2
;; Keywords: convenience
;; Package-Requires: ((jedi "0.2.0alpha1"))

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
;; This package provides integration with jedi virtualenv and buildout.
;; When working within a virtualenv, configure python sys.path passed
;; to `jedi:server-args' such jedi commands `jedi:complete',
;; `jedi:goto-definition' and `jedi:doc' show the correct sources.
;;
;; Installation:
;; (add-to-list 'load-path  'path-to-this-file)
;; (require 'pungi)
;;
;; Usage:
;;   The are no user-facing features, but you can test it works by
;;   entering a project that uses buildout or virtualenv,
;;   and checking that `jedi:goto-definition' works when you execute
;;   the command on symbol in a python buffer.
;;
;;; Code:

(unless (require 'python-mode nil :noerr)
  (require 'python))
(require 'jedi)

(defvar pungi-local-variables-hook nil
  "Hook to be set when `hack-local-variables-hook' is run.
Enables jedi to run with a specific sys.path when in a virtual environment.")

(defvar pungi-additional-paths nil
  "Addtional paths that will be set if a virtualenv is detected.")

(defun pungi--setup-jedi-maybe ()
  "Setup jedi if it is installed."
  (when (require 'jedi nil t)
    (jedi:setup)))

(add-hook 'hack-local-variables-hook
          '(lambda ()
             (when pungi-local-variables-hook
               (pungi--run-local-vars-hook-for-major-mode))))

(defun pungi--run-local-vars-hook-for-major-mode ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(defun pungi--set-jedi-paths-for-detected-environment ()
  "Sets `jedi:server-args' for the detected environment."
  (let ((venv (pungi--detect-buffer-venv buffer-file-name))
        (egg-dirs (pungi--detect-buffer-eggs-dirs buffer-file-name)))
    (make-local-variable 'jedi:server-args)
    (when venv
      (set 'jedi:server-args (list "--virtual-env" venv)))
    (when egg-dirs
      (dolist (egg egg-dirs)
        (set 'jedi:server-args (append jedi:server-args (list "--sys-path" egg))))))
  (make-local-variable 'pungi-additional-paths)
  (when pungi-additional-paths
    (dolist (path pungi-additional-paths)
      (set 'jedi:server-args (append jedi:server-args (list "--sys-path" path))))))

(defun pungi--detect-buffer-venv (path)
  "Detect a python virtualenv from the given PATH."
  (let ((buffer-dir (file-name-directory path)))
    (while (and (not (file-exists-p
                      (concat buffer-dir "bin/activate")))
                buffer-dir)
      (setq buffer-dir
            (if (equal buffer-dir "/")
                nil
              (file-name-directory (directory-file-name buffer-dir)))))
    buffer-dir))

(defun pungi--detect-buffer-eggs-dirs (path)
  "Detect if the file pointed to by PATH uses buildout eggs."
  (let ((buffer-dir (file-name-directory path)))
    (while (and (not (file-exists-p
                      (concat buffer-dir "eggs")))
                buffer-dir)
      (setq buffer-dir
        (if (equal buffer-dir "/")
            nil
          (file-name-directory (directory-file-name buffer-dir)))))
    (if buffer-dir
        (directory-files (concat buffer-dir "eggs") t ".\.egg")
        nil)))

(defun pungi--setup-jedi-extra-args--maybe ()
  "Configure jedi server's extra arguments."
  (when (require 'jedi nil t)
    (pungi--set-jedi-paths-for-detected-environment)))

(add-hook 'python-mode-local-vars-hook 'pungi--setup-jedi-extra-args--maybe)
(add-hook 'python-mode-local-vars-hook 'pungi--setup-jedi-maybe)
(add-hook 'python-mode-hook 'pungi--setup-jedi-maybe)

(provide 'pungi)
;;; pungi.el ends here
