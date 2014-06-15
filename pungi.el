;;; pungi.el --- Integrates jedi with virtualenv and buildout python environments

;; Copyright (C) 2014  Matthew Russell

;; Author: Matthew Russell <matthew.russell@horizon5.org>
;; Version: 0.9.5
;; Keywords: convenience
;; Package-Requires: ((jedi "0.2.0alpha2"))

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
;;
;;; Commentary:
;; This package provides integration with jedi virtualenv and buildout.
;; When working within a virtualenv, configure python sys.path passed
;; to `jedi:server-args' such jedi commands `jedi:complete',
;; `jedi:goto-definition' and `jedi:doc' show the correct sources.
;;
;; Installation:
;; If not using ELPA (i.e list-packages), then add the following to
;; you init.el/.emacs:
;;
;; (add-to-list 'load-path 'path-to-this-file)
;; (require 'pungi)
;;
;; Usage:
;;   When you'd like project specific variables to be taken into account,
;;   e.g python-mode specific changes, you can place a file at the root
;;   of the project directory called .dir-locals.el, in which
;;   you can set variables on a per-mode, or global basis.
;;   See http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;;   for documentation.
;;   Set the `pungi-setup-jedi' to a non-nil value in order for `jedi:setup' to
;;   take those settings into account.
;;
;;   If jedi has been required, then jedi:setup will be triggered when
;;   python-mode-hook is fired.
;;
;; Testing:
;;   When visiting a python buffer, move the cursor over a symbol
;;   and check that invoking M-x `jedi:goto-definition' opens a
;;   new buffer showing the source of that python symbol.
;;
;;; Code:

(unless (require 'python-mode nil :noerr)
  (require 'python))
(require 'jedi)

(defvar pungi-setup-jedi t
  "Whether pungi should setup jedi.
Enables jedi to run with a specific sys.path when in a virtual environment.")

(defvar pungi-additional-paths nil
  "Addtional paths that will be set if a virtualenv is detected.")

(defun pungi--setup-jedi-maybe ()
  "Setup jedi if it is installed."
  (when (and (require 'jedi nil t) pungi-setup-jedi)
    (pungi--set-jedi-paths-for-detected-environment)
    (jedi:ac-setup)
    (when jedi:import-python-el-settings
      ;; This is added to the beginning of `hack-local-variables-hook'
      ;; internally from `jedi:setup', so it will never be run when
      ;; `pungi--setup-jedi-maybe' is run as a `hack-local-variables-hook', so
      ;; the internal of `jedi:setup' must be copyed and pasted here.
      (jedi:import-python-el-settings-setup))
    (jedi-mode 1)))

(defun pungi--python-mode-hook ()
  "Hook to setup pungi when python-mode is active."
  (add-hook 'hack-local-variables-hook 'pungi--setup-jedi-maybe nil t))

(add-hook 'python-mode-hook 'pungi--python-mode-hook)

(defun pungi--set-jedi-paths-for-detected-environment ()
  "Set `jedi:server-args' for the detected environment."
  (let ((venv (pungi--detect-buffer-venv buffer-file-name))
        (omelette (pungi--detect-buffer-omelette buffer-file-name)))
    (make-local-variable 'jedi:server-args)
    (when venv
      (set 'jedi:server-args (list "--virtual-env" venv)))
    (when omelette
      (set 'jedi:server-args (append jedi:server-args (list "--sys-path" omelette)))))
  (make-local-variable 'pungi-additional-paths)
  (when pungi-additional-paths
    (dolist (path pungi-additional-paths)
      (set 'jedi:server-args (append jedi:server-args (list "--sys-path" path))))))

(defun pungi--find-directory-container-from-path (directory path)
  "Find a DIRECTORY located in a subdirectory of given PATH."
  (let ((buffer-dir (file-name-directory path)))
    (while (and (not (file-exists-p
                      (concat buffer-dir directory)))
                buffer-dir)
      (setq buffer-dir
            (if (equal buffer-dir "/")
                nil
              (file-name-directory (directory-file-name buffer-dir)))))
    buffer-dir))

(defun pungi--detect-buffer-venv (path)
  "Detect a python virtualenv from the given PATH."
  (pungi--find-directory-container-from-path "bin/activate" path))

(defun pungi--detect-buffer-omelette (path)
  "Detect if the file pointed to by PATH use buildout omelette."
  (let ((parent-dir (pungi--find-directory-container-from-path "omelette" path)))
    (if (not parent-dir)
        (setq parent-dir
              (concat (pungi--find-directory-container-from-path "parts" path) "parts/")))
    (concat parent-dir "omelette")))

(provide 'pungi)
;;; pungi.el ends here
