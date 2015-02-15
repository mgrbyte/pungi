;;; pungi.el --- Integrates jedi with virtualenv and buildout python environments

;; Copyright (C) 2014  Matthew Russell

;; Author: Matthew Russell <matthew.russell@horizon5.org>
;; Version: 0.9.9
;; Keywords: convenience
;; Package-Requires: ((jedi "0.2.0alpha2") (pyvenv "1.5"))

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
;; If not using ELPA (i.e list-packages), then add the following to
;; your init.el/.emacs:
;;
;; (add-to-list 'load-path 'path-to-this-file)
;;
;; Using ELPA (When installed from `list-packages'):
;; (require 'pungi)
;;
;; In all cases, call `pungi:setup-jedi' within a `python-mode-hook':
;;
;; (add-hook #'python-mode-hook
;;           '(lambda ()
;;              (pungi:setup-jedi)))
;;
;; When visiting a python buffer, move the cursor over a symbol
;; and check that invoking M-x `jedi:goto-definition' opens a
;; new buffer showing the source of that python symbol.
;;
;;; Code:
(require 'python)
(require 'pyvenv)
(require 'jedi)


(defvar pungi-setup-jedi t
  "Whether pungi should setup jedi.
Enables jedi to run with a specific sys.path when in a virtual environment.")

(defvar pungi-prefer-buildout t
  "Whether pungi should prefer buildout over virtualenv when both are detected." )

(defvar pungi-additional-paths nil
  "Addtional paths that will be set independantly of the environment detected.")

(defun pungi:setup-jedi ()
  "Setup jedi if it is installed."
  (pungi--set-jedi-paths-for-detected-environment)
  (jedi:setup))

(defun pungi--python-mode-hook ()
  "Hook to setup pungi when `python-mode` is active."
  (add-hook 'python-mode 'pungi--setup-jedi-maybe nil t))

(add-hook 'python-mode-hook 'pungi--python-mode-hook)

(defun pungi--set-jedi-paths-for-detected-environment ()
  "Set `jedi:server-args' for the detected environment."
  (let* ((venv pyvenv-virtual-env)
	 (omelette (pungi--detect-buffer-omelette buffer-file-name)))
    (when omelette
      (setq python-shell-extra-pythonpaths (list omelette)))
    (if (and venv omelette omelette (not pungi-prefer-buildout))
	(setq python-shell-virtualenv-path venv))))

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

(defun pungi--detect-buffer-omelette (path)
  "Detect if the file pointed to by PATH is in use by buildout.

;;; Commentary:
buildout recipes usually contain a `part` called `omelette`,
which is a hierarchy of symlinks,
generated from the python eggs specified by the buildout configuration."
  (let ((parent-dir (pungi--find-directory-container-from-path "omelette" path)))
    (if (not parent-dir)
        (setq parent-dir
              (concat (pungi--find-directory-container-from-path "parts" path) "parts/")))
    (concat parent-dir "omelette")))

(provide 'pungi)
;;; pungi.el ends here
