;; pungi-mode
;; "Pungi" is what indian snake charmers are called.
;; Provides an opionated Python developement environment.
(require 'jedi)
(require 'python-mode)
(require 'flymake-python-pyflakes)

(defun pungi-annotate-pdb ()
  "Highlight pdb statements."
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))

(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)

(defun run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (message (format "Running mode-hook ***: %s" (intern (concat (symbol-name major-mode) "-local-vars-hook"))))
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))


(defun detect-buffer-venv (path)
  "Detect a python virtualenv from the given path."
  (message "detect-buffer-venv")
  (let ((buffer-dir (file-name-directory path)))

    (while (and (not (file-exists-p
                      (concat buffer-dir "bin/activate")))
                buffer-dir)
      (setq buffer-dir
        (if (equal buffer-dir "/")
            nil
            (file-name-directory (directory-file-name buffer-dir))
            ))
      )
    ;; return the buffer-dir (or nil)
    buffer-dir
    )
  )

(defun detect-buffer-eggs-dirs (path)
  "Detect if the file point to by path uses buildout eggs."
  (message "detect-buffer-eggs-dirs")
  (let (
        (buffer-dir (file-name-directory path))
        )
    (while (and (not (file-exists-p
                      (concat buffer-dir "eggs")))
                buffer-dir
                )
      (setq buffer-dir
        (if (equal buffer-dir "/")
            nil
            (file-name-directory (directory-file-name buffer-dir))
            )
        )
      )
    (if buffer-dir
        (directory-files (concat buffer-dir "eggs") t ".\.egg")
        nil
      )
    )
  )

(defvar pungi-additional_paths nil)

(defun setup-jedi-extra-args ()
  "Configure jedi server's extra arguments."
  (let
      ((venv (detect-buffer-venv buffer-file-name))
       (egg-dirs (detect-buffer-eggs-dirs buffer-file-name))
       )
    (make-local-variable 'jedi:server-args)
    (when venv
      (set 'jedi:server-args (list "--virtual-env" venv)))
    (when egg-dirs
      (dolist (egg egg-dirs)
        (set 'jedi:server-args (append jedi:server-args (list "--sys-path" egg))))
      )
    )
  (make-local-variable 'pungi-additional_paths)
  (when pungi-additional_paths
    (dolist (path pungi-additional_paths)
        (set 'jedi:server-args (append jedi:server-args (list "--sys-path" path)))
      ))
  )


(add-hook 'python-mode-local-vars-hook 'setup-jedi-extra-args)
(add-hook 'python-mode-local-vars-hook 'jedi:setup)
(add-hook 'python-mode-hook 'pungi-annotate-pdb)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(provide 'pungi)
