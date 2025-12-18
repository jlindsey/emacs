;;; python-helpers.el --- Misc helper funcs for Python mode -*- lexical-binding: t; -*-


;;; Commentary:
;; Some functions
;;

;;; Code:

(defun python/setup-python-from-uv-inline-meta ()
  "Update venv and python interpreter for scripts with the UV metadata preamble."
  (interactive)
  (if buffer-file-name
	  (let* ((_ (shell-command (format "uv sync --script '%s'" buffer-file-name)))
			 (p-interpreter (replace-regexp-in-string
							 "[ \t\n\r]+$" ""
							 (shell-command-to-string
							  (format "uv python find --script '%s'" buffer-file-name)))))
		(if (not (string= "" p-interpreter))
			(message (format "found script python: '%s'" p-interpreter))
			(let* ((venv-dir (expand-file-name (format "%s/../../" p-interpreter))))
			  (setq-local pet-virtualenv-root (lambda () venv-dir)))
			(warn "No python interpreter found!")))
	(warn "Not in a named file buffer!")))

(provide 'python-helpers)

;;; python-helpers.el ends here
