;;; package-indicators.el --- Macro for project type detection -*- lexical-binding: t -*-

;;; Commentary:
;;; Macro that detects certain marker files or directories in a project.el project

;;; Code:
(require 'project)

(defun project/has-indicators-p (indicators)
  "Check if the current buffer is in a project that has the given INDICATORS."
  (when-let ((file (buffer-file-name))
			 (project (project-current)))
	(let ((project-root (project-root project)))
	  (cl-some (lambda (indicator)
				 (when-let ((found-dir (locate-dominating-file file indicator)))
				   ;; Ensure we're still within project bounds
				   (string-prefix-p project-root found-dir)))
			   indicators))))

(defvar project/ansible-indicators
  '("ansible.cfg" "roles" "tasks" "group_vars"))

(defun project/is-ansible-project ()
  "Return t if the current project is an Ansible project."
  (project/has-indicators-p project/ansible-indicators))

(provide 'project-indicators)
;;; project-indicators.el ends here
