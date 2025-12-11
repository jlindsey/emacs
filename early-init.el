;;; early-init.el -- early fun times -*- lexical-binding: t -*-

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; MacOS UI improvements
(setq
 ns-use-native-fullscreen nil
 mac-redisplay-dont-reset-vscroll t
 mac-mouse-wheel-smooth-scroll nil
 delete-by-moving-to-trash t)

;; Use straight.el
(setq 
 straight-use-package-by-default t
 package-enable-at-startup nil)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
		(url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
