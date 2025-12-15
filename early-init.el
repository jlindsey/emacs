;;; early-init.el -- early fun times -*- lexical-binding: t -*-

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Disable package.el as we use straight
(setq package-enable-at-startup nil)
