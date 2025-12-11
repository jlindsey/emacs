;;; init.el -- tangled from init.org -*- lexical-binding: t -*-

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun start/org-babel-tangle-config ()
  "Automatically tangle our init.org config file"
  (interactive)
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(use-package emacs
  :custom
	;; Enable these for Vertico
  (context-menu-mode t)
  (enable-recursive-minibuffers t)

	;; Turn off all the window chrome
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)

  (delete-selection-mode t)
  (electric-indent-mode t)
  (electric-pair-mode nil)

  (blink-cursor-mode nil)
  (global-auto-revert-mode t)

  (fill-column 120) ;; Using evil's Vgq to format paragraphs, wrap on col 120
  (tab-width 4)
  (line-spacing 0.12)

  (make-backup-files nil) ;; Do not make ~ files
  (auto-save-default nil) ;; Do not make # files

  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(readonly t cursor-intangible t face minibuffer-prompt))

  :config
  (set-face-attribute
   'default nil
   :family "Hack"
   :height 125
   :weight 'medium)

  :hook
  ; hide minor modes
  (prog-mode . (lambda () (hs-minor-mode t)))
  
  ; enable line numbers but not globally
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  (conf-mode . display-line-numbers-mode)

  :bind
  (([escape] . keyboard-escape-quit)))

(use-package savehist
  :ensure nil
  :init (savehist-mode))

(use-package evil
  :init
  (evil-mode)
  :custom
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-undo-system 'undo-redo)
  (evil-visual-update-x-selection-p nil)
  :config
  (add-hook 'after-change-major-mode-hook (lambda () (setq-local evil-shift-width tab-width))))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult info
										  help helpful compilation flycheck))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-lion
  :after evil
  :config (evil-lion-mode))

(use-package evil-escape
  :after evil
  :custom
  (evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (evil-escape-excluded-major-modes '(treemacs-mode))
  (evil-escape-key-sequence nil)
  (evil-escape-delay 0.15)
  :init
  (evil-escape-mode)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; `evil-escape' in the minibuffer is more disruptive than helpful. That is,
  ;; unless we have `evil-collection-setup-minibuffer' enabled, in which case we
  ;; want the same behavior in insert mode as we do in normal buffers.
  (add-hook 'evil-escape-inhibit-functions
			(defun my/evil-inhibit-escape-in-minibuffer-fn ()
			  (and (minibufferp)
				   (or (not (bound-and-true-p evil-collection-setup-minibuffer))
					   (evil-normal-state-p))))))

(use-package general
  :custom
  (general-use-package-emit-autoloads t)
  :config
  (general-evil-setup)
  
  (general-define-key
   :states '(visual)
   "s-/" 'comment-or-uncomment-region)

  (general-define-key
   :states '(normal)
   "s-/" 'comment-line)

  (general-create-definer start/leader-keys
	:states '(normal emacs)
	:keymaps 'override
	:prefix "SPC"
	:global-prefix "SPC")

  (general-create-definer start/mode-leader
	:states '(normal emacs)
	:keymaps 'override
	:prefix "SPC m"
	:global-prefix "SPC m")
  
  (start/leader-keys
	"SPC" '(project-find-file :wk "Find file")
	":" '(execute-extended-command :wk "M-x"))
  
  (start/leader-keys
	"b" '(:ignore t :wk "Buffers")
	"b i" '(ibuffer :wk "ibuffer")
	"b d" '(evil-delete-buffer :wk "Delete buffer"))
  
  (start/leader-keys
	"C" '(:ignore t :wk "Configs")
	"C i" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Edit init.el")
	"C e" '((lambda () (interactive) (find-file "~/.config/emacs/early-init.el")) :wk "Edit early-init.el")
	"C r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload init.el"))

  (start/leader-keys
	"q" '(:ignore t :wk "Quit")
	"q r" '(restart-emacs :wk "Restart")
	"q q" '(evil-quit-all :wk "Quit All"))
  
  (start/leader-keys
	"w" '(:ignore t :wk "Windows / Splits")
	"w s" 'evil-window-split
	"w v" 'evil-window-vsplit
	"w S" 'evil-window-new
	"w V" 'evil-window-vnew
	"w h" 'evil-window-left
	"w j" 'evil-window-down
	"w k" 'evil-window-up
	"w l" 'evil-window-right
	"w H" 'evil-window-move-far-left
	"w J" 'evil-window-move-very-bottom
	"w K" 'evil-window-move-very-top
	"w L" 'evil-window-move-far-right)
  )

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4)
  (org-return-follows-link t)
  :hook (org-mode . org-indent-mode))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-colors")
  :config
  (load-theme 'doom-material t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backed 'project-el)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-navigation-cycle t)
  (dashboard-display-icons t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :config (dashboard-setup-startup-hook))

(use-package shackle
  :config
  (setq shackle-rules
        '(
          (("\\*Help\\*" helpful-mode) :align right :size 0.35 :select nil)
		  (lsp-help-mode :align right :size 0.35 :select nil)

		  ("\\*compilation\\*" :align below :size 0.25 :noselect t)
		  (("\\*Flycheck errors\\*" flycheck-error-list-mode) :align below :size 0.25 :select t)
		  (("\\*Warnings\\*" special-mode) :align below :size 0.2 :select nil)

		  ("\\*Messages\\*" :align below :size 0.3 :select nil)
		  ("\\*Backtrace\\*" :align below :size 0.3 :select t))
		)
  (shackle-mode 1))

(use-package popper
  :custom
  (popper-display-control nil)
  (popper-group-function #'popper-group-by-project)
  (popper-mode-line nil)

  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Flycheck errors\\*"
          "\\*helpful.*\\*"
          special-mode
          help-mode
		  lsp-help-mode
          compilation-mode))

  :config
  (popper-mode +1)
  (popper-echo-mode +1)

  (defun my/quit-popup-or-default ()
    "Quit popup window if in a popup, otherwise run normal quit-window"
    (interactive)
    (if (and (bound-and-true-p popper-popup-status)
                     (popper-popup-p (current-buffer)))
            (popper-toggle)
      (quit-window)))

  (defun my/keyboard-quit-or-toggle-popup ()
	"Toggle popup if in one, otherwise run keyboard-quit"
	(interactive)
	(if (and (bound-and-true-p popper-popup-status)
			 (popper-popup-p (current-buffer)))
		(popper-toggle)
	  (keyboard-quit)))

  :general
  ;; (general-define-key
  ;;  :states '(motion)
  ;;  :keymaps '(help-mode-map helpful-mode-map compilation-mode-map flycheck-error-list-mode-map)
  ;;  "q" 'my/quit-popup-window-or-default)

  ;; Fix: Make C-g work in popups
  ;; (general-define-key
  ;;  :states '(normal insert visual emacs)
  ;;  :keymaps 'override
  ;;  "C-g" 'my/keyboard-quit-or-toggle-popup)

  (start/leader-keys
	"w q" 'popper-kill-latest-popup
	"w p" 'popper-toggle
	"w TAB" 'popper-cycle))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

(use-package page-break-lines
  :init (page-break-lines-mode))

(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode +1))

(use-package helpful
  :general
  (start/leader-keys
    "h" '(:ignore t :wk "Help")
    "h h" 'helpful-at-point
    "h c" 'helpful-command
    "h f" 'helpful-callable
    "h k" 'helpful-key
    "h v" 'helpful-variable))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package which-key
  :ensure nil
  :init (which-key-mode 1)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-min-display-lines 6)
  (which-key-idle-delay 0.5)
  (which-key-max-description-length 35)
  (which-key-allow-imprecise-window-fit nil))

(use-package project
  :ensure nil
  :defer t
  :init
  (setq project-list-file (file-name-concat
						   (file-name-parent-directory user-init-file)
						   "projects"))
  :general
  (start/leader-keys
	"p" '(:ignore t :wk "Projects")
	"p p" 'project-switch-project
	"p f" 'project-find-file))

(use-package ibuffer-project
  :init
  (defun my/ibuffer-project-filter-predicate ()
	(setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
	(unless (eq ibuffer-sorting-mode 'project-file-relative)
	  (ibuffer-do-sort-by-project-file-relative)))
  :hook (ibuffer . my/ibuffer-project-filter-predicate))

(use-package treemacs
  :custom
  (treemacs-width 30)
  (treemacs-follow-after-init t)
  (treemacs-sotring 'alphabetic-case-insensitive-asc)
  (treemacs-indent-guide-style 'line)
  (treemacs-is-never-other-window t)
  :config
  (treemacs-project-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  (treemacs-git-commit-diff-mode t)
  (treemacs-indent-guide-mode t))

(use-package treemacs-evil
  :after treemacs)

(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme 'nerd-icons))

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-separator ?\s)
  (corfu-popupinfo-delay 0.1)
  (corfu-preselect 'prompt)
  (completion-ignore-case t)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  :general
  (general-define-key
   :states '(normal insert)
   "C-SPC" 'completion-at-point))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  :general
  (start/leader-keys
    "a" '(cape-prefix-map :wk "Cape")))

(use-package vertico
  :custom
  (vertico-scroll-margin 5)
  (vertico-count 15)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package yasnippet
  :after cape
  :config (yas-global-mode 1))

(use-package yasnippet-capf
  :after (cape lsp))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yas-reload-all))

(use-package consult
  :custom
  (register-preview-delay 0.3)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :general
  (start/leader-keys
    "s" '(:ignore t :wk "Search")
    "s b" 'consult-buffer
    "s r" 'consult-recent-file
    "s f" 'consult-fd
    "s g" 'consult-ripgrep
    "s l" 'consult-line
    "s i" 'consult-imenu))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides `((file (styles basic partial-completion)))))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-completion-enable t)
  (lsp-completion-default-behavior :insert)
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-completion-mode . my/lsp-mode-setup-completion))
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
	(setq-local completion-at-point-functions
				(list (cape-capf-super #'lsp-completion-at-point #'yasnippet-capf)))))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  :general
  (start/leader-keys
	"l t s" 'lsp-treemacs-symbols
	"l t r" 'lsp-treemacs-references
	"l t i" 'lsp-treemacs-implementations
	"l t c" 'lsp-treemacs-call-hierarchy
	"l t t" 'lsp-treemacs-type-hierarchy))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-italic-underscore t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-fontify-whole-heading-line t)
  (markdown-fontify-code-blocks-natively t))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(setq gc-cons-threshold (* 2 1000 1000))

(setq read-process-output-max (* 1024 1024)) ;; 1mb
