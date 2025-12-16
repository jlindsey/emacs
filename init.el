;;; init.el -- tangled from init.org -*- lexical-binding: t -*-

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'project-indicators)

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

(setq package-enable-at-startup nil ;; also set in early-init.el
      straight-use-package-version 'straight
      straight-use-package-by-default t)

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

(defun straight/cleanup ()
  "Prunes cache and build dirs, and purges unused repos"
  (interactive)
  (straight-prune-build)
  (straight-remove-unused-repos t))

(defun straight/update ()
  "Pulls and updates all packages"
  (interactive)
  (straight-pull-recipe-repositories)
  (straight-pull-all))

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

  (general-define-key
   :states '(normal)
   "g t" 'tab-line-switch-to-next-tab
   "g T" 'tab-line-switch-to-prev-tab)

  (general-create-definer start/leader-keys
	:states '(normal emacs)
	:keymaps 'override
	:prefix "SPC"
	:global-prefix "SPC")
  
  (start/leader-keys
	"SPC" '(find-file :wk "Find file")
	":" '(execute-extended-command :wk "M-x")
	"TAB" '(treemacs-select-window :wk "Treemacs"))

  (start/leader-keys
	"~" '(:ignore t :wk "Straight")
	"~ c" '(straight/cleanup :wk "Cleanup")
	"~ u" '(straight/update :wk "Update")
	"~ f" 'straight-freeze-versions
	"~ t" `straight-thaw-versions)
  
  (start/leader-keys
	"b" '(:ignore t :wk "Buffers")
	"b i" '(ibuffer :wk "ibuffer")
	"b d" '(evil-delete-buffer :wk "Delete buffer"))
  
  (start/leader-keys
	"C" '(:ignore t :wk "Configs")
	"C i" '((lambda () (interactive) (find-file "~/.config/emacs/init.org")) :wk "Edit init.org")
	"C e" '((lambda () (interactive) (find-file "~/.config/emacs/early-init.el")) :wk "Edit early-init.el")
	"C r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload init.el"))

  (start/leader-keys
	"l" '(:ignore t :wk "LSP"))

  (start/leader-keys
	"q" '(:ignore t :wk "Quit")
	"q r" '(restart-emacs :wk "Restart")
	"q q" '(evil-quit-all :wk "Quit All"))

  (start/leader-keys
	"t" '(:ignore t :wk "Toggles")
	"t f" 'toggle-frame-fullscreen
	"t t" 'tab-line-mode)
  
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
  :straight nil
  :custom
  (org-edit-src-content-indentation 4)
  (org-return-follows-link t)
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . my/set-literate-init-capfs))
  :init
  (defun my/set-literate-init-capfs ()
	(setq-local completion-at-point-functions
				(list #'cape-elisp-block #'cape-file #'cape-keyword #'cape-dabbrev))))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-tempo
  :ensure nil
  :straight nil
  :after org)

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
  (doom-modeline-total-line-number t)
  (doom-modeline-project-name t)
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
  :init
  (setq dashboard-items '((recents . 10)
						  (projects . 5)))
  :config (dashboard-setup-startup-hook))

(use-package page-break-lines
  :init (page-break-lines-mode))

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
		  "\\*Info\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Flycheck errors\\*"
          "\\*helpful.*\\*"
		  Info-mode
          special-mode
          help-mode
		  lsp-help-mode
          compilation-mode))

  :config
  (popper-mode +1)
  (popper-echo-mode +1)

  :general
  (start/leader-keys
	"w q" 'popper-kill-latest-popup
	"w p" 'popper-toggle
	"w TAB" 'popper-cycle))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package hl-line
  :ensure nil
  :straight nil
  :hook (after-init . global-hl-line-mode))

;; MacOS UI improvements
(setq
 ns-use-native-fullscreen nil
 mac-redisplay-dont-reset-vscroll t
 mac-mouse-wheel-smooth-scroll nil
 delete-by-moving-to-trash t)

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
  (which-key-min-display-lines 5)
  (which-key-idle-delay 0.5)
  (which-key-max-description-length 35)
  (which-key-allow-imprecise-window-fit nil))

(use-package project
  :straight nil
  :ensure nil
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
  (treemacs-is-never-other-window nil)
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
  :config
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

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-italic-underscore t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-fontify-whole-heading-line t)
  (markdown-fontify-code-blocks-natively t))

(use-package emacs-lisp-mode
  :straight nil
  :ensure nil
  :hook (emacs-lisp-mode . my/set-elisp-capfs)
  :init
  (defun my/set-elisp-capfs ()
	(setq-local completion-at-point-functions 
				(list #'cape-elisp-symbol #'cape-file #'cape-keyword #'cape-dabbrev))))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package go-mode
  :init (setq go-ts-mode-hook go-mode-hook)
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(use-package python
  :ensure nil
  :straight nil
  :hook
  ((python-base-mode . lsp-deferred)
   (python-base-mode . (lambda () (highlight-indentation-mode -1))) ; using indent-bars-mode instead
   (python-base-mode . indent-bars-mode)))

(use-package elpy
  :hook (python-mode . elpy-enable)
  :general
  (general-create-definer python/mode-leader
	:states '(normal)
	:keymaps 'python-base-mode-map
	:prefix "SPC m")
	
  (python/mode-leader
	"i" '(:ignore t :wk "Imports")))

(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "basedpyright"))

(use-package pyimport
  :commands (pyimport-remove-unused pyimport-insert-missing)
  :general
  (python/mode-leader
	"i r" 'pyimport-remove-unused
	"i m" 'pyimport-insert-missing))

(use-package py-isort
  :commands (py-isort-region py-isort-buffer)
  :general
  (python/mode-leader
	"i s" 'py-isort-buffer))

(use-package pet
  :hook (python-base-mode . pet-mode))

(use-package yaml-mode
  :init (setq yaml-ts-mode-hook yaml-mode-hook)
  :hook
  ((yaml-mode . lsp-deferred)
   (yaml-mode . indent-bars-mode)))

(use-package ansible
  :hook (yaml-mode . (lambda () (when (project/is-ansible-project) (ansible-mode 1)))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package apheleia
  :hook (prog-mode . (lambda () apheleia-global-mode +1))
  :general
  (start/leader-keys
	"t a" '(apheleia-global-mode :wk "Toggle Apheleia")))

(use-package indent-bars
  :custom
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-color '(default :face-bg t :blend 0.3))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.25)
  (indent-bars-pad-frac 0.1))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(setq gc-cons-threshold (* 2 1000 1000))

(setq read-process-output-max (* 1024 1024)) ;; 1mb
