;; Core Setup
;; Make startup faster by reducing the frequency of garbage collection. This will be set back when startup finishes.

;; [[file:init.org::*Core Setup][Core Setup:1]]
;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;; Core Setup:1 ends here

;; Auto-tangle Hook

;; [[file:init.org::*Auto-tangle Hook][Auto-tangle Hook:1]]
(defun start/org-babel-tangle-config ()
  "Automatically tangle our init.org config file"
  (interactive)
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))
;; Auto-tangle Hook:1 ends here

;; Repos
;; Even though we don't use ~package.el~ and straight ignores this, it's useful to set so you can still do ~M-x install-package~.


;; [[file:init.org::*Repos][Repos:1]]
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
;; Repos:1 ends here

;; Emacs Defaults
;; Configure baseline Emacs defaults.


;; [[file:init.org::*Emacs Defaults][Emacs Defaults:1]]
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
;; Emacs Defaults:1 ends here

;; Savehist
;; Persists various selection and file histories to a file, to be restored on startup. Core Emacs built-in, but must be
;; used and enabled. By default, saves to ~~/.config/emacs/history~.


;; [[file:init.org::*Savehist][Savehist:1]]
(use-package savehist
  :ensure nil
  :init (savehist-mode))
;; Savehist:1 ends here

;; Evil
;; Setup Evil mode and associated plugins.

;; [[file:init.org::*Evil][Evil:1]]
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
;; Evil:1 ends here



;; Include ~evil-collection~, which is a set of keymaps and bindings for special buffer types like ~help-mode~ and
;; ~dired~. I set it up as an explicit whitelist rather than letting it default to trying to remap everything it knows
;; about. Note that this means you need to update this list whenever a package is added with a new mode.

;; [[file:init.org::*Evil][Evil:2]]
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult info
										  help helpful compilation flycheck))
  (evil-collection-init))
;; Evil:2 ends here



;; ~evil-surround~ is a port of tpope's ~vim-surround~ which provides mappings to easily surround a text object with pairs of quotes, parens, brackets, etc.

;; [[file:init.org::*Evil][Evil:3]]
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))
;; Evil:3 ends here



;; ~evil-lion~ ports ~vim-lion~, for easy line alignment motions.

;; [[file:init.org::*Evil][Evil:4]]
(use-package evil-lion
  :after evil
  :config (evil-lion-mode))
;; Evil:4 ends here



;; ~evil-escape~ universalizes escaping in all types of buffers. I bind it to ~C-g~ as a global "cancel".

;; [[file:init.org::*Evil][Evil:5]]
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
;; Evil:5 ends here

;; Keybinds
;; This config uses ~general~ for keybind management because:
;;         + I prefer the doom/spacemacs single leader key style, and it makes this easy to set up
;; ~       + It supports using ~use-package~ to set up keys at the point where they are most relevent
          
;; Given that second point, only keys which aren't associated with a particular package should be set here. Things like
;; window split management and movement, quitting/restarting, etc. Package-specific bindings should use the ~:general~ form
;; in ~use-package~.


;; [[file:init.org::*Keybinds][Keybinds:1]]
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
;; Keybinds:1 ends here



;; Also included in this section is ~which-key~, a built-in package (as of 30) that displays a list of keybindings along
;; the bottom in the minibuffer after a short delay when you start a chord or leader sequence.

;; [[file:init.org::*Keybinds][Keybinds:2]]
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
;; Keybinds:2 ends here

;; Org
;; I don't use Org mode much except for this config, so just set up a couple simple packages and configs here.


;; [[file:init.org::*Org][Org:1]]
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
;; Org:1 ends here

;; Theme
;; Use ~doom-themes~, which is a theme collection package. Nice to be able to switch sometimes.


;; [[file:init.org::*Theme][Theme:1]]
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-colors")
  :config
  (load-theme 'doom-material t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config))
;; Theme:1 ends here

;; Modeline
;; Use ~doom-modeline~ instead of the default, which is minimal enough while still having lots of functionality and interop
;; with other packages like Flycheck and LSP.


;; [[file:init.org::*Modeline][Modeline:1]]
(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  :hook (after-init . doom-modeline-mode))
;; Modeline:1 ends here

;; Nerd Icons
;; Use Nerd Icons everywhere we can. Here is where we put the base ~nerd-icons~ package as well as the integrations with
;; core modes like Dired and Ibuffer. Integrations with other packages like Treemacs and Corfu are in those sections.


;; [[file:init.org::*Nerd Icons][Nerd Icons:1]]
(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
;; Nerd Icons:1 ends here

;; Dashboard
;; A nice startup screen showing recent files, projects, etc.


;; [[file:init.org::*Dashboard][Dashboard:1]]
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
;; Dashboard:1 ends here

;; Shackle
;; Set rules for how these kinds of windows are displayed: size, position alignment, whether they steal focus, etc.


;; [[file:init.org::*Shackle][Shackle:1]]
(use-package shackle
  :config
  (setq shackle-rules
        '(
          (("\\*Help\\*" helpful-mode) :align right :size 0.35 :select nil)

		  ("\\*compilation\\*" :align below :size 0.25 :noselect t)
		  (("\\*Flycheck errors\\*" flycheck-error-list-mode) :align below :size 0.25 :select t)
		  (("\\*Warnings\\*" special-mode) :align below :size 0.2 :select nil)

		  (("\\*Treemacs\\*" treemacs-mode) :align left :size 0.15 :select t)
		  
		  ("\\*Messages\\*" :align below :size 0.3 :select nil)
		  ("\\*Backtrace\\*" :align below :size 0.3 :select t))
		)
  (shackle-mode 1))
;; Shackle:1 ends here

;; Popper
;; Manager for "popup" windows. Matching window names or buffer modes are treated as inferior, easily dismissable, and
;; toggleable. You can cycle through these kinds of windows, list them all, group them by project, etc.


;; [[file:init.org::*Popper][Popper:1]]
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
          "\\*Treemacs\\*"
          special-mode
          treemacs-mode
          help-mode
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
;; Popper:1 ends here

;; Solaire
;; De-emphasize these kinds of windows by dimming their background.

;; [[file:init.org::*Solaire][Solaire:1]]
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))
;; Solaire:1 ends here

;; Misc
;; Several minor UI packages that don't need their own TOC heading.

;; Highlight the current line:

;; [[file:init.org::*Misc][Misc:1]]
(use-package hl-line
  :hook (after-init . global-hl-line-mode))
;; Misc:1 ends here



;; Display page breaks as <hr>-style lines (used in Dashboard):

;; [[file:init.org::*Misc][Misc:2]]
(use-package page-break-lines
  :init (page-break-lines-mode))
;; Misc:2 ends here



;; On Mac, make the title bar match the current theme:

;; [[file:init.org::*Misc][Misc:3]]
(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode +1))
;; Misc:3 ends here

;; Project Management
;; For now, use ~project.el~ for project management, with a few plugins for interop.


;; [[file:init.org::*Project Management][Project Management:1]]
(use-package project
  :ensure nil
  :defer t
  :init
  (setq project-list-file (file-name-concat
						   (file-name-parent-directory user-init-file)
						   "projects")))
;; Project Management:1 ends here



;; Separate ibuffer lists into projects.

;; [[file:init.org::*Project Management][Project Management:2]]
(use-package ibuffer-project
  :init
  (defun my/ibuffer-project-filter-predicate ()
	(setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
	(unless (eq ibuffer-sorting-mode 'project-file-relative)
	  (ibuffer-do-sort-by-project-file-relative)))
  :hook (ibuffer . my/ibuffer-project-filter-predicate))
;; Project Management:2 ends here

;; Treemacs
;; Having a top-down filesystem view of your project is handy. We use a couple different plugins for this.

;; Note that ~project.el~ compatibility is built-in; the ~project-treemacs~ project on MELPA uses Treemacs for project root
;; definitions and only works while the Treemacs buffer is open; not a great UX.


;; [[file:init.org::*Treemacs][Treemacs:1]]
(use-package treemacs
  :custom
  (treemacs-width 30)
  (treemacs-follow-after-init t)
  (treemacs-sotring 'alphabetic-case-insensitive-asc)
  (treemacs-indent-guide-style 'line)
  :config
  (treemacs-project-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  (treemacs-git-commit-diff-mode t)
  (treemacs-indent-guide-mode t))
;; Treemacs:1 ends here


   
;; Evil integration plugin

;; [[file:init.org::*Treemacs][Treemacs:2]]
(use-package treemacs-evil
  :after treemacs)
;; Treemacs:2 ends here



;; Add icons to the file lists

;; [[file:init.org::*Treemacs][Treemacs:3]]
(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme 'nerd-icons))
;; Treemacs:3 ends here

;; Corfu
;; This is the primary ~completion-at-point~ UI. It calls CAPFs and displays their outputs, allowing you to filter, search,
;; and select from them. It does not provide any completion backends on its own. For that we use both Cape and LSP.


;; [[file:init.org::*Corfu][Corfu:1]]
(use-package corfu
  :init
  (global-corfu-mode)

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-enable t)
  (corfu-popupinfo-delay 0.1)
  (corfu-preselect 'prompt)
  (completion-ignore-case t)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  :general
  (general-define-key
   :states '(normal insert)
   "C-SPC" 'completion-at-point))
;; Corfu:1 ends here



;; Adds nerd-icons to Corfu based on the type of thing being completed (variable, function, file, etc)

;; [[file:init.org::*Corfu][Corfu:2]]
(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
;; Corfu:2 ends here

;; Cape
;; Completion-at-point extensions library, which provides both a couple useful built-in CAPFs and several functions to
;; compose and transform other CAPFs. Notably includes the ability to adapt company backends to generic CAPFs that Corfu
;; can use.


;; [[file:init.org::*Cape][Cape:1]]
(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  :general
  (start/leader-keys
    "a" '(cape-prefix-map :wk "Cape")))
;; Cape:1 ends here

;; Vertico
;; Whereas Corfu handles ~completion-at-point-functions~, Vertico handles ~completing-read~ (in-buffer vs minibuffer
;; completions). Vertico handles things like listing history and candidates for ~M-x~, searching for files with
;; ~find-file~, etc. Rule of thumb: anything that pops up at the bottom of the screen uses this.

;; Vertico also interacts with Consult: Vertico is the display, Consult is the backend.


;; [[file:init.org::*Vertico][Vertico:1]]
(use-package vertico
  :custom
  (vertico-scroll-margin 5)
  (vertico-count 15)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode))
;; Vertico:1 ends here

;; Marginalia
;; Adds help and descriptions to the right of autocomplete suggestions in Vertico:


;; [[file:init.org::*Marginalia][Marginalia:1]]
(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))
  :init (marginalia-mode))
;; Marginalia:1 ends here



;; Also adds nerd-icons to suggestions based on their type:

;; [[file:init.org::*Marginalia][Marginalia:2]]
(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
;; Marginalia:2 ends here

;; Consult
;; Provides a series of ~completing-read~ backends; in other words, it is to Vertico what Cape is to Corfu. Also adds
;; descriptions compatible with Marginalia. It has a lot of different functions for things like looking up buffers, listing
;; files, finding things (via ~rg~ or ~fd~, or lines within a file, etc). Most of the functions can also be scoped to the
;; current project.


;; [[file:init.org::*Consult][Consult:1]]
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
;; Consult:1 ends here

;; Orderless
;; A better way of searching through completion candidates in both modes. The search pattern is divided into
;; space-separated components, and then candidates are matched against them in any order.


;; [[file:init.org::*Orderless][Orderless:1]]
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides `((file (styles basic partial-completion)))))
;; Orderless:1 ends here

;; Markdown
;; Everybody's favorite README language


;; [[file:init.org::*Markdown][Markdown:1]]
(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-italic-underscore t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-fontify-whole-heading-line t)
  (markdown-fontify-code-blocks-natively t))
;; Markdown:1 ends here

;; Misc
;; Various other minor packages that don't fit in the other sections.

;; Colorize pairs of delimiters (parens, braces, angles, etc):

;; [[file:init.org::*Misc][Misc:1]]
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; Misc:1 ends here



;; Automatically trim whitespace from edited lines:

;; [[file:init.org::*Misc][Misc:2]]
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))
;; Misc:2 ends here

;; Performance and Finalize

;; [[file:init.org::*Performance and Finalize][Performance and Finalize:1]]
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; Performance and Finalize:1 ends here
