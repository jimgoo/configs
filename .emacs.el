;; straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable message: "Symbolic link to Git-controlled source file; follow link? (y or n)"
(setq vc-follow-symlinks nil)

;; turn off the menu bar at the top (File, Edit, Options, etc.)
(menu-bar-mode -1)

;; ?
(when (require 'mwheel nil 'noerror)
  (mouse-wheel-mode t))

;; tab-bar for open files
(straight-use-package 'tabbar)
(customize-set-variable 'tabbar-separator '(2.0))
(customize-set-variable 'tabbar-use-images nil)
(tabbar-mode)

;; customize tab organization

;; to show all normal files in one group
;; (setq tabbar-buffer-groups-function
;;       (lambda ()
;; 	(list "All")))

;; to show two groups, user and emacs
(defun my-tabbar-buffer-groups ()
      "Returns the name of the tab group names the current buffer belongs to.
    There are two groups: Emacs buffers (those whose name starts with '*', plus
    dired buffers), and the rest.  This works at least with Emacs v24.2 using
    tabbar.el v1.7."
      (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
                  ((eq major-mode 'dired-mode) "emacs")
                  (t "user"))))
    (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; auto-completion
(straight-use-package 'auto-complete)
(global-auto-complete-mode t)

;; prettier completion mechanism (used by projectile)
(straight-use-package 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; project nav, needs emacs 27
(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; git integration
(straight-use-package 'magit)

;; confirm before closing
;(setq confirm-kill-emacs 'y-or-n-p)

;; auto-reload any open buffers that were modified
(global-auto-revert-mode 1)
;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;----------------------------------------------------------------------------------
;; key mappings

;; enable scrolling through tmux
(xterm-mouse-mode t)
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;; Turn CUA mode on
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1)               ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t)

;; Take y for yes, n for no
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-r" 'comment-region)
(global-set-key "\C-t" 'uncomment-region)
; (global-set-key "\C-i" 'indent-region)

;; copy-paste to terminal
; (setq x-select-enable-clipboard t)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; stop creating those #auto-save# files
(setq auto-save-default nil)

;; disable annoying Ctrl+z shortcut that exits to background
;; and requires `fg` command to resume
(global-unset-key (kbd "C-z"))

;; undo/redo like normal
(straight-use-package 'undo-tree)

;; delete selection when typing over it
(delete-selection-mode 1)

;;----------------------------------------------------------------------------------
;; language modes

;; yaml mode
(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; docker mode
(straight-use-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; markdown syntax highlighting
(straight-use-package 'markdown-mode)

;; python coding
;;(straight-use-package 'elpy)
;;(elpy-enable)

;; Enable Flycheck
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(straight-use-package 'flycheck)
;; permanently enable syntax checking with Flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;;Anaconda support
;(straight-use-package 'conda)
;(setq conda-env-home-directory "/home/jimmie/miniconda3")
;;get current environment--from environment variable CONDA_DEFAULT_ENV
;(conda-env-activate 'getenv "CONDA_DEFAULT_ENV")
;(conda-env-autoactivate-mode t)
;(setq-default mode-line-format (cons mode-line-format '(:exec conda-env-current-name)))

;; R (Emacs Speaks Statistics)
(straight-use-package 'ess)

;;----------------------------------------------------------------------------------
;; project navigation tree

(straight-use-package 'neotree)
;; NeoTree can be opened (toggled) at projectile project root as follows
(setq projectile-switch-project-action 'neotree-projectile-action)
(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

;; default toggle method (no projectile)
;; (global-set-key [f8] 'neotree-toggle)

;; projectile toggle method: highlights opened file in tree for projects
(global-set-key [f8] 'neotree-project-dir)

;;----------------------------------------------------------------------------------
;; color theme setting

;;(straight-use-package 'color-identifiers-mode)

;; (straight-use-package 'color-theme-modern)
;; (load-theme 'calm-forest t t)
;; (enable-theme 'calm-forest)

;; most of these have blue backgrounds
;(straight-use-package 'sublime-themes)
;(load-theme 'brin t)
;(load-theme 'spolsky t)
;(load-theme 'odersky t)
;(load-theme 'hickey t)

;; sublime looking version
;(straight-use-package 'monokai-theme)
;(load-theme 'monokai t)

;; too bright for a dark theme
;(straight-use-package 'zenburn-theme)
;(load-theme 'zenburn t)

;; gives blue background
;(straight-use-package 'solarized-theme)
;(load-theme 'solarized-dark t)

(straight-use-package 'spacemacs-theme)
(load-theme 'spacemacs-dark t)

;; clicking through the file name can't be done with this as easily
;(straight-use-package 'spaceline)
;;(spaceline-spacemacs-theme)
;(spaceline-emacs-theme)

;; can't see list of autocompletions due to same color background as text
;; (straight-use-package 'doom-themes)
;; (load-theme 'doom-one t)

;;----------------------------------------------------------------------------------
;; custom

;;(add-hook 'after-init-hook 'global-color-identifiers-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" default))
 '(tabbar-separator '(2.0))
 '(tabbar-use-images nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
