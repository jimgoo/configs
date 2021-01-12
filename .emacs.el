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
;;(customize-set-variable 'tabbar-background-color "gray20")
(customize-set-variable 'tabbar-separator '(2.0))
(customize-set-variable 'tabbar-use-images nil)
(tabbar-mode)

;; line numbers
;;(global-linum-mode 1)
;; rule and margin after line numbers
;;(setq linum-format "%4d \u2502 ")

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

;; enable scrolling through tmux
(xterm-mouse-mode t)
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;; Take y for yes, n for no
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-l" 'goto-line)
; (global-set-key "\M-[" 'tabbar-backward)
; (global-set-key "\M-]" 'tabbar-forward)
(global-set-key "\C-r" 'comment-region)
(global-set-key "\C-t" 'uncomment-region)
; (global-set-key "\C-i" 'indent-region)

;; copy-paste to terminal
; (setq x-select-enable-clipboard t)

;; color theme setting
(straight-use-package 'color-theme-modern)
(load-theme 'calm-forest t t)
(enable-theme 'calm-forest)

;; Turn CUA mode on
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) 

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; stop creating those #auto-save# files
(setq auto-save-default nil)

;;(straight-use-package 'elpy)
;;(elpy-enable)

;;(straight-use-package 'color-identifiers-mode)
;;(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; Yaml mode
(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
