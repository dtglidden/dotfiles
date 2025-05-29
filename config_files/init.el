(if (not (getenv "TERM_PROGRAM"))
  (let ((path (shell-command-to-string
          "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
    (setenv "PATH" path)))
(setq exec-path (append (split-string (getenv "PATH") ":")
                        (list exec-directory)))

(require 'package)

(dolist (pkg '(("org" . "https://orgmode.org/elpa/")
               ("melpa" . "https://melpa.org/packages/")
               ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (cl-pushnew pkg package-archives))

(cl-pushnew "~/.emacs.d/my-packages/PubMode/lisp" load-path)
(autoload 'pub-med "pub" "PubMed Interface for Emacs" t)
(global-set-key (kbd "C-c p") 'pub-med)

(cl-pushnew "~/.emacs.d/my-packages/LilyPond/site-lisp" load-path)
(autoload 'LilyPond-mode "lilypond-mode")
(cl-pushnew '("\\.ly$" . LilyPond-mode) auto-mode-alist)

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))


(defun dtg-restart-emacs ()
  (interactive)
  (when (y-or-n-p "Restart emacs?")
    (restart-emacs)))
(global-set-key (kbd "C-c r") 'dtg-restart-emacs)


;; Activate installed packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; UI
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

(menu-bar-mode -1)          ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Coding defaults
(setq-default indent-tabs-mode nil)
(show-paren-mode t)
(setq show-trailing-whitespace nil)
(transient-mark-mode t)
(setq python-indent-offset 2)

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(dolist (file `(,(expand-file-name "bookmarks" user-emacs-directory)
                ,(expand-file-name "~/.elfeed/index")))
  (cl-pushnew file recentf-exclude))

;; which-key setup
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(load-file (expand-file-name "dtg-elfeed.el" user-emacs-directory))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'inferior-ess-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'latex-mode-hook 'turn-on-visual-line-mode)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(cl-pushnew '(fullscreen . maximized) initial-frame-alist)
(cl-pushnew '(font . "Menlo-18") default-frame-alist)


(require 'ess-smart-underscore)
(require 'ess-view)
(setq ess-view--spreadsheet-program "open")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'elfeed-tube)
(elfeed-tube-setup)
(define-key elfeed-show-mode-map (kbd "F") 'elfeed-tube-fetch)
(define-key elfeed-show-mode-map [remap save-buffer] 'elfeed-tube-save)
(define-key elfeed-search-mode-map (kbd "F") 'elfeed-tube-fetch)
(define-key elfeed-search-mode-map [remap save-buffer] 'elfeed-tube-save)

(require 'elfeed-tube-mpv)
(define-key elfeed-show-mode-map (kbd "<return>") 'elfeed-tube-mpv)
(define-key elfeed-show-mode-map (kbd "C-c C-f") 'elfeed-tube-mpv-follow-mode)
(define-key elfeed-show-mode-map (kbd "C-c C-w") 'elfeed-tube-mpv-where)

(define-key elfeed-search-mode-map (kbd "C-c u") 'elfeed-update)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  ;(evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd ">") 'org-metaright)
  (evil-define-key 'normal org-mode-map (kbd "<") 'org-metaleft)
  ;(evil-define-key 'insert ess-r-mode-map (kbd "_") #'ess-insert-assign)
  ;(evil-define-key 'insert inferior-ess-r-mode-map (kbd "_") #'ess-insert-assign)
                                          ;
  ;; SUPPOSED FIX FOR ORG-CYCLE IN NORMAL MODE
  ;(evil-define-key 'normal evil-jumper-mode-map (kbd "TAB") nil)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

  (dolist (mode '(elfeed-search-mode
          elfeed-show-mode
          vterm-mode))
    (cl-pushnew mode evil-emacs-state-modes))

  ;; Allows evil mode to respect visual line mode
  (defun evil-next-line--check-visual-line-mode (orig-fun &rest args)
    (if visual-line-mode
        (apply 'evil-next-visual-line args)
      (apply orig-fun args)))

  (advice-add 'evil-next-line :around 'evil-next-line--check-visual-line-mode)

  (defun evil-previous-line--check-visual-line-mode (orig-fun &rest args)
    (if visual-line-mode
        (apply 'evil-previous-visual-line args)
      (apply orig-fun args)))

  (advice-add 'evil-previous-line :around 'evil-previous-line--check-visual-line-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-aho --group-directories-first"))
  :config
  ;; Makes sure dired ls function works properly on Mac
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"))

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-maybe-insert-subdir))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Enables a dired feature to launch native programs for selected files
(dired-launch-enable)

(load-theme 'zenburn t)

(defun dtg/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dtg/org-mode-visual-fill))
