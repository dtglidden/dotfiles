(if (not (getenv "TERM_PROGRAM"))
  (let ((path (shell-command-to-string
          "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
    (setenv "PATH" path)))
(setq exec-path (append (split-string (getenv "PATH") ":")
                        (list exec-directory)))

(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/my-packages/PubMode/lisp")
(autoload 'pub-med "pub" "PubMed Interface for Emacs" t)
(global-set-key (kbd "C-c p") 'pub-med)

(add-to-list 'load-path "~/.emacs.d/my-packages/LilyPond/site-lisp")
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))


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

(setq-default indent-tabs-mode nil)

(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(load-file "~/.emacs.d/dtg-elfeed.el")

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'inferior-ess-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'latex-mode-hook 'turn-on-visual-line-mode)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

(add-to-list 'initial-frame-alist
             '(fullscreen . maximized))
(add-to-list 'default-frame-alist
             '(font . "Menlo-18"))

;; Makes sure dired ls function works properly on Mac
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; Enables a dired feature to launch native programs for selected files
(dired-launch-enable)

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

(advice-add 'evil-previous-line :around 'evil-previous-line--check-visual-line-mode)

; Give us back Ctrl+U for vim emulation
;(setq evil-want-C-u-scroll t)
; You really need C-u for other things. Suck it up and use M-v to page-up

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

(require 'evil)
(evil-mode t)
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
        dired-mode
        vterm-mode))
  (cl-pushnew mode evil-emacs-state-modes))

(global-linum-mode)
(load-theme 'zenburn t)
