(require 'package)
(if (not (getenv "TERM_PROGRAM"))
  (let ((path (shell-command-to-string
          "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
    (setenv "PATH" path)))
(setq exec-path (append (split-string (getenv "PATH") ":")
                        (list exec-directory)))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/my-packages/PubMode/lisp")
(autoload 'pub-med "pub" "PubMed Interface for Emacs" t)
(global-set-key (kbd "C-c p") 'pub-med)

(add-to-list 'load-path "~/.emacs.d/my-packages/LilyPond/site-lisp")
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

(setq-default indent-tabs-mode nil)

(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(load-file "~/.emacs.d/dtg-elfeed.el")

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'inferior-ess-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'latex-mode-hook 'turn-on-visual-line-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LilyPond-pdf-command "open")
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(backup-directory-alist `((".*" \, temporary-file-directory)))
 '(column-number-mode t)
 '(elfeed-search-filter "@1-week-ago +unread")
 '(ess-default-style 'RStudio)
 '(ess-style 'RStudio)
 '(ess-use-eldoc nil)
 '(evil-shift-width 2)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(inhibit-startup-screen t)
 '(ledger-reports
   '(("ledger-report.txt" "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(make-backup-files nil)
 '(org-agenda-files nil)
 '(org-babel-load-languages '((R . t) (emacs-lisp . t)))
 '(org-capture-templates
   '(("i" "GTD In-Basket" entry
      (file+headline "~/org/gtd.org" "In-Basket")
      "* %?
  %i")))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file (concat org-directory "/scratch.org"))
 '(org-latex-listings t)
 '(org-latex-listings-options
   '(("backgroundcolor" "\\color{Apricot}")
     ("breaklines" "true")
     ("columns" "flexible")
     ("basicstyle" "\\small\\ttfamily")))
 '(org-latex-packages-alist
   '(("letterpaper, margin=1in" "geometry" nil)
     ("usenames, dvipsnames" "color" nil)
     ("" "listings" nil)))
 '(org-startup-indented t)
 '(org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE")))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(dired-launch dired-toggle-sudo noaa bbdb bbdb-vcard elfeed elfeed-tube elfeed-tube-mpv org-bullets vterm helpful lua-mode gnu-apl-mode ess-smart-underscore docker-compose-mode dockerfile-mode php-mode ein projectile package-utils ess-view ledger-mode org-ref auto-complete hledger-mode ess zenburn-theme minesweeper magit helm evil-visual-mark-mode ensime))
 '(python-indent-offset 2)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

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

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
  (package-refresh-contents))

;; Activate installed packages
(package-initialize)

(ensure-package-installed 'evil
                          'projectile
                          'magit
                          'org
                          'zenburn-theme
                          'ess
                          'ensime
)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
