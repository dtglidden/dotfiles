(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

;; Prevents Emacs from generating annoying backup files
(setq make-backup-files nil)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
   `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))

(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Color settings
(set-foreground-color "green")
(set-background-color "black")
(set-cursor-color "green")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(transient-mark-mode t))

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
                          'org)

; Give us back Ctrl+U for vim emulation
;(setq evil-want-C-u-scroll t)
; You really need C-u for other things. Suck it up and use M-v to page-up


(setq evil-shift-width 2)

(require 'evil)
(evil-mode t)
;(evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
(evil-define-key 'normal org-mode-map (kbd ">") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "<") 'org-metaleft)

(global-linum-mode)
