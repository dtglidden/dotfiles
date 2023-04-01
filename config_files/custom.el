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
   '(auto-package-update use-package dired-launch dired-toggle-sudo noaa bbdb bbdb-vcard elfeed elfeed-tube elfeed-tube-mpv org-bullets vterm helpful lua-mode gnu-apl-mode ess-smart-underscore docker-compose-mode dockerfile-mode php-mode ein projectile package-utils ess-view ledger-mode org-ref auto-complete hledger-mode ess zenburn-theme minesweeper magit helm evil-visual-mark-mode ensime))
 '(python-indent-offset 2)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
