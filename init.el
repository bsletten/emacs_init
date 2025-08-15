;; Brian Sletten's init.el file across machines.

;; Common

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "M-S-z") 'zap-up-to-char)
                
;; Scrim

(setq server-use-tcp t)
(server-start)
(require 'org-protocol)

;; Package Management

(require 'package)
(add-to-list 'package-archives
   '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list 'package-archives
   '("org" . "https://orgmode.org/elpa/")
 t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Line-numbering

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode `(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Remap buffer list to ibuffer

(global-set-key [remap list-buffers] 'ibuffer)

;; Remap other window to M-o for convenience.

(global-set-key (kbd "M-o") 'other-window)

;; Visual Elements

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Themes

;; (load-theme 'tango-dark)

;; Command Logging

;; (use-package command-log-mode)

;; Turn on ability to switch windows with SHIFT and arrow keys

(windmove-default-keybindings)

;; Org-mode

(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-n") #'org-next-link)
  (define-key org-mode-map (kbd "M-p") #'org-previous-link))

(setq org-log-done t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(org-agenda-files
   '("~/Dropbox/org-mode-files/emacs.org"
     "~/Dropbox/org-mode-files/misc.org"
     "~/Dropbox/org-mode-files/elsevier.org"     
     "~/Dropbox/org-mode-files/attending.org"
     "~/Dropbox/org-mode-files/projects.org"
     "~/Dropbox/org-mode-files/syssetup.org"
     "~/Dropbox/org-mode-files/pinboard-bookmarks.org"))
 
 '(package-selected-packages
   '(org)))

(setq org-directory "~/Dropbox/org-mode-files")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
        '(("capture"
           "Org Protocol Capture"
           entry
           (file "") ; an empty string defaults to org-default-notes-file
           (function (lambda ()
                       (string-join
                        '("* %:description"
                          "%:annotation"
                          "%i"
                          "%?")
                        "\n")))
           :empty-lines 1)))

;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell      . t)
   (python     . t)
   (js         . t)
   (emacs-lisp . t)
   (ruby       . t)
   (clojure    . t)
   (css        . t)
   (dot        . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; Zotero

(require 'zotero)
(require 'zotero-browser)

;; Quarto
(require 'quarto-mode)

;; RSS-Feeds

; (global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '(("https://huyenchip.com/feed.xml" blog ai ml)
	("https://lilianweng.github.io/index.xml" blog ai ml)
        ("https://sachachua.com/blog/feed/" blog emacs)
	("https://nicholas.carlini.com/writing/feed.xml" ml ai security threats)
        ("https://realpython.com/atom.xml" blog python)
       )
)

(defun sk/elfeed-db-remove-entry (id)
  "Removes the entry for ID"
  (avl-tree-delete elfeed-db-index id)
  (remhash id elfeed-db-entries))

(defun sk/elfeed-search-remove-selected ()
  "Remove selected entries from database"
  (interactive)
  (let* ((entries (elfeed-search-selected))
	 (count (length entries)))
    (when (y-or-n-p (format "Delete %d entires?" count))
      (cl-loop for entry in entries
	       do (sk/elfeed-db-remove-entry (elfeed-entry-id entry)))))
  (elfeed-search-update--force))
