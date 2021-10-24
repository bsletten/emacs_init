;; Common

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Package Management

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 t)

;; Org-mode

(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Dropbox/org-mode-files/misc.org" "~/Dropbox/org-mode-files/attending.org" "~/Dropbox/org-mode-files/projects.org" "~/Dropbox/org-mode-files/pinboard-bookmarks.org"))
 '(package-selected-packages
   '(org)))