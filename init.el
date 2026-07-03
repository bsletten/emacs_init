;; Brian Sletten's init.el file across machines.

;; Common

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "M-S-z") 'zap-up-to-char)

;; Path-Management

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/opt/homebrew/bin") ; Apple Silicon
(setenv "PATH" (string-join exec-path path-separator))

;; Scrim

(setq server-use-tcp t)
(server-start)
(require 'org-protocol)

;; Package Management

(require 'package)
;; (add-to-list 'package-archives
;;    '("melpa" . "https://melpa.org/packages/")
;;  t)
;; (add-to-list 'package-archives
;;    '("org" . "https://orgmode.org/elpa/")
;;   t)
;; Add package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
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

;; Which-key: Discover keybindings
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; Undo-tree: Visualize undo history
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

;; Visual Elements

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Themes
'(custom-enabled-themes '(wheatgrass))

(load-theme 'wheatgrass)

;; (load-theme 'tango-dark)

;; Command Logging

;; (use-package command-log-mode)

;; Turn on ability to switch windows with SHIFT and arrow keys

;; (windmove-default-keybindings)

;; Dired

(use-package image-dired
  :ensure nil  ; Built-in
  :bind (:map dired-mode-map
              ("C-t i" . image-dired)))

;; Better: dired-rainbow for color-coded files
(use-package dired-rainbow
  :ensure t
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl")))

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Show git status in dired
(use-package diff-hl
  :ensure t
  :hook (dired-mode . diff-hl-dired-mode))

;; Find files by name
(use-package find-dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-f" . find-name-dired)))

;; fd integration (faster find)
(use-package fd-dired
  :ensure t)

;; Org-mode

(require 'org)
(load "~/Dropbox/org-mode-files/org-setup.el")
;;(global-set-key (kbd "C-c l") 'org-store-link)
;;(global-set-key (kbd "C-c a") 'org-agenda)
;;(global-set-key (kbd "C-c c") 'org-capture)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-n") #'org-next-link)
  (define-key org-mode-map (kbd "M-p") #'org-previous-link))

(setq org-log-done t)

(setq org-link-abbrev-alist
      '(("bugzilla"        . "https://10.1.2.9/bugzilla/show_bug.cgi?id=")
        ("Nu Html Checker" . "https://validator.w3.org/nu/?doc=%h")
        ("duckduckgo"      . "https://duckduckgo.com/?q=%s")
        ("omap"            . "https://nominatim.openstreetmap.org/search?q=%s&polygon=1")
        ("ads"             . "https://ui.adsabs.harvard.edu/search/q=%20author%3A\"%s\"")))

;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

;; '(org-agenda-files
;;   '("~/Dropbox/org-mode-files/emacs.org"
;;     "~/Dropbox/org-mode-files/books.org"
;;     "~/Dropbox/org-mode-files/work.org"
;;     "~/Dropbox/org-mode-files/notes.org"     
;;     "~/Dropbox/org-mode-files/elsevier.org"     
;;     "~/Dropbox/org-mode-files/attending.org"
;;     "~/Dropbox/org-mode-files/projects.org"
;;     "~/Dropbox/org-mode-files/syssetup.org"
;;     "~/Dropbox/org-mode-files/pinboard-bookmarks.org"))
 ;;


 
;; '(package-selected-packages
;;   '(org)))

;;(setq org-default-notes-file (concat org-directory "/notes.org"))
;;(setq org-capture-templates
;;        '(("capture"
;;           "Org Protocol Capture"
;;           entry
;;           (file+headline "" "Unfiled") ; an empty string defaults to org-default-notes-file
;;           (function (lambda ()
;;                       (string-join
;;                        '("*** %:description"
;;                          "%:annotation"
;;                          "%i"
;;                          "%?")
;;                        "\n")))
;;           :empty-lines 1)
;;            ("t" "Todo" entry (file+headline "~/Dropbox/org-mode-files/tasks.org" "Inbox")
;;             "* TODO %?\n  %i\n  %a")
;;            ("j" "Journal" entry (file+datetree "~/Dropbox/org-mode-files/journal.org")
;;             "* %?\nEntered on %U\n  %i\n  %a")
;;            ("m" "Meeting" entry (file+headline "~/Dropbox/org-mode-files/meetings.org" "Meetings")
;;             "* MEETING with %? :meeting:\n  %T")
;;          )
;;        )
;;
;; (global-set-key (kbd "C-c c") 'org-capture)

;; Magit

(use-package magit
  :ensure t)

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

;; TRAMP Performance Optimizations
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; Configure org-babel integration
(defun ellama-org-babel-execute ()
  "Execute ellama code blocks in org-mode."
  (interactive)
  (let ((query (org-element-property :value (org-element-at-point))))
    (ellama-ask-about query)))

;; Add keybinding for org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-e") #'ellama-org-babel-execute)))

(defun my-ellama-code-review ()
  "Review selected code for improvements."
  (interactive)
  (ellama-ask-about
   (format "Review this code for best practices, bugs, and improvements:\n\n%s"
           (buffer-substring-no-properties (region-beginning) (region-end)))))

;; Generate documentation automatically
(defun my-ellama-document-function ()
  "Generate documentation for function at point."
  (interactive)
  (ellama-code-add
   "Add comprehensive docstring explaining parameters, return value, and side effects"))

;; Zotero

(require 'zotero)
(require 'zotero-browser)

;; Quarto
(require 'quarto-mode)

;; Rest Client

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;; Mastodon

(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://mastodon.social"
        mastodon-active-user "bsletten"))

;; RSS-Feeds

; (global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '(("https://huyenchip.com/feed.xml" blog ai ml)
	("https://lilianweng.github.io/index.xml" blog ai ml)
        ("https://sachachua.com/blog/feed/" blog emacs)
	("https://nicholas.carlini.com/writing/feed.xml" ml ai security threats)
        ("https://realpython.com/atom.xml" blog python)
        ("https://ruben.verborgh.org/blog/latest.xml" blog data)
        ("https://corwin.bru.st/index.xml" blog emacs)
        ("https://lucidmanager.org/tags/emacs/index.xml" blog emacs)
        ("http://www.reddit.com/r/emacs/.rss" blog reddit emacs)
        ("https://xenodium.com/feed" blog emacs)
        ("https://irreal.org/blog/?feed=rss2" blog emacs)
        ("https://protesilaos.com/master.xml" blog)
        ("https://coredumped.dev/index.xml" blog rust emacs)
        ("https://bzg.fr/en/notes/index.xml" blog emacs)
        ("https://200ok.ch/atom.xml" blog emacs)
        ("https://chrismaiorana.com/feed/" blog emacs)
        ("https://rakhim.exotext.com/rss.xml" blog)
        ("https://taonaw.com/categories/emacs-org-mode/feed.xml" blog emacs org)
        ("https://kyo.iroiro.party/en/rss.xml" blog emacs java lisp)
        ("https://lambdaland.org/index.xml" blog emacs lisp)
        ("https://joshblais.com/index.xml" blog emacs)
        ("https://bzg.fr/en/tags/emacs/feed.xml" blog emacs)
        ("https://scheatkode.com/rss.xml" blog emacs)
        ("https://brainbaking.com/index.xml" blog emacs)
        ("https://www.bloomberg.com/authors/ARbTQlRLRjE/matthew-s-levine.rss" blog finance markets)
        ("https://raw.githubusercontent.com/Olshansk/rss-feeds/main/feeds/feed_anthropic_research.xml" blog anthropic research)
        ("https://blog.bluedot.org/feed" blog bluedot)
        ("https://graphrag.info/feed/" blog knowledgegraph)
        ("https://vllm.ai/blog/rss.xml" blog ai llm)
        ("https://llm-d.ai/blog/atom.xml" blog ai llm)
        ("https://simonwillison.net/atom/everything/" blog ai)
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
