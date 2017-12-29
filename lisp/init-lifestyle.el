;;; init-lifestyle.el --- Init code for lifestyle. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)

(defun connect-irc ()
  "Connect to freenode irc."
  (interactive)
  (erc-tls :server "irc.freenode.net" :port "6697" ;; SSL 6697 non-SSL 6667
           :nick "jiegec"))

(use-package erc
  :defer
  :commands
  erc-tls
  :bind
  ("C-c e" . connect-irc)
  :config
  (my/set erc-nick "jiegec"
          erc-user-full-name "Jiajie Chen"
          erc-server-auto-reconnect t
          erc-rename-buffers t
          erc-interpret-mirc-color t
          erc-kill-buffer-on-part t
          erc-kill-queries-on-quit t
          erc-kill-server-buffer-on-quit t
          erc-autojoin-channels-alist
          '(("freenode.net" "#spanish" "#haskell" "#english" "#emacs" "#clojure" "#vim" "#MacOSX" "#linux" "#archlinux" "#wikipedia-zh"))))

(use-package elfeed
  :ensure t
  :defer
  :bind
  ("C-c w" . elfeed)
  :config
  (my/set elfeed-feeds
          '("http://planet.emacsen.org/atom.xml"
            "http://emacsredux.com/atom.xml"
            "http://batsov.com/atom.xml"
            "http://www.ruanyifeng.com/blog/atom.xml"
            "http://sachachua.com/blog/feed/"
            "http://oremacs.com/atom.xml"
            "http://www.matrix67.com/blog/feed"
            "http://endlessparentheses.com/atom.xml"
            "http://icodeit.org/atom.xml"
            "http://blog.mojang.com/feed.xml"
            "http://jiegec.github.io/feed.xml"
            "http://planet.emacsen.org/zh/atom.xml")
          elfeed-use-curl t))

(use-package org-eww
  :ensure org-eww
  :commands
  org-eww-mode
  :init
  (add-hook 'org-mode-hook 'org-eww-mode))

(use-package htmlize
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :mode
  ("\\.org\\'" . org-mode)
  :config

  (my/set org-agenda-files (list "~/Library/Mobile Documents/com~apple~CloudDocs/main.org")
          org-contacts-files "~/iCloud/contacts.org"
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-pretty-entities t
          org-pretty-entities-include-sub-superscripts t
          org-startup-folded 'showall
          org-startup-indented t
          org-footnote-auto-adjust t
          ;; org-export-async-init-file "~/.emacs.d/lisp/init-org-async.el"
          org-confirm-babel-evaluate nil)

  ;; allow quotes in inline in verbatim and code
  ;; and allow Chinese characters before and after
  (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{\\cc")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[\\cc")
  (setcar (nthcdr 2 org-emphasis-regexp-components) (remove ?\' (car (nthcdr 2 org-emphasis-regexp-components))))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh         . nil)
     (js         . t)
     (C          . t)
     (coq        . t)
     (shell      . t)
     (emacs-lisp . t)
     (tangle     . t)
     (python     . t)
     (haskell    . t)
     (dot        . t)
     (ruby       . t)))

  (use-package ox-latex
    :config
    ;; From Kuashal Modi's configuation
    ;; Prevent an image from floating to a different location.
    ;; http://tex.stackexchange.com/a/8633/52678
    (add-to-list 'org-latex-packages-alist '("" "float"))
    ;; Prevent tables/figures from one section to float into another section
    ;; http://tex.stackexchange.com/a/282/52678
    (add-to-list 'org-latex-packages-alist '("section" "placeins"))
    :config
    ;; org-latex-preview
    (add-to-list 'org-latex-packages-alist '("mathscr" "eucal"))
    (add-to-list 'org-latex-packages-alist '("" "latexsym"))
    ;; XeLaTeX
    ;; (my/set org-latex-compiler "xelatex")
    (my/set org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
    (add-to-list 'org-latex-packages-alist "\\setCJKmainfont{Songti SC}")
    (add-to-list 'org-latex-packages-alist '("" "xeCJK"))

    ;; Code listing in LaTeX
    (my/set org-latex-listings 'minted
            org-latex-minted-options '(("linenos")
                                       ("numbersep" "5pt")
                                       ("breaklines")))
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))
  (use-package ox-clip
    :ensure t)
  (use-package ox-gfm
    :ensure t)
  (use-package ox-odt
    :config
    (my/set org-odt-preferred-output-format "docx")))

;; (quelpa '(ox-reveal :fetcher github :repo "alvarogonzalezsotillo/org-reveal"))
(el-get-bundle ox-reveal
  :url "https://github.com/alvarogonzalezsotillo/org-reveal.git")
(use-package ox-reveal
  :after org
  :config
  (my/set org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.2.0/"
          org-reveal-plugins '(classList markdown zoom notes highlight)))

;; ;; See https://emacs-china.org/t/file/696
;; ;; And http://lists.gnu.org/archive/html/emacs-orgmode/2016-07/msg00136.html
;; (defun check-file-exists-advice (orig-fun
;;                                  &optional arg
;;                                  info
;;                                  params)
;;   ;; Copied from ob-core.el. May not be compatible.
;;   (let* ((org-babel-current-src-block-location
;; 	  (or org-babel-current-src-block-location
;; 	      (nth 6 info)
;; 	      (org-babel-where-is-src-block-head)
;; 	      ;; inline src block
;; 	      (and (org-babel-get-inline-src-block-matches)
;; 		   (match-beginning 0))))
;; 	 (info (if info
;; 		   (copy-tree info)
;; 		 (org-babel-get-src-block-info)))
;; 	 (merged-params (org-babel-merge-params (nth 2 info) params))) 
;;     (when (cdr (assoc :file merged-params))
;;       (unless (file-exists-p (cdr (assoc :file merged-params)))
;;         (error "File does not exist"))))
;;   (funcall orig-fun arg info params))

;; (advice-add 'org-babel-execute-src-block :around #'check-file-exists-advice)

(use-package solarized-theme
  :ensure t
  :config
  (my/set solarized-height-minus-1 1)
  (my/set solarized-height-plus-1 1)
  (my/set solarized-height-plus-2 1)
  (my/set solarized-height-plus-3 1)
  (my/set solarized-height-plus-4 1)
  (my/set solarized-scale-org-headlines nil)
  (unless (custom-theme-enabled-p 'solarized-dark)
    (load-theme 'solarized-dark t)))

(use-package markdown-mode
  :ensure t
  :defer
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package dash
  :ensure t
  :defer
  :config
  (dash-enable-font-lock))

(use-package nlinum
  :ensure t
  :config
  (add-hook 'nlinum-mode-hook
            (lambda ()
              (when nlinum-mode
                (my/set nlinum--width
                        (1+ (length (number-to-string
                                     (count-lines (point-min) (point-max))))))
                ;; (nlinum--flush)
                )))
  (global-nlinum-mode 1))

(use-package langtool
  :ensure t
  :commands
  langtool-check
  :config
  (my/set langtool-language-tool-jar "/usr/local/opt/languagetool/libexec/languagetool-commandline.jar"
          langtool-default-language "en-US"))

;; For save-frame-config
(use-package frame-cmds
  :ensure t)

(use-package wttrin
  :ensure t
  :commands
  wttrin 
  :config
  (my/set wttrin-default-cities '("Beijing" "Shenzhen"))
  (defun wttrin-save-frame (_)
    (window-configuration-to-register :pre-wttrin)
    (delete-other-windows)
    (save-frame-config)
    (set-frame-width (selected-frame) 170)
    (set-frame-height (selected-frame) 48))
  (advice-add 'wttrin :before 'wttrin-save-frame)
  (defun wttrin-restore-frame ()
    (jump-to-frame-config-register)
    (jump-to-register :pre-wttrin))
  (advice-add 'wttrin-exit :after 'wttrin-restore-frame))

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :config
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1)))))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-c $" . flyspell-correct-word-generic)))

;; (use-package yahoo-weather
;;   :ensure t
;;   :defer 10
;;   :config
;;   (my/set yahoo-weather-location "深圳"
;;            yahoo-weather-format "[%(weather) %(atmosphere-humidity) %(atmosphere-visibility) %(temperature) ℃]"
;;            yahoo-weather-update-interval (* 60 60 6))
;;   (my/enable-modes yahoo-weather-mode))

;; (use-package golden-ratio
;;   :diminish golden-ratio-mode
;;   :config
;;   (add-to-list 'golden-ratio-extra-commands 'ace-window)
;;   (golden-ratio-mode 1))

(use-package google-this
  :ensure t
  :defer
  :diminish google-this-mode
  :config
  (google-this-mode 1))

(use-package eloud
  :ensure t
  :defer
  :config
  (my/set eloud-espeak-path "/usr/local/bin/espeak"))

;; (use-package hyperbole
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-h h") 'hyperboleG))

(use-package steam
  :ensure t
  :defer
  :config
  (my/set steam-username "jackchen2345"))

(use-package sunrise-commander
  :ensure t
  :defer
  :config)

(use-package sunrise-x-buttons
  :ensure t
  :defer
  :config)

(use-package sunrise-x-checkpoints
  :ensure t
  :defer
  :config)

(use-package sunrise-x-loop
  :ensure t
  :defer
  :config)

(use-package sunrise-x-mirror
  :ensure t
  :defer
  :config)

(use-package sunrise-x-modeline
  :ensure t
  :defer
  :config)

(use-package sunrise-x-popviewer 
  :ensure t
  :defer
  :config)

(use-package sunrise-x-tabs
  :ensure t
  :defer
  :config)

(use-package sunrise-x-tree
  :ensure t
  :defer
  :config)

(use-package sunrise-x-w32-addons 
  :ensure t
  :defer
  :config)

(use-package shx
  :ensure t
  :defer
  :config)

(use-package dired-sidebar
  :ensure t
  :init
  (evil-leader/set-key
    "sb" 'dired-sidebar-toggle-sidebar)
  :commands
  (dired-sidebar-toggle-sidebar)
  :config
  (if (display-graphic-p)
      (my/set dired-sidebar-theme 'icons)
    (my/set dired-sidebar-theme 'nerd))
  (my/set dired-sidebar-use-term-integration t)
  (use-package all-the-icons-dired
    :ensure t
    :commands (all-the-icons-dired-mode)))

;; (use-package mode-icons
;;   :ensure t
;;   :config
;;   (mode-icons-mode t))

;; (use-package solaire-mode
;;   :ensure t
;;   :config
;;   (add-hook 'after-change-major-mode-hook #'turn-on-solarie-mode)
;;   (add-hook 'after-revert-hook #'turn-on-solarie-mode)
;;   (add-hook 'minibuffer-setup-hook #'solarie-mode-in-minibuffer))

(provide 'init-lifestyle)
;;; init-lifestyle.el ends here
