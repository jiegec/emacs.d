;;; init-utils.el --- Init file for utils. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)
(require 'init-builtins)

(use-package keyfreq
  :ensure t
  :init
  (evil-leader/set-key
    "kf" 'keyfreq-show)
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode t)
  :diminish
  whitespace-cleanup-mode)

(use-package expand-region
  :ensure t
  :defer
  :init
  (evil-leader/set-key
    "xx" 'er/expand-region
    "xz" 'er/contract-region)
  :commands
  er/contract-region
  er/expand-region)

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :demand
  :init
  (evil-leader/set-key
    "ag" 'counsel-ag
    "rg" 'counsel-rg
    "gg" 'counsel-git-grep
    "gl" 'counsel-git-lot
    "gs" 'counsel-git-stash
    "lk" 'counsel-locate
    "rc" 'counsel-recoll)
  :bind
  ("C-s" . counsel-grep-or-swiper)
  ("C-x 8 RET" . counsel-unicode-char)
  ("C-x C-f" . counsel-find-file)
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h B" . counsel-descbinds)
  ("C-c a" . swiper-all)
  :config
  (my/set ivy-use-virtual-buffers t
          ivy-display-style 'fancy
          swiper-action-recenter t)
  (ivy-mode t)
  :diminish
  ivy-mode)

(use-package osx-dictionary
  :ensure t
  :bind
  ("C-c d" . osx-dictionary-search-input)
  :config
  (my/set osx-dictionary-dictionary-choice '("Simplified Chinese"
                                              "Simplified Chinese - English"
                                              "American English"
                                              "American English Thesaurus"
                                              "British English"
                                              "British English Thesaurus"))
  ;; (my/set osx-dictionary-dictionary-choice '("American English"))
  )

(use-package crux
  :ensure t
  :bind
  ("C-c i" . crux-ispell-word-then-abbrev)
  ("C-c I" . crux-find-user-init-file)
  ("C-c s" . crux-find-shell-init-file))

(use-package super-save
  :ensure t
  :config
  (my/set super-save-auto-save-when-idle t)
  (super-save-mode t)
  :diminish
  super-save-mode)

(use-package smart-mode-line
  :ensure t
  :config
  (my/set sml/no-confirm-load-theme t ;; This comes first or it will ask you whether load themes that load lisp code
          sml/theme 'respectful)
  (add-to-list 'sml/replacer-regexp-list '("^~/Library/Mobile Documents/com~apple~CloudDocs/" ":iCloud:") t)
  (add-to-list 'sml/replacer-regexp-list '("^/Volumes/Data/" ":Data:") t)
  (sml/setup))

(use-package company
  :ensure t
  :config

  (use-package company-statistics
    :ensure t
    :config
    (company-statistics-mode t))

  (use-package company-quickhelp
    :ensure t
    :config
    (my/set company-quickhelp-delay 5)
    (company-quickhelp-mode 1))

  (my/set company-idle-delay 0.5)
  (global-company-mode 1)
  :diminish
  company-mode)

(use-package region-state
  :ensure t
  :config
  (region-state-mode 1))

(use-package editorconfig
  :ensure t
  :diminish
  editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :ensure t
  :config
  (my/set undo-tree-auto-save-history t
          undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo-tree-history")))
  (global-undo-tree-mode 1)
  :diminish
  undo-tree-mode)

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  :diminish
  aggressive-indent-mode)

(use-package which-key
  :ensure t
  :config
  (my/set which-key-idle-delay 3.0
          which-key-allow-evil-operators t)
  (which-key-mode 1)
  :diminish
  which-key-mode)

(use-package flycheck
  :ensure t
  :config
  (my/set flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode 1))

;; (defvar-local org-html-export-on-save nil)

;; (defun org-html-export-to-html-async ()
;;   "Export org to html async."
;;   (org-html-export-to-html t))

;; (defun toggle-org-html-export-on-save ()
;;   "Toggle `org-html-export-on-save'."
;;   (interactive)
;;   (if (eq major-mode 'org-mode)
;;       (if (memq 'org-html-export-to-html-async after-save-hook)
;;           (progn
;;             (remove-hook 'after-save-hook 'org-html-export-to-html-async t)
;;             (setq-local org-html-export-on-save nil)
;;             (message "Disabled org html export on save for current buffer..."))
;;         (add-hook 'after-save-hook 'org-html-export-to-html-async nil t)
;;         (setq-local org-html-export-on-save t)
;;         (message "Enabled org html export on save for current buffer..."))
;;     (message "Only effect in org mode buffers")))

(use-package hydra
  :ensure t
  :config

  (defhydra hydra-toggle ()
    "
_a_ abbrev-mode:              %`abbrev-mode
_d_ debug-on-error:           %`debug-on-error
_e_ realtime-elisp-doc        %`my/realtime-elisp-doc-enabled
_t_ truncate-lines:           %`truncate-lines
_w_ whitespace-mode:          %`whitespace-mode
_i_ indent-tabs-mode:         %`indent-tabs-mode
_p_ smartparens-mode:         %`smartparens-mode
_c_ counsel-mode:             %`counsel-mode
_f_ auto-fill-mode:           %`auto-fill-function

"
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("e" my/realtime-elisp-doc nil)
    ("t" toggle-truncate-lines nil)
    ("w" whitespace-mode nil)
    ("i" toggle-indent-tabs-mode nil)
    ("p" smartparens-mode nil)
    ("c" counsel-mode nil)
    ("f" auto-fill-mode nil)
    ("q" nil "quit")
    ("<f5>" nil "quit"))
  (global-set-key (kbd "<f5>") 'hydra-toggle/body))

(use-package magit
  :ensure t
  :bind
  ("C-c G" . magit-status))

(use-package helm
  :ensure t
  :defer
  :config
  (my/set helm-candidate-number-limit 1024
          helm-input-idle-delay 0.01
          helm-move-to-line-cycle-in-source t))

(use-package yasnippet
  :ensure t
  :config
  (my/set yas-snippet-dirs '("~/.emacs.d/snippets"
                             "~/.emacs.d/yasnippet-snippets/snippets"))
  (yas-global-mode t)
  :diminish
  yas-minor-mode)

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(use-package ace-window
  :ensure t
  :bind
  ("M-p" . ace-window)
  :config
  (my/set aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; (use-package ycmd
;;   :ensure t
;;   :defer
;;   :config

;;   (use-package ycmd-next-error)

;;   (use-package ycmd-eldoc
;;     :config
;;     (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

;;   (use-package company-ycmd
;;     :ensure t
;;     :config
;;     (company-ycmd-setup))

;;   (use-package flycheck-ycmd
;;     :ensure t
;;     :config
;;     (flycheck-ycmd-setup))

;;   (my/set ycmd-server-command '("python" "/Volumes/Data/ycmd/ycmd")))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1)
  :diminish
  volatile-highlights-mode)

;; (use-package evil-escape
;;   :ensure t
;;   :config
;;   (evil-escape-mode 1))

(use-package pdf-tools
  :ensure t
  :defer
  :mode
  (("\\.pdf\\'" . pdf-mode))
  :config
  (pdf-tools-install)
  (my/set pdf-view-resize-factor 1.1
          pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode -1))))

(use-package dtrt-indent
  :ensure t
  :diminish
  dtrt-indent-mode
  :config
  (dtrt-indent-mode 1))

(use-package git-messenger
  :ensure t
  :bind
  ("C-c g" . git-messenger:popup-message)
  :config
  (my/set git-messenger:show-detail t))

(use-package git-timemachine
  :ensure t
  :commands
  (git-timemachine-toggle)
  :init
  ;; (evil-set-initial-state 'git-timemachine-mode 'emacs)
  (add-hook 'git-timemachine-mode-hook 'evil-emacs-state)
  (evil-leader/set-key
    "gt" 'git-timemachine-toggle))

(use-package git-link
  :ensure t
  :commands
  (git-link)
  :init
  (evil-leader/set-key
    "gl" 'git-link
    "gc" 'git-link-commit
    "gh" 'git-link-homepage))

(use-package quickrun
  :ensure t
  :bind
  ("C-c r" . quickrun))

(use-package realgud
  :ensure t
  :config
  (add-hook 'realgud:gdb 'tool-bar-mode))

(use-package simpleclip
  :ensure t
  :config
  (simpleclip-mode 1))

(use-package make-it-so
  :ensure t
  :config
  (mis-config-default))

(use-package vdiff
  :ensure t)

(use-package refine
  :ensure t
  :commands
  refine
  :init
  (evil-leader/set-key
    "rf" 'refine))

(use-package find-file-in-project
  :ensure t
  :bind
  ("s-F" . find-file-in-current-directory))

(use-package projectile-sift
  :commands
  projectile-sift
  :ensure t)

(use-package projectile
  :ensure t
  :demand t
  :bind
  ("s-p" . projectile-commander)
  :bind-keymap*
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t)
  (my/set projectile-completion-system 'ivy
          projectile-use-git-grep t
          projectile-enable-caching t
          projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
          projectile-switch-project-action 'projectile-commander)
  (def-projectile-commander-method ?s
    "Open a *eshell* buffer for the project."
    (projectile-run-eshell))
  (def-projectile-commander-method ?S
    "Find sift on project"
    (call-interactively #'projectile-sift))
  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (projectile-compile-project nil))
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
  (def-projectile-commander-method ?F
    "Git fetch."
    (call-interactively #'magit-status)
    (call-interactively #'magit-fetch)))

(use-package counsel-projectile
  :ensure t
  :bind
  ("s-P" . counsel-projectile)
  ("s-f" . counsel-projectile-find-file)
  ("s-b" . counsel-projectile-switch-to-buffer)
  :config
  (counsel-projectile-mode 1))

(use-package suggest
  :ensure t)

(use-package tldr
  :ensure t
  :bind
  ("C-c t" . tldr))

(use-package dumb-jump
  :ensure t
  :bind
  ("C-c j" . dumb-jump-go)
  ("C-c J" . dumb-jump-back)
  ("C-c l" . dumb-jump-quick-look))

;; (quelpa '(lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode"))
;; (el-get-bundle lsp-mode
;;   :url "https://github.com/emacs-lsp/lsp-mode.git")
(use-package lsp-mode
  :diminish
  lsp-mode
  :config
  (require 'lsp-imenu)
  (global-set-key (kbd "M-F") #'lsp-format-buffer)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

;; (el-get-bundle lsp-ui
;;   :url "https://github.com/emacs-lsp/lsp-ui.git")
;; (quelpa '(lsp-ui :fetcher github :repo "emacs-lsp/lsp-ui"))
(use-package lsp-ui
  :commands
  lsp-ui-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1)))
  (my/set lsp-ui-doc-include-signature nil
          lsp-ui-sideline-show-hover nil
          lsp-ui-sideline-show-symbol nil))

(use-package company-lsp
  :ensure t
  :after lsp-mode
  :init
  (my/set company-lsp-async t)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-lsp)))

(use-package emr
  :diminish
  emr-c-mode
  :commands
  emr-initialize
  :init
  (evil-leader/set-key
    "re" 'emr-show-refactor-menu)
  (add-hook 'prog-mode-hook 'emr-initialize))

(use-package ein
  :defer
  :ensure t)

;; (use-package xcode-mode
;;   :ensure t
;;   :config
;;   (my/set xcode-completing-read-function 'ivy-completing-read)
;;   (add-hook 'objc-mode-hook 'xcode-mode))

;; (quelpa '(electric-align :fetcher github :repo "zk-phi/electric-align"))
;; (require 'electric-align)
;; (add-hook 'prog-mode-hook 'electric-align-mode)

;; (quelpa
;;  '(discourse :fetcher github :repo "lujun9972/discourse-api"))
;; (el-get-bundle discourse
;;   :url "https://github.com/lujun9972/discourse-api.git")
;; (use-package discourse)

;; (el-get-bundle discourse
;;   :url "https://github.com/lujun9972/discourse-view.el.git")
;; (quelpa
;;  '(discourse-view :fetcher github :repo "lujun9972/discourse-view.el"))
;; (use-package discourse-view)

(use-package vterm
  :load-path "/Volumes/Data/emacs-libvterm"
  :commands
  vterm
  :init
  (evil-leader/set-key
    "sh" 'vterm))

(use-package neotree
  :ensure t
  :commands
  neotree-toggle
  :init
  (evil-leader/set-key
    "nt" 'neotree-toggle)
  :defer)

(use-package imenu-list
  :ensure t
  :commands
  imenu-list-smart-toggle
  :init
  (evil-leader/set-key
    "il" 'imenu-list-smart-toggle)
  :config
  (my/set imenu-list-focus-after-activation t
          imenu-list-position 'left
          imenu-list-auto-resize t))

(use-package copy-as-format
  :ensure t
  :commands
  (copy-as-format)
  :init
  (defun copy-as-format-choose ()
    (interactive)
    (let ((current-prefix-arg "C-u"))
      (call-interactively 'copy-as-format))
    (simpleclip-set-contents (car kill-ring)))
  (evil-leader/set-key
    "cp" 'copy-as-format-choose))

(use-package indent-guide
  :ensure t
  :diminish
  indent-guide-mode
  :config
  (indent-guide-global-mode 1))

(use-package ivy-xref
  :ensure t
  :init
  (my/set xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package daemons
  :ensure t
  :commands
  daemons)

(provide 'init-utils)
;;; init-utils.el ends here
