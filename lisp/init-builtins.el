;;; init-builtins --- The init file for builtin stuffs. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)
(require 'linum)

(my/disable-modes
  tool-bar-mode
  menu-bar-mode
  scroll-bar-mode
  ido-mode
  cua-mode)

(diminish 'eldoc-mode)

(my/enable-modes
  column-number-mode
  size-indication-mode
  tooltip-mode
  delete-selection-mode
  global-eldoc-mode
  minibuffer-depth-indicate-mode
  show-paren-mode
  global-auto-revert-mode
  save-place-mode)

;; (validate-setq resize-mini-windows t)

;; (let ((shell-file-name "/usr/local/bin/fish"))
;;   (setenv "PATH" (shell-command-to-string "echo $PATH")))

(my/set resize-mini-windows t
        shell-file-name "/usr/local/bin/zsh"
        inhibit-startup-message t
        inhibit-splash-screen t
        auto-save-interval 20
        auto-save-timeout 3
        history-delete-duplicates t
        user-full-name "Jiajie Chen"
        user-mail-address "jiegec@qq.com"
        load-prefer-newer t
        indent-tabs-mode nil
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control 5
        switch-to-buffer-preserve-window-point t
        enable-recursive-minibuffers t
        tab-always-indent 'complete
        gc-cons-threshold 16777216 ;; 16MB
        large-file-warning-threshold 268435456 ;; 256MB
        auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/\\1" t))
        backup-directory-alist '((".*" . "~/.emacs.d/backup"))
        save-interprogram-paste-before-kill t
        custom-file "~/.emacs.d/custom.el")
(add-to-list 'exec-path "/usr/local/bin")

;; (load custom-file)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(setq-default find-function-C-source-directory "/Volumes/Data/emacs/src"
              indent-tabs-mode nil)

(setq-default linum-format "%4d ")

(defun toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'."
  (interactive)
  (cond ((eq indent-tabs-mode nil) (setq-local indent-tabs-mode t))
        (:else (setq-local indent-tabs-mode nil))))

(fset 'yes-or-no-p 'y-or-n-p)

;; (setq-default header-line-format
;;               '((which-func-mode (""  which-func-format " "))))
;; (setq-default which-func-unknown "n/a")
(my/set mode-line-misc-info
         (assq-delete-all 'which-func-mode mode-line-misc-info))

(when (and window-system (eq system-type 'darwin))
  (my/enable-modes
    menu-bar-mode))

(when (eq system-type 'darwin)
  (my/set ns-function-modifier 'hyper))

(use-package autorevert
  :diminish
  auto-revert-mode)

(my/set select-enable-clipboard nil
         select-enable-primary t)

;; (xterm-mouse-mode t)
;; (global-set-key [mouse-4] (lambda ()
;;                             (interactive)
;;                             (scroll-down 1)))
;; (global-set-key [mouse-5] (lambda ()
;;                             (interactive)
;;                             (scroll-up 1)))
;; (defun track-mouse (&rest args)
;;   "Dummy hack for `track-mouse'. Ignore ARGS.")

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(use-package mwheel
  :config
  (my/set mouse-wheel-scroll-amount '(1 ((shift) . 3)) ;; one line at a time
           mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
           mouse-wheel-follow-mouse t))

(use-package abbrev
  :config
  (my/set save-abbrevs 'silently)
  :diminish
  abbrev-mode)

(my/set savehist-additional-variables
         '(kill-ring
           search-ring
           mark-ring
           xref--marker-ring
           regexp-search-ring)
         savehist-autosave-interval 60
         savehist-save-minibuffer-history t
         savehist-file "~/.emacs.d/savehist")
(my/enable-modes
  savehist-mode)

(use-package eshell
  :commands
  eshell
  :config
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply))

(my/set ispell-program-name (executable-find "aspell"))

(my/enable-modes
  global-ede-mode
  ;; semantic-mode
  ;; global-semantic-stickyfunc-mode
  ;; global-semantic-decoration-mode
  ;; global-semanticdb-minor-mode
  ;; global-semantic-idle-scheduler-mode
  ;; global-semantic-idle-summary-mode
  ;; global-semantic-idle-completion-mode
  ;; global-semantic-idle-local-symbol-highlight-mode
  ;; global-semantic-highlight-edits-mode
  ;; global-semantic-show-parser-state-mode
  ;; global-semantic-show-unmatched-syntax-mode
  ;; global-semantic-highlight-func-mode)
  )

(my/enable-modes
  global-prettify-symbols-mode)

;; (when (fboundp 'pixel-scroll-mode)
;;   (pixel-scroll-mode 1))

;; From http://mbork.pl/2016-07-25_Making_directories_on_the_fly
(defun make-parent-directory ()
  "Make sure the directory of variable `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

(run-with-idle-timer (* 10 60) t (lambda () (custom-save-all)))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

(use-package epa
  :config
  (epa-file-enable))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
              (setq TeX-command-default "XeLaTeX")
              (setq TeX-save-query nil)
              (setq TeX-show-compilation t)))
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t))

(use-package xref
  :config
  (evil-leader/set-key
    "xr" 'xref-find-references
    "xd" 'xref-find-definitions))

(use-package etags
  :config
  (my/set tags-revert-without-query t))

(evil-leader/set-key
  "w" 'save-buffer
  "q" 'save-buffers-kill-terminal
  "bf" 'beginning-of-defun
  "ef" 'end-of-defun
  "eb" 'eval-buffer
  "tl" 'toggle-truncate-lines)

(my/set ring-bell-function 'ignore)

(provide 'init-builtins)
;;; init-builtins.el ends here
