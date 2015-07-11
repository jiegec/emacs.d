;;; init.el --- Jiege Chen's init file.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec/emacs.d

;;; Commentary:
;;  Please report issues to my github repo.

;;; Code:

(tool-bar-mode -1)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/el-get/recipes")

(setq el-get-packages
      (append
       '(;; Swift
         swift-mode

         ;; Markdown
         markdown-mode
         markdown-preview-mode

         ;; Go
         go-mode
         go-company
         go-test
         go-lint
         go-flymake
         go-errcheck

         ;; Scheme
         geiser
         quack
         racket-mode

         ;; Clojure
         clojure-mode
         cider
         clojure-snippets

         ;; LaTeX
         auctex
	 latex-preview-pane

         ;; Javascript
         js2-mode
         js2-refactor
         swank-js
         json-mode
         js-comint

         ;; PHP
         php-mode

         ;; Coffeescript
         coffee-mode
         flymake-coffee

         ;; Standard ML
         sml-mode

         ;; OCaml
         tuareg-mode

         ;; Erlang
         erlang-mode
         distel
         edts

         ;; Lua
         flymake-lua
         lua-mode

         ;; Haskell
         haskell-mode
         ghc-mod
         structured-haskell-mode
         flycheck-haskell
         company-ghc
         company-cabal

         ;; JVM
         groovy-emacs-mode
         scala-mode2
         ensime
         sbt-mode
         eclim
         groovy-emacs-mode

         ;; Web
         elnode
         emmet-mode
         web-mode
         skewer-mode

         ;; Android
         android-mode

         ;; Python
         elpy
         flymake-pycheckers

         ;; Imenu
         imenu+
         imenu-anywhere

         ;; Others
         slime
         ggtags
         dash-at-point
         aggressive-indent-mode
         2048.el
         projectile
         elfeed
         keyfreq
         smex
         el-get
         flycheck
         flymake
         flyspell
         expand-region
         company-mode
         gtags
         helm
         helm-gtags
         yasnippet
         color-theme-solarized
         magit
         ace-isearch
         rainbow-delimiters
         smooth-scrolling
         emacs-w3m
         smartparens
         evil
         evil-leader
         evil-nerd-commenter
         evil-matchit
         window-number
         evil-surround
         evil-escape
         powerline-evil
         relative-line-numbers
         dtrt-indent
         ag
         diminish
         neotree
         prodigy
         undo-tree
         highlight-symbol
         rainbow-mode
         visual-regexp
         discover
         discover-my-major
         guide-key
         guide-key-tip
         git-timemachine
         helm-ag
         )))

(el-get 'sync el-get-packages)

(el-get-cleanup el-get-packages)

(load-theme 'solarized-light t)

(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Done."))

(global-set-key [f6] 'iwb)

(window-number-mode 1)

(global-flycheck-mode 1)

(flyspell-mode 1)

(flymake-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(helm-mode 1)

(rainbow-delimiters-mode 1)

(setq inferior-lisp-program "/usr/local/bin/sbcl")

(global-company-mode 1)

(global-ace-isearch-mode 1)

(projectile-global-mode 1)

(global-aggressive-indent-mode t)

(window-number-mode 1)

(scroll-bar-mode -1)

(evil-mode 1)
(setq evil-move-cursor-back nil)
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(global-evil-surround-mode 1)

(evilnc-default-hotkeys)

(global-evil-matchit-mode 1)

(powerline-evil-vim-color-theme)
(display-time-mode 1)

(relative-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "kj")
(evil-escape-mode 1)

(delete-selection-mode 1)

(dtrt-indent-mode 1)

(smartparens-global-mode 1)

(global-set-key "\C-cd" 'dash-at-point)

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(global-set-key [f5] 'neotree-toggle)

(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 
          (lambda()
	    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
	    (setq TeX-command-default "XeLaTeX")
	    (setq TeX-save-query nil)
	    (setq TeX-show-compilation t)))

(latex-preview-pane-enable)

(diminish 'company-mode)
(diminish 'helm-mode)
(diminish 'evil-escape-mode)
(diminish 'undo-tree-mode)
(diminish 'smartparens-mode)
(diminish 'aggressive-indent-mode)
(diminish 'flyspell-mode)
(diminish 'abbrev-mode)
(diminish 'auto-complete-mode)

(defun try-to-add-imenu ()
  "Add a Imenu to menubar."
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;; Disable popup dialog
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent 'yes-or-no-p from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent 'y-or-n-p from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))

(provide 'init)
;;; init.el ends here
