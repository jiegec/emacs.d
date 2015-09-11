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

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

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
         window-number
         relative-line-numbers
         dtrt-indent
         ag
         diminish
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
         editorconfig
         po-mode
         which-key
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

(global-auto-complete-mode 0)

(projectile-global-mode 1)

(global-aggressive-indent-mode t)

(window-number-mode 1)

(scroll-bar-mode -1)

(relative-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(delete-selection-mode 1)

(dtrt-indent-mode 1)

(which-key-mode 1)

(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

(smartparens-global-mode 1)

(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

(diminish 'company-mode)
(diminish 'helm-mode)
(diminish 'smartparens-mode)
(diminish 'aggressive-indent-mode)
(diminish 'flyspell-mode)
(diminish 'abbrev-mode)
(diminish 'ace-isearch-mode)

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
