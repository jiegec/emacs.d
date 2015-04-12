;;; init.el --- Jiege Chen's init file.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec/emacs.d

;;; Commentary:
;;  Please report issues to my github repo.

;;; Code:

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'exec-path "/usr/local/bin")

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

         ;; Clojure
         clojure-mode
         cider

         ;; LaTeX
         auctex

         ;; Javascript
         js2-mode
         js2-refactor
         swank-js
         json-mode

         ;; Coffeescript
         coffee-mode
         flymake-coffee

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
         sbt-mode

         ;; Web
         elnode
         web-mode

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

(global-linum-mode 1)

(setq linum-format (lambda
                     (line)
                     (propertize
                      (format (concat "%"
                                      (number-to-string
                                       (length
                                        (number-to-string
                                         (line-number-at-pos
                                          (point-max)))))
                                      "d ")
                              line)
                      'face
                      'linum)))

(window-number-mode 1)

(global-flycheck-mode 1)

(flymake-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(global-font-lock-mode t)

(global-set-key (kbd "C-x w") 'elfeed)

(helm-mode 1)

(rainbow-delimiters-mode 1)

(setq inferior-lisp-program "/usr/local/bin/sbcl")

(global-company-mode 1)

(global-ace-isearch-mode 1)

(projectile-global-mode 1)

(global-aggressive-indent-mode t)

(window-number-mode 1)

(global-evil-leader-mode 1)
(evil-leader/set-leader ",")

(evil-mode 1)

(global-evil-surround-mode 1)

(require 'evil-nerd-commenter)
(evilnc-default-hotkeys)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "kj")
(evil-escape-mode 1)

(delete-selection-mode 1)

(smartparens-global-mode 1)


(eval-after-load "evil"
  '(setq expand-region-contract-fast-key "z"))

(evil-leader/set-key
  "0" '(lambda () (interactive) (window-number-select 0))
  "1" '(lambda () (interactive) (window-number-select 1))
  "2" '(lambda () (interactive) (window-number-select 2))
  "3" '(lambda () (interactive) (window-number-select 3))
  "4" '(lambda () (interactive) (window-number-select 4))
  "5" '(lambda () (interactive) (window-number-select 5))
  "6" '(lambda () (interactive) (window-number-select 6))
  "7" '(lambda () (interactive) (window-number-select 7))
  "8" '(lambda () (interactive) (window-number-select 8))
  "9" '(lambda () (interactive) (window-number-select 9))
  "xx" 'er/expand-region)

(setq elfeed-feeds
      '("http://planet.emacsen.org/atom.xml"
        "http://www.ruanyifeng.com/blog/atom.xml"
        "http://www.matrix67.com/blog/feed"
        "http://nullprogram.com/feed/"
        "http://endlessparentheses.com/atom.xml"
        "http://www.geek.com/feed/"
        "http://planet.emacsen.org/zh/atom.xml"))

(global-set-key "\C-cd" 'dash-at-point)

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
