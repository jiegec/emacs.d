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

(setq el-get-packages
      (append
       '( ;; Swift
         swift-mode

         ;; Clojure
         clojure-mode
         cider

         ;; Javascript
         js2-mode
         js2-refactor

         ;; Coffeescript
         coffee-mode
         flymake-coffee

         ;; Lua
         flymake-lua
         lua-mode

         ;; Haskell
         haskell-mode
         company-ghc
         ghc-mod
         structured-haskell-mode
         flycheck-haskell

         ;; Web
         elnode
         web-mode

         ;; Python
         jedi
         elpy
         flymake-pycheckers

         ;; Imenu
         imenu+
         imenu-anywhere

         ;; Others
         elfeed
         keyfreq
         el-get
         flycheck
         flymake
         flyspell
         expand-region
         company-mode
         window-numbering
         yasnippet
         color-theme-solarized
         magit
         smooth-scroll)))

(el-get 'sync el-get-packages)

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

(global-flycheck-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(smooth-scroll-mode 1)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(global-font-lock-mode t)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '("http://planet.emacsen.org/atom.xml"
	"http://www.ruanyifeng.com/blog/atom.xml"
        "http://www.matrix67.com/blog/feed"
	"http://nullprogram.com/feed/"
        "http://planet.emacsen.org/zh/atom.xml"))

(defun try-to-add-imenu ()
  "Add a Imenu to menubar."
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;; Disable popup dialog
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(provide 'init)
;;; init.el ends here
