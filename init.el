(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;(add-to-list 'load-path "~/.emacs.d/cl-lib/")
;(require 'cl-lib)


(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get)

(setq el-get-verbose t)

(setq el-get-packages
      (append
       '(;; LaTeX
         reftex
         cdlatex-mode

         ;; Clojure
         projectile
         clojure-mode
         cider

         ;; Javascript
         js2-mode
         js2-refactor

         ;; Coffeescript
         flymake-coffee

         ;; Lua
         flymake-lua
         lua-mode

         ;; Haskell
         haskell-mode
         company-ghc
         ghc-mod
         structured-haskell-mode

         ;; Web
         elnode
         web-mode

         ;; Others
         cl-lib
         flycheck
         flydoc
         flymake
         flyspell
         company-mode
         yasnippet
         expand-region
         rainbow-delimiters
         smex
         color-theme-solarized
         window-numbering
         magit
         smartparens
         gtags
         tabbar
         unicad
         smooth-scroll)))


(el-get 'sync el-get-packages)

(load-theme 'solarized-light t)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Done."))

(global-set-key [f6] 'iwb)

;; Disable popup dialog
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; LaTeX
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'tex-site)
(load "auctex.el" nil t t)
(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
(setq TeX-view-program-list
      '(("Default" "open %q")))
