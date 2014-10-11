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
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq my-packages
      '(;; Swift
        swift-mode

        ;; C family
        company-c-headers
        flymake-google-cpplint
        flycheck-google-cpplint
        flymake-cppcheck
        google-c-style

        ;; Go
        go-mode
        go-company
        flymake-go
        goflymake
        go-oracle

        ;; Clojure
        clojure-mode
        cider

        ;; Javascript
        js2-mode
        js2-refactor
        nodejs-repl
        flymake-jslint
        nvm
        flymake-jshint

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
        ghci-completion
        flycheck-haskell
        company-ghc
        flycheck-hdevtools
        flymake-haskell-multi
        flymake-hlint

        ;; JVM
        gradle-mode
        Emacs-Groovy-Mode
        scala-mode2
        sbt-mode

        ;; Web
        elnode
        web-mode

        ;; Android
        android-mode

        ;; Python
        elpy
        nose
        flymake-python-pyflakes
        flycheck-pyflakes

        ;; Imenu
        imenu+
        imenu-anywhere

        ;; Common Lisp
        slime
        slime-company

        ;; Others
        auto-dictionary
        2048.el
        company-mode
        projectile
        elfeed
        keyfreq
        smex
        el-get
        flycheck
        flymake
        flyspell
        flyparens
        expand-region
        helm
        helm-company
        helm-gtags
        helm-flycheck
        helm-flymake
        yasnippet
        color-theme-solarized
        magit
        ace-isearch
        smooth-scroll))

(setq n 0)
(dolist (pkg my-packages)
  (unless (or
           (package-installed-p pkg)
           (assoc pkg
                  package-archive-contents))
    (setq n (+ n 1))))
(when (> n 0)
  (package-refresh-contents))

(dolist (pkg my-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))

(defun package-list-unaccounted-packages ()
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                                   (not (package-built-in-p x))
                                   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

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

(global-flycheck-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(smooth-scroll-mode 1)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(global-font-lock-mode t)

(global-set-key (kbd "C-x w") 'elfeed)

(helm-mode 1)

(global-ace-isearch-mode 1)

(projectile-global-mode 1)

(global-company-mode 1)

(setq elfeed-feeds
      '("http://planet.emacsen.org/atom.xml"
        "http://www.ruanyifeng.com/blog/atom.xml"
        "http://www.matrix67.com/blog/feed"
        "http://nullprogram.com/feed/"
        "http://endlessparentheses.com/atom.xml"
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
