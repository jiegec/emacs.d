;;; init-package.el --- Init code for package.el.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

;; Pending: company-coq dict ghci go lua math shell web

;; From https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(setq-default tls-checktrust t)
(let ((trustfile "/usr/local/etc/openssl/cert.pem"))
  (setq-default tls-program
                (list
                 (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                         (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq-default gnutls-verify-error t
                gnutls-trustfiles (list trustfile)))

(require 'package)
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;; 			 ("org" . "http://orgmode.org/elpa/") ; Hope to use https
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ))
;; (setq package-archives '(("ec-gnu" . "https://git.oschina.net/EmacsChina/elpa/raw/master/gnu/")
;; 			 ("ec-org" . "https://git.oschina.net/EmacsChina/elpa/raw/master/org/")
;; 			 ("ec-melpa" . "https://git.oschina.net/EmacsChina/elpa/raw/master/melpa/")))
;; (package-initialize)
;; (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")
;;                          ("org" . "http://elpa.emacs-china.org/org/")
;;                          ("sunrise-commander" . "http://elpa.emacs-china.org/sunrise-commander/")))

(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("sunrise-commander" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/sunrise-commander/")))
(package-initialize)
;; Oh no I want to use HTTPS!
;; (setq package-archives '(("ec-gnu" . "http://elpa.emacs-china.org/gnu/")
;; 			 ("ec-org" . "http://elpa.emacs-china.org/org/")
;; 			 ("ec-melpa" . "http://elpa.emacs-china.org/melpa/")))
;; (package-initialize)

(when (and (fboundp 'daemonp)
           (daemonp))
  (package-refresh-contents))

(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
(require 'quelpa)
(setq quelpa-checkout-melpa-p nil)
;; temporary fix
(quelpa '(use-package :fetcher github :repo "jwiegley/use-package"))
(require 'use-package)
(setq use-package-verbose 'debug)


(provide 'init-package)
;;; init-package.el ends here