;;; init.el --- Jiege Chen's init file. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:
;;  Please report issues to my github repo.

;;; Code:

(defconst emacs-start-time (current-time))

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq debug-on-error nil)

(require 'init-package)

(require 'init-evil)
(require 'init-builtins)
(require 'init-lifestyle)
(require 'init-rust)
(require 'init-python)

;; (require 'init-c)
;; (require 'init-clojure)
;; (require 'init-elm)
;; (require 'init-email)
;; (require 'init-erlang)
;; (require 'init-go)
;; (require 'init-haskell)
;; (require 'init-input)
;; (require 'init-java)
;; (require 'init-lisp)
;; (require 'init-mail)
;; (require 'init-proof)
(require 'init-utils)
;; (require 'init-web)

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
