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

(require 'init-c)
(require 'init-builtins)
(require 'init-evil)
(require 'init-go)
(require 'init-lifestyle)
(require 'init-python)
(require 'init-rust)
(require 'init-utils)

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
