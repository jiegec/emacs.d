;;; init-python --- The init file for python. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

;; python-lsp-server LSP
(use-package eglot
  :hook (python-mode . eglot-ensure)
  :defer t
  :config
  (add-to-list 'eglot-server-programs
             '(python-mode . ("pylsp"))))

(provide 'init-python)
;;; init-python.el ends here
