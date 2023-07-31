;;; init-rust.el --- Init file for rust. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(use-package rust-mode
  :ensure t
  :defer t
  :custom
  (rust-format-on-save t))

;; rust-analyzer LSP
(use-package eglot
  :hook (rust-mode . eglot-ensure)
  :defer t
  :config
  (add-to-list 'eglot-server-programs
             '(rust-mode . ("rust-analyzer"))))

(provide 'init-rust)
;;; init-rust.el ends here
