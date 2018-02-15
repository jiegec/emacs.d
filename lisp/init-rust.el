;;; init-rust.el --- Init file for rust. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)

(use-package rust-mode
  :ensure t
  :config
  (my/set rust-format-on-save t))

(use-package lsp-rust
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(use-package racer
  :ensure t
  :defer
  :commands
  racer-mode
  :init
  ;; (add-hook 'rust-mode-hook 'racer-mode)
  )


(provide 'init-rust)
;;; init-rust.el ends here
