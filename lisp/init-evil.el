;;; init-evil.el --- Init code for evil. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)

(use-package evil
  :ensure t
  :custom
  (evil-symbol-word-search t)
  (evil-default-cursor t)
  :config
  (evil-mode t))

(provide 'init-evil)
;;; init-evil.el ends here
