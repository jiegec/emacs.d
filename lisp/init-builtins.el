;;; init-builtins --- The init file for builtin stuffs. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

;; Disable modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable modes
(column-number-mode t)
(size-indication-mode t)
(delete-selection-mode t)
(tooltip-mode t)
(global-eldoc-mode t)
(minibuffer-depth-indicate-mode t)
(show-paren-mode t)
(global-auto-revert-mode t)
(save-place-mode t)

;; Hide eldoc in mode line
(diminish 'eldoc-mode)

;; Default settings
(customize-set-value 'custom-file "~/.emacs.d/custom.el")
(customize-set-value 'user-full-name "Jiajie Chen")
(customize-set-value 'user-mail-address "c@jia.je")
(customize-set-value 'inhibit-startup-screen t)

(provide 'init-builtins)
;;; init-builtins.el ends here
