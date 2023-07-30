;;; init-lifestyle.el --- Init code for lifestyle. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(use-package solarized-theme
  :ensure t
  :custom
  (solarized-height-minus-1 1)
  (solarized-height-plus-1 1)
  (solarized-height-plus-2 1)
  (solarized-height-plus-3 1)
  (solarized-height-plus-4 1)
  (solarized-scale-org-headlines nil)
  :config
  (load-theme 'solarized-dark t))

(provide 'init-lifestyle)
;;; init-lifestyle.el ends here
