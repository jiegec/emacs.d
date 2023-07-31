;;; init-utils.el --- Init file for utils. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x) ; M-x completion
  :config
  (ivy-mode)
  :diminish
  ivy-mode)

;; Code completion
(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  :diminish
  company-mode)

(provide 'init-utils)
;;; init-utils.el ends here
