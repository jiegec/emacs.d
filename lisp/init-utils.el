;;; init-utils.el --- Init file for utils. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

;; Better minibuffer completion
(use-package counsel
  :ensure t
  :defer t
  :bind
  ("C-x C-f" . counsel-find-file); find file
  ("M-x" . counsel-M-x) ; M-x
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

;; Git
(use-package magit
  :ensure t
  :defer t)

;; xref
(use-package xref
  :defer t
  :bind
  ;; Mimic VSCode
  ("s-<mouse-1>" . xref-find-definitions-at-mouse)
  ("<f12>" . xref-find-definitions)
  ("C--" . xref-go-back))

(provide 'init-utils)
;;; init-utils.el ends here
