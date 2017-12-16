;;; init-proof.el --- Init file for proof. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(use-package proof
  :defer
  :mode
  (("\\.v\\'" . coq-mode))
  :load-path
  "/usr/local/share/emacs/site-lisp/proof-general/generic"
  :config
  (my/set proof-three-window-enable t))

(defun disable-modes-in-coq ()
  "Disable unwanted modes in Coq."
  (interactive)
  (company-coq-mode -1)
  (projectile-mode -1))

(add-hook 'coq-mode-hook #'disable-modes-in-coq)
(use-package company-coq
  :ensure t
  :defer
  :commands
  company-coq-mode)

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(use-package idris-mode
  :defer
  :mode
  (("\\.lidr$" . idris-mode)
   ("\\.idr$" . idris-mode))
  :ensure t)

(provide 'init-proof)
;;; init-proof.el ends here
