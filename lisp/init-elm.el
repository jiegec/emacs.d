;;; init-elm.el --- Init file for elm. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)

(defun electric-indent-un-inhibit ()
  "ENABLE electric-indent."
  (my/set electric-indent-inhibit nil)
  (electric-indent-mode t))

(use-package elm-mode
  :defer
  :ensure t
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm))
  (my/set elm-sort-imports-on-save t
          elm-format-on-save t)
  (add-hook 'elm-mode-hook 'turn-off-elm-indent)
  (add-hook 'elm-mode-hook 'electric-indent-un-inhibit))


(provide 'init-elm)
;;; init-elm.el ends here
