;;; init-python --- The init file for python.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(use-package elpy
  :defer
  :ensure t
  :config
  (elpy-enable))

(use-package anaconda-mode
  :ensure t
  :commands
  anaconda-mode
  anaconda-eldoc-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(provide 'init-python)
;;; init-python.el ends here