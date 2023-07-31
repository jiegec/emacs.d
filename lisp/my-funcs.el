;;; my-funcs.el --- My util functions. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(defun my/reload-emacs-init-file ()
  "Reload the ~/.emacs.d/init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(provide 'my-funcs)
;;; my-funcs.el ends here
