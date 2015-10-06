;;; init.el --- Jiege Chen's init file.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec/emacs.d

;;; Commentary:
;;  Please report issues to my github repo.

;;; Code:

(tool-bar-mode -1)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; (require 'homebrew-mode)
;; (add-hook 'after-init-hook 'global-homebrew-mode)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-M") 'smex-major-mode-commands)

(provide 'init)
;;; init.el ends here
