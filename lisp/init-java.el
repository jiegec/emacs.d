;;; init-java.el --- Init file for java. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:
(require 'my-funcs)

(use-package autodisass-java-bytecode
  :ensure t
  :defer)

(use-package meghanada
  :ensure t
  :defer
  :commands
  (meghanada-mode)
  :config
  (my/set meghanada-server-remote-debug t
          indent-tabs-mode nil
          tab-width 2
          c-basic-offset 2)
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (meghanada-mode t)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))


(provide 'init-java)
;;; init-java.el ends here
