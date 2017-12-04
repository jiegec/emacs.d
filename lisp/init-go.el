;;; init-go.el --- Init code for go.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(add-to-list 'load-path (substitute-in-file-name "$GOPATH/src/github.com/dougm/goflymake"))

(use-package go-mode
  :ensure t
  :defer
  :config
  (add-hook 'go-mode-hook 'go-oracle-mode)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode (lambda ()
                     (use-package go-flycheck)
                     (use-package go-flymake)
                     (use-package go-eldoc
                       :ensure t
                       :config
                       (go-eldoc-setup))
                     (use-package go-errcheck
                       :ensure t)
                     (use-package go-gopath
                       :ensure t)
                     (use-package go-projectile
                       :ensure t)
                     (use-package go-snippets
                       :ensure t)
                     (use-package golint
                       :ensure t)
                     ))

(provide 'init-go)
;;; init-go.el ends here
