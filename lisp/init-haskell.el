;;; init-haskell.el --- Init file for haskell.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(use-package haskell-mode
  :ensure t
  :defer
  :config
  (setq-default haskell-tags-on-save nil
		haskell-process-suggest-remove-import-lines t
		haskell-process-auto-import-loaded-modules t
		haskell-process-type 'stack-ghci)
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  :diminish
  haskell-doc-mode)

(use-package shm
  :ensure t
  :defer
  :config
  ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  :diminish
  structured-haskell-mode)

(use-package company-ghc
  :ensure t
  :defer
  :init
  ;; (with-eval-after-load 'company
  ;;   (add-to-list 'company-backends 'company-ghc))
  :commands
  company-ghc)

(use-package ghc
  :ensure t
  :defer
  :commands ghc-init)

(use-package intero
  :ensure t
  :defer
  :commands intero-mode)

;; (use-package lsp-haskell
;;   :ensure t
;;   :commands
;;   (lsp-haskell-enable))

(add-hook 'haskell-mode-hook (lambda ()
                               (aggressive-indent-mode -1)
                               ;; (lsp-haskell-enable)
                               (intero-mode t)
                               ))

(provide 'init-haskell)
;;; init-haskell.el ends here
