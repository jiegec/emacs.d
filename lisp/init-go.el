;;; init-go.el --- Init code for go. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

;; go tree-sitter
(use-package treesit
  :config
  (add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
  (unless (treesit-language-available-p 'go)
    (treesit-install-language-grammar 'go))
  (add-to-list 'treesit-language-source-alist '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
  (unless (treesit-language-available-p 'gomod)
    (treesit-install-language-grammar 'gomod)))
(use-package go-ts-mode)

;; gopls LSP
(use-package eglot
  :hook (go-ts-mode . eglot-ensure)
  :defer t
  :config
  (add-to-list 'eglot-server-programs
             '(go-ts-mode . ("gopls"))))

(provide 'init-go)
;;; init-go.el ends here
