;;; init-c.el --- Init file for c. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)

(use-package irony
  :ensure t
  :defer
  :commands
  irony-mode
  :config
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package company-irony
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-irony))

  (use-package flycheck-irony
    :ensure t
    :after flycheck
    :config
    (add-to-list 'flycheck-checkers 'irony))

  (use-package irony-eldoc
    :ensure t
    :after eldoc
    :config
    (add-hook 'irony-mode-hook 'irony-eldoc))
  :diminish
  irony-mode)

(use-package company-c-headers
  :ensure t
  :defer
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package cmake-ide
  :ensure t
  :defer
  :config
  (cmake-ide-setup))

(use-package cpputils-cmake
  :ensure t
  :defer
  :commands
  cppcm-reload-all)

(use-package ggtags
  :ensure t
  :defer
  :diminish
  ggtags-mode
  :init
  (evil-leader/set-key
    "ft" 'ggtags-find-tag-dwim
    "fr" 'ggtags-find-reference
    "fd" 'ggtags-find-definition
    "fc" 'ggtags-find-tag-continue)
  :commands
  ggtags-find-tag-dwim
  ggtags-find-reference
  ggtags-find-definition
  ggtags-find-tag-continue)

(use-package disaster
  :ensure t
  :defer
  :init
  (evil-leader/set-key-for-mode 'c-mode
    "ds" 'disaster)
  (evil-leader/set-key-for-mode 'c++-mode
    "ds" 'disaster)
  :commands
  disaster
  :config
  (my/set disaster-objdump "gobjdump -C -d -M intel -Sl --no-show-raw-insn"
          disaster-make-flags "-k CFLAGS=\"-g\" CXXFLAGS=\"-g --std=c++14\""))

(use-package clang-format
  :ensure t
  :init
  (evil-leader/set-key-for-mode 'c-mode
    "fb" 'clang-format-buffer
    "fr" 'clang-format)
  (evil-leader/set-key-for-mode 'c++-mode
    "fb" 'clang-format-buffer
    "fr" 'clang-format)
  :commands
  clang-format
  clang-format-buffer
  :defer)

(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode))
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-cmake)))

(use-package gdb-mi
  :defer
  :commands
  gdb
  :init
  (dolist (mode '(c++-mode c-mode objc-mode))
    (evil-leader/set-key-for-mode mode
      "db" 'gdb))
  :config
  (my/set gdb-many-windows t
          gdb-show-main t))

(use-package bison-mode
  :ensure t)

(use-package cquery
  :commands
  (lsp-cquery-enable)
  :init
  (dolist (mode '(c++-mode c-mode objc-mode))
    (evil-leader/set-key-for-mode mode
      "xc" (lambda (&rest _)
             (interactive)
             (lsp-ui-peek-find-custom nil "$cquery/callers"))
      "xb" (lambda (&rest _)
             (interactive)
             (lsp-ui-peek-find-custom nil "$cquery/base"))
      "xD" (lambda (&rest _)
             (interactive)
             (lsp-ui-peek-find-custom nil "$cquery/derived"))
      "xv" (lambda (&rest _)
             (interactive)
             (lsp-ui-peek-find-custom nil "$cquery/vars"))))
  :config
  (my/set
   cquery-extra-init-params '(:index (:comments 2 :builtinTypes t) :cacheFormat "msgpack") ;; for MessagePack
   cquery-executable "/Volumes/Data/cquery/install/bin/cquery"
   cquery-additional-arguments '("--log-stdin-stdout-to-stderr" "--log-file" "/Volumes/Data/temp/cquery.log")))

;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'company-clang))
(defun init-c ()
  "Init C/C++/Obj-C modes."
  ;; (c-toggle-auto-newline 1)
  ;; (lsp-clangd-enable)
  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (lsp-cquery-enable))
  ;; (my/enable-modes
  ;;   ggtags-mode
  ;;   irony-mode)
  )

(dolist (hook '(c++-mode-hook
                c-mode-hook
                objc-mode-hook))
  (add-hook hook 'init-c))

(provide 'init-c)
;;; init-c.el ends here
