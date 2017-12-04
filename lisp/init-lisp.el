;;; init-lisp.el --- Init code for lisp modes.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

;; (use-package parinfer-mode
;;   :config
;;   (parinfer-mode t))

(require 'init-evil)

(use-package macrostep
  :ensure t
  :defer
  :commands
  macrostep-mode
  :bind
  ("C-c m" . macrostep-mode)
  ("C-x m" . macrostep-mode)
  :config
  (add-hook 'macrostep-mode-hook (lambda () (evil-emacs-state)))
  (add-hook 'macrostep-mode-off-hook (lambda () (evil-normal-state))))

(use-package hy-mode
  :defer
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  :diminish
  smartparens-mode)

(use-package sly
  :ensure t
  :defer
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(use-package sly-macrostep
  :ensure t
  :after sly)

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sly-company
  :ensure t
  :after sly
  :config
  (add-hook 'sly-mode-hook 'sly-company-mode)
  (add-to-list 'company-backends 'sly-company))

(use-package sly-named-readtables
  :ensure t
  :after sly)

(use-package sly-repl-ansi-color
  :ensure t
  :after sly)

(use-package lispy
  :ensure t
  :defer
  :config)
;; (dolist (hook '(emacs-lisp-mode-hook
;;                 hy-mode-hook
;;                 clojure-mode-hook))
;;   (add-hook hook (lambda () (lispy-mode 1))))


;; (defun conditionally-enable-lispy ()
;;   "Enable lispy if in M-:."
;;   (when (eq this-command 'eval-expression)
;;     (lispy-mode 1)))
;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)

;; (quelpa
;;  '(parinfer-mode
;;    :fetcher
;;    github
;;    :repo
;;    "DogLooksGood/parinfer-mode"))
(use-package parinfer
  :ensure t
  :commands
  parinfer-mode
  :config
  (my/set parinfer-extensions
          '(defaults
             evil
             smart-tab
             smart-yank))
  (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode))
;; (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;; (add-hook 'clojure-mode-hook #'parinfer-mode))


(provide 'init-lisp)
;;; init-lisp.el ends here
