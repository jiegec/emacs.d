;;; init-evil.el --- Init code for evil.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :ensure t
  :config
  (my/set evil-symbol-word-search t
          evil-default-cursor t)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'refine-mode 'emacs)
  (evil-set-initial-state 'neotree-more 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'osx-dictionary-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (defun unbind-M-dot ()
    (define-key evil-normal-state-map (kbd "M-.") nil))
  (add-hook 'evil-mode-hook 'unbind-M-dot)
  (evil-mode t))

(provide 'init-evil)
;;; init-evil.el ends here
