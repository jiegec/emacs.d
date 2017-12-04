;;; init-erlang.el --- Init code for erlang language.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'subr-x)
(when-let (file (car (file-expand-wildcards "/usr/local/lib/erlang/lib/tools-*/emacs")))
  (add-to-list 'load-path file))
(add-to-list 'load-path "/Volumes/Data/lfe/emacs")

(use-package alchemist
  :ensure t
  :defer)

(use-package elixir-mode
  :ensure t
  :defer)

(use-package erlang-start
  :defer)

(use-package erlang-flymake
  :defer)

(use-package lfe-start
  :defer)

(provide 'init-erlang)
;;; init-erlang.el ends here
