;;; init-clojure --- The init file for clojure code.
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(use-package clojure-mode
  :ensure t
  :defer)

(use-package cider
  :ensure t
  :defer)

(use-package clj-refactor
  :ensure t
  :defer)

(provide 'init-clojure)
;;; init-clojure.el ends here
