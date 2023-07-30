;;; init-package.el --- Init code for package.el. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

;; Setup TUNA mirrors
(require 'package)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(customize-set-variable 'load-prefer-newer t)
(package-initialize)

;; Setup use-package
(require 'use-package)
(setq use-package-verbose 'debug
      use-package-minimum-reported-time 0)

(provide 'init-package)
;;; init-package.el ends here
