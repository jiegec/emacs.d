;;; my-funcs.el --- My util functions. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'validate)

(defmacro my/able-mode (arg mode)
  "Use ARG to able MODE."
  (when (fboundp mode) `(,mode ,arg)))

(defmacro my/able-modes (arg &rest modes)
  "Use ARG to able every mode in MODES."
  `(progn
     ,@(mapcar (lambda (mode) `(my/able-mode ,arg ,mode)) modes)))

(defmacro my/disable-modes (&rest modes)
  "For every mode in MODES, disable it."
  `(my/able-modes -1 ,@modes))
(put 'my/disable-modes 'lisp-indent-function 0)

(defmacro my/enable-modes (&rest modes)
  "If any one of MODES exists, enable it."
  `(my/able-modes 1 ,@modes))
(put 'my/enable-modes 'lisp-indent-function 0)

(defun my/reload-emacs-init-file ()
  "Reload the ~/.emacs.d/init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defmacro my/set (&rest body)
  "`Set' version supporting custom variables.
BODY is same as `setq'."
  (when body
    (cl-destructuring-bind (sym val &rest rst) body
      `(progn
         (if (custom-variable-p ',sym)
             (progn
                                        ;(validate-value ,val (custom-variable-type ',sym))
               (customize-set-variable ',sym ,val))
           (setq ,sym ,val))
         (my/set ,@rst)))))

(defun my/notify-osx (title message)
  "Notify with TITLE and MESSAGE."
  (when (eq system-type 'darwin)
    (call-process "terminal-notifier"
                  nil 0 nil
                  "-group" "Emacs"
                  "-title" title
                  "-sender" "org.gnu.Emacs"
                  "-message" message
                  "-activate" "org.gnu.Emacs")))
(defun my/timestamp ()
  "Insert current timestamp into buffer."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S +0800")))

(defun get-project-root ()
  "Find the project's root directory."
  (cond
   ((and (featurep 'projectile) (projectile-project-p)) (projectile-project-root))
   ((vc-backend default-directory) (expand-file-name (vc-root-dir)))
   (t default-directory)))

(defun cps-convert (term cont)
  "CPS convert term and call cont."
  (pcase term
    (`(,f ,e)
     (let (($f (gensym 'f))
           ($e (gensym 'e)))
       (cps-convert f `(lambda (,$f)
                         ,(cps-convert e `(lambda (,$e)
                                            (funcall ,$f ,$e ,cont)))))))
    (`(lambda (,v) ,e)
     (let (($k (gensym 'k)))
       `(funcall ,cont (lambda (,v ,$k)
                         ,(cps-convert e $k)))))
    (_
     `(funcall ,cont ,term))))

(defun cps-convert-program (term)
  "CPS convert a program."
  (cps-convert term '(lambda (ans) ans)))

(defun M (expr)
  "M : expr => aexp."
  (pcase expr
    (`(lambda (,var) ,expr)
     (let (($k (gensym 'k)))
       `(lambda (,var ,$k) ,(T expr $k))))
    (_
     expr)))

(defun T (expr cont)
  "T : expr . aexp => cexp."
  (pcase expr
    (`(lambda . ,_) `(funcall ,cont ,(M expr)))
    (`(,f ,e)
     (let (($f (gensym '$f))
           ($e (gensym '$e)))
       (T f `(lambda (,$f)
               ,(T e `(lambda (,$e)
                        (funcall ,$f ,$e ,cont)))))))
    (_ `(funcall ,cont ,(M expr)))))
(defun cps-convert-program-2 (term)
  "CPS convert a program."
  (T term '(lambda (ans) ans)))

(evil-leader/set-key
  "ts" 'my/timestamp
  "rl" 'my/reload-emacs-init-file)

(use-package ffi
  :load-path "/Volumes/Data/emacs-ffi")

(provide 'my-funcs)
;;; my-funcs.el ends here
