;;; init-input.el --- Init code for input. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)
(require 'init-evil)

;; (quelpa '(pyim :fetcher github :repo "tumashu/pyim"))
(use-package pyim
  :ensure t
  :config
  (my/set default-input-method "pyim")
  (global-set-key (kbd "C-\\") (lambda ()
                                 (interactive)
                                 (if current-input-method
                                     (deactivate-input-method)
                                   (activate-input-method "pyim"))))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-org-speed-commands
                  pyim-probe-org-structure-template
                  pyim-probe-program-mode))
  
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  
  (my/set pyim-isearch-enable-pinyin-search t
          pyim-page-tooltip 'child-frame
          pyim-dcache-prefer-emacs-thread nil
          pyim-enable-words-predict '(pinyin-similar
                                      pinyin-shouimu
                                      pinyin-znabc)
          pyim-page-length 5)
  ;; (setq pyim-dicts
  ;;       '((:name "pyim-bigdict" :file "~/.emacs.d/dicts/pyim-bigdict.pyim" :coding utf-8-unix :dict-type pinyin-dict)))
  (global-set-key (kbd "M-f") 'pyim-forward-word)
  (global-set-key (kbd "M-b") 'pyim-backward-word)
  (global-set-key (kbd "s-l") 'pyim-convert-code-at-point))


(use-package pyim-greatdict
  :ensure t
  :after pyim
  :config
  (pyim-greatdict-enable))

(use-package pyim-basedict
  :ensure t
  :after pyim
  :config
  (pyim-basedict-enable))

(use-package fcitx
  :ensure t
  :config 
  (fcitx-M-x-turn-on)
  (fcitx-aggressive-setup))


;; (defun --set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (if (eq system-type 'darwin)
;;       ;; For NS/Cocoa
;;       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
;;     ;; For Linux
;;     (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; ;; For when Emacs is started in GUI mode:
;; (--set-emoji-font nil)
;; ;; Hook for when a frame is created with emacsclient
;; ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
;; (add-hook 'after-make-frame-functions '--set-emoji-font)

;; (when (member "Inconsolata" (font-family-list))
;;   (set-face-attribute 'default nil :font "Inconsolata-16"))

(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")

(defun set-fonts ()
  "Set fonts for FRAME."
  (interactive)
  (set-frame-font "fontset-default" nil t)
  (set-fontset-font "fontset-default" 'unicode "Source Code Pro-15")
  (set-fontset-font "fontset-default" 'symbol "Arial Unicode MS" nil 'append)
  (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono-18" nil 'append)
  (set-fontset-font "fontset-default" 'symbol "Symbola" nil 'append)
  (set-fontset-font "fontset-default" 'unicode "Inconsolata for Powerline" nil 'append))

;; (global-set-key (kbd "C-c f") 'set-fonts)
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))
;; (call-interactively #'set-fonts)
;; (add-hook 'server-visit-hook 'set-fonts)
;; (add-hook 'window-setup-hook 'set-fonts)

;; {%org-mode%}
;; here are 20 hanzi and 40 english chars, see if they are the same width
;; 你你你你你你你你你你你你你你你你你你你你
;; 你好你是谁？为什么这么慢！你好
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; abcdefghijklmnopqrstuvwxyzabcdefghijklmn
;; 😂😹😀😃
;; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
;; {%/org-mode%}

(defun cnfonts-set-symbol-fonts (fontsizes-list)
  "Set symbol fonts in cnfonts.  Ignoring FONTSIZES-LIST."
  (let* ((fontname "Apple Color Emoji")
         (fontsize (nth 0 fontsizes-list))
         (fontspec (font-spec :name fontname
                              :size fontsize
                              :weight 'normal
                              :slant 'normal)))
    (set-fontset-font "fontset-default" 'unicode fontspec nil 'append)))

(use-package cnfonts
  :ensure t
  :init
  (evil-leader/set-key
    "sf" 'cnfonts-set-font-with-saved-step
    "if" 'cnfonts-increase-fontsize
    "df" 'cnfonts-decrease-fontsize)
  (evil-set-initial-state 'cnfonts-ui-mode 'emacs)
  :config
  (my/set cnfonts-personal-fontnames
          '(("Arial Unicode MS"
             "Inconsolata for Powerline")
            ("WenQuanYi Micro Hei Mono")
            ("Hanazono Mincho B"))
          cnfonts-use-face-font-rescale t)
  (add-hook 'cnfonts-set-font-finish-hook 'cnfonts-set-symbol-fonts)
  (setq cnfonts-use-face-font-rescale t)
  (cnfonts-enable))

(use-package emojify
  :ensure t
  :config
  (my/set emojify-display-style 'unicode)
  (emojify-set-emoji-styles '(unicode github))
  (global-emojify-mode))

(provide 'init-input)
;;; init-input.el ends here
