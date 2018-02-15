;;; init-email.el --- Init file for email. -*- lexical-binding: t -*-
;; Author: Jiege Chen <jiegec@qq.com>
;; Homepage: https://github.com/jiegec

;;; Commentary:

;;; Code:

(require 'my-funcs)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu")

(use-package bbdb
  :ensure t
  :defer
  :config
  (bbdb-initialize 'message 'gnus 'mu4e 'mail))

(use-package w3m
  :ensure t
  :defer
  :config
  (my/set w3m-default-display-inline-images t))


(use-package gnus
  :defer
  :config
  (my/set gnus-topic-topology '(("gnus" visible)
                                (("jiegec" visible nil nil))
                                (("emacs" visible nil nil))
                                (("haskell" visible nil nil))
                                (("git" visible nil nil))))
  (my/set gnus-topic-alist '(("jiegec"
                              "nnimap+jiegec@qq.com:INBOX"
                              "nnimap+jiegec@qq.com:Drafts"
                              "nnimap+jiegec@qq.com:Sent Messages"
                              "nnimap+jiegec@qq.com:Deleted Messages"
                              "nnimap+jiegec@qq.com:Junk")
                             ("emacs"
                              "gmane.emacs.devel"
                              "gmane.emacs.orgmode"
                              "gmane.emacs.bugs")
                             ("haskell"
                              "gmane.comp.lang.haskell.ghc.devel")
                             ("git"
                              "gmane.comp.version-control.git")))
  (my/set gnus-select-method '(nntp "news.gmane.org")
          gnus-use-cache t
          gnus-read-active-file 'some
          gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
          gnus-thread-hide-subtree t
          gnus-thread-ignore-subject t
          gnus-cache-enter-articles '(ticked dormant read unread)
          gnus-cache-remove-articles nil
          gnus-cacheable-groups "^nnimap"
          gnus-use-adaptive-scoring t
          gnus-save-score t
          mm-text-html-renderer 'w3m
          gnus-secondary-select-methods '((nnimap "jiegec@qq.com"
                                                  (nnimap-address "imap.qq.com")
                                                  (nnimap-server-port 993)
                                                  (nnimap-stream ssl)
                                                  (nnir-search-engine imap)
                                                  (nnimap-authenticator login))))
  (my/set gnus-thread-sort-functions '((not gnus-thread-sort-by-date)
                                       (not gnus-thread-sort-by-number)))
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (define-key gnus-group-mode-map
    (kbd "o") (lambda ()
                (interactive)
                (gnus-group-list-all-groups 5))))

(with-eval-after-load 'gnus-group
  (progn
    (defhydra hydra-gnus-group (:color blue)
      "Do?"
      ("a" gnus-group-list-active "REMOTE groups A A")
      ("l" gnus-group-list-all-groups "LOCAL groups L")
      ("c" gnus-topic-catchup-articles "Read all c")
      ("G" gnus-group-make-nnir-group "Search server G G")
      ("g" gnus-group-get-new-news "Refresh g")
      ("s" gnus-group-enter-server-mode "Servers")
      ("m" gnus-group-new-mail "Compose m OR C-x m")
      ("#" gnus-topic-mark-topic "mark #")
      ("q" nil "cancel"))
    ;; y is not used by default
    (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(with-eval-after-load 'gnus-sum
  (progn
    (defhydra hydra-gnus-summary (:color blue)
      "Do?"
      ("s" gnus-summary-show-thread "Show thread")
      ("h" gnus-summary-hide-thread "Hide thread")
      ("n" gnus-summary-insert-new-articles "Refresh / N")
      ("f" gnus-summary-mail-forward "Forward C-c C-f")
      ("!" gnus-summary-tick-article-forward "Mail -> disk !")
      ("p" gnus-summary-put-mark-as-read "Mail <- disk")
      ("c" gnus-summary-catchup-and-exit "Read all c")
      ("e" gnus-summary-resend-message-edit "Resend S D e")
      ("R" gnus-summary-reply-with-original "Reply with original R")
      ("r" gnus-summary-reply "Reply r")
      ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
      ("w" gnus-summary-wide-reply "Reply all S w")
      ("#" gnus-topic-mark-topic "mark #")
      ("q" nil "cancel"))
    ;; y is not used by default
    (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(with-eval-after-load 'gnus-art
  (progn
    (defhydra hydra-gnus-article (:color blue)
      "Do?"
      ("f" gnus-summary-mail-forward "Forward")
      ("R" gnus-article-reply-with-original "Reply with original R")
      ("r" gnus-article-reply "Reply r")
      ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
      ("o" gnus-mime-save-part "Save attachment at point o")
      ("w" gnus-article-wide-reply "Reply all S w")
      ("q" nil "cancel"))
    ;; y is not used by default
    (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

(use-package smtpmail
  :defer
  :config
  (my/set smtpmail-smtp-server "smtp.qq.com"
           smtpmail-smtp-service 587
           smtpmail-stream-type 'starttls))

(use-package message
  :defer
  :config
  (my/set message-send-mail-function 'smtpmail-send-it
           message-kill-buffer-on-exit t))

(use-package sendmail
  :defer
  :config
  (my/set send-mail-function 'smtpmail-send-it))

(el-get-bundle mu4e-multi
  :url "https://github.com/fgallina/mu4e-multi.git")
;; (quelpa '(mu4e-multi
;;           :fetcher
;;           github
;;           :repo
;;           "fgallina/mu4e-multi"))

(use-package mu4e-multi
  :defer
  :config
  (my/set mu4e-multi-account-alist
          '(("Jiegec"
             (user-mail-address . "jiegec@qq.com")
             (mu4e-drafts-folder . "/Jiegec/Drafts")
             (mu4e-sent-folder . "/Jiegec/Sent Messages")
             (mu4e-trash-folder . "/Jiegec/Deleted Messages")
             )
            ("TankJackChen"
             (user-mail-address . "tank_jackchen@qq.com")
             (mu4e-drafts-folder . "/TankJackChen/Drafts")
             (mu4e-sent-folder . "/TankJackChen/Sent Messages")
             (mu4e-trash-folder . "/TankJackChen/Deleted Messages")
             )))
  (mu4e-multi-enable))
(use-package mu4e
  :defer
  :commands
  mu4e
  :bind
  (:map mu4e-view-mode-map
        ("<tab>" . shr-next-link)
        ("<backtab>" . shr-prevous-link))
  :config
  (my/set mu4e-maildir "~/Maildir"
          mu4e-get-mail-command "offlineimap"
          mu4e-change-filenames-when-moving t
          mu4e-view-show-images t
          mu4e-use-fancy-chars nil
          mu4e-view-prefer-html t
          mu4e-headers-auto-update t
          mu4e-update-interval 300 ;; 5 min
          ;;mu4e-html2text-command "w3m -T text/html"
          mu4e-html2text-command 'mu4e-shr2text)
  (mu4e t)
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (flyspell-mode 1))))

(use-package mu4e-alert
  :ensure t
  :config
  (my/set mu4e-alert-notify-repeated-mails t)
  (mu4e-alert-set-default-style 'notifier) ;; Should be attached to user namespace if under Tmux
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(provide 'init-email)
;;; init-email.el ends here
