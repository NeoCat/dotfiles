(setq load-path
      (append (list
	       (expand-file-name "~/.elisp/"))
	      load-path))

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [select] [(shift up)])

(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)

(setq indent-line-function 'tab-to-tab-stop)

(setq-default transient-mark-mode t)

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq vc-follow-symlinks nil)

(display-time)
(column-number-mode t)
(line-number-mode t)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.elisp/snippets/")

;; (require 'pabbrev)
;; (global-pabbrev-mode)

(ignore-errors (require 'xcscope))
(setq cscope-do-not-update-database t)
(setq cscope-display-cscope-buffer nil)
(global-set-key "\M-]" 'cscope-find-global-definition-no-prompting)
(global-set-key "\M--" 'cscope-pop-mark)

(require 'anything-config)
(setq anything-sources
	  (list anything-c-source-buffers
			anything-c-source-calculation-result
			anything-c-source-file-name-history
			anything-c-source-info-pages
			anything-c-source-man-pages
			anything-c-source-locate
			anything-c-source-emacs-commands))
(global-set-key "\C-^" 'anything)

(require 'twittering-mode)

;(server-start)
;
(autoload 'whitespace-mode "whitespace-mode")
(autoload 'systemtap-mode "systemtap-mode")
(add-to-list 'auto-mode-alist '("\\.stp\\'" . systemtap-mode))
;
(autoload 'ruby-mode "ruby-mode" nil t)
(autoload 'ruby-mode "ruby-electric" nil t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
      '(lambda ()
         (inf-ruby-keys)
))

(add-hook 'perl-mode-hook
          '(lambda ()
	     (setq tab-width 4)
             (setq perl-indent-level 4)))

(add-hook 'c-mode-common-hook
          '(lambda ()
	     (define-key c-mode-base-map "\C-cc" 'compile)
	     (define-key c-mode-base-map "\C-ce" 'next-error)
	     (c-toggle-hungry-state 1)
             (c-set-style "linux")
	     (setq tab-width 8)))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


(defun indent-4s ()
  "Set indent to 4-spaces mode"
  (interactive "")
  (c-set-style "stroustrup")
  (setq indent-tabs-mode nil))

(defun indent-8t ()
  "Set indent to 8-spaces tab mode"
  (interactive "")
  (c-set-style "linux")
  (setq indent-tabs-mode t))

;;
;;(set-scroll-bar-mode 'right)

;; suitable for Black Window style
;;(set-face-background 'modeline "white")
;;(set-face-foreground 'modeline "grey30")
(set-face-background 'highlight "grey10")
(set-face-foreground 'highlight "red")

(autoload 'tetris "tetris" "tetris game" t nil)

(setq-default show-trailing-whitespace t)

(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)
