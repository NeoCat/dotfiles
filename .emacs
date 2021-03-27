(setq load-path
      (append (list
	       (expand-file-name "~/.elisp/"))
	      load-path))

(setq byte-compile-warnings '(not cl-functions obsolete))

(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory "~/.elisp/"))
  (load-theme 'clarity t t)
  (enable-theme 'clarity))

(unless window-system (menu-bar-mode 0))

(when (getenv "POWERLINE_FONT_ENABLED")
  (require 'powerline)
  (set-face-attribute 'mode-line nil
                      :foreground "#fff"
                      :background "#800")
  (set-face-attribute 'powerline-active1 nil
                      :background "#333")
  (set-face-attribute 'powerline-active2 nil
                      :foreground "#999"
                      :background "#537"
		      :bold t)
  (powerline-default-theme))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [select] [(shift up)])

(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)

(setq indent-line-function 'indent-to-left-margin)

(setq-default transient-mark-mode t)

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq vc-follow-symlinks nil)

(setq display-time-24hr-format t)
(display-time)
(column-number-mode t)
(line-number-mode t)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.elisp/snippets"))
(yas-global-mode 1)

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

(autoload 'whitespace-mode "whitespace-mode")
(autoload 'systemtap-mode "systemtap-mode")
(add-to-list 'auto-mode-alist '("\\.stp\\'" . systemtap-mode))

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

(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
		   js-indent-level 2)))

(defun indent-2s ()
  "Set indent to 2-spaces mode"
  (interactive "")
  (setq indent-tabs-mode nil)
  (c-set-style "stroustrup")
  (setq c-basic-offset 2))

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

(defun indent-4ts ()
  "Set indent to 4-spaces and tab mixture mode"
  (interactive "")
  (c-set-style "linux")
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4))

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

(require 'flymake)
(setq flymake-gui-warnings-enabled nil)
(global-set-key "\C-cd" 'flymake-popup-current-error-menu)
(defun flymake-c-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-with-folder-structure))
	 (local-file  (file-relative-name
		       temp-file
		       (file-name-directory buffer-file-name))))
    (list "gcc" (list "-Wall" "-Wextra" "-fsyntax-only" "-fdiagnostics-color=never" local-file))))
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-with-folder-structure))
	 (local-file  (file-relative-name
		       temp-file
		       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" "-fdiagnostics-color=never" local-file))))
(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (flymake-mode t)))

(global-set-key "\M-n" 'flymake-goto-next-error)
(global-set-key "\M-p" 'flymake-goto-prev-error)

(setq fly-hack-helper "~/.elisp/fly-hack.py")
(require 'fly-hack nil t)

(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))
          (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "~/.elisp/flymake-ruby.sh" (list local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '(".+\\.rake$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(push '("^\\(.*\\):\\([0-9]+\\):[0-9]+: \\(.\\): \\(.*\\)$" 1 2 3 4) flymake-err-line-patterns)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(require 'rspec-mode)
(setenv "PAGER" (executable-find "cat"))
(add-hook 'ruby-mode-hook
          '(lambda ()
             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode t))
	     (inf-ruby-minor-mode)
	     (inf-ruby-switch-setup)
	     (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
          ))
(when (locate-library "mozc")
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay))

(setq ruby-insert-encoding-magic-comment nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values '((whitespace-line-column . 80))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-220"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-117"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-141"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-155"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-219"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-117"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-141"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-148"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-183"))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit rainbow-delimiters-base-face :foreground "red")))))
