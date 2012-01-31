;; -*-emacs-lisp-*-

(require 'cl)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bell-volume 0)
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (other . "bsd"))))
 '(column-number-mode t)
 '(ecb-options-version "2.40")
 '(ecb-source-path nil)
 '(get-frame-for-buffer-default-instance-limit nil)
 '(gutter-buffers-tab-visible-p nil)
 '(make-backup-files nil)
 '(paren-mode (quote paren) nil (paren))
 '(show-paren-mode t)
 '(sound-load-list nil)
 '(tool-bar-mode nil)
 '(visible-bell f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar libdir (expand-file-name "~/.emacs.d"))
(defun libdir-file (file) (concat libdir "/" file))

;; Generates load-path variables
;; This is pretty useless now but will remain just in case non-ELPA packages are added in the future
(defvar lib-dirs '("elpa"))
(mapcar #'(lambda (path) (add-to-list 'load-path (libdir-file path))) lib-dirs)

;; Load ELPA
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure ELPA-loaded packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch Windows Graphically
(require 'switch-window)

;; Visible bookmarks and other bookmark amenities
(require 'bm)
(setq bm-highlight-style 'bm-highlight-only-line)
(global-set-key (kbd "M-N") 'bm-next)
(global-set-key (kbd "M-P") 'bm-previous)
(global-set-key (kbd "M-SPC") 'bm-toggle)

;; Maxmize the window and start with 50/50 vertical split.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)
(add-hook 'window-setup-hook 'split-window-horizontally)

(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; Enable Yasnippet for templating system
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (libdir-file "elpa/yasnippet-0.6.1/snippets"))

;; Enable IDO to handle buffer switching and such (provides fuzzy pattern matching)
(require 'ido)
(ido-mode t)
(setq ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido") ; ignore these
      ido-everywhere t                           ; use for many file dialogs
	  ido-case-fold  t                           ; be case-insensitive
	  ido-enable-flex-matching t                 ; be flexible
	  ido-max-prospects 10                       ; don't spam my minibuffer (but I got some screen real estate to spare)
	  ido-confirm-unique-completion t            ; wait for RET, even with unique completion
	  ido-auto-merge-werk-directories-length -1) ; new file if no match

(require 'hl-line)
(global-hl-line-mode 1)
(require 'autopair)
(autopair-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;; Always show the buffer name in the title bar.
(setq-default
 frame-title-format
 (list '((buffer-file-name
          "Emacs - %f"
          (dired-directory
           dired-directory
           (revert-buffer-function " %b" ("%b - Dir:  " default-directory)))))))

(setq-default
 icon-title-format
 (list '((buffer-file-name
          "Emacs - %f"
          (dired-directory
           dired-directory
           (revert-buffer-function " %b"("%b - Dir:  " default-directory)))))))

;; Don't show the damn splash screen.
(setq inhibit-splash-screen t)

;; Typing replaces the selected region.
(delete-selection-mode t)

;; Don't make me type out 'yes' and 'no'.
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't prompt me when killing buffers with active processes.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;Auto revert when a file is changed
(global-auto-revert-mode t)

;; Default to 'string' mode when using re-builder.
(setq reb-re-syntax 'string)

;; Always show line numbers.
(global-linum-mode t)

;; I write C++ so default to the correct mode based on the filetypes I commonly use.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions & keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prevent accidentally killing emacs.
(defun confirm-exit-from-emacs()
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
      (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'confirm-exit-from-emacs)

;; Indent & Outdent
(global-set-key (kbd "C-<tab>") (kbd "C-u 4 C-x <tab>"))
(global-set-key (kbd "<backtab>") (kbd "C-u -4 C-x <tab>"))

;; Find & Replace
(global-set-key (kbd "C-x C-r") 'replace-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode setup and mode hooks                                                                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C mode specific stuff.
(add-hook 'c-mode-common-hook
          (lambda()
			(idle-highlight-mode +1)
            (progn
              (local-set-key  (kbd "C-c o") 'ff-find-other-file)
              (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t))))))

;; Python mode specific stuff.
(add-hook 'python-mode-hook
		  (lambda()
			(idle-highlight-mode +1)
            (setq tab-width 4
                  py-indent-offset 4
                  indent-tabs-mode t
                  py-smart-indentation t
                  python-indent 4)))

;; Fire up the emacs server
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "outline" :family "Consolas"))))
 '(bm-face ((((class color) (background dark)) (:background "#222222"))))
 '(hl-line ((t (:background "#023a48")))))
