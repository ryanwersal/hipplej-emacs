;; -*-emacs-lisp-*-

(require 'cl)

;; Windows isn't welcome in my home so this is an easy way to tell if I'm working at the office
(defvar at-the-office-p (string-match "windows" (symbol-name system-type)))

(defvar libdir
  (if at-the-office-p
	  (expand-file-name "~/.emacs.d")
	(expand-file-name "~/src/hipplej-emacs")))
 
(defun libdir-file (file)
  (concat libdir "/" file))

(defvar lib-dirs
  '("auto-complete"
    "clojure-mode"
    "color-theme/color-theme"
	"csharp-mode"
    "js2-mode"
	"switch-window"
    "yasnippet"
	))

;; Add all the libs to the load path
(mapcar #'(lambda (path)
            (add-to-list 'load-path (libdir-file path)))
        lib-dirs)

;;(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
;;(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
 
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bell-volume 0)
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (other . "bsd"))))
 '(column-number-mode t)
 '(get-frame-for-buffer-default-instance-limit nil)
 '(gutter-buffers-tab-visible-p nil)
 '(make-backup-files nil)
 '(paren-mode (quote paren) nil (paren))
 '(show-paren-mode t)
 '(sound-load-list nil)
 '(tool-bar-mode nil)
 '(visible-bell t))

;; Set the default font
(defvar my-font
  (if at-the-office-p
      "-outline-Bitstream Vera Sans Mono-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1"
    "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
(setq initial-frame-alist `((font . ,my-font)))
(setq default-frame-alist `((font . ,my-font)))

;; Ensure the Command key is Meta on OSX
(if (not at-the-office-p)
    (progn
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))

;; If at the office then maximize the window.
(if at-the-office-p
    (add-hook 'window-setup-hook
			  (lambda()
				(interactive)
				(when (eq system-type 'windows-nt)
				  (w32-send-sys-command 61488)))))

;; Start with 50/50 vertical split.
(add-hook 'window-setup-hook 'split-window-horizontally)

;; Syntax Highlighting
(require 'color-theme)
(color-theme-initialize)
(color-theme-subdued)

;; Indentation madness
(setq-default tab-width 4)
(setq-default indent-tabs-mode at-the-office-p)

;; Always show the buffer name in the title bar
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

;; Set a reasonable email
(setq user-mail-address
      (if at-the-office-p "hipplej@zuerchertech.com" "brokenreality@gmail.com"))

;; Don't show the damn splash screen
(setq inhibit-splash-screen t)

;; Typing replaces the selected region
(delete-selection-mode t)

;; Don't make me type out 'yes' and 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't prompt me when killing buffers with active processes.
(setq kill-buffer-query-functions
	  (remq 'process-kill-buffer-query-function
			kill-buffer-query-functions))

;; Default to 'string' mode when using re-builder
(setq reb-re-syntax 'string)

;; Enable Line numbers
(global-linum-mode t)

;; Snippets for fancy completion
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (libdir-file "yasnippet/snippets"))

;; Fancy window switching
(require 'switch-window)

;; Use IDO to handle buffer switching and such
(require 'ido)
(ido-mode t)
(setq ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido") ; ignore these
	  ido-everywhere t            ; use for many file dialogs
	  ido-case-fold  t            ; be case-insensitive
	  ido-enable-flex-matching t  ; be flexible
	  ido-max-prospects 5         ; don't spam my minibuffer
	  ido-confirm-unique-completion t ; wait for RET, even with unique completion
	  ido-auto-merge-werk-directories-length -1) ; new file if no match

;; C mode specific stuff
(add-hook 'c-mode-common-hook
		  (lambda() 
			(local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Python mode specific stuff
(add-hook 'python-mode-hook
          (lambda()
            (setq tab-width 4
                  py-indent-offset 4
                  indent-tabs-mode at-the-office-p
                  py-smart-indentation (not at-the-office-p)
                  python-indent 4)))

;; Clojure mode specific stuff
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; C# mode specific stuff
(require 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; Fancy autocompletion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

; Prevent accidentally killing emacs.
(defun confirm-exit-from-emacs()
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
      (save-buffers-kill-emacs)))
(global-set-key "\C-x\C-c" 'confirm-exit-from-emacs)

;; Experimental Crap
;; (global-ede-mode 1)
;; (require 'semantic/sb)
;; (semantic-mode 1)

;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_GUI_EXPORT" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_CORE_EXPORT" . ""))

;; (semantic-add-system-include "C:/Qt/3.3.5-debug/include" 'c++-mode)

;; (add-to-list 'auto-mode-alist '("C:/Qt/3.3.5-debug/include" . c++-mode))

;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "C:/Qt/3.3.5-debug/include/qconfig.h")
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "C:/Qt/3.3.5-debug/include/qconfig-large.h")
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "C:/Qt/3.3.5-debug/include/qglobal.h")
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "C:/Qt/3.3.5-debug/include/qmodules.h")
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "C:/Qt/3.3.5-debug/include/qgplugin.h")

;; (semantic-add-system-include "C:/src/leds/branches/unstable/ztoolkit/include/" 'c++-mode)

;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "C:/src/leds/branches/unstable/ztoolkit/include/zconfig.h")
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "C:/src/leds/branches/unstable/ztoolkit/include/zglobal.h")
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "C:/src/leds/branches/unstable/ztoolkit/include/zexport.h")
