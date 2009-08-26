;; -*-emacs-lisp-*-

(require 'cl)

;; Windows isn't welcome in my home so this is an easy way to tell if I'm working at the office
(defvar at-the-office-p (string-match "windows" (symbol-name system-type)))

(defvar libdir
  (expand-file-name "~/src/hipplej-emacs"))
 
(defun libdir-file (file)
  (concat libdir "/" file))

(defvar lib-dirs
  '("slime"
    "color-theme/color-theme"
    "js2-mode"
    "clojure-mode"
    "swank-clojure"
    "yasnippet"))

;; Add all the libs to the load path
(mapcar #'(lambda (path)
            (add-to-list 'load-path (libdir-file path)))
        lib-dirs)
 
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

;; Syntax Highlighting
(require 'color-theme)
(color-theme-initialize)
(color-theme-subdued)

;; Indentation madness
(setq-default tab-width 4)
(setq-default indent-tabs-mode at-the-office-p)

;; Always show the buffer name in the title bar
(setq frame-title-format "emacs - %b")

;; Set a reasonable email
(setq user-mail-address
      (if at-the-office-p "hipplej@zuerchertech.com" "hipplej@flipchain.com"))

;; Don't show the damn splash screen
(setq inhibit-splash-screen t)

;; Typing replaces the selected region
(delete-selection-mode t)

;; Don't make me type out 'yes' and 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable Line numbers
(global-linum-mode t)

;; Snippets for fancy completion
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (libdir-file "yasnippet/snippets"))

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

;; C Mode specific stuff
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

;; Steve Yegge's fantastic Javascript mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Clojure Mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; Initialize Slime
(require 'slime)
(slime-setup)

;: Initialize Clojure Swank
;; This will obviosly fail miserably at work but unfortunately we don't
;; use Clojure at work so for now I don't really have to care too much
(setq swank-clojure-binary "/usr/local/bin/clj")
(require 'swank-clojure-autoload)
(swank-clojure-config
 (setq swank-clojure-jar-path "~/src/clojure/clojure.jar")
 (setq swank-clojure-extra-classpaths
       (list "~/src/clojure-contrib/clojure-contrib.jar")))
