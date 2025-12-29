;;;-*- mode:Emacs-Lisp; coding: utf-8-unix; fill-column:100 -*-

;;;=================================================================================================
;;; Rust mode
;;;=================================================================================================
;; (require 'rust-mode)
;; (autoload 'rust-mode "rust-mode" "Major mode for editing Rust code." t)
;; (add-to-list 'auto-mode-alist '("\\.rs[i]?\\'" . rust-mode))

;;;=================================================================================================
;;; Lua mode
;;;=================================================================================================
;; (require 'lua-mode)
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" .  lua-mode))

;;;=================================================================================================
;;; Haskell mode
;;;=================================================================================================
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode")
;; (load "haskell-mode-autoloads.el")

;; (require 'haskell-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-to-list 'auto-mode-alist '("\\.hs$" .  haskell-mode))
;; (autoload 'haskell-mode "haskell-mode" "Haskell editing mode." t)

;;;=================================================================================================
;;; org-mode
;;;=================================================================================================
;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org$" .  org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)
;; (setq org-export-with-LaTeX-fragments t)

;;;=================================================================================================
;;; gnuplot
;;;=================================================================================================
(add-to-list 'auto-mode-alist '("\\.gnuplot$" .  conf-mode))

;;;=================================================================================================
;;; Arch Linux PKGBUILD files
;;;=================================================================================================
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

;;;=================================================================================================
;;; C / C++
;;;=================================================================================================
;;; Use C-c C-c to comment a region.
(mapcar
 (lambda (mode)
   (let ((mode-hook (intern (concat (symbol-name mode) "-hook")))
         (mode-map (intern (concat (symbol-name mode) "-map"))))
     (add-hook mode-hook
               `(lambda nil
                  (local-set-key
                   (kbd "\C-c \C-c") 'comment-region))))) '(c-mode c++-mode cperl-mode emacs-lisp-mode
                                                                   java-mode html-mode lisp-mode ruby-mode
                                                                   sh-mode python-mode lua-mode awk-mode
                                                                   haskell-mode scheme-mode latex-mode
                                                                   conf-mode conf-xdefaults-mode))

;;; indent width for C like languages
(setq c-basic-offset 2)

(defun my-c++-custom-hook ()
  (c-set-style "linux")
  ;; Do not indent merely because we are in a namespace.
  (c-set-offset 'innamespace [0])
  ;; Remove excessive indentation inside a lambda.
  ;; (c-set-offset 'inlambda [0])
  )
(add-hook 'c++-mode-hook 'my-c++-custom-hook)

;;; If the args list of a C++ function is on a new line, make it use the default
;;; indentation instead of aligning it with the open parenthesis '('.
(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'c++-mode-hook 'my-indent-setup)

;;;=================================================================================================
;;; misc
;;;=================================================================================================

;; "C-x a" shows the pathname of the current file in minibuffer.
(defun show-file-name ()
  "Show the  pathname of the current file in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key "\C-xa" 'show-file-name)

(server-start)
;;; Set blink-cursor-mode to -1 doesn't work with urxvt >= 9.21, need the
;;; following:
;; (blink-cursor-mode -1)
(setq visible-cursor nil)
(menu-bar-mode -1)
; (tool-bar-mode nil)
(global-hl-line-mode t)
(transient-mark-mode t)
(if (equal window-system nil)
    (xterm-mouse-mode 1))
(setq display-time-24hr-format t)
(display-time)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(column-number-mode 1)

;;;--Replace yes/no+enter prompts with y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;;;turn on auto-fill mode on text-mode and derived modes.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; turn on hs-minor mode on programming modes.
;;; copied code from function turn-on-auto-fill
;; (defun turn-on-hs-minor-mode ()
;;   "Unconditionally turn on hs minor mode."
;;   (hs-minor-mode 1))
;; (add-hook 'c-mode-common-hook 'turn-on-hs-minor-mode)
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist '(("". "/tmp")))
(setq-default make-backup-files nil)
(setq inhibit-startup-message t)
(show-paren-mode t)

;; Deal with slowness when viewing long lines (even in fundamental mode).
(setq-default bidi-display-reordering nil)
(setq line-move-visual nil)
(set-default 'truncate-lines t)

;;;=================================================================================================
;;;custom faces
;;;=================================================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8-unix))))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "brightmagenta"))))
 '(highlight ((t (:background "gray20"))))
 '(mode-line ((t (:background "brightblack" :foreground "black" :box (:line-width 1)))))
 '(region ((nil (:inverse-video t)))))
