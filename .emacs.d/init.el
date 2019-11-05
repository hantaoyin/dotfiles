;;;-*- mode:Emacs-Lisp; coding: utf-8-unix; fill-column:100 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU-Emacs Configuration
;;; =======================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;=================================================================================================
;;;Set up d-mode (for D programming language)
;;;=================================================================================================
;; (add-to-list 'load-path "~/.emacs.d/d-mode")
;; (require 'd-mode)
;; (autoload 'd-mode "d-mode" "Major mode for editing D code." t)
;; (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;;;=================================================================================================
;;;Set up rust-mode (for Rust programming language)
;;;=================================================================================================
(add-to-list 'load-path "~/.emacs.d/rust-mode")
(require 'rust-mode)
(autoload 'rust-mode "rust-mode" "Major mode for editing Rust code." t)
(add-to-list 'auto-mode-alist '("\\.rs[i]?\\'" . rust-mode))

;;;=================================================================================================
;;;Set up EMMS for mpv.
;;;=================================================================================================
;;; Copied from: https://github.com/dochang/emms-player-mpv/blob/master/emms-player-mpv.el
(require 'emms-setup)
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-compat)
(require 'emms-player-simple)
(require 'emms-mode-line)
(emms-mode-line nil)

(defcustom emms-mpv-input-file
  (expand-file-name (locate-user-emacs-file "emms-mpv-input-file"))
  "The file to send command to mpv."
  :type 'file
  :group 'emms)

(define-emms-simple-player mpv '(file url streamlist playlist)
  (concat "\\`\\(http\\|mms\\)://\\|"
          (emms-player-simple-regexp
           "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
           "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
           "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
           "flv" "webm"))
  "mpv" "--quiet" "--really-quiet" "-no-video")

(defadvice emms-player-mpv-start (around append-arguments activate)
  (unless (file-exists-p emms-mpv-input-file)
    (call-process "mkfifo" nil nil nil emms-mpv-input-file))
  (let* ((input-file (format "--input-file=%s" emms-mpv-input-file))
         (track-arg (let* ((track (ad-get-arg 0))
                           (track-type (emms-track-get track 'type))
                           (track-name (emms-track-name track)))
                      (if (memq track-type '(streamlist playlist))
                          (format "--playlist=%s" track-name)
                        track-name)))
         (process (apply 'start-process
                         emms-player-simple-process-name
                         nil
                         emms-player-mpv-command-name
                         (append emms-player-mpv-parameters
                                 (list input-file track-arg)))))
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-mode-line nil)
  (emms-player-started emms-player-mpv))

(emms-player-set emms-player-mpv
                 'pause
                 'emms-player-mpv-pause)

(emms-player-set emms-player-mpv
                 'resume
                 'emms-player-mpv-resume)

(emms-player-set emms-player-mpv
                 'seek
                 'emms-player-mpv-seek)

(emms-player-set emms-player-mpv
                 'seek-to
                 'emms-player-mpv-seek-to)

(defun emms-player-mpv-pause ()
    "Depends on mpv's --input-file option."
      (let ((cmd (format "echo 'set pause yes' > %s" emms-mpv-input-file)))
            (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-resume ()
    "Depends on mpv's --input-file option."
      (let ((cmd (format "echo 'set pause no' > %s" emms-mpv-input-file)))
            (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-seek (sec)
    "Depends on mpv's --input-file option."
      (let ((cmd (format "echo 'seek %d' > %s" sec emms-mpv-input-file)))
            (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-seek-to (sec)
    "Depends on mpv's --input-file option."
      (let ((cmd (format "echo 'seek %d absolute' > %s" sec emms-mpv-input-file)))
            (call-process-shell-command cmd nil nil nil)))

(provide 'emms-player-mpv)

(defun emms-player-mpv-volume-change (v)
  "Change the volume by V for mpv."
  (let ((cmd (format "echo 'add volume %d' > %s" v emms-mpv-input-file)))
    (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-volume-set (v)
  "Set the volume to V for mpv."
  (interactive "nmpv volume : ")
  (let ((cmd (format "echo 'set volume %d' > %s" v emms-mpv-input-file)))
    (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-volume-mute ()
  "Toggle mute status for mpv."
  (interactive)
  (let ((cmd (format "echo 'cycle mute' > %s" emms-mpv-input-file)))
    (call-process-shell-command cmd nil nil nil)))


(setq emms-player-list '(emms-player-mpv))
(emms-standard)
(setq emms-cache-file-coding-system 'utf-8)
(emms-toggle-repeat-playlist)

;;; When asked for emms-play-directory, always start from this one.
(setq emms-source-file-default-directory "/media/sdb2/audio") 

;;; Set up the track-description-function: function used to display
;;; the information of each track in the interactive playlist.
;; (defun my-simple-track-description-function (track)
;;   "Return a description of the current track."
;;     (format "%s"
;;              (file-relative-name
;;               (emms-track-name track) "/media/sdb2/audio/")))

;; (setq emms-track-description-function
;;       'my-simple-track-description-function)

(global-set-key [(XF86AudioPrev)] 'emms-previous)
(global-set-key [(XF86AudioNext)] 'emms-next)

;;;=================================================================================================
;;; Lua mode
;;;=================================================================================================
;; (require 'lua-mode)
;; (setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;;=================================================================================================
;;; Haskell mode
;;;=================================================================================================
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode")
(load "haskell-mode-autoloads.el")

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(autoload 'haskell-mode "haskell-mode" "Haskell editing mode." t)

;;;=================================================================================================
;;; org-mode
;;;=================================================================================================
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" .  org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-export-with-LaTeX-fragments t)

;;;=================================================================================================
;;; misc
;;;=================================================================================================
;; "C-x a" shows the pathname of the current file in minibuffer.
(defun show-file-name ()
  "Show the  pathname of the current file in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key "\C-xa" 'show-file-name)

;;; If the args list of a C++ function is on a new line, make it use the default
;;; indentation instead of aligning it with the open parenthesis '('.
(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'c++-mode-hook 'my-indent-setup)

;;; (setq font-lock-maximum-decoration 2)
(server-start)
(blink-cursor-mode -1)
;;; Set blink-cursor-mode to -1 doesn't work with urxvt >= 9.21, need the
;;; following:
(setq visible-cursor nil)
(menu-bar-mode -1)
; (tool-bar-mode nil)
(global-hl-line-mode t)
(transient-mark-mode t)
(if (equal window-system nil)
    (xterm-mouse-mode 1))
(setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(display-time)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(column-number-mode 1)

;;;--Replace yes/no+enter prompts with y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)

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
;;;Programming
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

;;;=================================================================================================
;;;for gnuplot editing
;;;=================================================================================================
(add-to-list 'auto-mode-alist '("\\.gnuplot$" .  conf-mode))
(add-to-list 'auto-mode-alist '("\\.script$" .  conf-mode))

;;;=================================================================================================
;;;Archlinux specific
;;;=================================================================================================
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

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
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "color-235"))))
 '(mode-line ((t (:background "lightgrey" :foreground "black" :box (:line-width 1 :style none) :width condensed :family "Trebuchet MS"))))
 '(region ((nil (:inverse-video t)))))
