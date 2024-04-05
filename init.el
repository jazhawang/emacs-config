

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'calendar)

(defvar jack-mode-dir "~/.config/emacs/jack_mode.el")
(defvar package-dir "~/.config/emacs/packages.el")


(load jack-mode-dir)
(load package-dir)


(setq initial-major-mode 'lisp-mode)

(setq initial-scratch-message (format "\

;;      ██╗ █████╗  ██████╗██╗  ██╗██╗███████╗
;;      ██║██╔══██╗██╔════╝██║ ██╔╝╚═╝██╔════╝
;;      ██║███████║██║     █████╔╝    ███████╗
;; ██   ██║██╔══██║██║     ██╔═██╗    ╚════██║
;; ╚█████╔╝██║  ██║╚██████╗██║  ██╗   ███████║
;;  ╚════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝   ╚══════╝
                                            
;; ███████╗███╗   ███╗ █████╗  ██████╗███████╗ 
;; ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝ 
;; █████╗  ██╔████╔██║███████║██║     ███████╗ 
;; ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║ 
;; ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║ 
;; ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝                                
;; %s
" (calendar-date-string (calendar-current-date) nil)))


;; Sets the default indent style for c-like programming languages
(setq c-default-style '((java-mode . "bsd")
                        (awk-mode . "awk")
                        (other . "bsd")))

;; Sets the default indent spacing for c-like programming languages
(setq c-basic-offset 4)

;; changes how scrolling works, only scrolls down by one line at a time
(setq scroll-step 1
      scroll-conservatively 10000)

(setq-default indent-tabs-mode nil) ; stop using tabs to indent


(setq-default tab-width 4)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; disable startup screen
(setq inhibit-startup-screen t)


(menu-bar-mode -1) ;; no menubar
(tool-bar-mode -1) ;; no toolbar


(setq-default truncate-lines 1) ;; no wordwrap


(setq user-full-name "Zhao Wang")

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; configure auto-save directory
(setq temporary-file-directory (concat user-emacs-directory "/backup"))

;; create directory if not exists
(unless (file-directory-p temporary-file-directory)
  (make-directory temporary-file-directory))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week)) 
      (message "%s" file)
      (delete-file file))))

;; keep init.el clean by having Custom write variables to separate file
(setq custom-file (concat user-emacs-directory "/custom.el"))


(load-theme 'solarized-light t)


(jack-mode)

(toggle-debug-on-error t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; macos specific stuff
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
