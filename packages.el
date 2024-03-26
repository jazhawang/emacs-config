(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

; (package-initialize)

(defvar required-packages
  '(
    avy
    anzu
    ace-window    
    flyspell
    solarized-theme
    smart-mode-line
    helm
    which-key
    org
    which-key
    multiple-cursors
    docker
    dockerfile-mode
    gruvbox-theme
    helm
    dirvish
    marginalia
    use-package
    vterm
    )
  "A list of required packages. Will be either installed or updated at launch")

;; check if all packages are installed
(defun packages-installed-p (required-packages)
  "Predicate function, takes a list of packages. 
   Checks if all packages in a list are installed/updated"
  (if (null required-packages)
      t ; true
    (and (package-installed-p (car required-packages))
         (packages-installed-p (cdr required-packages)))))

(defun install-packages (required-packages)
  (dolist (package required-packages)
    (unless (package-installed-p package)
      (package-install package))))
 
(unless (packages-installed-p required-packages)
  ;; check for new packages by refreshing package database
  (message "%s" "Emacs is now refreshing package database")
  (package-refresh-contents)
  (message "%s" "Done refreshing!")
  ;; installs/updates all missing packages
  (install-packages required-packages))
  
(require 'jack-mode)


(require 'helm)

(helm-mode 1)
(setq helm-M-x-fuzzy-match t) ;; enable fuzzy matching for helm-M-x

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t) ;; fuzzy matching for helm-mini

(define-key jack-mode-map (kbd "C-c h") 'helm-command-prefix)
(define-key jack-mode-map [remap apropos-command] 'helm-apropos)
(define-key jack-mode-map [remap execute-extended-command] 'helm-M-x)
(define-key jack-mode-map [remap find-file] 'helm-find-files)
(define-key jack-mode-map [remap switch-to-buffer] 'helm-mini)


(defvar jack-main-row-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

(require 'ace-window)
(setq aw-keys jack-main-row-keys)
(define-key jack-mode-map [remap other-window] 'ace-select-window)


(require 'avy)
(setq avy-style 'at-full)
(setq avy-keys jack-main-row-keys)
(define-key jack-mode-map (kbd "C-c C-g") 'avy-goto-line)
(define-key jack-mode-map (kbd "C-c C-c") 'avy-goto-char)


(require 'anzu)
(global-anzu-mode 1)


(require 'flyspell)
(add-hook 'text-mode-hook
          (lambda () (flyspell-mode 1)))
(mapc (lambda (mode-hook) (add-hook mode-hook (lambda () (flyspell-prog-mode))))
      '(c-mode-common-hook emacs-lisp-mode-hook java-mode-hook python-mode-hook))



(require 'which-key)
(which-key-mode) ; activate


(require 'org)
(define-key jack-mode-map "\C-cl" 'org-store-link)
(define-key jack-mode-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(require 'multiple-cursors)
(define-key jack-mode-map (kbd "C->") 'mc/mark-next-like-this)
(define-key jack-mode-map (kbd "C-<") 'mc/mark-previous-like-this)
(define-key jack-mode-map (kbd "C-c d") 'mc/mark-all-in-region)


(require 'marginalia)
(marginalia-mode 1)


(use-package dirvish
  :ensure t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))
