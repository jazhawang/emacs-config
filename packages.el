(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))


(defvar required-packages
  '(
    avy
    anzu
    ace-window    
    flyspell
    solarized-theme
    smart-mode-line
    org
    which-key
    multiple-cursors
    docker
    dockerfile-mode
    use-package
    vterm
    magit
    projectile
    lsp-mode
    arduino-mode
    treemacs
    treemacs-projectile
    scad-mode
    company
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


;; (require 'helm)

;; (helm-mode 1)
;; (setq helm-M-x-fuzzy-match t) ;; enable fuzzy matching for helm-M-x

;; (setq helm-buffers-fuzzy-matching t
;;       helm-recentf-fuzzy-match t) ;; fuzzy matching for helm-mini

;; (define-key jack-mode-map (kbd "C-c h") 'helm-command-prefix)
;; (define-key jack-mode-map [remap apropos-command] 'helm-apropos)
;; (define-key jack-mode-map [remap execute-extended-command] 'helm-M-x)
;; (define-key jack-mode-map [remap find-file] 'helm-find-files)
;; (define-key jack-mode-map [remap switch-to-buffer] 'helm-mini)


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
      '(c-mode-common-hook emacs-lisp-mode-hook java-mode-hook python-mode-hook c++-mode-hook arduino-mode-hook scad-mode-hook))


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

;; (require 'lsp-mode)

;; https://emacs-lsp.github.io/lsp-mode/page/adding-new-language/

;; (defcustom lsp-arduino-executable
;;   '("arduino-language-server")
;;   "Command to start the arduino lsp server"
;;   :risky t
;;   :type 'file
;;   )

;; (require 'lsp-mode)
;; (add-hook 'scad-mode-hook 'lsp-mode)

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration
;;                '(arduino-mode . "arduino"))
;;   ;; (add-to-list 'lsp-language-id-configuration
;;   ;;          '(c-mode . "arduino"))
;;   ;; (add-to-list 'lsp-language-id-configuration
;;   ;;              '(c++-mode . "arduino"))

;;   (lsp-register-client
;;    (make-lsp--client
;;     :new-connection (lsp-stdio-connection
;;                      '("~/Documents/tarhorn/src/pico/arduino-language-server/arduino-language-server" "--fqbn" "rp2040:rp2040:rpipicow" "-cli" "/home/zhao/Documents/tarhorn/src/pico/arduino-cli" "-cli-config" "$HOME/.arduino15/arduino-cli.yaml" "-clangd" "/usr/bin/clangd"))
;;     :activation-fn (lsp-activate-on "arduino")
;;     :priority -1 ; should take lower priority than the regular c/c++ modes, for those files. The arduino mode can be enabled per project using the directory variable: lsp-enabled-clients
;;     :server-id 'arduino-language-server))

;;   ;; (lsp-register-client
;;   ;;  (make-lsp--client
;;   ;;   :new-connection (lsp-stdio-connection '("openscad-lsp"))
;;   ;;   :activation-fn (lsp-activate-on "openscad")
;;   ;;   :server-id 'openscad-language-server))
;;   )

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ; (setq lsp-keymap-prefix "C-c l")
  :hook ((scad-mode . (lambda () (lsp)))
         (add-hook c-mode-hook lsp)
         (add-hook c++-mode-hook lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)


(require 'treemacs)


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


(use-package casual-dired
  :ensure t
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))


(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; (use-package savehist                  
;;   :init
;;   (savehist-mode))

