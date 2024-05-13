(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/"))) 


(defvar required-packages
  '(
    avy
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
    lsp-mode
    arduino-mode
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

(defvar jack-main-row-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
(require 'ace-window)
(setq aw-keys jack-main-row-keys)
(define-key jack-mode-map [remap other-window] 'ace-select-window)


(require 'avy)
(setq avy-keys jack-main-row-keys)
(define-key jack-mode-map (kbd "C-c C-g") 'avy-goto-line)
(define-key jack-mode-map (kbd "C-c C-c") 'avy-goto-char)


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


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package casual-dired
  :ensure t
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))


(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))


(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t

  :bind (("C-s" . consult-line)
         ("C-h C-m" . consult-man)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)         
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         )
  

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
)


(use-package orderless
 :ensure t
 :init
 (setq completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles . (partial-completion))))))
