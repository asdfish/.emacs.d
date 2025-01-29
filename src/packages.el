; gpg cannot use windows paths
(if (string= system-type "windows-nt")
  (setq package-gnupghome-dir
        (replace-regexp-in-string "^c:" "/c" package-gnupghome-dir)))

; packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar my/ensure-installed-packages '(counsel
                                        catppuccin-theme
                                        compile-angel
                                        dashboard
                                        doom-modeline
                                        evil
                                        evil-collection
                                        evil-commentary
                                        ivy
                                        markdown-mode
                                        nerd-icons
                                        neotree
                                        tab-bar-buffers
                                        vim-tab-bar) "List of package to ensure are installed")

(defun my/packages-missing-p()
  "Check if all required packages are installed"
  (dolist (package my/ensure-installed-packages)
    (unless (package-installed-p package)
      t)))

(defun my/packages-install()
  "Install all packages in my/ensure-installed-packages"
  (interactive)
  (if (my/packages-missing-p)
    (package-refresh-contents)
    (dolist (package my/ensure-installed-packages)
      (unless (package-installed-p package)
        (package-install package)))))
(my/packages-install)

(use-package catppuccin-theme
             :after compile-angel
             :defines catppuccin-flavor
             :functions catppuccin
             :config
             (setq catppuccin-flavor 'macchiato)
             :init
             (load-theme 'catppuccin :no-confirm))

(use-package compile-angel
             :ensure t
             :demand t
             :functions
             compile-angel-on-load-mode
             compile-angel-on-save-local-mode
             ;; compile-angel-verbose
             ;; :custom
             ;; (compile-angel-verbose nil)
             :config
             (compile-angel-on-load-mode t)
             (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

(use-package counsel
             :after compile-angel
             :functions counsel-mode
             :config
             (counsel-mode 1))

(use-package dashboard
             :after compile-angel
             :after evil
             :init
             (setq dashboard-startup-banner 'logo
                   dashboard-center-content t
                   dashboard-vertically-center-content t
                   dashboard-items '((recents  . 5)
                                     (projects . 5)))
             :config
             (dashboard-setup-startup-hook)
             (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

(use-package doom-modeline
             :after compile-angel
             :init
             (doom-modeline-mode 1))

(use-package evil
             :after compile-angel
             :defines
             evil-want-keybinding
             evil-normal-state-map
             :functions evil-mode
             :init
             (setq evil-want-keybinding nil)
             :config
             (evil-mode 1)
             (define-key evil-normal-state-map (kbd "<SPC>tg") 'counsel-rg)
             (define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle))
(use-package evil-collection
             :after compile-angel
             :after evil
             :functions evil-collection-init
             :config
             (evil-collection-init))
(use-package evil-commentary
             :after compile-angel
             :after evil
             :functions evil-commentary-mode
             :config
             (evil-commentary-mode 1))

(use-package ivy
             :after compile-angel
             :functions ivy-mode
             :config
             (ivy-mode 1))

(use-package nerd-icons
             :after compile-angel
             :functions nerd-icons-install-fonts
             :config
             (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
               (nerd-icons-install-fonts t)))

(use-package neotree
             :after compile-angel
             :defines neo-theme
             :init
             (setq neo-theme 'nerd))

(use-package tab-bar-buffers
             :after compile-angel
             :functions tab-bar-buffers-mode
             :init
             (tab-bar-buffers-mode t))

(use-package vim-tab-bar
             :after compile-angel
             :functions vim-tab-bar-mode
             :init
             (vim-tab-bar-mode 1))
