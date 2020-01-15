(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(unless (package-installed-p 'bash-completion)
  (package-install 'bash-completion))

(autoload 'bash-completion-dynamic-complete "bash-completion"
    "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
     'bash-completion-dynamic-complete)

(unless (package-installed-p 'ag)
  (package-install 'ag))

(set-default 'truncate-lines t)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
(projectile-mode +1)
(setq projectile-project-search-path '("~/git" "~/arweave"))

(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))
(load-theme 'solarized-dark t)
(set-default-font "Inconsolata:pixelsize=22")

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode)
(setq-default show-trailing-whitespace t)

(require 'whitespace)
(setq whitespace-style '(tabs tab-mark))
(global-whitespace-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren 1)

(setq inhibit-startup-screen t)

(add-hook 'focus-in-hook (lambda () (shell-command "text")))
(add-hook 'focus-out-hook (lambda () (shell-command "dv")))

;; (grep-apply-setting 'grep-command "ag --filename --nogroup --column")

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ag projectile proof-general solarized-theme magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "DarkOrchid3"))))
 '(agda2-highlight-function-face ((t (:inherit default))))
 '(agda2-highlight-number-face ((t nil)))
 '(agda2-highlight-postulate-face ((t (:foreground "DarkOrchid3"))))
 '(agda2-highlight-primitive-face ((t (:foreground "DarkOrchid3"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "DarkOrchid3"))))
 '(agda2-highlight-record-face ((t (:foreground "DarkOrchid3"))))
 '(line-number ((t (:background "#073642"))))
 '(line-number-current-line ((t (:inherit line-number :weight bold)))))
