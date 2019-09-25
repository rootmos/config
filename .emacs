(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(load-theme 'solarized-dark t)
(set-default-font "Inconsolata:pixelsize=22")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(global-display-line-numbers-mode)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(custom-set-variables)
(custom-set-faces
 '(agda2-highlight-datatype-face ((t (:foreground "DarkOrchid3"))))
 '(agda2-highlight-function-face ((t (:inherit default))))
 '(agda2-highlight-number-face ((t nil)))
 '(agda2-highlight-postulate-face ((t (:foreground "DarkOrchid3"))))
 '(agda2-highlight-primitive-face ((t (:foreground "DarkOrchid3"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "DarkOrchid3"))))
 '(agda2-highlight-record-face ((t (:foreground "DarkOrchid3"))))
 '(line-number ((t (:background "#073642"))))
 '(line-number-current-line ((t (:inherit line-number :weight bold)))))
