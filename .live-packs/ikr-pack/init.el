;; User pack init file
;;
;; User this file to initiate the pack configuration.
;; See README for more information.

(live-load-config-file "bindings.el")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(electric-pair-mode 1)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-fill-column 100)
            (auto-fill-mode)))

(setq sgml-basic-offset 4)
