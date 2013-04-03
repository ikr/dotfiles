(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq fill-column 100)
(setq column-number-mode t)
(global-hl-line-mode 1)


(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

(make-directory "~/.emacs.d/autosaves/" t)


(load "~/.emacs.d/extensions/nxhtml/autostart.el")


(add-to-list 'load-path "~/.emacs.d/extensions")

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


(server-start)
