;; Indent left, indent right

(global-set-key (kbd "C-M-]")
    #'(lambda (arg)
        (interactive "p")
        (save-excursion
            (let ((deactivate-mark nil))
            (indent-rigidly (min (point) (mark))
                (max (point) (mark))
                4)))))

(global-set-key (kbd "C-M-[")
    #'(lambda (arg)
        (interactive "p")
        (save-excursion
            (let ((deactivate-mark nil))
            (indent-rigidly (min (point) (mark))
                (max (point) (mark))
                (- 4))))))

(global-set-key (kbd "C-M-}")
    #'(lambda (arg)
        (interactive "p")
        (save-excursion
            (let ((deactivate-mark nil))
            (indent-rigidly (min (point) (mark))
                (max (point) (mark))
                2)))))

(global-set-key (kbd "C-M-{")
    #'(lambda (arg)
        (interactive "p")
        (save-excursion
            (let ((deactivate-mark nil))
            (indent-rigidly (min (point) (mark))
                (max (point) (mark))
                (- 2))))))

;; CamelCase <-> snake_case

(defun mo-toggle-identifier-naming-style ()
  "Toggles the symbol at point between C-style naming,
e.g. `hello_world_string', and camel case,
e.g. `HelloWorldString'."
  (interactive)
  (let* ((symbol-pos (bounds-of-thing-at-point 'symbol))
         case-fold-search symbol-at-point cstyle regexp func)
    (unless symbol-pos
      (error "No symbol at point"))
    (save-excursion
      (narrow-to-region (car symbol-pos) (cdr symbol-pos))
      (setq cstyle (string-match-p "_" (buffer-string))
            regexp (if cstyle "\\(?:\\_<\\|_\\)\\(\\w\\)" "\\([A-Z]\\)")
            func (if cstyle
                     'capitalize
                   (lambda (s)
                     (concat (if (= (match-beginning 1)
                                    (car symbol-pos))
                                 ""
                               "_")
                             (downcase s)))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match (funcall func (match-string 1))
                       t nil))
      (widen))))

(global-set-key (kbd "M--") 'mo-toggle-identifier-naming-style)

;; MELPA repository ------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(package-selected-packages
   (quote
    (company jinja2-mode elm-mode window-number lua-mode zenburn-theme undo-tree rjsx-mode editorconfig markdown-mode php-mode yaml-mode multiple-cursors))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Tweaking --------------------------------------------------------------------

(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq elm-format-on-save t)

(column-number-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode 1)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(load-theme 'zenburn t)

(set-face-attribute 'default nil
		    :family "mononoki"
		    :height 110
		    :weight 'bold)

(global-hl-line-mode 1)

(require 'company)
(add-to-list 'company-backends 'company-elm)
(add-hook 'after-init-hook 'global-company-mode)

(setq-default indent-tabs-mode nil)
(editorconfig-mode 1)

(global-undo-tree-mode)
(show-paren-mode 1)

(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

(setq-default fill-column 100)

(add-hook 'markdown-mode-hook
  (lambda ()
    (flyspell-mode)
  ))

(add-hook 'vc-git-log-edit-mode-hook
  (lambda ()
    (flyspell-mode)
    (setq-local company-dabbrev-downcase nil)
  ))

(require 'log-edit)
(require 'vc-git)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . vc-git-log-edit-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

(server-start)
