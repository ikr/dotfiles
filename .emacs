;; Indent left, indent right ---------------------------------------------------

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

;; CamelCase <-> snake_case ----------------------------------------------------

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

;; Increment number at point ---------------------------------------------------

(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

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
    (org clang-format flycheck-kotlin prettier-js persistent-scratch groovy-mode kotlin-mode tide graphviz-dot-mode bash-completion cider ace-jump-mode company jinja2-mode elm-mode window-number lua-mode zenburn-theme undo-tree rjsx-mode editorconfig markdown-mode php-mode yaml-mode multiple-cursors))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Tweaking of built-ins -------------------------------------------------------

(setq make-backup-files nil)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(column-number-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-undo-tree-mode)
(show-paren-mode 1)
(setq-default fill-column 100)
(setq exec-path (append exec-path '("/home/ikr/bin")))
(desktop-save-mode 1)
(global-auto-revert-mode t)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Hooks -----------------------------------------------------------------------

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq elm-format-on-save t)

(add-hook 'markdown-mode-hook
  (lambda ()
    (flyspell-mode)
  ))

(add-hook 'vc-git-log-edit-mode-hook
  (lambda ()
    (flyspell-mode)
    (setq fill-column 72)
    (turn-on-auto-fill)))

;; Macros ----------------------------------------------------------------------

(fset 'import-react-js
   [?i ?m ?p ?o ?r ?t ?  ?* ?  ?a ?s ?  ?R ?e ?a ?c ?t ?  ?f ?r ?o ?m ?  ?\' ?r ?e ?a ?c ?t ?\' return])

(fset 'import-assert
   [?i ?m ?p ?o ?r ?t ?  ?* ?  ?a ?s ?  ?a ?s ?s ?e ?r ?t ?  ?f ?r ?o ?m ?  ?\' ?a ?s ?s ?e ?r ?t ?\C-e return])

;; Key bindings ----------------------------------------------------------------

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun insert-jira-issue-id ()
  (interactive)
  (insert (shell-command-to-string "jira-prefix")))

(global-set-key (kbd "C-c C-j") 'insert-jira-issue-id)
(global-set-key (kbd "C-M-|") 'clang-format-buffer)


;; Theme -----------------------------------------------------------------------

(load-theme 'zenburn t)
(set-face-attribute 'default nil
		    :family "mononoki"
		    :height 95
		    :weight 'bold)

;; Autocomplete ----------------------------------------------------------------

(require 'company)
(add-to-list 'company-backends 'company-elm)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)

;; JavaScript ------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(require 'js2-mode)
(js2-mode-hide-warnings-and-errors)

;; TypeScript ------------------------------------------------------------------

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; single quotes instead of the default double quotes
(setq web-mode-auto-quote-style nil)

;; formats the buffer before saving
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'tide-mode-hook 'prettier-js-mode)

;; enable typescript-tslint checker
(require 'flycheck)
(flycheck-add-mode 'typescript-tslint 'web-mode)

(global-set-key (kbd "M-&") 'tide-references)
(global-set-key (kbd "M-RET") 'tide-fix)
(global-set-key (kbd "C-M-m") 'tide-refactor)
(global-set-key (kbd "C-M-S-o") 'tide-organize-imports)
(global-set-key [(shift f6)] 'tide-rename-symbol)

;; C++ -------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; Create Header Guards with f12
(global-set-key
 [f12] '(lambda ()
          (interactive)
          (if (buffer-file-name)
              (let*
                  ((fName (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
                   (ifDef (concat "#ifndef " fName "_H" "\n#define " fName "_H" "\n"))
                   (begin (point-marker))
                   )
                (progn
                  ; If less then 5 characters are in the buffer, insert the class definition
                  (if (< (- (point-max) (point-min)) 5 )
                      (progn
                        (insert "\nclass " (capitalize fName) "{\npublic:\n\nprivate:\n\n};\n")
                        (goto-char (point-min))
                        (next-line-nomark 3)
                        (setq begin (point-marker))
                        )
                    )

                  ;Insert the Header Guard
                  (goto-char (point-min))
                  (insert ifDef)
                  (goto-char (point-max))
                  (insert "\n#endif" " //" fName "_H")
                  (goto-char begin))
                )
            ;else
            (message (concat "Buffer " (buffer-name) " must have a filename")))))

;; Misc. modes tweaking --------------------------------------------------------

(setq-default graphviz-dot-auto-indent-on-semi nil)
(editorconfig-mode 1)

(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

(require 'log-edit)
(require 'vc-git)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . vc-git-log-edit-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))
(bash-completion-setup)
(persistent-scratch-setup-default)

;; -----------------------------------------------------------------------------

(server-start)
