;; MELPA repository ------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Code-gen-ed -----------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9ffe970317cdfd1a9038ee23f4f5fe0b28b99950281799e4397e1a1380123147" default))
 '(electric-pair-mode t)
 '(package-selected-packages
   '(quelpa-use-package quelpa salt-mode nginx-mode persistent-scratch kotlin-mode cider paredit clojure-mode flx-ido ace-window yaml-mode editorconfig flycheck lsp-ui company-lsp lsp-mode zenburn-theme multiple-cursors))
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(quelpa '(copilot :fetcher github
                  :repo "zerolfx/copilot.el"
                  :branch "main"
                  :files ("dist" "*.el")))

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
(show-paren-mode 1)
(setq-default fill-column 100)
(desktop-save-mode 1)
(global-auto-revert-mode t)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq exec-path (append exec-path '("/home/ikr/bin")))
(setq exec-path (append exec-path '("/home/ikr/.cargo/bin")))

(setenv "PATH" (concat
                "/home/ikr/bin" path-separator
                (getenv "PATH")))

;; Theme -----------------------------------------------------------------------

(load-theme 'zenburn t)
(set-face-attribute 'default nil
		    :family "mononoki"
		    :height 110
		    :weight 'bold)

;; Jumping around --------------------------------------------------------------

(avy-setup-default)
(global-set-key (kbd "C-M-'") 'avy-goto-char-2)
(setq avy-background t)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Multi-line edit -------------------------------------------------------------

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/unmark-previous-like-this)

;; IDE -------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . text-mode))

(add-hook 'text-mode-hook
  (lambda ()
    (flyspell-mode)
    (setq fill-column 72)
    (turn-on-auto-fill)))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'lsp-format-buffer t t)))

(add-hook 'rust-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'lsp-format-buffer t t)))

(editorconfig-mode 1)

(setq lsp-keymap-prefix "s-'")
(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(setq lsp-clients-clangd-args '("--header-insertion=never"))

(persistent-scratch-setup-default)
(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-v") 'uncomment-region)

(defalias 'duplicate-current-line (kmacro "C-a C-k C-k C-y C-y C-b M-m"))
(global-set-key (kbd "C-x C-x") (quote duplicate-current-line))

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
