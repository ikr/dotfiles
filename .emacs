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
   '(sqlformat rust-mode lsp-metals scala-mode free-keys company ivy ace-window cider clojure-mode company-lsp editorconfig flx-ido flycheck kotlin-mode lsp-mode lsp-ui multiple-cursors nginx-mode paredit persistent-scratch quelpa quelpa-use-package salt-mode yaml-mode zenburn-theme))
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (comp))))
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

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(menu-bar-mode -1)

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
(global-set-key (kbd "C-c M-'") 'avy-goto-char-2)
(setq avy-background t)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "M-o") 'ace-window)

;; Multi-line edit -------------------------------------------------------------

(require 'multiple-cursors)
(global-set-key (kbd "C-c M-m") 'mc/edit-lines)
(global-set-key (kbd "C-c M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c M-b") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-c M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-q") 'mc/unmark-previous-like-this)

;; Insert Jira ticket reference ------------------------------------------------

(defun first-match (regex string)
  (when (string-match regex string)
    (match-string 0 string)))

(defun insert-jira-issue-id ()
  (interactive)
  (insert (first-match "^[A-Z0-9]+-[0-9]+"
                       (shell-command-to-string "git rev-parse --symbolic --abbrev-ref HEAD"))))

(global-set-key (kbd "C-c C-j") 'insert-jira-issue-id)


;; IDE -------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . text-mode))

(add-hook 'text-mode-hook
  (lambda ()
    (flyspell-mode)
    (setq fill-column 100)))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'lsp-format-buffer t t)))

(add-hook 'rust-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'lsp-format-buffer t t)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'lsp-format-buffer t t)))

(require 'sqlformat)
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g" "-u1"))
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)

(editorconfig-mode 1)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'lsp-mode)
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c '") lsp-command-map))
(add-hook 'clojure-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(setq lsp-clients-clangd-args '("--header-insertion=never"))

(persistent-scratch-setup-default)

(global-set-key (kbd "C-c C-b") 'comment-region)
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

;; WSL clipboard copy

(defun on-wsl-p ()
  (getenv "WSL_DISTRO_NAME"))

(defun wsl-update-clip (&rest _args)
  "Update Windows clipboard with latest kill ring entry."
  (when-let ((str (current-kill 0)))
    (with-temp-buffer
      (insert str)
      (shell-command-on-region (point-min) (point-max) "clip.exe" nil nil))))

(if (on-wsl-p)
    (advice-add 'kill-new :after #'wsl-update-clip))
