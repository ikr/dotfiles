;; Package system --------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; use-package ships with Emacs 29+. Every (use-package foo) block below also
;; installs foo if it is missing, so this file is the complete package list.
(require 'use-package)
(setq use-package-always-ensure t)

;; Keep Custom's generated code (package-selected-packages, theme hashes, ...)
;; out of this file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;; Tweaking of built-ins -------------------------------------------------------

(setq make-backup-files nil)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq warning-suppress-types '((lsp-mode) (comp)))
(column-number-mode)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(menu-bar-mode -1)

(delete-selection-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)                      ; minibuffer history across sessions
(setq enable-recursive-minibuffers t)
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

;; clojure-lsp, clj and bb live in ~/.local/bin; cargo tools in ~/.cargo/bin.
(dolist (dir '("~/.local/bin" "~/.cargo/bin"))
  (add-to-list 'exec-path (expand-file-name dir) t))

;; Theme -----------------------------------------------------------------------

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(set-face-attribute 'default nil
                    :family "mononoki"
                    :height 110
                    :weight 'normal)

;; Jumping around --------------------------------------------------------------

(use-package avy
  :bind ("C-c M-'" . avy-goto-char-2)
  :custom (avy-background t)
  :config (avy-setup-default))

(use-package ace-window
  :bind ("M-o" . ace-window))

;; Minibuffer completion -------------------------------------------------------

;; vertico shows candidates vertically, orderless matches space-separated terms in
;; any order, marginalia annotates candidates, consult provides the enhanced
;; commands (buffers, lines, grep, imenu...), embark acts on the candidate at
;; point. All plain completing-read, so every built-in command benefits.

(use-package vertico
  :init (vertico-mode 1))

;; Part of vertico: RET on a directory enters it, DEL deletes whole path parts.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s g" . consult-git-grep))   ; no ripgrep installed; else consult-ripgrep
  :custom
  ;; Let consult's project-aware commands use projectile's notion of a project.
  (consult-project-function (lambda (_) (projectile-project-root))))

;; C-. and C-; are taken by flyspell in nearly every buffer, and terminals cannot
;; send Ctrl+punctuation anyway, hence C-c .
(use-package embark
  :bind (("C-c ." . embark-act)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Multi-line edit -------------------------------------------------------------

(use-package multiple-cursors
  :bind (("C-c m" . mc/edit-lines)
         ("C-c n" . mc/mark-next-like-this)
         ("C-c b" . mc/unmark-next-like-this)
         ("C-c M-p" . mc/mark-previous-like-this)
         ("C-c M-q" . mc/unmark-previous-like-this)))

;; Insert Jira ticket reference ------------------------------------------------

(defun my/first-match (regex string)
  (when (string-match regex string)
    (match-string 0 string)))

(defun my/insert-jira-issue-id ()
  (interactive)
  (insert (my/first-match "^[A-Z0-9]+-[0-9]+"
                          (shell-command-to-string "git rev-parse --symbolic --abbrev-ref HEAD"))))

(global-set-key (kbd "C-c C-j") #'my/insert-jira-issue-id)

;; Markdown / Obsidian wiki-links ----------------------------------------------

;; markdown-mode already opens .md files and binds C-c C-o
;; (markdown-follow-thing-at-point) to follow the thing at point. These settings
;; teach it to follow Obsidian-style [[wiki-links]] too, e.g. [[Foo-Bar]] and
;; [[Foo-Bar|display text]] jump to Foo-Bar.md.
;; Obsidian-style "show backlinks": list [[This-Note]] references from the other
;; .md files in the same directory, in a navigable grep buffer.
(defun my/markdown-show-backlinks ()
  "List wiki-link backlinks to the current note from sibling .md files.
Greps this note's directory for [[This-Note]], [[This-Note|alias]] and
[[This-Note#heading]] references and shows them in a grep buffer, where
each hit is navigable (RET / n / p) like any grep result."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (require 'grep)
  (let* ((note (file-name-base buffer-file-name))
         (default-directory (file-name-directory buffer-file-name))
         (pattern (format "\\[\\[%s[]#|]" note)))
    (grep (format "grep -nE -- %s ./*.md" (shell-quote-argument pattern)))))

(use-package markdown-mode
  :custom
  (markdown-enable-wiki-links t)        ; treat [[Foo-Bar]] as a navigable link
  (markdown-wiki-link-alias-first nil)  ; Obsidian order is [[link|alias]]
  :bind (:map markdown-mode-map
              ("C-c C-w" . my/markdown-show-backlinks)))

;; Text modes ------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.jjdescription\\'" . text-mode))

(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode)
            (setq fill-column 100)))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package yaml-mode :defer t)
(use-package nginx-mode :defer t)
(use-package protobuf-mode :defer t)

(use-package sqlformat
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s2" "-g" "-u1"))
  :hook (sql-mode . sqlformat-on-save-mode))

;; IDE: project & editor conventions -------------------------------------------

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package company :defer t)   ; auto-enabled by lsp-mode in LSP buffers
(use-package flycheck :defer t)  ; auto-enabled by lsp-mode in LSP buffers

(use-package projectile
  :demand t
  :custom (projectile-completion-system 'default)   ; completing-read, i.e. vertico
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode +1))

;; IDE: LSP --------------------------------------------------------------------

(use-package lsp-mode
  :init
  ;; Must be set before lsp-mode loads: it is baked into lsp-mode-map.
  (setq lsp-keymap-prefix "C-c '")
  :hook ((c++-mode rust-mode scala-mode) . lsp-deferred)
  :custom
  (lsp-enable-file-watchers nil)
  ;; clangd's on-type formatting fights with the C++ auto-indent.
  (lsp-enable-on-type-formatting nil)
  (lsp-clients-clangd-args '("--header-insertion=never"))
  :config
  ;; Format with the language server on save, in every LSP-managed buffer.
  (add-hook 'lsp-mode-hook
            (lambda ()
              (when lsp-mode
                (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

  ;; lsp-mode hands xref the LSP 0-based line; xref is 1-based. The off-by-one is
  ;; usually cosmetic, but Metals' synthetic line-0 "Add ';' ..." workspace/symbol
  ;; hint becomes xref line 0, and xref--insert-xrefs does (floor (log 0 10)) =>
  ;; (overflow-error). Adding 1+ fixes both the crash and the off-by-one.
  (lsp-defun lsp--symbol-information-to-xref
    ((&SymbolInformation :kind :name
                         :location (&Location :uri :range (&Range :start (&Position :line
                                                                                    :character)))))
    "Return a `xref-item' from SYMBOL information."
    (xref-make (format "[%s] %s" (alist-get kind lsp-symbol-kinds) name)
               (xref-make-file-location (lsp--uri-to-path uri)
                                        (1+ line)
                                        character))))

(use-package lsp-ui
  :bind ("C-c d" . lsp-ui-doc-toggle))

(use-package consult-lsp
  :bind ("C-c s" . consult-lsp-symbols))

;; IDE: languages --------------------------------------------------------------

(defun my/clojure-mode-setup ()
  "Shared setup for Clojure buffers (clojure-mode now, clojure-ts-mode later)."
  (enable-paredit-mode)
  (lsp-deferred))

(use-package paredit :defer t)

(use-package clojure-mode
  :hook (clojure-mode . my/clojure-mode-setup))

(use-package cider :defer t)

(use-package rustic :defer t)
(use-package scala-mode :defer t)
(use-package lsp-metals :defer t)   ; picked up by lsp-mode via lsp-client-packages
(use-package kotlin-mode :defer t)

;; Misc editing commands -------------------------------------------------------

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(global-set-key (kbd "C-c C-b") 'comment-region)
(global-set-key (kbd "C-c C-v") 'uncomment-region)

(defalias 'my/duplicate-current-line (kmacro "C-a C-k C-k C-y C-y C-b M-m"))
(global-set-key (kbd "C-x C-x") #'my/duplicate-current-line)

(defun my/copy-buffer-file-path ()
  "Copy the full path of the current buffer's file to the kill ring."
  (interactive)
  (when-let ((path (buffer-file-name)))
    (kill-new path)
    (message "Copied: %s" path)))

;; C-c <letter> is reserved for users, so no major or minor mode will shadow it.
;; (The previous C-c C-c / C-c C-d / C-c C-n bindings clobbered CIDER's
;; eval-defun, doc map and set-ns.)
(global-set-key (kbd "C-c f") #'my/copy-buffer-file-path)

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

(defun my/toggle-identifier-naming-style ()
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

(global-set-key (kbd "C-c M--") #'my/toggle-identifier-naming-style)

;; WSL clipboard copy ----------------------------------------------------------

(defun my/on-wsl-p ()
  (getenv "WSL_DISTRO_NAME"))

(defun my/wsl-update-clip (&rest _args)
  "Update Windows clipboard with latest kill ring entry."
  (when-let ((str (current-kill 0)))
    (with-temp-buffer
      (insert str)
      (shell-command-on-region (point-min) (point-max) "clip.exe" nil nil))))

(if (my/on-wsl-p)
    (advice-add 'kill-new :after #'my/wsl-update-clip))
