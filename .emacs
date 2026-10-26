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

;; In-buffer completion --------------------------------------------------------

;; corfu pops up completion-at-point candidates, matched by orderless like the
;; minibuffer; cape adds extra sources (words in open buffers, file names). In
;; terminals corfu-terminal draws the popup with overlays instead of a child
;; frame; it disables itself on graphical displays.

(use-package corfu
  :custom
  (corfu-auto t)             ; pop up while typing...
  (corfu-auto-prefix 2)      ; ...after two characters...
  (corfu-auto-delay 0.2)     ; ...and a short pause
  (corfu-cycle t)
  (global-corfu-modes '((not text-mode) t))   ; not in prose: commit messages, markdown
  :init
  (global-corfu-mode 1))

(use-package corfu-terminal
  :after corfu
  :config (corfu-terminal-mode 1))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;; TAB indents the line; when it is already indented, TAB completes instead.
(setq tab-always-indent 'complete)

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

;; After a prefix key (C-c ', C-c C-d, C-c p, ...) show what can follow.
(use-package which-key
  :ensure nil   ; built into Emacs 30
  :config (which-key-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck :defer t)  ; auto-enabled by lsp-mode in LSP buffers

(use-package projectile
  :demand t
  :custom (projectile-completion-system 'default)   ; completing-read, i.e. vertico
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode +1))

;; IDE: LSP --------------------------------------------------------------------

(defun my/lsp-completion-setup ()
  "Match LSP candidates with orderless instead of lsp-mode's passthrough style."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

;; Language servers send large JSON payloads; the Emacs defaults (800 kB GC
;; threshold, 64 kB per process read) make lsp-mode noticeably sluggish.
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(use-package lsp-mode
  :init
  ;; Must be set before lsp-mode loads: it is baked into lsp-mode-map.
  (setq lsp-keymap-prefix "C-c '")
  :hook (((c++-mode rust-mode scala-mode) . lsp-deferred)
         (lsp-completion-mode . my/lsp-completion-setup)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-completion-provider :none)   ; plain completion-at-point, shown by corfu
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
  "Shared setup for Clojure buffers, in clojure-ts-mode and clojure-mode alike."
  (enable-paredit-mode)
  ;; clojure-mode indents instantly and agrees with cljfmt, which still formats
  ;; the buffer on save through clojure-lsp. LSP indentation would round-trip to
  ;; the server on every TAB and indent-region instead.
  (setq-local lsp-enable-indentation nil)
  (lsp-deferred))

(use-package paredit :defer t)

(use-package clojure-mode
  :hook (clojure-mode . my/clojure-mode-setup))

;; Tree-sitter based major mode: parser-accurate highlighting and indentation,
;; and the direction CIDER is heading. clojure-mode stays installed because
;; CIDER depends on it. The grammars are compiled on first use (needs gcc,
;; git). To go back, drop the remaps below.
(use-package clojure-ts-mode
  :hook (clojure-ts-mode . my/clojure-mode-setup)
  :init
  (dolist (remap '((clojure-mode . clojure-ts-mode)
                   (clojurescript-mode . clojure-ts-clojurescript-mode)
                   (clojurec-mode . clojure-ts-clojurec-mode)))
    (add-to-list 'major-mode-remap-alist remap))
  :config
  ;; The pinned markdown-inline grammar (docstring highlighting) is built with
  ;; tree-sitter ABI 15; Ubuntu 24.04's libtree-sitter only loads up to 14.
  ;; Without this, every Clojure buffer would retry cloning and compiling it.
  (when (< (treesit-library-abi-version) 15)
    (setq clojure-ts-use-markdown-inline nil
          clojure-ts-grammar-recipes
          (assq-delete-all 'markdown-inline clojure-ts-grammar-recipes))))

;; Division of labor with clojure-lsp: lsp drives navigation, completion and docs
;; while editing; CIDER's REPL-backed versions take precedence once connected
;; (they register later in the same hooks). Formatting stays with lsp/cljfmt.
(use-package cider
  :hook (cider-repl-mode . enable-paredit-mode)
  :custom
  ;; lsp hover already documents the symbol at point; CIDER's eldoc then only
  ;; shows the arglist of the enclosing call, instead of duplicating it.
  (cider-eldoc-display-for-symbol-at-point nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)   ; show the REPL, keep focus
  (cider-repl-display-help-banner nil)
  (cider-save-file-on-load t)                           ; no prompt on C-c C-k
  (cider-repl-history-file (locate-user-emacs-file "cider-history"))
  (nrepl-hide-special-buffers t))                       ; *nrepl-server* etc. out of C-x b

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

;; C-c plus a letter is reserved for users and works in terminals too; the
;; former C-M-[ { } bindings only ever reached Emacs in the GUI (C-M-[ is ESC
;; ESC, and terminals cannot send Ctrl+Shift combinations). C-c < > { } would
;; be terminal-safe as well but are reserved for major modes (markdown, org).

(defun my/indent-region-rigidly (columns)
  "Shift the region between point and mark by COLUMNS, keeping it active."
  (interactive "nColumns (negative shifts left): ")
  (let ((deactivate-mark nil))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) columns))))

(defun my/indent-region-right-4 () (interactive) (my/indent-region-rigidly 4))
(defun my/indent-region-left-4 () (interactive) (my/indent-region-rigidly -4))
(defun my/indent-region-right-2 () (interactive) (my/indent-region-rigidly 2))
(defun my/indent-region-left-2 () (interactive) (my/indent-region-rigidly -2))

(global-set-key (kbd "C-c i") #'my/indent-region-right-4)   ; indent
(global-set-key (kbd "C-c u") #'my/indent-region-left-4)    ; unindent
(global-set-key (kbd "C-c I") #'my/indent-region-right-2)
(global-set-key (kbd "C-c U") #'my/indent-region-left-2)

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

;; Terminal clipboard ----------------------------------------------------------

;; In a GUI frame Emacs talks to the system clipboard itself. In a terminal
;; frame it cannot, so every kill is piped to the first clipboard tool
;; available: clip.exe on WSL, wl-copy on Wayland, xclip on X11. (OSC 52 escape
;; sequences would avoid external tools, but GNOME's VTE-based terminals ignore
;; them.) The decision is made per kill, not at startup, because a daemon serves
;; GUI and `emacsclient -t' frames alike and starts before any of them exists.

(defun my/terminal-clipboard-command ()
  "Clipboard tool for the current environment, as an argument list, or nil."
  (cond ((getenv "WSL_DISTRO_NAME") '("clip.exe"))
        ((and (getenv "WAYLAND_DISPLAY") (executable-find "wl-copy")) '("wl-copy"))
        ((and (getenv "DISPLAY") (executable-find "xclip")) '("xclip" "-selection" "clipboard"))))

(defun my/clipboard-copy (text)
  "Copy TEXT to the system clipboard, natively or through an external tool."
  (if (display-graphic-p)
      (gui-select-text text)
    ;; A daemon's own environment may lack DISPLAY; an emacsclient frame carries
    ;; the client's environment, so consult that first.
    (let ((process-environment (append (frame-parameter nil 'environment)
                                       process-environment)))
      (when-let ((command (my/terminal-clipboard-command)))
        (let* ((process-connection-type nil)   ; a pipe, not a pty
               (proc (apply #'start-process "clipboard" nil command)))
          (set-process-query-on-exit-flag proc nil)
          (process-send-string proc text)
          (process-send-eof proc))))))

(setq interprogram-cut-function #'my/clipboard-copy)
