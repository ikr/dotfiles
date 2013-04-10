(custom-set-variables
  '(indent-tabs-mode nil)
  '(tab-width 4)
  '(standard-indent 4)
  '(fill-column 100))

(setq column-number-mode t)


(defface hl-line '((t (:background "#220000")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)


(global-set-key (kbd "M-]")
    #'(lambda (arg)
        (interactive "p")
        (save-excursion
            (let ((deactivate-mark nil))
            (indent-rigidly (min (point) (mark))
                (max (point) (mark))
                4)))))

(global-set-key (kbd "M-[")
    #'(lambda (arg)
        (interactive "p")
        (save-excursion
            (let ((deactivate-mark nil))
            (indent-rigidly (min (point) (mark))
                (max (point) (mark))
                (- 4))))))


(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

(make-directory "~/.emacs.d/autosaves/" t)


(add-to-list 'load-path "~/.emacs.d/extensions")

(require 'pi-php-mode)

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)


(global-set-key (kbd "C-x f") 'find-file-in-project)

(defvar ffip-patterns
  '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl"
    "*.sh" "*.erl" "*.hs" "*.ml" "*.php" "*.json" "*.sls" "*.ini" "*.conf" "*.xml")
  "List of patterns to look for with `find-file-in-project'.")

(defvar ffip-find-options "-not -path '*/vendor*'" "Extra options to pass to `find' when using find-file-in-project.")


(server-start)
