;; Place your bindings here.

;; For example:
;;(define-key global-map (kbd "C-+") 'text-scale-increase)
;;(define-key global-map (kbd "C--") 'text-scale-decrease)

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

;---------------------------------------------------------------------------------------------------

(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat
   'identity
   (mapcar
    '(lambda (word) (capitalize (downcase word)))
    (split-string s "_"))
   ""))

(defun camelize-previous-snake (&optional beg end)
  "Camelize the previous snake cased string.

    If transient-mark-mode is active and a region is activated,
    camelize the region."
  (interactive "r")
  (unless (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
    (setq end (point)
          beg (+ (point) (skip-chars-backward "[:alnum:]_"))))
  (save-excursion
    (let ((c (camelize (buffer-substring-no-properties beg end))))
      (delete-region beg end)
      (goto-char beg)
      (insert c))))

(global-set-key (kbd "M-_") 'camelize-previous-snake)
