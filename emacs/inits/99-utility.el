;;; 99-utility.el --- functions, keybinds and alias  -*- lexical-binding: t; -*-

;;--------------------------------------------------
;; Functions
;;--------------------------------------------------
;;; Information
(defun my/get-current-path ()
  (interactive)
  (or (buffer-file-name) (expand-file-name default-directory)))

(defun my/get-text-encoding ()
  (interactive)
  (message (format "%s" buffer-file-coding-system)))

(defun my/get-face (&optional point)
  (interactive)
  (or point (setq point (point)))
  (let ((face (or (get-char-property point 'read-face-name)
                  (get-char-property point 'face))))
    (if face
        (message (format "%s" face))
      (message "no face"))))

(defun my/get-background-color ()
  (interactive)
  (princ (face-attribute 'default :background)))

;;; Replace
(defun my/replace-strings-in-region-by-list (list)
  "Replace strings in a region according to list"
  (if mark-active
      (let* ((beg  (region-beginning))
             (end  (region-end))
             (word (buffer-substring-no-properties beg end)))
        (mapc (lambda (r)
                (setq word (replace-regexp-in-string (car r) (cdr r) word)))
              list)
        (delete-region beg end)
        (insert word))
    (error "Need to make region")))

(defun my/join-multi-lines-to-one ()
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("\\(\n\\s-*\\)+" . ""))))

(defun my/escape-in-region ()
  "Escape code in region"
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("\t" . "  ")
     ("\<" . "&lt;")
     ("\>" . "&gt;")
     ("\&" . "&amp;")
     ("\'" . "&#039;")
     ("\"" . "&quot;"))))

(defun my/convert-to-single-byte-number ()
  "Convert multi-byte numbers in region into single-byte number"
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("１" . "1")
     ("２" . "2")
     ("３" . "3")
     ("４" . "4")
     ("５" . "5")
     ("６" . "6")
     ("７" . "7")
     ("８" . "8")
     ("９" . "9")
     ("０" . "0"))))

(defun my/convert-yakumono-to-half-width ()
  "Replace multi byte punctuation marks to half width chars"
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("、" . "､")
     ("。" . "｡")
     ("「" . "｢")
     ("」" . "｣")
     ("［" . "[")
     ("］" . "]")
     ("｛" . "{")
     ("｝" . "}")
     ("（" . "(")
     ("）" . ")")
     ("・" . "･"))))

;;; Other
(defun my/count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(add-to-list 'mode-line-format '(:eval (my/count-lines-and-chars)))

(defun my/other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(defun my/killregion-or-deletewindow ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-window)))

(defun my/goto-line-beginning-or-indent (&optional position)
  (interactive)
  (or position (setq position (point)))
  (let ((starting-position (progn (back-to-indentation) (point))))
    (if (eq starting-position position)
        (move-beginning-of-line 1))))

(defun my/delete-DS_Store-under-current-directory-recursively ()
  (interactive)
  (shell-command "find . -name '*.DS_Store' -type f -delete")
  (if (eq major-mode 'dired-mode)
      (revert-buffer)))

;;--------------------------------------------------
;; Keybind
;;--------------------------------------------------
(bind-keys*
 ("C-a" . my/goto-line-beginning-or-indent)
 ("C-h" . backward-delete-char)
 ("C-o" . my/other-window-or-split)
 ("C-w" . my/killregion-or-deletewindow)
 ("C-z" . help-command)
 ("C-S-f" . forward-word)
 ("C-S-b" . backward-word)
 ("C-S-v" . cua-scroll-down)
 ("C-c l" . toggle-truncate-lines))

;;--------------------------------------------------
;; Alias
;;--------------------------------------------------
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'ddl 'delete-duplicate-lines)
(defalias 'rvr 'reverse-region)
(defalias 'srl 'sort-lines)
(defalias 'delds 'my/delete-DS_Store-under-current-directory-recursively)

;;; 99-utility.el ends here
