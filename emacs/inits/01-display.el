;;; 01-display.el --- Setup display configulation
;;
;;; Code:

;;;------------------------------
;;; Color
;;;------------------------------
(use-package color)

;;; Current line
(defface my/hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景色がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
    "hl-line's my face")
(setq hl-line-face 'my/hl-line-face)
(global-hl-line-mode -1)

;;; Symbol
(el-get-bundle! highlight-symbol
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 0.5))

;;; Parentheses
(electric-pair-mode)
(electric-layout-mode)
(add-to-list 'electric-pair-pairs   '(?{ . ?}))
(add-to-list 'electric-layout-rules '(?{ . after))
(show-paren-mode)
(setq show-paren-delay 0.3)
(el-get-bundle rainbow-delimiters)
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init     (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (cl-loop for index from 1 to rainbow-delimiters-max-face-count do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30))))

;;; Trailing whitespace
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#B14770")

;;; Difference
(el-get-bundle! volatile-highlights
  (volatile-highlights-mode)
  ;; supprot undo-tree
  (vhl/define-extension 'undo-tree 'undo-tree-yany 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

;;;------------------------------
;;; Frame
;;;------------------------------
;;; Bar
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode -1)

;;; Margin
(global-linum-mode)
(setq linum-format "%3d: ")

;;; Mode line
(line-number-mode)
(column-number-mode)

;;;------------------------------
;;; Minibuffer
;;;------------------------------
(fset 'yes-or-no-p 'y-or-n-p)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; 01-display.el ends here
