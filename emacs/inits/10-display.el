;;; 01-display.el --- UI configulation               -*- lexical-binding: t; -*-

;;--------------------------------------------------
;; Startup
;;--------------------------------------------------
;;; Disbaled startup display
(setq inhibit-startup-screen +1)
(setq initial-scratch-message "")

;;--------------------------------------------------
;; Notification
;;--------------------------------------------------
;;; Disabled beep and flash
(setq ring-bell-function 'ignore)

;;--------------------------------------------------
;; Color
;;--------------------------------------------------
(use-package color)

;;; Color theme
(load-theme 'wombat +1)
;; (el-get-bundle! badwolf-theme
;;   :type github
;;   :pkgname "bkruczyk/badwolf-emacs")

;;; Current line
(defface my/hl-line-face
  '((((class color) (background dark))
     (:background "NavyBlue" +1))
    (((class color) (background light))
     (:background "LightGoldenrodYellow" +1))
    (t (:bold +1)))
  "hl-line's my face")
(setq hl-line-face 'my/hl-line-face)
(global-hl-line-mode -1)

;;; Symbol
(use-package highlight-symbol
  :init (el-get-bundle highlight-symbol)
  :hook (prog-mode . highlight-symbol-mode)
  :config (setq highlight-symbol-idle-delay 0.5))

;;; Parentheses
(show-paren-mode)
(setq show-paren-delay 0.3)
(use-package rainbow-delimiters
  :init (el-get-bundle rainbow-delimiters)
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Whitespaces
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing tabs space tab-mark))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (set-face-attribute 'whitespace-trailing nil
                      :background "#B14770")
  (set-face-attribute 'whitespace-tab nil
                      :foreground "LightSkyblue"
                      :underline t)
  (global-whitespace-mode))

;;; Difference
(use-package volatile-highlights
  :init (el-get-bundle volatile-highlights)
  :config
  (volatile-highlights-mode)
  ;; supprot undo-tree
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

;;; Indent
(use-package indent-guide
  :init (el-get-bundle indent-guide)
  :hook (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-delay 0.1)
  (set-face-foreground 'indent-guide-face "DarkCyan"))

;;--------------------------------------------------
;; Frame
;;--------------------------------------------------
;;; Bar
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode   -1)
  (scroll-bar-mode -1))

;;; Margin
(global-linum-mode)
(setq linum-format "%4d: ")

;;; Mode line
(line-number-mode)
(column-number-mode)
(use-package spaceline-config
  :init (el-get-bundle spaceline)
  :config (spaceline-spacemacs-theme))

;;; 01-display.el ends here
