;;; 20-language.el --- For each language configulation  -*- lexical-binding: t; -*-

;;--------------------------------------------------
;; Compiler language
;;--------------------------------------------------
;;; Rust
(use-package rust-mode
  :init (el-get-bundle rust-mode)
  :config (setq rust-format-on-save +1))
(use-package racer
  :init (el-get-bundle racer-rust/emacs-racer
          :name racer
          :depends (rust-mode dash s f pos-tip))
  :hook ((rust-mode  . racer-mode)
         (racer-mode . eldoc-mode)))
(el-get-bundle cargo)
(el-get-bundle flycheck-rust)

;;--------------------------------------------------
;; Interpreter language
;;--------------------------------------------------
;;; Emacs lisp
(defun my/elisp-mode-hooks ()
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p +1))
(add-hook 'emacs-lisp-mode-hook 'my/elisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'my/elisp-mode-hooks)

;;; Fish
(el-get-bundle emacs-fish
  (add-hook 'fish-mode-hook
            (lambda () (add-hook 'before-save-hook 'fish_indent-before-save))))

;;--------------------------------------------------
;; Documentaion language
;;--------------------------------------------------
;;; HTML, CSS
(use-package web-mode
  :init (el-get-bundle web-mode)
  :mode (("\\.html?$"     . web-mode)
         ("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode))
  :config
  ;; tab
  (setq tab-width 2)
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-css-indent-offset    tab-width)
  (setq web-mode-code-indent-offset   tab-width)
  (setq web-mode-style-padding        tab-width)
  (setq web-mode-script-padding       tab-width)
  (setq web-mode-block-padding        tab-width)
  ;; color
  (custom-set-faces
   '(web-mode-doctype-face
     ((t (:foreground "#82AE46"))))
   '(web-mode-html-tag-face
     ((t (:foreground "#E6B422" :weight bold))))
   '(web-mode-html-attr-name-face
     ((t (:foreground "#C97586"))))
   '(web-mode-html-attr-value-face
     ((t (:foreground "#82AE46"))))
   '(web-mode-comment-face
     ((t (:foreground "#D9333F"))))
   '(web-mode-server-comment-face
     ((t (:foreground "#D9333F"))))
   '(web-mode-css-rule-face
     ((t (:foreground "#A0D8EF"))))
   '(web-mode-css-pseudo-class-face
     ((t (:foreground "#FF7F00"))))
   '(web-mode-css-at-rule-face
     ((t (:foreground "#FF7F00"))))))

;;; Markdown
(use-package markdown-mode
  :init (el-get-bundle markdown-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;--------------------------------------------------
;; Configulation language
;;--------------------------------------------------
;;; CMake
(el-get-bundle cmake-mode)

;;; Dockerfile
(el-get-bundle dockerfile-mode)

;;; Toml
(el-get-bundle toml-mode)

;;: Yaml
(el-get-bundle yaml-mode)

;;; 20-language.el ends here
