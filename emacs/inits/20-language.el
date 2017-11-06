;;; 20-language.el --- for each language configulation
;;
;;; Code:

;;;------------------------------
;;; Compiler language
;;;------------------------------
;;; Rust
(el-get-bundle rust-mode
  (with-eval-after-load-feature 'rust-mode
    (setq rust-format-on-save t)))

;;;------------------------------
;;; Interpreter language
;;;------------------------------
;;; Emacs lisp
(defun my/elisp-mode-hooks ()
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t))
(add-hook 'emacs-lisp-mode-hook 'my/elisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'my/elisp-mode-hooks)

;;; Fish
(el-get-bundle emacs-fish
  (add-hook 'fish-mode-hook
            (lambda () (add-hook 'before-save-hook 'fish_indent-before-save))))

;;;------------------------------
;;; Documentaion language
;;;------------------------------
;; HTML, CSS
(el-get-bundle web-mode)
(use-package web-mode
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

;; Markdown
(el-get-bundle markdown-mode)
(use-package markdonw-mode
  :mode ("\\.md$" . gfm-mode))

;;;------------------------------
;;; Configulation language
;;;------------------------------
;;; Docker
(el-get-bundle dockerfile-mode)

;;; Yaml
(el-get-bundle yaml-mode)
(use-package yaml-mode
  :mode   ("\\.ya?ml$" . yaml-mode)
  :config (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))

;;; 20-language.el ends here
