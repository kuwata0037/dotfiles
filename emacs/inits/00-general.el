;;; 00-general.el --- General configulation          -*- lexical-binding: t; -*-

;;--------------------------------------------------
;; Environment
;;--------------------------------------------------
;;; Path
(when (memq window-system '(mac ns))
  (el-get-bundle! exec-path-from-shell
    (exec-path-from-shell-initialize)))

;;; Coding System
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;; for windows
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system   'cp932))
;; for mac
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system   'utf-8-hfs))

;;--------------------------------------------------
;; Minibuffer
;;--------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)
(setq read-buffer-completion-ignore-case +1)
(setq read-file-name-completion-ignore-case +1)

;;--------------------------------------------------
;; Coding
;;--------------------------------------------------
;;; Tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Region
(cua-mode)
(setq cua-enable-cua-keys nil)
(use-package expand-region
  :init (el-get-bundle expand-region)
  :bind ("C-j" . er/expand-region))

;;; Template
(use-package autoinsert
  :config (auto-insert-mode))

;;; Temporary file
(use-package open-junk-file
  :init (el-get-bundle open-junk-file)
  :bind ("C-c C-j" . open-junk-file)
  :config (setq open-junk-file-format "/var/tmp/junk/%Y-%m%d-%H%M%S."))

;;; Completion
;; for parentheses
(electric-pair-mode)
(electric-layout-mode)
(add-to-list 'electric-pair-pairs '(?' . ?'))
(add-to-list 'electric-pair-pairs '(?{ . ?}))
;; for text
(use-package company
  :diminish
  :init (el-get-bundle company-mode)
  :config
  ;; general
  (global-company-mode +1)
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around +1)
  ;; keybind
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-f" . company-complete-selection)
             ("C-e" . company-complete-selection)
             ("C-s" . company-filter-candidates))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous))
  ;; color
  (set-face-attribute 'company-tooltip nil
                      :foreground "black"
                      :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black"
                      :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white"
                      :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black"
                      :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :foreground "lightgrey"
                      :background nil
                      :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40"))

;;; Syntax check
(use-package flycheck
  :diminish
  :init (el-get-bundle flycheck)
  :config
  (global-flycheck-mode)
  (defun my/flycheck-cpp-language ()
    (setq flycheck-gcc-language-standard   "c++1y")
    (setq flycheck-clang-language-standard "c++1y"))
  (add-hook 'c++-mode-hook 'my/flycheck-cpp-language)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; Execution
(use-package quickrun
  :init (el-get-bundle quickrun)
  :bind ("C-\\" . my/quickrun-sc)
  :config
  (defun my/quickrun-sc ()
    (interactive)
    (if (use-region-p)
        (quickrun :strat (region-beginning) :end (region-end))
      (quickrun)))
  (quickrun-add-command "c++/g++"
    '((:exec         . ("%c -std=c++1y -x c++ %o -o %e %s" "%e %a"))
      (:compile-only .  "%c -Wall -Werror -std=c++1y %o -o  %e %s"))
    :override +1)
  (quickrun-add-command "c++/clang++"
    '((:exec         . ("%c -std=c++1y -x c++ %o -o %e %s" "%e %a"))
      (:compile-only .  "%c -Wall -Werror -std=c++1y %o -o  %e %s"))
    :override +1))

;;; VCS
(setq vc-follow-symlinks +1)
;; git
(el-get-bundle git-modes)
(use-package magit
  :if (executable-find "git")
  :init (el-get-bundle magit)
  :bind ("C-c C-m" . magit-status))

;;; Translation
(use-package google-translate
  :init (el-get-bundle google-translate)
  :bind ("C-c t" . google-translate-enja-or-jaen)
  :config
  (defvar google-translate-english-chars "[:ascii:]’“”–"
    "When these characters are included, they are regarded as English")
  (defun google-translate-enja-or-jaen (&optional string)
    "Google translate region or current sentence with automatic language discrimination"
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string))))

;;; Printout
(use-package htmlize
  :init (el-get-bundle htmlize)
  :config
  (defun my/htmlize-and-browse ()
    (interactive)
    (defcustom
      htmlize-and-browse-directory-path temporary-file-directory
      "htmlize-and-browse-temporary-file-directory"
      :type  'string
      :group 'htmlize-and-browse)
    (setq htmlize-and-browse-buffer-file-name
          (concat "htmlize-and-browse-"
                  (format-time-string "%Y%m%d%H%M%S" (current-time)) ".html"))
    (setq htmlize-and-browse-buffer-file-path
          (concat htmlize-and-browse-directory-path htmlize-and-browse-buffer-file-name))
    (with-current-buffer (htmlize-buffer)
      (write-file htmlize-and-browse-buffer-file-path)
      (set-buffer-modified-p nil)
      (kill-buffer htmlize-and-browse-buffer-file-name)
      (shell-command (concat "open " htmlize-and-browse-buffer-file-path)))))

;;--------------------------------------------------
;; History
;;--------------------------------------------------
(defvar my/history-dir (locate-user-emacs-file ".history/"))
(defun my/set-history (&rest args)
  (concat my/history-dir (mapconcat 'identity args "")))

;;; Autosave
(defvar my/autosave-dir (my/set-history "autosave/"))
(setq auto-save-file-name-transforms `((".*", my/autosave-dir, t)))
(setq auto-save-list-file-prefix
      (concat my/autosave-dir ".saves-"))
(setq auto-save-timeout  15)
(setq auto-save-interval 60)
(use-package super-save
  :diminish
  :init (el-get-bundle bbatsov/super-save)
  :config
  (setq super-save-auto-save-when-idle +1)
  (setq super-save-idle-duration 10)
  (super-save-mode))

;;; Backup
(add-to-list 'backup-directory-alist (cons "." (my/set-history "backup/")))

;;; Recentf
(setq recentf-save-file (my/set-history "recentf"))
(setq recentf-max-menu-items    10)
(setq recentf-max-saved-items 2000)
(setq recentf-auto-cleanup  'never)
(setq recentf-exclude '("recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" ))
(setq recentf-auto-save-timer (run-with-idle-timer 60 t 'recentf-save-list))
(recentf-mode)

;;; Saveplace
(setq save-place-file (my/set-history "saveplace"))
(save-place-mode)

;;; Savehist
(setq savehist-file (my/set-history "savehist"))
(setq history-length 3000)
(savehist-mode)

;;; Lockfile
(setq create-lockfiles -1)

;;; Cookie
(setq url-cookie-file (my/set-history "cookies"))

;;; Transient
(setq transient-history-file (my/set-history "transient"))

;;; Trash
(setq delete-by-moving-to-trash +1)
(setq trash-directory "~/.Trash")

;;; Undo
(el-get-bundle! undohist
  (setq undohist-directory (my/set-history "undohist/"))
  (undohist-initialize))
(el-get-bundle! undo-tree
  (global-undo-tree-mode)
  (global-set-key (kbd "C-/") 'undo-tree-undo)
  (global-set-key (kbd "M-/") 'undo-tree-redo))
(el-get-bundle! emacswiki:point-undo
  (global-set-key (kbd "M-]") 'point-undo)
  (global-set-key (kbd "M-[") 'point-redo))

;;--------------------------------------------------
;; Mics
;;--------------------------------------------------
(auto-image-file-mode)
(auto-compression-mode)
(global-auto-revert-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;; 00-general.el ends here
