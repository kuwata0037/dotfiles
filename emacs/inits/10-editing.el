;;; 10-editing.el ---
;;
;;; Code:

;;; Add saved hook
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Region
(cua-mode)
(setq cua-enable-cua-keys nil)

;;; Auto insert
(use-package autoinsert
  :config
  (auto-insert-mode))

;;; Open junk file
(el-get-bundle open-junk-file)
(use-package open-junk-file
  :bind   ("C-c C-j" . open-junk-file)
  :config (setq open-junk-file-format "/var/tmp/junk/%Y-%m%d-%H%M%S."))

;;; Completion
(el-get-bundle company-mode
  (global-company-mode t)
  (with-eval-after-load-feature 'company
    ;; general
    (setq company-idle-delay 0.2)
    (setq company-minimum-prefix-length 2)
    (setq company-selection-wrap-around t)
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
                        :background "gray40")))

;;; Syntax check
(el-get-bundle flycheck
  (global-flycheck-mode)
  (with-eval-after-load-feature 'flycheck
    (defun my/flycheck-cpp-language ()
      (setq flycheck-gcc-language-standard   "c++1y")
      (setq flycheck-clang-language-standard "c++1y"))
    (add-hook 'c++-mode-hook 'my/flycheck-cpp-language)))

;;; Execution
(el-get-bundle quickrun)
(use-package quickrun
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
    :override t)
  (quickrun-add-command "c++/clang++"
    '((:exec         . ("%c -std=c++1y -x c++ %o -o %e %s" "%e %a"))
      (:compile-only .  "%c -Wall -Werror -std=c++1y %o -o  %e %s"))
    :override t))

;;; Printout source code
(el-get-bundle htmlize)
(use-package htmlize
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

;;; 10-editing.el ends here
