;;; 00-common.el
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup display
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;;; Path
(when (memq window-system '(mac ns))
  (el-get-bundle! exec-path-from-shell
    (exec-path-from-shell-initalize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/history-dir (locate-user-emacs-file ".history/"))
(defun my/set-history (&rest args)
  (concat my/history-dir (mapconcat 'identity args "")))

;;; Backup
(add-to-list 'backup-directory-alist  (cons "." (my/set-history "backup/")))

;;; Autosave
(defvar my/autosave-dir (my/set-history "autosave/"))
(setq auto-save-file-name-transforms `((".*", my/autosave-dir, t)))
(setq auto-save-list-file-prefix
      (concat my/autosave-dir ".saves-"))
(setq auto-save-timeout  15)
(setq auto-save-interval 60)

;;; Lockfiles
(setq create-lockfiles nil)

;;; Trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

;;; Recentf
(recentf-mode)
(setq recentf-save-file (my/set-history "recentf"))
(setq recentf-max-menu-items    10)
(setq recentf-max-saved-items 2000)
(setq recentf-auto-cleanup  'never)
(setq recentf-exclude '("recentf"))
(setq recentf-auto-save-timer (run-with-idle-timer 60 t 'recentf-save-list))

;;; Saveplace
(save-place-mode)
(setq save-place-file (my/set-history "saveplace"))

;;; Savehist
(savehist-mode)
(setq savehist-file (my/set-history "savehist"))
(setq history-length 3000)

;;; Undo
(el-get-bundle! undohist
  (setq undohist-directory (my/set-history "undohist/"))
  (undohist-initialize))
(el-get-bundle! undo-tree
  (global-undo-tree-mode)
  (global-set-key (kbd "C-/") 'undo-tree-undo)
  (global-set-key (kbd "C-.") 'undo-tree-redo))
(el-get-bundle! point-undo
  (global-set-key (kbd "M-]") 'point-undo)
  (global-set-key (kbd "M-[") 'point-redo))
