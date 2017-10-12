;;; 11-vcs.el --- versoin control system configulaton
;;
;;; Code:

;;; Common
(setq vc-follow-symlinks t)

;;; Git
(el-get-bundle git-modes)
(el-get-bundle magit)
(use-package magit
  :if   (executable-find "git")
  :bind ("C-c C-m" . magit-status))

;;; 11-vcs.el ends here
