;;; Common
(setq vc-follow-symlinks +1)

;;; Git
(el-get-bundle git-modes)
(use-package magit
  :if (executable-find "git")
  :init (el-get-bundle magit)
  :bind ("C-c C-m" . magit-status))
