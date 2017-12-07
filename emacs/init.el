;;; init.el --- Emacs configuration
;;
;;; Code:

;;; Require
(require 'cl-lib)

;;; Setup specific direcotry of loading emacs config
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Add load path
(defvar my/inits-dir    (locate-user-emacs-file "inits/"))
(defvar my/packages-dir (locate-user-emacs-file (concat "packages/" emacs-version)))
(dolist (path (list my/inits-dir my/packages-dir))
  (let ((default-directory path))
    (add-to-list 'load-path default-directory)
    (if (and (file-directory-p default-directory)
             (fboundp 'normal-top-level-add-subdirs-to-load-path))
        (normal-top-level-add-subdirs-to-load-path))))

;;; Setup package manager
;; ELPA
(defvar package-user-dir my/packages-dir)
(package-initialize)
;; El-get
(defvar el-get-dir my/packages-dir)
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; El-get plugins
;; Lock package version by installed el-get
(el-get-bundle tarao/el-get-lock
  (el-get-lock))
;; Avoid warnings during the compilation to access a variable defined in the feature
(el-get-bundle tarao/with-eval-after-load-feature-el)

;;; Setup advanced require package macro
(el-get-bundle use-package)

;;; Load init files
(el-get-bundle! init-loader
  (setq init-loader-show-log-after-init 'error-only)
  (init-loader-load my/inits-dir))

;;; init.el ends here
