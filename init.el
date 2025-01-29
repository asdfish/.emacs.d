(setq load-prefer-newer t)
(setq native-comp-jit-compilation t)

(setq custom-file (concat user-emacs-directory "src/custom.el"))

(defvar my/source-directory
  (concat user-emacs-directory "src")
  "A the path gets scanned for .el files to be loaded at startup.")

(defvar my/startup-completed-hook nil "Hook that runs at the end of the startup process.")

; lower garbage collection during startup
(customize-set-variable 'gc-cons-threshold most-positive-fixnum)
(add-hook 'my/startup-completed-hook
	  (lambda ()
	    (customize-set-variable 'gc-cons-threshold 800000))
	  (garbage-collect))

(dolist (el-file (directory-files-recursively my/source-directory "\\.el$"))
  (load (file-name-sans-extension el-file)))

(run-hooks 'my/startup-completed-hook)
