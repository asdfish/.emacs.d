(eval-after-load "treesit"
  '(progn
     (eval-and-compile
       (defun my/get-or-init(variable initter)
	 "Sets variable to the output of initter if nil, then return variable."
	 (symbolp variable)
	 (functionp initter)

         (unless (symbol-value variable)
           (set variable (funcall initter)))

         (symbol-value variable))
       (defun my/is-executable-p(executable)
         "Whether or not path is a command or a executable file."
         (stringp executable)

         (or (file-executable-p executable)
             (executable-find executable)))
       (defun my/first-executable(executables)
         "Returns the first valid executable from executables"
         (listp executables)

         (car (seq-filter 'my/is-executable-p executables)))

       (defvar my/c-compiler
         nil
         "Default c compiler.")
       (defvar my/cxx-compiler
         nil
         "Default c++ compiler.")

       (defvar my/c-compilers
         '("C:/msys64/mingw64/bin/gcc.exe"
           "clang"
           "gcc"
           "cc")
         "A list of c compilers.")
       (defvar my/cxx-compilers
         '("C:/msys64/mingw64/bin/g++.exe"
           "clang++"
           "g++"
           "c++")
         "A list of c++ compilers.")

       (defun my/get-c-compiler()
         "Returns c compiler or gets first valid executable from `my/c-compilers'"
         (my/get-or-init 'my/c-compiler (lambda () (my/first-executable my/c-compilers))))
       (defun my/get-cxx-compiler()
         "Returns cxx compiler or gets first valid executable from `my/cxx-compilers'"
         (my/get-or-init 'my/cxx-compiler (lambda () (my/first-executable my/cxx-compilers))))

       (setq treesit-language-source-alist
             (list
              (list 'bash  "https://github.com/tree-sitter/tree-sitter-bash.git" "v0.23.3" "src" (my/get-c-compiler) (my/get-cxx-compiler))
              (list 'c     "https://github.com/tree-sitter/tree-sitter-c.git"    "v0.23.4" "src" (my/get-c-compiler) (my/get-cxx-compiler))
              (list 'cpp   "https://github.com/tree-sitter/tree-sitter-cpp.git"  "v0.23.4" "src" (my/get-c-compiler) (my/get-cxx-compiler))
              (list 'rust  "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.23.2" "src" (my/get-c-compiler) (my/get-cxx-compiler))))

       (defun my/treesit-install()
         "Install all treesitter languages"
         (interactive)

         (dolist (language treesit-language-source-alist)
           (unless (treesit-language-available-p (car language))
             (treesit-install-language-grammar (car language)))))
       (my/treesit-install)

       (setq major-mode-remap-alist
             '((bash-mode . bash-ts-mode)
               (c-mode    . c-ts-mode)
               (c++-mode  . c++-ts-mode))))))

					; major modes
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-ts-mode))

					; project compile command
(defvar my/project-compile-command-markers
  '(("make -k" "Makefile")
    ("cargo b" "Cargo.toml"))
  "A list of commands to run if files are found in the root of a project.
  The first entry is the command and the rest are files to search for.")

(defun my/project-compile-command-guess()
  "Attempt to set the project compile command."
  (interactive)
  (if (and (project-current) (project-root (project-current)))
      (dolist (project-compile-command-marker my/project-compile-command-markers)
	(dolist (file-name (cdr project-compile-command-marker))
          (if (file-exists-p (concat (project-root (project-current)) file-name))
              (setq-local compile-command (car project-compile-command-marker)))))))

(my/project-compile-command-guess)
(add-hook 'find-file-hook 'my/project-compile-command-guess)
