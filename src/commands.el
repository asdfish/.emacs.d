(defun my/edit-config-file (source-file)
  (interactive "MEdit ~/.emacs.d/")
  (evil-edit (concat user-emacs-directory source-file)))
