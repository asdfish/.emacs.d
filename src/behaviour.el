(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

(blink-cursor-mode 0)

(indent-tabs-mode nil)
(setq tab-width 2)

(setq visible-bell 1
      ring-bell-function #'ignore)

(setq scroll-step 1
      scroll-conservatively 101)

(electric-pair-mode t)

(desktop-save-mode 0)
