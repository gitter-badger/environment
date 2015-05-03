;;; org-configs.el --- configs file of org configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;;; Configurable variables
;; ========================

(defvar d12/org-home-path "~/Dropbox/org/"
  "Directory where org files are stored.
   All org files in this directory and all subdirectories will be used as agenda
   files for org agenda. If you want to ignore some files, checkout
   'd12/org-agenda-ignore-dirs variable. Currently you can ignore files
   only by putting them into ignored directory.
   Warning! Don't forget to add trailing slash at the end.")

(defvar d12/org-agenda-ignore-dirs
  '("temporary/"
    "tmp/")
  "List of directories to be ignored for agenda. Every path should be relative
   to d12/org-home-path and must contain trailing slash.")

(defvar d12/org-time-format
  "%H:%M:%S"
  "Format for 'd12/org-insert-time function.")

(defvar d12/org-date-format
  "%d %B %Y, %A"
  "Format for 'd12/org-insert-date function.")

(defvar d12/org-default-title
  "Yet another org file"
  "Default title for org files. Is used by 'd12/org-guess-title
   when it failt to get the ttitle from buffer name.")

(defvar d12/org-author-name
  user-full-name
  "Author name (used in 'd12/org-new-file-template function.)")

(defvar d12/org-author-email
  user-mail-address
  "Author email (used in 'd12/org-new-file-template function.)")

;;; Configurations
;; ===============

(use-package org
  :ensure t
  :pin org
  :mode ("\\.org$" . org-mode)          ; todo - add org journal stuff as well
  :bind (("C-c o a"   . org-agenda)
         ("C-c o i t" . d12/org-insert-time)
         ("C-c o i d" . d12/org-insert-date))
  :config
  (require 's)
  (setq-local d12/org-ignored-dirs
                (-flatten
                 (-non-nil
                  (-map (lambda (dir)
                          (d12/org-dir-and-subdirs dir))
                        d12/org-agenda-ignore-dirs))))

  (setq-local d12/org-agenda-dirs
              (-difference (d12/org-dir-and-subdirs "") d12/org-ignored-dirs))

  (setq-local d12/org-agenda-files
              (-flatten (-map (lambda (dir)
                                (d12/org-files-in-folder dir))
                              d12/org-agenda-dirs)))

  (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
    (let ((rlt ad-return-value)
          (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
          (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
          old-flag
          b e)
      (when ad-return-value
        (save-excursion
          (setq old-flag case-fold-search)
          (setq case-fold-search t)
          (setq b (re-search-backward begin-regexp nil t))
          (if b (setq e (re-search-forward end-regexp nil t)))
          (setq case-fold-search old-flag))
        (if (and b e (< (point) e)) (setq rlt nil)))
      (setq ad-return-value rlt)))

  (setq org-todo-keywords
        '((sequence
           ;; The item is ready to be done at the earliest opportunity or
           ;; at the date (and maybe time) indicated in the SCHEDULED tag.
           ;; Some tasks are given a DEADLINE date which is useful for
           ;; scheduling the tasks during my daily planning.
           "TODO(t)"

           ;; I should use this tag when I start on a task, but if I clock
           ;; in to a TODO item, I don't really need this task.
           "STARTED(s)"

           ;; I did some work on this task but I am waiting for a response.
           ;; If I use this task I schedule the task into the future as a
           ;; reminder to follow up with some notes in the body of the task.
           "WAITING(w)"

           ;; Used to tag an activity that can only be done at the specified
           ;; time and date, instead of tasks that can be
           ;; completed at any time.
           "APPT(a)"

           "|"

           ;; The task is completed.
           "DONE(d)"

           ;; I decided not to do this task but have left the task on file
           ;; with this status.
           "CANCELLED(c)"

           ;; Used to identify a task that will not be activated just yet.
           ;; The reason will be included in the task notes.
           "DELAYED(l)"))

        org-agenda-window-setup 'current-window
        org-src-fontify-natively t
        org-directory d12/org-home-path
        org-agenda-files d12/org-agenda-files
        org-agenda-inhibit-startup nil
        org-mobile-inbox-for-pull (concat d12/org-home-path "mobile.org")
        org-mobile-force-id-on-agenda-items nil
        org-mobile-directory "~/Dropbox/Apps/MobileOrg/")

  (bind-key "C-c c s" 'd12/org-sort-current-level org-mode-map)
  (bind-key "C-c c S" 'd12/org-sort-upper-level org-mode-map)
  (bind-key "C-c c #" 'd12/org-insert-block-template org-mode-map)

  (add-hook 'org-mode-hook 'd12/org-auto-insert-template))

(use-package org-indent
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "✿" "❀" "✸")))

(use-package org-journal
  :load-path "packages/org-journal/"
  :pin manual
  :mode (".*/[0-9]*-[0-9]*-[0-9]*$" . org-mode)
  :bind (("C-c o c"  . calendar)
         ("C-c o n"  . org-journal-new-entry)
         ("C-c o v"  . org-journal-visit-entry))
  :init
  (add-hook 'calendar-mode-hook
            (lambda nil
              (require 'org-journal)))
  :config
  (global-unset-key (kbd "C-c C-j"))
  (setq org-journal-dir (concat d12/org-home-path "journal/")
        org-journal-date-format "%d %B %Y, %A"
        org-journal-file-format "%Y-%m-%d"
        org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
        org-journal-do-hide-entries-on-new nil))
