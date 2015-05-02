;;; configs.el --- configs file for org configuration layer
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12@icloud.com>
;; Maintainer: Boris Buliga <d12@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Configurable variables
;; =======================

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
  d12/name
  "Author name (used in 'd12/org-new-file-template function.)")

(defvar d12/org-author-email
  d12/email
  "Author email (used in 'd12/org-new-file-template function.)")

;;; Configurations
;; ===============

(use-package org
  :mode ("\\.org$" . org-mode)          ; todo - add org journal stuff as well
  :requires (s org-journal org-bullets)
  :init
  :config
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

  ;; (evil-leader/set-key-for-mode 'org-mode
  ;;   "m C-s" 'd12/org-sort-current-level
  ;;   "m C-S" 'd12/org-sort-upper-level
  ;;   "m#" 'd12/org-insert-block-template
  ;;   "m C-d" 'd12/org-insert-date
  ;;   "m C-t" 'd12/org-insert-time)

  ;; (spacemacs/declare-prefix "oj" "org/journal")

  ;; (evil-leader/set-key "ojl" 'org-store-link)
  ;; (evil-leader/set-key "oit" 'd12/org-insert-time)
  ;; (evil-leader/set-key "oid" 'd12/org-insert-date)

  (add-hook 'org-mode-hook 'd12/org-auto-insert-template)
  )

(use-package org-indent
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package org-bullets
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "✿" "❀" "✸")))

(use-package org-journal
  :mode (".*/[0-9]*-[0-9]*-[0-9]*$" . org-mode)
  :init
  (add-hook 'calendar-mode-hook (lambda nil
                                  (require 'org-journal)))
  :config
  (global-unset-key (kbd "C-c C-j"))
  ;; (evil-leader/set-key "ojc" 'calendar)
  ;; (evil-leader/set-key "ojn" 'org-journal-new-entry)
  ;; (evil-leader/set-key "ojv" 'org-journal-visit-entry)
  (setq org-journal-dir (concat d12/org-home-path "journal/")
        org-journal-date-format "%d %B %Y, %A"
        org-journal-file-format "%Y-%m-%d"
        org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
        org-journal-do-hide-entries-on-new nil))
