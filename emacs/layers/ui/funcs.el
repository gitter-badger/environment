;;; funcs.el --- funcs file
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

(defun d12/toggle-fullscreen ()
  "Cycle thorugh full screen options by rule 'nil -> maximized -> fullboth -> nil'."
  (interactive)
  (let ((x (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (cond ((not x) 'maximized)
			       ((eq x 'maximized) 'fullboth)
			       (t nil)))))
