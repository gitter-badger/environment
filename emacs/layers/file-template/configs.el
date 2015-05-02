;;; configs.el --- configs file of file-template configuration layer
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

(use-package file-template
  :defer t
  :init
  (autoload 'file-template-auto-insert "file-template" nil t)
  (autoload 'file-template-find-file-not-found-hook "file-template" nil t)
  (setq file-template-insert-automatically t
        file-template-full-name d12/name
        file-template-paths '()
        user-mail-address d12/email)
  (add-to-list 'file-template-paths
               (concat d12/load-dir "templates/"))
  (add-hook 'find-file-not-found-hooks
            'file-template-find-file-not-found-hook 'append))
