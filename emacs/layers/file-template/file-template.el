;;; file-template.el --- file-template file of d12frosted configurations
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


(setq file-template-insert-automatically 'ask
      file-template-full-name d12/name
      file-template-paths '("~/.emacs.d/templates/")
      user-mail-address d12/email)
