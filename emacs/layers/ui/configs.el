;;; configs.el --- configs file
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

;; disable GUI stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; load theme
(load-theme 'leuven t)

;; maximize on startup
(set-frame-parameter nil 'fullscreen 'fullboth)
