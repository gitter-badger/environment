;;; configs.el --- configs file of core configuration layer
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

;;; Load path
;; ==========

;; add all packages to load path
;; but don't load them yet

(dolist (path
         (d12/directory-dirs (concat d12/load-dir
                                     "packages/")))
  (add-to-list 'load-path path))

;;; Core Packages
;; ==============

;; require essential packages

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package dash)

;;; Deffered packages
;; ------------------

(use-package s
  :defer 1)

;;; Load custom layers
;; -------------------

(-each d12/custom-layers
  (lambda (layer)
    (d12/load-layer layer)))

;;; Global configurations
;; ======================

(fset 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(set-auto-saves)

;;; setq-default
;; -------------

(setq-default indent-tabs-mode nil)

;;; setq
;; -----

(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx

      tab-width 2
      scroll-step 1)

;;; hooks
;; ------

(add-hook 'before-save-hook 'delete-trailing-whitespace)
