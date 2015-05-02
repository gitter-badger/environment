;;; init.el --- init file of d12frosted configurations
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

(require 'cl)

;;; About myself
;; =============

(setq d12/name		"Boris Buliga"
      d12/email		"d12frosted@icloud.com"
      d12/github	"https://github.com/d12frosted"
      d12/home		"http://d12frosted.github.io")

(setq d12/load-dir (file-name-directory load-file-name))

;;; Layers
;; =======

(defun d12/load-layer (layer)
  "Load LAYER.."
  (let ((layer-path (concat d12/load-dir
			    "layers/"
			    layer
			    "/")))
    (load (concat layer-path "funcs.el"))
    (load (concat layer-path "configs.el"))))

;;; Custom layers
;; --------------

(defvar d12/custom-layers '()
  "List of custom layers to load.")

(setq d12/custom-layers
      '("file-template"
        "org"))

;;; Core layer
;; -----------

(d12/load-layer "core")
