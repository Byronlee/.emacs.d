;;; hotset-cmds.el --- commands for hot setting ; -*- coding: utf-8 -*-

;;; file name: lisp/language/hotset-cmds.el
;;; version: 3.1
;;; Updated: 2013-11-03

;;; Copyright (C) 2013 Careone <emacslocale@126.com>.
;;; License: GPL V3 and/or BSD License

;;; Author: 
;;; Maintainer: 

;;; Keywords: hotset-cmds, hotset

;;; Notice: for changes of separator code since Emacs 23.2, we need
;;; another shift version for Emacs 23.1 and earlier versions
;
;;; menu: Options => Enable Hot Settings

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tmm)

;; ;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;; customization variables end ;;;;;;;;;;;;;;;;;;;;;;;;;
 
;(or (lookup-key global-map [menu-bar])
;    (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))

(defvar menu-bar-options-menu
	(make-sparse-keymap "Enable Hot Settings"))

;;;
(defun hotset () 
  "Enable Emacs hot settings"
(interactive)

;; linum.el builtin since Emacs 23, and void for 22 and older
(cond 
 ((>= emacs-major-version 23)
  (progn
   (global-linum-mode 1))))

;;
(size-indication-mode 1)
(line-number-mode 1)
(column-number-mode 1)
)

(define-key-after menu-bar-options-menu [hotset]
  `(menu-item ,(purecopy "Enable Hot Settings") hotset
	      :help ,(purecopy "(command: hotset)")))

(provide 'hotset-cmds)

;; arch-tag: 
;;; hotset-cmds.el ends here
