;;; lang-cmds.el --- commands for toggle menu language ; -*- coding: utf-8 -*-

;;; file name: lisp/language/lang-cmds.el
;;; version: 3.0
;;; Updated: 2013-10-28

;;; Copyright (C) 2013 Careone <emacslocale@126.com>.
;;; License: GPL V3 and/or BSD License
;;; Author: 
;;; Maintainer: 

;;; Keywords: lang-cmds

;;; menu: Options => Select Menu Language => ...

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tmm)

;; ;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup language nil
  "Toggle menu language to special language."
  :prefix "lang-"
  :group 'options)

(defcustom lang-en "English"
  "Toggle menu language to English."
  :group 'language
  :type 'string)

(defcustom lang-zh-cn "Simplified Chinese (zh_CN)"
  "Toggle menu language to Simplified Chinese, China."
  :group 'language
  :type 'string)

(defcustom lang-zh-tw "Traditional Chinese (zh_TW)"
  "Toggle menu language to Traditional Chinese, Taiwan."
  :group 'language
  :type 'string)

;; ;;;;;;;;;;;; customization variables end ;;;;;;;;;;;;;;;;;;;;;;;;;
 
;(or (lookup-key global-map [menu-bar])
;    (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))

(defvar menu-bar-language-menu (make-sparse-keymap "Select Menu Language"))

;; English
(defun lang-en () 
  "Toggle menu language to English (en)"
(interactive)
(load-library "menu-bar")
;(load-library "loaddefs")
;(load-library "loadup")
)

;; Chinese
(defun lang-zh-cn () 
  "Toggle menu language to Simplified Chinese, China (zh_CN)"
(interactive)
(load-library "loadlang-zh-cn"))

(defun lang-zh-tw () 
  "Toggle menu language to Traditional Chinese, Taiwan (zh_TW)"
(interactive)
(load-library "loadlang-zh-tw"))

(define-key-after menu-bar-options-menu [lang]
  `(menu-item ,(purecopy "Select Menu Language") ,menu-bar-language-menu))

(define-key-after menu-bar-language-menu [en]
  `(menu-item ,(purecopy "English") lang-en
	      :help ,(purecopy "English (command: lang-en)")))

;;; ---------------
;;; notice
;(define-key-after menu-bar-language-menu [separator-en]
;'("--"))             ;separator for Emacs 23.1 and older
;menu-bar-separator)  ;separator for Emacs 23.2 and later
;;; ---------------

(cond
  ((>= emacs-major-version 24)
     (progn
    (define-key-after menu-bar-language-menu [separator-en]
    menu-bar-separator)))

  ((<= emacs-major-version 22)
     (progn
    (define-key-after menu-bar-language-menu [separator-en]
   '("--"))))

  ((and (= emacs-major-version 23) (>= emacs-minor-version 2))
     (progn
    (define-key-after menu-bar-language-menu [separator-en]
    menu-bar-separator)))

  ((and (= emacs-major-version 23) (<= emacs-minor-version 1))
     (progn
    (define-key-after menu-bar-language-menu [separator-en]
    '("--")))))

;;; Chinese	      
(define-key-after menu-bar-language-menu [zh-cn]
  `(menu-item ,(purecopy "Simplified Chinese (zh_CN)") lang-zh-cn
	      :help ,(purecopy "Simplified Chinese, China (command: lang-zh-cn)")))

(define-key-after menu-bar-language-menu [zh-tw]
  `(menu-item ,(purecopy "Traditional Chinese (zh_TW)") lang-zh-tw
	      :help ,(purecopy "Traditional Chinese, Taiwan (command: lang-zh-tw)")))

(provide 'lang-cmds)

;; arch-tag: 
;;; lang-cmds.el ends here
