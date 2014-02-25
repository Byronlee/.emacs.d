;;; emacs-locale-zh-cn.el -- set Emacs menu language and related to Simplified Chinese ;;; -*- coding: utf-8 -*-
;;; 2013-11-01 updated, by Careone <emacslocale@126.com>

;;; version 15.0

;;; optional file 1
;; lisp/language/lang-cmds.el, non-builtin lisp file
;; used to select Emacs menu language, now support English, 
;; Simplified Chinese (zh_CN), and Traditional Chinese (zh_TW).
(require 'lang-cmds)

;; defun in lang-cmds.el, for zh_CN
(lang-zh-cn)

;; defun in lang-cmds.el, for zh_TW
;(lang-zh-tw)

;;; --------------------------------
;;; optional file 2
;; lisp/hotset-cmds.el, non-builtin lisp file
;; enable hot settings, such as show date and time, line number, 
;; column number and some others
(require 'hotset-cmds)

;; defun in hotset-cmds.el
(hotset)

;;; --------------------------------
;;; optional file 3
;; lisp/language/load-codings-zh.el, non-builtin lisp file
;; set Chinese file codings and decoding

;;---
(require 'load-codings-zh)

;;;--- method 1: set codings (by manual)
;
;; defun in load-codings-zh.el
;; zh_CN, for Unix/Linux/BSD
;(load-codings-zh-cn-unix)

;; zh_CN, for Windows
;(load-codings-zh-cn-dos)

;; zh_TW, for Unix/Linux/BSD
;(load-codings-zh-tw-unix)

;; zh_TW, for Windows
;(load-codings-zh-tw-dos)

;;;---------------
;;;--- method 2: set codings (auto)
(cond
 ((eq system-type 'gnu/linux) ; linux
  (progn (load-codings-zh-cn-unix)))

 ((eq system-type 'berbeley-unix) ; BSD
  (progn (load-codings-zh-cn-unix)))

 ((eq system-type 'windows-nt) ; Microsoft Windows
  (progn (load-codings-zh-cn-dos)))
  
 ((eq system-type 'darwin)   ; Mac OS X
  (progn (load-codings-zh-cn-unix)))
  )

;;;---------------
;(provide 'emacs-locale-zh-cn)

;; arch-tag: 
;;; emacs-locale-zh-cn.el ends here
