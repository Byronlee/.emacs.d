;;; load-codings-zh.el --- set Chinese language environment; -*- coding: utf-8 -*-

;;; file name: lisp/language/load-codings-zh.el
;;; version: 3.0
;;; Updated: 2013-11-01

;;; Copyright (C) 2013 Careone <emacslocale@126.com>.
;;; License: GPL V3 and/or BSD License

;;; Author: 
;;; Maintainer: 

;;; Keywords: codings, 

;;; Commentary:

;;; Code:

;; Unix/Linux:  
;; set-language-environment: both UTF-8 or Chinese-GB are ok, and recommend
;; set to UTF-8, other codings settings recommend set to utf-8 
;
;; Windows:
;; set-language-environment: recommend set to Chinese-GB
;; other codings settings recommend set to euc-cn 
;
;; important !!! decode order for zh_CN (Chinese, China): 
;; the last line of prefer-coding-system value utf-8 decode first, 
;; and first line cp950 (means Taiwan big5) decode last!
;; so care with your orders!
;; utf-16le-with-signature in Emacs means unicode in Windows,
;

;;;-------------------
(defun load-codings-zh-cn-dos () 
 "Set Simplified Chinese (zh_CN) file codings"
(interactive)
;;;--------------------

;; codings for Emacs Windows/zh_CN
;; for Linux/Unix/BSD
;(set-language-environment 'UTF-8)))

;; for Windows/DOS
(set-language-environment 'Chinese-GB)

(set-keyboard-coding-system 'euc-cn)
(set-clipboard-coding-system 'euc-cn)
(set-terminal-coding-system 'euc-cn)
(set-buffer-file-coding-system 'euc-cn)
(set-selection-coding-system 'euc-cn)
(modify-coding-system-alist 'process "*" 'euc-cn)
(setq default-process-coding-system '(euc-cn . euc-cn))
(setq-default pathname-coding-system 'euc-cn)
(setq file-name-coding-system 'euc-cn)
;
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)

(cond
   ((>= emacs-major-version 23)
    (progn
       ;; gb18030 supported since Emacs 23
(prefer-coding-system 'gb18030))))

;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)
)
;;;--------------------

;; codings for Emacs Linux/zh_CN
(defun load-codings-zh-cn-unix () 
 "Set Simplified Chinese (zh_CN) file codings"
(interactive)

;; for Linux/Unix/BSD
(set-language-environment 'UTF-8)

;; for Windows/DOS
;(set-language-environment 'Chinese-GB)

(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
;
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)

(cond
    ((>= emacs-major-version 23)
      ;; gb18030 supported since Emacs 23
    (progn
(prefer-coding-system 'gb18030))))

;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)
)

;;;-------------------
(defun load-codings-zh-tw-dos () 
 "Set Traditional Chinese (zh_TW) file codings"
(interactive)
;;;--------------------

;; codings for Emacs Windows/zh_TW
;; for Linux/Unix/BSD
;(set-language-environment 'UTF-8)))

;; for Windows/DOS
(set-language-environment 'Chinese-BIG5)

(set-keyboard-coding-system 'euc-tw)
(set-clipboard-coding-system 'euc-tw)
(set-terminal-coding-system 'euc-tw)
(set-buffer-file-coding-system 'euc-tw)
(set-selection-coding-system 'euc-tw)
(modify-coding-system-alist 'process "*" 'euc-tw)
(setq default-process-coding-system '(euc-tw . euc-tw))
(setq-default pathname-coding-system 'euc-tw)
(setq file-name-coding-system 'euc-tw)
;
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)

(cond
   ((>= emacs-major-version 23)
    (progn
       ;; gb18030 supported since Emacs 23
(prefer-coding-system 'gb18030))))

;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)
)
;;;--------------------

;; codings for Emacs Linux/zh_TW
(defun load-codings-zh-tw-unix () 
  "Set Traditional Chinese (zh_TW) file codings"
(interactive)

;; for Linux/Unix/BSD
(set-language-environment 'UTF-8)

;; for Windows/DOS
;(set-language-environment 'Chinese-BIG5)

(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
;
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)

(cond
    ((>= emacs-major-version 23)
      ;; gb18030 supported since Emacs 23
    (progn
(prefer-coding-system 'gb18030))))

;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)
)

;; codings for Emacs zh_CN
(defun load-codings-zh-cn () 
  "Set Simplified Chinese (zh_CN) file codings"
(interactive)

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
)

;; codings for Emacs zh_TW
(defun load-codings-zh-tw () 
  "Set Traditional Chinese (zh_TW) file codings"
(interactive)

(cond
 ((eq system-type 'gnu/linux) ; linux
  (progn (load-codings-zh-tw-unix)))

 ((eq system-type 'berbeley-unix) ; BSD
  (progn (load-codings-zh-tw-unix)))

 ((eq system-type 'windows-nt) ; Microsoft Windows
  (progn (load-codings-zh-tw-dos)))
  
 ((eq system-type 'darwin)   ; Mac OS X
  (progn (load-codings-zh-tw-unix)))
  )
)

(provide 'load-codings-zh)

;; arch-tag: 
;;; load-codings-zh.el ends here
