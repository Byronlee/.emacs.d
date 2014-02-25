;;; ediff-hook.el --- setup for Ediff's menus and autoloads

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;   These must be placed in menu-bar.el in Emacs
;;
;;      (define-key menu-bar-tools-menu [ediff-misc]
;;	'("Ediff 杂项" . menu-bar-ediff-misc-menu))
;;      (define-key menu-bar-tools-menu [epatch]
;;	'("应用补丁" . menu-bar-epatch-menu))
;;      (define-key menu-bar-tools-menu [ediff-merge]
;;	'("合并" . menu-bar-ediff-merge-menu))
;;      (define-key menu-bar-tools-menu [ediff]
;;	'("比对" . menu-bar-ediff-menu))

;; Compiler pacifier
(defvar ediff-menu)
(defvar ediff-merge-menu)
(defvar epatch-menu)
(defvar ediff-misc-menu)
;; end pacifier

;; allow menus to be set up without ediff-wind.el being loaded
(defvar ediff-window-setup-function)

;; This autoload is useless in Emacs because ediff-hook.el is dumped with
;; emacs, but it is needed in XEmacs
;;;###autoload
(if (featurep 'xemacs)
    (progn
      (defun ediff-xemacs-init-menus ()
	(when (featurep 'menubar)
	  (add-submenu
	   '("工具") ediff-menu "OO-浏览...")
	  (add-submenu
	   '("工具") ediff-merge-menu "OO-浏览...")
	  (add-submenu
	   '("工具") epatch-menu "OO-浏览...")
	  (add-submenu
	   '("工具") ediff-misc-menu "OO-浏览...")
	  (add-menu-button
	   '("工具") "-------" "OO-浏览...")
	  ))
      (defvar ediff-menu
	'("比对"
	  ["两个文件..."  ediff-files t]
	  ["两个缓冲区..." ediff-buffers t]
	  ["三个文件..."  ediff-files3 t]
	  ["三个缓冲区..." ediff-buffers3 t]
	  "---"
	  ["两个目录..." ediff-directories t]
	  ["三个目录..." ediff-directories3 t]
	  "---"
	  ["带修订版本的文件..."  ediff-revision t]
	  ["目录修订版本..."  ediff-directory-revisions t]
	  "---"
	  ["窗格字对字..." ediff-windows-wordwise t]
	  ["窗格行对行..." ediff-windows-linewise t]
	  "---"
	  ["区域字对字..." ediff-regions-wordwise t]
	  ["区域行对行..." ediff-regions-linewise t]
	  ))
      (defvar ediff-merge-menu
	'("合并"
	  ["文件..."  ediff-merge-files t]
	  ["带补丁包的文件..." ediff-merge-files-with-ancestor t]
	  ["缓冲区..."  ediff-merge-buffers t]
	  ["带补丁包的缓冲区..."
	   ediff-merge-buffers-with-ancestor t]
	  "---"
	  ["目录..."  ediff-merge-directories t]
	  ["带补丁包的目录..."
	   ediff-merge-directories-with-ancestor t]
	  "---"
	  ["修订版本..."  ediff-merge-revisions t]
	  ["带补丁包的修订版本..."
	   ediff-merge-revisions-with-ancestor t]
	  ["目录修订版本..." ediff-merge-directory-revisions t]
	  ["带补丁包的目录修订版本..."
	   ediff-merge-directory-revisions-with-ancestor t]
	  ))
      (defvar epatch-menu
	'("应用补丁"
	  ["到文件..."  ediff-patch-file t]
	  ["到缓冲区..." ediff-patch-buffer t]
	  ))
      (defvar ediff-misc-menu
	'("Ediff 杂项"
	  ["Ediff 手册" ediff-documentation t]
	  ["自定义 Ediff" ediff-customize t]
	  ["列出 Ediff 会话" ediff-show-registry t]
	  ["为 Ediff 缓冲控制区使用分离的框架"
	   ediff-toggle-multiframe
	   :style toggle
	   :selected (if (and (featurep 'ediff-util)
			      (boundp 'ediff-window-setup-function))
			 (eq ediff-window-setup-function
			     'ediff-setup-windows-multiframe))]
	  ["使用带 Ediff 缓冲控制区的工具栏"
	   ediff-toggle-use-toolbar
	   :style toggle
	   :selected (if (featurep 'ediff-tbar)
			 (ediff-use-toolbar-p))]))

      ;; put these menus before Object-Oriented-Browser in Tools menu
      (if (and (featurep 'menubar) (not (featurep 'infodock))
	       (not (featurep 'ediff-hook)))
	  (ediff-xemacs-init-menus)))
  ;; Emacs
  ;; initialize menu bar keymaps
  (defvar menu-bar-ediff-misc-menu
    (make-sparse-keymap "Ediff 杂项"))
  (fset 'menu-bar-ediff-misc-menu
	(symbol-value 'menu-bar-ediff-misc-menu))
  (defvar menu-bar-epatch-menu (make-sparse-keymap "应用补丁"))
  (fset 'menu-bar-epatch-menu (symbol-value 'menu-bar-epatch-menu))
  (defvar menu-bar-ediff-merge-menu (make-sparse-keymap "合并"))
  (fset 'menu-bar-ediff-merge-menu
	(symbol-value 'menu-bar-ediff-merge-menu))
  (defvar menu-bar-ediff-menu (make-sparse-keymap "比对"))
  (fset 'menu-bar-ediff-menu (symbol-value 'menu-bar-ediff-menu))

  ;; define ediff compare menu
  (define-key menu-bar-ediff-menu [ediff-misc]
    `(menu-item ,(purecopy "Ediff 杂项") menu-bar-ediff-misc-menu))
  (define-key menu-bar-ediff-menu [separator-ediff-misc] menu-bar-separator)
  (define-key menu-bar-ediff-menu [window]
    `(menu-item ,(purecopy "当前和下一个窗格") compare-windows
		:help ,(purecopy "Compare the current window and the next window")))
  (define-key menu-bar-ediff-menu [ediff-windows-linewise]
    `(menu-item ,(purecopy "窗格行对行...") ediff-windows-linewise
		:help ,(purecopy "Compare windows line-wise")))
  (define-key menu-bar-ediff-menu [ediff-windows-wordwise]
    `(menu-item ,(purecopy "窗格字对字...") ediff-windows-wordwise
		:help ,(purecopy "Compare windows word-wise")))
  (define-key menu-bar-ediff-menu [separator-ediff-windows] menu-bar-separator)
  (define-key menu-bar-ediff-menu [ediff-regions-linewise]
    `(menu-item ,(purecopy "区域行对行...") ediff-regions-linewise
		:help ,(purecopy "Compare regions line-wise")))
  (define-key menu-bar-ediff-menu [ediff-regions-wordwise]
    `(menu-item ,(purecopy "区域字对字...") ediff-regions-wordwise
		:help ,(purecopy "Compare regions word-wise")))
  (define-key menu-bar-ediff-menu [separator-ediff-regions] menu-bar-separator)
  (define-key menu-bar-ediff-menu [ediff-dir-revision]
    `(menu-item ,(purecopy "目录修订版本...") ediff-directory-revisions
		:help ,(purecopy "Compare directory files with their older versions")))
  (define-key menu-bar-ediff-menu [ediff-revision]
    `(menu-item ,(purecopy "带修订版本的文件...") ediff-revision
		:help ,(purecopy "Compare file with its older versions")))
  (define-key menu-bar-ediff-menu [separator-ediff-directories] menu-bar-separator)
  (define-key menu-bar-ediff-menu [ediff-directories3]
    `(menu-item ,(purecopy "三个目录...") ediff-directories3
		:help ,(purecopy "Compare files common to three directories simultaneously")))
  (define-key menu-bar-ediff-menu [ediff-directories]
    `(menu-item ,(purecopy "两个目录...") ediff-directories
		:help ,(purecopy "Compare files common to two directories simultaneously")))
  (define-key menu-bar-ediff-menu [separator-ediff-files] menu-bar-separator)
  (define-key menu-bar-ediff-menu [ediff-buffers3]
    `(menu-item ,(purecopy "三个缓冲区...") ediff-buffers3
		:help ,(purecopy "Compare three buffers simultaneously")))
  (define-key menu-bar-ediff-menu [ediff-files3]
    `(menu-item ,(purecopy "三个文件...") ediff-files3
		:help ,(purecopy "Compare three files simultaneously")))
  (define-key menu-bar-ediff-menu [ediff-buffers]
    `(menu-item ,(purecopy "两个缓冲区...") ediff-buffers
		:help ,(purecopy "Compare two buffers simultaneously")))
  (define-key menu-bar-ediff-menu [ediff-files]
    `(menu-item ,(purecopy "两个文件...") ediff-files
		:help ,(purecopy "Compare two files simultaneously")))

  ;; define ediff merge menu
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-dir-revisions-with-ancestor]
    `(menu-item ,(purecopy "带补丁包的目录修订版本...")
      ediff-merge-directory-revisions-with-ancestor
      :help ,(purecopy "Merge versions of the files in the same directory by comparing the files with common ancestors")))
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-dir-revisions]
    `(menu-item ,(purecopy "目录修订版本...") ediff-merge-directory-revisions
      :help ,(purecopy "Merge versions of the files in the same directory (without using ancestor information)")))
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-revisions-with-ancestor]
    `(menu-item ,(purecopy "带补丁包的修订版本...")
      ediff-merge-revisions-with-ancestor
      :help ,(purecopy "Merge versions of the same file by comparing them with a common ancestor")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-revisions]
    `(menu-item ,(purecopy "修订版本...") ediff-merge-revisions
      :help ,(purecopy "Merge versions of the same file (without using ancestor information)")))
  (define-key menu-bar-ediff-merge-menu [separator-ediff-merge] menu-bar-separator)
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-directories-with-ancestor]
    `(menu-item ,(purecopy "带补丁包的目录...")
      ediff-merge-directories-with-ancestor
      :help ,(purecopy "Merge files common to a pair of directories by comparing the files with common ancestors")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-directories]
    `(menu-item ,(purecopy "目录...") ediff-merge-directories
		:help ,(purecopy "Merge files common to a pair of directories")))
  (define-key
    menu-bar-ediff-merge-menu [separator-ediff-merge-dirs] menu-bar-separator)
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-buffers-with-ancestor]
    `(menu-item ,(purecopy "带补丁包的缓冲区...") ediff-merge-buffers-with-ancestor
      :help ,(purecopy "Merge buffers by comparing their contents with a common ancestor")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-buffers]
    `(menu-item ,(purecopy "缓冲区...") ediff-merge-buffers
      :help ,(purecopy "Merge buffers (without using ancestor information)")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-files-with-ancestor]
    `(menu-item ,(purecopy "带补丁包的文件...") ediff-merge-files-with-ancestor
      :help ,(purecopy "Merge files by comparing them with a common ancestor")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-files]
    `(menu-item ,(purecopy "文件...") ediff-merge-files
      :help ,(purecopy "Merge files (without using ancestor information)")))

  ;; define epatch menu
  (define-key menu-bar-epatch-menu [ediff-patch-buffer]
    `(menu-item ,(purecopy "到缓冲区...") ediff-patch-buffer
      :help ,(purecopy "Apply a patch to the contents of a buffer")))
  (define-key menu-bar-epatch-menu [ediff-patch-file]
    `(menu-item ,(purecopy "到文件...") ediff-patch-file
      :help ,(purecopy "对文件打补丁")))

  ;; define ediff miscellanea
  (define-key menu-bar-ediff-misc-menu [emultiframe]
    `(menu-item ,(purecopy "使用单独控制的缓冲区架")
      ediff-toggle-multiframe
      :help ,(purecopy "Switch between the single-frame presentation mode and the multi-frame mode")))
  (define-key menu-bar-ediff-misc-menu [eregistry]
    `(menu-item ,(purecopy "列出 Ediff 会话") ediff-show-registry
		:help ,(purecopy "List all active Ediff sessions; it is a convenient way to find and resume such a session")))
  (define-key menu-bar-ediff-misc-menu [ediff-cust]
    `(menu-item ,(purecopy "自定义 Ediff") ediff-customize
		:help ,(purecopy "Change some of the parameters that govern the behavior of Ediff")))
  (define-key menu-bar-ediff-misc-menu [ediff-doc]
    `(menu-item ,(purecopy "Ediff 手册") ediff-documentation
		:help ,(purecopy "Bring up the Ediff manual"))))

(provide 'ediff-hook)


;; arch-tag: 512f8656-8a4b-4789-af5d-5c6144498df3
;;; ediff-hook.el ends here
;; Simplified Chinese (zh_CN) localization resources for Emacs.
;; translated by Careone <careone@wo.com.cn>, 20130106

