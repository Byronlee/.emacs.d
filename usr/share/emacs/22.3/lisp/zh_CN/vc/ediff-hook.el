;;; ediff-hook.el --- setup for Ediff's menus and autoloads

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;;;###autoload
(defvar ediff-window-setup-function)

;; This macro is used to avoid compilation warnings.
;; The macro will expand into the form that is appropriate to the
;; compiler at hand (emacs or xemacs).
;; The autoload, below, is useless in Emacs because ediff-hook.el
;; is dumped with emacs, but it is needed in XEmacs
;;;###autoload (defmacro ediff-cond-compile-for-xemacs-or-emacs (xemacs-form emacs-form) (if (string-match "XEmacs" emacs-version) xemacs-form emacs-form))

(defmacro ediff-cond-compile-for-xemacs-or-emacs (xemacs-form emacs-form)
  (if (string-match "XEmacs" emacs-version)
      xemacs-form emacs-form))

;; This autoload is useless in Emacs because ediff-hook.el is dumped with
;; emacs, but it is needed in XEmacs
;;;###autoload
(ediff-cond-compile-for-xemacs-or-emacs
 ;; xemacs form
 (defun ediff-xemacs-init-menus ()
   (if (featurep 'menubar)
       (progn
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
	 )))
 nil ; emacs form
 )


;; This autoload is useless in Emacs because ediff-hook.el is dumped with
;; emacs, but it is needed in XEmacs
;;;###autoload
(ediff-cond-compile-for-xemacs-or-emacs
 (progn
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
		      (ediff-use-toolbar-p))]
       ))

   ;; put these menus before Object-Oriented-Browser in Tools menu
   (if (and (featurep 'menubar) (not (featurep 'infodock))
	    (not (featurep 'ediff-hook)))
	   (ediff-xemacs-init-menus)))

 ;; Emacs--only if menu-bar is loaded
 (if (featurep 'menu-bar)
     (progn
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

       ;; define ediff-menu
       (define-key menu-bar-ediff-menu [window]
	 '("当前和下一个窗格" . compare-windows))
       (define-key menu-bar-ediff-menu [ediff-windows-linewise]
	 '("窗格行对行..." . ediff-windows-linewise))
       (define-key menu-bar-ediff-menu [ediff-windows-wordwise]
	 '("窗格字对字..." . ediff-windows-wordwise))
       (define-key menu-bar-ediff-menu [separator-ediff-windows] '("--"))
       (define-key menu-bar-ediff-menu [ediff-regions-linewise]
	 '("区域行对行..." . ediff-regions-linewise))
       (define-key menu-bar-ediff-menu [ediff-regions-wordwise]
	 '("区域字对字..." . ediff-regions-wordwise))
       (define-key menu-bar-ediff-menu [separator-ediff-regions] '("--"))
       (define-key menu-bar-ediff-menu [ediff-dir-revision]
	 '("目录修订版本..." . ediff-directory-revisions))
       (define-key menu-bar-ediff-menu [ediff-revision]
	 '("带修订版本的文件..." . ediff-revision))
       (define-key menu-bar-ediff-menu [separator-ediff-directories] '("--"))
       (define-key menu-bar-ediff-menu [ediff-directories3]
	 '("三个目录..." . ediff-directories3))
       (define-key menu-bar-ediff-menu [ediff-directories]
	 '("两个目录..." . ediff-directories))
       (define-key menu-bar-ediff-menu [separator-ediff-files] '("--"))
       (define-key menu-bar-ediff-menu [ediff-buffers3]
	 '("三个缓冲区..." . ediff-buffers3))
       (define-key menu-bar-ediff-menu [ediff-files3]
	 '("三个文件..." . ediff-files3))
       (define-key menu-bar-ediff-menu [ediff-buffers]
	 '("两个缓冲区..." . ediff-buffers))
       (define-key menu-bar-ediff-menu [ediff-files]
	 '("两个文件..." . ediff-files))

       ;; define merge menu
       (define-key
	 menu-bar-ediff-merge-menu [ediff-merge-dir-revisions-with-ancestor]
	 '("带补丁包的目录修订版本..."
	   . ediff-merge-directory-revisions-with-ancestor))
       (define-key
	 menu-bar-ediff-merge-menu [ediff-merge-dir-revisions]
	 '("目录修订版本..." . ediff-merge-directory-revisions))
       (define-key
	 menu-bar-ediff-merge-menu [ediff-merge-revisions-with-ancestor]
	 '("带补丁包的修订版本..."
	   . ediff-merge-revisions-with-ancestor))
       (define-key menu-bar-ediff-merge-menu [ediff-merge-revisions]
	 '("修订版本..." . ediff-merge-revisions))
       (define-key menu-bar-ediff-merge-menu [separator-ediff-merge] '("--"))
       (define-key
	 menu-bar-ediff-merge-menu [ediff-merge-directories-with-ancestor]
	 '("带补丁包的目录..."
	   . ediff-merge-directories-with-ancestor))
       (define-key menu-bar-ediff-merge-menu [ediff-merge-directories]
	 '("目录..." . ediff-merge-directories))
       (define-key
	 menu-bar-ediff-merge-menu [separator-ediff-merge-dirs] '("--"))
       (define-key
	 menu-bar-ediff-merge-menu [ediff-merge-buffers-with-ancestor]
	 '("带补丁包的缓冲区..." . ediff-merge-buffers-with-ancestor))
       (define-key menu-bar-ediff-merge-menu [ediff-merge-buffers]
	 '("缓冲区..." . ediff-merge-buffers))
       (define-key menu-bar-ediff-merge-menu [ediff-merge-files-with-ancestor]
	 '("带补丁包的文件..." . ediff-merge-files-with-ancestor))
       (define-key menu-bar-ediff-merge-menu [ediff-merge-files]
	 '("文件..." . ediff-merge-files))

       ;; define epatch menu
       (define-key menu-bar-epatch-menu [ediff-patch-buffer]
	 '("到缓冲区..." . ediff-patch-buffer))
       (define-key menu-bar-epatch-menu [ediff-patch-file]
	 '("到文件..." . ediff-patch-file))

       ;; define ediff miscellanea
       (define-key menu-bar-ediff-misc-menu [emultiframe]
	 '("切换分离式缓冲控制区框架"
	   . ediff-toggle-multiframe))
       (define-key menu-bar-ediff-misc-menu [eregistry]
	 '("列出 Ediff 会话" . ediff-show-registry))
       (define-key menu-bar-ediff-misc-menu [ediff-cust]
	 '("自定义 Ediff" . ediff-customize))
       (define-key menu-bar-ediff-misc-menu [ediff-doc]
	 '("Ediff 手册" . ediff-documentation))
       )

      ) ; emacs case
 ) ; ediff-cond-compile-for-xemacs-or-emacs

;; arrange for autoloads
(if purify-flag
    () ; if dumping, autoloads are set up in loaddefs.el
  ;; if the user decides to load this file, set up autoloads
  ;; compare files and buffers
  (autoload 'ediff "ediff" "Compare two files." t)
  (autoload 'ediff-files "ediff" "Compare two files." t)
  (autoload 'ediff-buffers "ediff" "Compare two buffers." t)
  (autoload 'ebuffers "ediff" "Compare two buffers." t)
  (autoload 'ediff3  "ediff"  "Compare three files." t)
  (autoload 'ediff-files3 "ediff" "Compare three files." t)
  (autoload 'ediff-buffers3 "ediff" "Compare three buffers." t)
  (autoload 'ebuffers3 "ediff" "Compare three buffers." t)

  (autoload 'erevision "ediff" "Compare versions of a file." t)
  (autoload 'ediff-revision "ediff" "Compare versions of a file." t)

  ;; compare regions and windows
  (autoload 'ediff-windows-wordwise
    "ediff" "Compare two windows word-by-word." t)
  (autoload 'ediff-regions-wordwise
    "ediff" "Compare two regions word-by-word." t)
  (autoload 'ediff-windows-linewise
    "ediff" "Compare two windows line-by-line." t)
  (autoload 'ediff-regions-linewise
    "ediff" "Compare two regions line-by-line." t)

  ;; patch
  (autoload 'ediff-patch-file "ediff" "Patch a file." t)
  (autoload 'epatch "ediff" "Patch a file." t)
  (autoload 'ediff-patch-buffer "ediff" "Patch a buffer.")
  (autoload 'epatch-buffer "ediff" "Patch a buffer." t)

  ;; merge
  (autoload 'ediff-merge "ediff" "Merge two files." t)
  (autoload 'ediff-merge-files "ediff" "Merge two files." t)
  (autoload 'ediff-merge-files-with-ancestor
    "ediff" "Merge two files using a third file as an ancestor." t)
  (autoload 'ediff-merge-buffers "ediff" "Merge two buffers." t)
  (autoload 'ediff-merge-buffers-with-ancestor
    "ediff" "Merge two buffers using a third buffer as an ancestor." t)

  (autoload 'ediff-merge-revisions "ediff" "Merge two versions of a file." t)
  (autoload 'ediff-merge-revisions-with-ancestor
    "ediff" "Merge two versions of a file." t)

  ;; compare directories
  (autoload 'edirs "ediff" "Compare files in two directories." t)
  (autoload 'ediff-directories "ediff" "Compare files in two directories." t)
  (autoload 'edirs3 "ediff" "Compare files in three directories." t)
  (autoload
    'ediff-directories3 "ediff" "Compare files in three directories." t)

  (autoload 'edir-revisions
    "ediff" "Compare two versions of a file." t)
  (autoload 'ediff-directory-revisions
    "ediff" "Compare two versions of a file." t)

  ;; merge directories
  (autoload 'edirs-merge "ediff" "Merge files in two directories." t)
  (autoload 'ediff-merge-directories
    "ediff" "Merge files in two directories." t)
  (autoload 'edirs-merge-with-ancestor
    "ediff"
    "Merge files in two directories using files in a third dir as ancestors."
    t)
  (autoload 'ediff-merge-directories-with-ancestor
    "ediff"
    "Merge files in two directories using files in a third dir as ancestors."
    t)

  (autoload 'edir-merge-revisions
    "ediff" "Merge versions of files in a directory." t)
  (autoload 'ediff-merge-directory-revisions
    "ediff" "Merge versions of files in a directory." t)
  (autoload 'ediff-merge-directory-revisions-with-ancestor
    "ediff"
    "Merge versions of files in a directory using other versions as ancestors."
    t)
  (autoload 'edir-merge-revisions-with-ancestor
    "ediff"
    "Merge versions of files in a directory using other versions as ancestors."
    t)

  ;; misc
  (autoload 'ediff-show-registry
    "ediff-mult"
    "Display the registry of active Ediff sessions."
    t)
  (autoload 'eregistry
    "ediff-mult"
    "Display the registry of active Ediff sessions."
    t)
  (autoload 'ediff-documentation
    "ediff"
    "Display Ediff's manual."
    t)
  (autoload 'ediff-version
    "ediff"
    "Show Ediff's version and last modification date."
    t)
  (autoload 'ediff-toggle-multiframe
    "ediff-util"
    "Toggle the use of separate frame for Ediff control buffer."
    t)
  (autoload 'ediff-toggle-use-toolbar
    "ediff-util"
    "Toggle the use of Ediff toolbar."
    t)

  ) ; if purify-flag


(provide 'ediff-hook)


;;; arch-tag: 512f8656-8a4b-4789-af5d-5c6144498df3
;;; ediff-hook.el ends here
;; Simplified Chinese (zh_CN) localization resources for Emacs.
;; translated by Careone <careone@wo.com.cn>, 20130106

