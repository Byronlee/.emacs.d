;;; Traditional Chinese(繁體中文) (zh_TW) localization resources for Emacs. ; -*- coding: utf-8 -*-
;;; translate script: version 2.0
;;; 2013-05-19 22:09:32 CST translated
;;; translator: Careone <emacs-locale@qq.com>, 2013
;;; menu-bar.el --- define a default menu bar

;; Copyright (C) 1993, 1994, 1995, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: RMS
;; Maintainer: FSF
;; Keywords: internal, mouse

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

;; Avishai Yacobi suggested some menu rearrangements.

;;; Commentary:

;;; Code:

;;; User options:

(defcustom buffers-menu-max-size 10
  "*Maximum number of entries which may appear on the Buffers menu.
If this is 10, then only the ten most-recently-selected buffers are shown.
If this is nil, then all buffers are shown.
A large number or nil slows down menu responsiveness."
  :type '(choice integer
		 (const :tag "全部" nil))
  :group 'mouse)

;; Don't clobber an existing menu-bar keymap, to preserve any menu-bar key
;; definitions made in loaddefs.el.
(or (lookup-key global-map [menu-bar])
    (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))
(defvar menu-bar-help-menu (make-sparse-keymap "幫助"))

;; Force Help item to come last, after the major mode's own items.
;; The symbol used to be called `help', but that gets confused with the
;; help key.
(setq menu-bar-final-items '(help-menu))

(define-key global-map [menu-bar help-menu] (cons "幫助" menu-bar-help-menu))
(defvar menu-bar-tools-menu (make-sparse-keymap "工具"))
(define-key global-map [menu-bar tools] (cons "工具" menu-bar-tools-menu))
;; This definition is just to show what this looks like.
;; It gets modified in place when menu-bar-update-buffers is called.
(defvar global-buffers-menu-map (make-sparse-keymap "緩沖區"))
(define-key global-map [menu-bar buffer]
  (cons "緩沖區" global-buffers-menu-map))
(defvar menu-bar-options-menu (make-sparse-keymap "選項"))
(define-key global-map [menu-bar options]
  (cons "選項" menu-bar-options-menu))
(defvar menu-bar-edit-menu (make-sparse-keymap "編輯"))
(define-key global-map [menu-bar edit] (cons "編輯" menu-bar-edit-menu))
(defvar menu-bar-file-menu (make-sparse-keymap "檔案"))
(define-key global-map [menu-bar file] (cons "檔案" menu-bar-file-menu))

;; This alias is for compatibility with 19.28 and before.
(defvar menu-bar-files-menu menu-bar-file-menu)

;; This is referenced by some code below; it is defined in uniquify.el
(defvar uniquify-buffer-name-style)


;; The "File" menu items
(define-key menu-bar-file-menu [exit-emacs]
  '(menu-item "離開 Emacs" save-buffers-kill-emacs
	      :help "Save unsaved buffers, then exit"))

(define-key menu-bar-file-menu [separator-exit]
  '("--"))

;; Don't use delete-frame as event name because that is a special
;; event.
(define-key menu-bar-file-menu [delete-this-frame]
  '(menu-item "刪除框架" delete-frame
	      :visible (fboundp 'delete-frame)
	      :enable (delete-frame-enabled-p)
	      :help "Delete currently selected frame"))
(define-key menu-bar-file-menu [make-frame-on-display]
  '(menu-item "在其它控製台新建框架..." make-frame-on-display
	      :visible (fboundp 'make-frame-on-display)
	      :help "Open a new frame on another display"))
(define-key menu-bar-file-menu [make-frame]
  '(menu-item "新建框架" make-frame-command
	      :visible (fboundp 'make-frame-command)
	      :help "Open a new frame"))

(define-key menu-bar-file-menu [one-window]
  '(menu-item "移除分割" delete-other-windows
	      :enable (not (one-window-p t nil))
	      :help "Selected window grows to fill the whole frame"))

(define-key menu-bar-file-menu [split-window]
  '(menu-item "分割窗格" split-window-vertically
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Split selected window in two windows"))

(define-key menu-bar-file-menu [separator-window]
  '(menu-item "--"))

(define-key menu-bar-file-menu [ps-print-region]
  '(menu-item "Postscript 列印選區(黑白)" ps-print-region
	      :enable mark-active
	      :help "Pretty-print marked region in black and white to PostScript printer"))
(define-key menu-bar-file-menu [ps-print-buffer]
  '(menu-item "Postscript 列印緩沖區(黑白)" ps-print-buffer
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Pretty-print current buffer in black and white to PostScript printer"))
(define-key menu-bar-file-menu [ps-print-region-faces]
  '(menu-item "Postscript 列印選區" ps-print-region-with-faces
	      :enable mark-active
	      :help "Pretty-print marked region to PostScript printer"))
(define-key menu-bar-file-menu [ps-print-buffer-faces]
  '(menu-item "Postscript 列印緩沖區" ps-print-buffer-with-faces
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Pretty-print current buffer to PostScript printer"))
(define-key menu-bar-file-menu [print-region]
  '(menu-item "列印選區" print-region
	      :enable mark-active
	      :help "Print region between mark and current position"))
(define-key menu-bar-file-menu [print-buffer]
  '(menu-item "列印緩沖區" print-buffer
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Print current buffer with page headings"))

(define-key menu-bar-file-menu [separator-print]
  '(menu-item "--"))

(define-key menu-bar-file-menu [recover-session]
  '(menu-item "恢復崩潰的會話" recover-session
	      :enable (and auto-save-list-file-prefix
			   (file-directory-p
                            (file-name-directory auto-save-list-file-prefix))
                           (directory-files
			    (file-name-directory auto-save-list-file-prefix)
			    nil
			    (concat "\\`"
				    (regexp-quote
				     (file-name-nondirectory
				      auto-save-list-file-prefix)))
			    t))
	      :help "Recover edits from a crashed session"))
(define-key menu-bar-file-menu [revert-buffer]
  '(menu-item "重讀緩沖區" revert-buffer
	      :enable (or revert-buffer-function
			  revert-buffer-insert-file-contents-function
			  (and buffer-file-number
			       (or (buffer-modified-p)
				   (not (verify-visited-file-modtime
					 (current-buffer))))))
	      :help "Re-read current buffer from its file"))
(define-key menu-bar-file-menu [write-file]
  '(menu-item "另存為..." write-file
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Write current buffer to another file"))
(define-key menu-bar-file-menu [save-buffer]
  '(menu-item "保存" save-buffer
	      :enable (and (buffer-modified-p)
			   (buffer-file-name)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Save current buffer to its file"))

(define-key menu-bar-file-menu [separator-save]
  '(menu-item "--"))

(defun menu-find-file-existing ()
  "編輯已有的某個檔案。"
  (interactive)
  (let* ((mustmatch (not (and (fboundp 'x-uses-old-gtk-dialog)
			      (x-uses-old-gtk-dialog))))
	 (filename (car (find-file-read-args "查找檔案: " mustmatch))))
    (if mustmatch
	(find-file-existing filename)
      (find-file filename))))

(define-key menu-bar-file-menu [kill-buffer]
  '(menu-item "關閉" kill-this-buffer
	      :enable (kill-this-buffer-enabled-p)
	      :help "Discard (kill) current buffer"))
(define-key menu-bar-file-menu [insert-file]
  '(menu-item "插入檔案..." insert-file
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Insert another file into current buffer"))
(define-key menu-bar-file-menu [dired]
  '(menu-item "打開目錄..." dired
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Read a directory, to operate on its files"))
(define-key menu-bar-file-menu [open-file]
  '(menu-item "打開檔案..." menu-find-file-existing
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Read an existing file into an Emacs buffer"))
(define-key menu-bar-file-menu [new-file]
  '(menu-item "訪問新檔案..." find-file
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Specify a new file's name, to edit the file"))


;; The "Edit" menu items

;; The "Edit->Search" submenu
(defvar menu-bar-last-search-type nil
  "Type of last non-incremental search command called from the menu.")

(defun nonincremental-repeat-search-forward ()
  "Search forward for the previous search string or regexp."
  (interactive)
  (cond
   ((and (eq menu-bar-last-search-type 'string)
	 search-ring)
    (search-forward (car search-ring)))
   ((and (eq menu-bar-last-search-type 'regexp)
	 regexp-search-ring)
    (re-search-forward (car regexp-search-ring)))
   (t
    (error "無上次搜索記錄"))))

(defun nonincremental-repeat-search-backward ()
  "Search backward for the previous search string or regexp."
  (interactive)
  (cond
   ((and (eq menu-bar-last-search-type 'string)
	 search-ring)
    (search-backward (car search-ring)))
   ((and (eq menu-bar-last-search-type 'regexp)
	 regexp-search-ring)
    (re-search-backward (car regexp-search-ring)))
   (t
    (error "無上次搜索記錄"))))

(defun nonincremental-search-forward (string)
  "Read a string and search for it nonincrementally."
  (interactive "搜索字符串: ")
  (setq menu-bar-last-search-type 'string)
  (if (equal string "")
      (search-forward (car search-ring))
    (isearch-update-ring string nil)
    (search-forward string)))

(defun nonincremental-search-backward (string)
  "Read a string and search backward for it nonincrementally."
  (interactive "搜索字符串: ")
  (setq menu-bar-last-search-type 'string)
  (if (equal string "")
      (search-backward (car search-ring))
    (isearch-update-ring string nil)
    (search-backward string)))

(defun nonincremental-re-search-forward (string)
  "Read a regular expression and search for it nonincrementally."
  (interactive "搜索正則表達式: ")
  (setq menu-bar-last-search-type 'regexp)
  (if (equal string "")
      (re-search-forward (car regexp-search-ring))
    (isearch-update-ring string t)
    (re-search-forward string)))

(defun nonincremental-re-search-backward (string)
  "Read a regular expression and search backward for it nonincrementally."
  (interactive "搜索正則表達式: ")
  (setq menu-bar-last-search-type 'regexp)
  (if (equal string "")
      (re-search-backward (car regexp-search-ring))
    (isearch-update-ring string t)
    (re-search-backward string)))

(defvar menu-bar-search-menu (make-sparse-keymap "搜索"))

;; The Edit->Search->Incremental Search menu
(defvar menu-bar-i-search-menu
  (make-sparse-keymap "漸進式搜索"))

(define-key menu-bar-i-search-menu [isearch-backward-regexp]
  '(menu-item "後退搜索正則表達式..." isearch-backward-regexp
	      :help "Search backwards for a regular expression as you type it"))
(define-key menu-bar-i-search-menu [isearch-forward-regexp]
  '(menu-item "向前搜索正則表達式..." isearch-forward-regexp
	      :help "Search forward for a regular expression as you type it"))
(define-key menu-bar-i-search-menu [isearch-backward]
  '(menu-item "後退搜索字符串..." isearch-backward
	      :help "Search backwards for a string as you type it"))
(define-key menu-bar-i-search-menu [isearch-forward]
  '(menu-item "向前搜索字符串..." isearch-forward
	      :help "Search forward for a string as you type it"))

(define-key menu-bar-search-menu [i-search]
  (list 'menu-item "漸進式搜索" menu-bar-i-search-menu))
(define-key menu-bar-search-menu [separator-tag-isearch]
  '(menu-item "--"))

(define-key menu-bar-search-menu [tags-continue]
  '(menu-item "繼續搜索 Tag 標識" tags-loop-continue
	      :help "Continue last tags search operation"))
(define-key menu-bar-search-menu [tags-srch]
  '(menu-item "搜索帶 Tag 標識的檔案..." tags-search
	      :help "Search for a regexp in all tagged files"))
(define-key menu-bar-search-menu [separator-tag-search]
  '(menu-item "--"))

(define-key menu-bar-search-menu [repeat-search-back]
  '(menu-item "繼續後退" nonincremental-repeat-search-backward
	      :enable (or (and (eq menu-bar-last-search-type 'string)
			       search-ring)
			  (and (eq menu-bar-last-search-type 'regexp)
			       regexp-search-ring))
	      :help "Repeat last search backwards"))
(define-key menu-bar-search-menu [repeat-search-fwd]
  '(menu-item "繼續向前" nonincremental-repeat-search-forward
	      :enable (or (and (eq menu-bar-last-search-type 'string)
			       search-ring)
			  (and (eq menu-bar-last-search-type 'regexp)
			       regexp-search-ring))
	      :help "Repeat last search forward"))
(define-key menu-bar-search-menu [separator-repeat-search]
  '(menu-item "--"))

(define-key menu-bar-search-menu [re-search-backward]
  '(menu-item "後退搜索正則表達式..." nonincremental-re-search-backward
	      :help "Search backwards for a regular expression"))
(define-key menu-bar-search-menu [re-search-forward]
  '(menu-item "向前搜索正則表達式..." nonincremental-re-search-forward
	      :help "Search forward for a regular expression"))

(define-key menu-bar-search-menu [search-backward]
  '(menu-item "後退搜索字符串..." nonincremental-search-backward
	      :help "Search backwards for a string"))
(define-key menu-bar-search-menu [search-forward]
  '(menu-item "向前搜索字符串..." nonincremental-search-forward
	      :help "Search forward for a string"))

;; The Edit->Replace submenu

(defvar menu-bar-replace-menu (make-sparse-keymap "替換"))

(define-key menu-bar-replace-menu [tags-repl-continue]
  '(menu-item "繼續替換" tags-loop-continue
	      :help "Continue last tags replace operation"))
(define-key menu-bar-replace-menu [tags-repl]
  '(menu-item "在帶 Tag 標識的檔案中進行替換..." tags-query-replace
	      :help "Interactively replace a regexp in all tagged files"))
(define-key menu-bar-replace-menu [separator-replace-tags]
  '(menu-item "--"))

(define-key menu-bar-replace-menu [query-replace-regexp]
  '(menu-item "替換正則表達式..." query-replace-regexp
	      :enable (not buffer-read-only)
	      :help "Replace regular expression interactively, ask about each occurrence"))
(define-key menu-bar-replace-menu [query-replace]
  '(menu-item "替換字符串..." query-replace
	      :enable (not buffer-read-only)
	      :help "Replace string interactively, ask about each occurrence"))

;;; Assemble the top-level Edit menu items.
(define-key menu-bar-edit-menu [props]
  '(menu-item "文本屬性" facemenu-menu))

(define-key menu-bar-edit-menu [fill]
  '(menu-item "調整" fill-region
	      :enable (and mark-active (not buffer-read-only))
	      :help
	      "Fill text in region to fit between left and right margin"))

(define-key menu-bar-edit-menu [separator-bookmark]
  '(menu-item "--"))

(define-key menu-bar-edit-menu [bookmark]
  '(menu-item "書簽" menu-bar-bookmark-map))

(defvar menu-bar-goto-menu (make-sparse-keymap "轉到"))

(define-key menu-bar-goto-menu [set-tags-name]
  '(menu-item "設置 Tag 標識檔案名..." visit-tags-table
	      :help "Tell Tags commands which tag table file to use"))

(define-key menu-bar-goto-menu [separator-tag-file]
  '(menu-item "--"))

(define-key menu-bar-goto-menu [apropos-tags]
  '(menu-item "相關的 Tag 標識..." tags-apropos
	      :help "Find function/variables whose names match regexp"))
(define-key menu-bar-goto-menu [next-tag-otherw]
  '(menu-item "其它窗格的下一個 Tag 標識"
	      menu-bar-next-tag-other-window
	      :enable (and (boundp 'tags-location-ring)
			   (not (ring-empty-p tags-location-ring)))
	      :help "Find next function/variable matching last tag name in another window"))

(defun menu-bar-next-tag-other-window ()
  "Find the next definition of the tag already specified."
  (interactive)
  (find-tag-other-window nil t))

(defun menu-bar-next-tag ()
  "Find the next definition of the tag already specified."
  (interactive)
  (find-tag nil t))

(define-key menu-bar-goto-menu [next-tag]
  '(menu-item "查找下一個 Tag 標識"
	      menu-bar-next-tag
	      :enable (and (boundp 'tags-location-ring)
			   (not (ring-empty-p tags-location-ring)))
	      :help "Find next function/variable matching last tag name"))
(define-key menu-bar-goto-menu [find-tag-otherw]
  '(menu-item "在其它窗格中查找 Tag 標識..." find-tag-other-window
	      :help "Find function/variable definition in another window"))
(define-key menu-bar-goto-menu [find-tag]
  '(menu-item "查找 Tag 標識..." find-tag
	      :help "Find definition of function or variable"))

(define-key menu-bar-goto-menu [separator-tags]
  '(menu-item "--"))

(define-key menu-bar-goto-menu [end-of-buf]
  '(menu-item "轉到緩沖區末尾" end-of-buffer))
(define-key menu-bar-goto-menu [beg-of-buf]
  '(menu-item "轉到緩沖區開頭" beginning-of-buffer))
(define-key menu-bar-goto-menu [go-to-pos]
  '(menu-item "轉到緩沖區位置..." goto-char
	      :help "Read a number N and go to buffer position N"))
(define-key menu-bar-goto-menu [go-to-line]
  '(menu-item "轉到某行..." goto-line
	      :help "Read a line number and go to that line"))

(define-key menu-bar-edit-menu [goto]
  (list 'menu-item "轉到" menu-bar-goto-menu))

(define-key menu-bar-edit-menu [replace]
  (list 'menu-item "替換" menu-bar-replace-menu))

(define-key menu-bar-edit-menu [search]
  (list 'menu-item "搜索" menu-bar-search-menu))

(define-key menu-bar-edit-menu [separator-search]
  '(menu-item "--"))

(define-key menu-bar-edit-menu [mark-whole-buffer]
  '(menu-item "全選" mark-whole-buffer
	      :help "Mark the whole buffer for a subsequent cut/copy."))
(define-key menu-bar-edit-menu [clear]
  '(menu-item "清除" delete-region
	      :enable (and mark-active
			   (not buffer-read-only)
			   (not (mouse-region-match)))
	      :help
	      "Delete the text in region between mark and current position"))
(defvar yank-menu (cons "選擇性粘貼(即召回)" nil))
(fset 'yank-menu (cons 'keymap yank-menu))
(define-key menu-bar-edit-menu [paste-from-menu]
  '(menu-item "Paste from kill menu" yank-menu
	      :enable (and (cdr yank-menu) (not buffer-read-only))
	      :help "Choose a string from the kill ring and paste it"))
(define-key menu-bar-edit-menu [paste]
  '(menu-item "粘貼" yank
	      :enable (and
		       ;; Emacs compiled --without-x doesn't have
		       ;; x-selection-exists-p.
		       (fboundp 'x-selection-exists-p)
		       (x-selection-exists-p) (not buffer-read-only))
	      :help "Paste (yank) text most recently cut/copied"))
(define-key menu-bar-edit-menu [copy]
  '(menu-item "復製" menu-bar-kill-ring-save
	      :enable mark-active
	      :help "Copy text in region between mark and current position"
	      :keys "\\[kill-ring-save]"))
(define-key menu-bar-edit-menu [cut]
  '(menu-item "剪切" kill-region
	      :enable (and mark-active (not buffer-read-only))
	      :help
	      "Cut (kill) text in region between mark and current position"))
(define-key menu-bar-edit-menu [undo]
  '(menu-item "撤消" undo
	      :enable (and (not buffer-read-only)
			   (not (eq t buffer-undo-list))
			   (if (eq last-command 'undo)
			       (listp pending-undo-list)
			     (consp buffer-undo-list)))
	      :help "Undo last operation"))

(defun menu-bar-kill-ring-save (beg end)
  (interactive "r")
  (if (mouse-region-match)
      (message "Selecting a region with the mouse does `copy' automatically")
    (kill-ring-save beg end)))

;; These are alternative definitions for the cut, paste and copy
;; menu items.  Use them if your system expects these to use the clipboard.

(put 'clipboard-kill-region 'menu-enable 'mark-active)
(put 'clipboard-kill-ring-save 'menu-enable 'mark-active)
(put 'clipboard-yank 'menu-enable
     '(or (and (fboundp 'x-selection-exists-p) (x-selection-exists-p))
	  (x-selection-exists-p 'CLIPBOARD)))

(defun clipboard-yank ()
  "Insert the clipboard contents, or the last stretch of killed text."
  (interactive "*")
  (let ((x-select-enable-clipboard t))
    (yank)))

(defun clipboard-kill-ring-save (beg end)
  "Copy region to kill ring, and save in the X clipboard."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (kill-ring-save beg end)))

(defun clipboard-kill-region (beg end)
  "Kill the region, and save it in the X clipboard."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (kill-region beg end)))

(defun menu-bar-enable-clipboard ()
  "Make CUT, PASTE and COPY (keys and menu bar items) use the clipboard.
Do the same for the keys of the same name."
  (interactive)
  ;; We can't use constant list structure here because it becomes pure,
  ;; and because it gets modified with cache data.
  (define-key menu-bar-edit-menu [paste]
    (cons "粘貼" (cons "從剪貼板粘貼文本" 'clipboard-yank)))
  (define-key menu-bar-edit-menu [copy]
    (cons "復製" (cons "復製選中的文字到剪貼板"
		       'clipboard-kill-ring-save)))
  (define-key menu-bar-edit-menu [cut]
    (cons "剪切" (cons "刪除選區內的文字并把它復製到剪貼板"
		      'clipboard-kill-region)))

  ;; These are Sun server keysyms for the Cut, Copy and Paste keys
  ;; (also for XFree86 on Sun keyboard):
  (define-key global-map [f20] 'clipboard-kill-region)
  (define-key global-map [f16] 'clipboard-kill-ring-save)
  (define-key global-map [f18] 'clipboard-yank)
  ;; X11R6 versions:
  (define-key global-map [cut] 'clipboard-kill-region)
  (define-key global-map [copy] 'clipboard-kill-ring-save)
  (define-key global-map [paste] 'clipboard-yank))

;; The "Options" menu items

(defvar menu-bar-custom-menu (make-sparse-keymap "自定義"))

(define-key menu-bar-custom-menu [customize-apropos-groups]
  '(menu-item "匹配正則表達式的分組..." customize-apropos-groups
	      :help "Browse groups whose names match regexp"))
(define-key menu-bar-custom-menu [customize-apropos-faces]
  '(menu-item "匹配正則表達式的外觀..." customize-apropos-faces
	      :help "Browse faces whose names match regexp"))
(define-key menu-bar-custom-menu [customize-apropos-options]
  '(menu-item "匹配正則表達式的選項..." customize-apropos-options
	      :help "Browse options whose names match regexp"))
(define-key menu-bar-custom-menu [customize-apropos]
  '(menu-item "匹配正則表達式的設置..." customize-apropos
	      :help "Browse customizable settings whose names match regexp"))
(define-key menu-bar-custom-menu [separator-1]
  '("--"))
(define-key menu-bar-custom-menu [customize-group]
  '(menu-item "指定分組..." customize-group
	      :help "Customize settings of specific group"))
(define-key menu-bar-custom-menu [customize-face]
  '(menu-item "指定外觀..." customize-face
	      :help "Customize attributes of specific face"))
(define-key menu-bar-custom-menu [customize-option]
  '(menu-item "指定選項..." customize-option
	      :help "Customize value of specific option"))
(define-key menu-bar-custom-menu [separator-2]
  '("--"))
(define-key menu-bar-custom-menu [customize-changed-options]
  '(menu-item "新建選項..." customize-changed-options
	      :help "Options added or changed in recent Emacs versions"))
(define-key menu-bar-custom-menu [customize-saved]
  '(menu-item "已保存的選項" customize-saved
	      :help "Customize previously saved options"))
(define-key menu-bar-custom-menu [separator-3]
  '("--"))
(define-key menu-bar-custom-menu [customize-browse]
  '(menu-item "瀏覽自定義分組" customize-browse
	      :help "Browse all customization groups"))
(define-key menu-bar-custom-menu [customize]
  '(menu-item "頂級自定義分組" customize
	      :help "The master group called `Emacs'"))

;(defvar menu-bar-preferences-menu (make-sparse-keymap "Preferences"))

(defmacro menu-bar-make-mm-toggle (fname doc help &optional props)
  "Make a menu-item for a global minor mode toggle.
FNAME is the minor mode's name (variable and function).
DOC is the text to use for the menu entry.
HELP is the text to use for the tooltip.
PROPS are additional properties."
  `'(menu-item ,doc ,fname
     ,@props
     :help ,help
     :button (:toggle . (and (default-boundp ',fname)
			     (default-value ',fname)))))

(defmacro menu-bar-make-toggle (name variable doc message help &rest body)
  `(progn
     (defun ,name (&optional interactively)
       ,(concat "是否轉換到 " (downcase (substring help 0 1))
		(substring help 1) ".
In an interactive call, record this option as a candidate for saving
by \"Save Options\" in Custom buffers.")
       (interactive "p")
       (if ,(if body `(progn . ,body)
	      `(progn
		 (custom-load-symbol ',variable)
		 (let ((set (or (get ',variable 'custom-set) 'set-default))
		       (get (or (get ',variable 'custom-get) 'default-value)))
		   (funcall set ',variable (not (funcall get ',variable))))))
	   (message ,message "全局啟用")
  	 (message ,message "全局禁用"))
       ;; The function `customize-mark-as-set' must only be called when
       ;; a variable is set interactively, as the purpose is to mark it as
       ;; a candidate for "保存選項", and we do not want to save options
       ;; the user have already set explicitly in his init file.
       (if interactively (customize-mark-as-set ',variable)))
     '(menu-item ,doc ,name
		 :help ,help
                 :button (:toggle . (and (default-boundp ',variable)
					 (default-value ',variable))))))

;;; Assemble all the top-level items of the "Options" menu
(define-key menu-bar-options-menu [customize]
  (list 'menu-item "自定義 Emacs" menu-bar-custom-menu))

(defun menu-bar-options-save ()
  "Save current values of Options menu items using Custom."
  (interactive)
  (let ((need-save nil))
    ;; These are set with menu-bar-make-mm-toggle, which does not
    ;; put on a customized-value property.
    (dolist (elt '(line-number-mode column-number-mode size-indication-mode
		   cua-mode show-paren-mode transient-mark-mode
		   blink-cursor-mode display-time-mode display-battery-mode))
      (and (customize-mark-to-save elt)
	   (setq need-save t)))
    ;; These are set with `customize-set-variable'.
    (dolist (elt '(scroll-bar-mode
		   debug-on-quit debug-on-error
		   tooltip-mode menu-bar-mode tool-bar-mode
		   save-place uniquify-buffer-name-style fringe-mode
		   indicate-empty-lines indicate-buffer-boundaries
		   case-fold-search
		   current-language-environment default-input-method
		   ;; Saving `text-mode-hook' is somewhat questionable,
		   ;; as we might get more than we bargain for, if
		   ;; other code may has added hooks as well.
		   ;; Nonetheless, not saving it would like be confuse
		   ;; more often.
		   ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
		   text-mode-hook))
      (and (get elt 'customized-value)
	   (customize-mark-to-save elt)
	   (setq need-save t)))
    ;; Save if we changed anything.
    (when need-save
      (custom-save-all))))

(define-key menu-bar-options-menu [save]
  '(menu-item "保存選項" menu-bar-options-save
	      :help "Save options set from the menu above"))

(define-key menu-bar-options-menu [custom-separator]
  '("--"))

(define-key menu-bar-options-menu [mouse-set-font]
  '(menu-item "設置字體/字符集..." mouse-set-font
	       :visible (display-multi-font-p)
	       :help "Select a font from list of known fonts/fontsets"))

;; The "Show/Hide" submenu of menu "Options"

(defvar menu-bar-showhide-menu (make-sparse-keymap "顯示/隱藏"))

(define-key menu-bar-showhide-menu [column-number-mode]
  (menu-bar-make-mm-toggle column-number-mode
			   "列號"
			   "Show the current column number in the mode line"))

(define-key menu-bar-showhide-menu [line-number-mode]
  (menu-bar-make-mm-toggle line-number-mode
			   "行號"
			   "Show the current line number in the mode line"))

(define-key menu-bar-showhide-menu [size-indication-mode]
  (menu-bar-make-mm-toggle size-indication-mode
			   "顯示檔案大小"
			   "Show the size of the buffer in the mode line"))

(define-key menu-bar-showhide-menu [linecolumn-separator]
  '("--"))

(define-key menu-bar-showhide-menu [showhide-battery]
  (menu-bar-make-mm-toggle display-battery-mode
			   "電池狀態"
			   "Display battery status information in mode line"))

(define-key menu-bar-showhide-menu [showhide-date-time]
  (menu-bar-make-mm-toggle display-time-mode
			   "時間-負載-郵件"
			   "Display time, system load averages and \
mail status in mode line"))

(define-key menu-bar-showhide-menu [datetime-separator]
  '("--"))

(define-key menu-bar-showhide-menu [showhide-speedbar]
  '(menu-item "快捷列" speedbar-frame-mode
	      :help "Display a Speedbar quick-navigation frame"
	      :button (:toggle
		       . (and (boundp 'speedbar-frame)
			      (frame-live-p (symbol-value 'speedbar-frame))
			      (frame-visible-p
			       (symbol-value 'speedbar-frame))))))

(defvar menu-bar-showhide-fringe-menu (make-sparse-keymap "包邊"))

(defvar menu-bar-showhide-fringe-ind-menu
  (make-sparse-keymap "緩沖區包邊"))

(defun menu-bar-showhide-fringe-ind-customize ()
  "Show customization buffer for `indicate-buffer-boundaries'."
  (interactive)
  (customize-variable 'indicate-buffer-boundaries))

(define-key menu-bar-showhide-fringe-ind-menu [customize]
  '(menu-item "其它(自定義) [Other (Customize)]"
	      menu-bar-showhide-fringe-ind-customize
	      :help "Additional choices available through Custom buffer"
	      :visible (display-graphic-p)
	      :button (:radio . (not (member indicate-buffer-boundaries
					     '(nil left right
					       ((top . left) (bottom . right))
					       ((t . right) (top . left))))))))

(defun menu-bar-showhide-fringe-ind-mixed ()
  "Display top and bottom indicators in opposite fringes, arrows in right."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries
			  '((t . right) (top . left))))

(define-key menu-bar-showhide-fringe-ind-menu [mixed]
  '(menu-item "反方向-右箭頭 (Opposite, Arrows Right)" menu-bar-showhide-fringe-ind-mixed
	      :help
	      "Show top/bottom indicators in opposite fringes, arrows in right"
	      :visible (display-graphic-p)
	      :button (:radio . (equal indicate-buffer-boundaries
				       '((t . right) (top . left))))))

(defun menu-bar-showhide-fringe-ind-box ()
  "Display top and bottom indicators in opposite fringes."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries
			  '((top . left) (bottom . right))))

(define-key menu-bar-showhide-fringe-ind-menu [box]
  '(menu-item "反方向-無箭頭 (Opposite, No Arrows)" menu-bar-showhide-fringe-ind-box
	      :help "Show top/bottom indicators in opposite fringes, no arrows"
	      :visible (display-graphic-p)
	      :button (:radio . (equal indicate-buffer-boundaries
				       '((top . left) (bottom . right))))))

(defun menu-bar-showhide-fringe-ind-right ()
  "Display buffer boundaries and arrows in the right fringe."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries 'right))

(define-key menu-bar-showhide-fringe-ind-menu [right]
  '(menu-item "在右側包邊內 (In Right Fringe)" menu-bar-showhide-fringe-ind-right
	      :help "Show buffer boundaries and arrows in right fringe"
	      :visible (display-graphic-p)
	      :button (:radio . (eq indicate-buffer-boundaries 'right))))

(defun menu-bar-showhide-fringe-ind-left ()
  "Display buffer boundaries and arrows in the left fringe."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries 'left))

(define-key menu-bar-showhide-fringe-ind-menu [left]
  '(menu-item "在左側包邊內 (In Left Fringe)" menu-bar-showhide-fringe-ind-left
	      :help "Show buffer boundaries and arrows in left fringe"
	      :visible (display-graphic-p)
	      :button (:radio . (eq indicate-buffer-boundaries 'left))))

(defun menu-bar-showhide-fringe-ind-none ()
  "Do not display any buffer boundary indicators."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries nil))

(define-key menu-bar-showhide-fringe-ind-menu [none]
  '(menu-item "不提示 (No Indicators)" menu-bar-showhide-fringe-ind-none
	      :help "Hide all buffer boundary indicators and arrows"
	      :visible (display-graphic-p)
	      :button (:radio . (eq indicate-buffer-boundaries nil))))

(define-key menu-bar-showhide-fringe-menu [showhide-fringe-ind]
  (list 'menu-item "緩沖區邊界" menu-bar-showhide-fringe-ind-menu
	:visible `(display-graphic-p)
	:help "Indicate buffer boundaries in fringe"))

(define-key menu-bar-showhide-fringe-menu [indicate-empty-lines]
  (menu-bar-make-toggle toggle-indicate-empty-lines indicate-empty-lines
			"指示空行"
			"提示空行 %s"
			"Indicate trailing empty lines in fringe, globally"))

(defun menu-bar-showhide-fringe-menu-customize ()
  "Show customization buffer for `fringe-mode'."
  (interactive)
  (customize-variable 'fringe-mode))

(define-key menu-bar-showhide-fringe-menu [customize]
  '(menu-item "自定義包邊" menu-bar-showhide-fringe-menu-customize
	      :help "Detailed customization of fringe"
	      :visible (display-graphic-p)))

(defun menu-bar-showhide-fringe-menu-customize-reset ()
  "Reset the fringe mode: display fringes on both sides of a window."
  (interactive)
  (customize-set-variable 'fringe-mode nil))

(define-key menu-bar-showhide-fringe-menu [default]
  '(menu-item "默認值 (Default)" menu-bar-showhide-fringe-menu-customize-reset
	      :help "Default width fringe on both left and right side"
	      :visible (display-graphic-p)
	      :button (:radio . (eq fringe-mode nil))))

(defun menu-bar-showhide-fringe-menu-customize-right ()
  "Display fringes only on the right of each window."
  (interactive)
  (require 'fringe)
  (customize-set-variable 'fringe-mode '(0 . nil)))

(define-key menu-bar-showhide-fringe-menu [right]
  '(menu-item "在右側 (On the Right)" menu-bar-showhide-fringe-menu-customize-right
	      :help "Fringe only on the right side"
	      :visible (display-graphic-p)
	      :button (:radio . (equal fringe-mode '(0 . nil)))))

(defun menu-bar-showhide-fringe-menu-customize-left ()
  "Display fringes only on the left of each window."
  (interactive)
  (require 'fringe)
  (customize-set-variable 'fringe-mode '(nil . 0)))

(define-key menu-bar-showhide-fringe-menu [left]
  '(menu-item "在左側 (On the Left)" menu-bar-showhide-fringe-menu-customize-left
	      :help "Fringe only on the left side"
	      :visible (display-graphic-p)
	      :button (:radio . (equal fringe-mode '(nil . 0)))))

(defun menu-bar-showhide-fringe-menu-customize-disable ()
  "Do not display window fringes."
  (interactive)
  (require 'fringe)
  (customize-set-variable 'fringe-mode 0))

(define-key menu-bar-showhide-fringe-menu [none]
  '(menu-item "無 (None)" menu-bar-showhide-fringe-menu-customize-disable
	      :help "關閉包邊 (Turn off fringe)"
	      :visible (display-graphic-p)
	      :button (:radio . (eq fringe-mode 0))))

(define-key menu-bar-showhide-menu [showhide-fringe]
  (list 'menu-item "包邊" menu-bar-showhide-fringe-menu
	:visible `(display-graphic-p)))

(defvar menu-bar-showhide-scroll-bar-menu (make-sparse-keymap "滑塊"))

(define-key menu-bar-showhide-scroll-bar-menu [right]
  '(menu-item "在右側 (On the Right)"
	      menu-bar-right-scroll-bar
	      :help "Scroll-bar on the right side"
	      :visible (display-graphic-p)
	      :button (:radio . (eq (cdr (assq 'vertical-scroll-bars
					       (frame-parameters))) 'right))))
(defun menu-bar-right-scroll-bar ()
  "Display scroll bars on the right of each window."
  (interactive)
  (customize-set-variable 'scroll-bar-mode 'right))

(define-key menu-bar-showhide-scroll-bar-menu [left]
  '(menu-item "在左側 (On the Left)"
	      menu-bar-left-scroll-bar
	      :help "Scroll-bar on the left side"
	      :visible (display-graphic-p)
	      :button (:radio . (eq (cdr (assq 'vertical-scroll-bars
					       (frame-parameters))) 'left))))

(defun menu-bar-left-scroll-bar ()
  "Display scroll bars on the left of each window."
  (interactive)
  (customize-set-variable 'scroll-bar-mode 'left))

(define-key menu-bar-showhide-scroll-bar-menu [none]
  '(menu-item "無 (None)"
	      menu-bar-no-scroll-bar
	      :help "關閉滑塊 (Turn off scroll-bar)"
	      :visible (display-graphic-p)
	      :button (:radio . (eq (cdr (assq 'vertical-scroll-bars
					       (frame-parameters))) nil))))

(defun menu-bar-no-scroll-bar ()
  "關閉滑塊。"
  (interactive)
  (customize-set-variable 'scroll-bar-mode nil))

(define-key menu-bar-showhide-menu [showhide-scroll-bar]
  (list 'menu-item "滑塊" menu-bar-showhide-scroll-bar-menu
	:visible `(display-graphic-p)))

(define-key menu-bar-showhide-menu [showhide-tooltip-mode]
  (list 'menu-item "工具列提示" 'tooltip-mode
	:help "Turn tooltips on/off"
	:visible  `(and (display-graphic-p) (fboundp 'x-show-tip))
	:button `(:toggle . tooltip-mode)))

(define-key menu-bar-showhide-menu [menu-bar-mode]
  '(menu-item "菜單列" menu-bar-mode
	      :help "打開/關閉菜單列"
	      :button (:toggle . menu-bar-mode)))

(define-key menu-bar-showhide-menu [showhide-tool-bar]
  (list 'menu-item "工具列" 'tool-bar-mode
	:help "打開/關閉工具列"
	:visible `(display-graphic-p)
	:button `(:toggle . tool-bar-mode)))

(define-key menu-bar-options-menu [showhide]
  (list 'menu-item "顯示/隱藏" menu-bar-showhide-menu))

(define-key menu-bar-options-menu [showhide-separator]
  '("--"))

(define-key menu-bar-options-menu [mule]
  ;; It is better not to use backquote here,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (list 'menu-item "Mule(多國語言環境)" mule-menu-keymap
;; Most of the MULE menu actually does make sense in unibyte mode,
;; e.g. language selection.
;;;	':visible 'default-enable-multibyte-characters
	))
;(setq menu-bar-final-items (cons 'mule menu-bar-final-items))
;(define-key menu-bar-options-menu [preferences]
;  (list 'menu-item "Preferences" menu-bar-preferences-menu
;	:help "Toggle important global options"))

(define-key menu-bar-options-menu [mule-separator]
  '("--"))

(define-key menu-bar-options-menu [debug-on-quit]
  (menu-bar-make-toggle toggle-debug-on-quit debug-on-quit
			"退出時 (或按 C-g 鍵) 進入調試器" "退出 %s 時進行調試"
			"按 C-g 鍵進入 Lisp 調試器"))
(define-key menu-bar-options-menu [debug-on-error]
  (menu-bar-make-toggle toggle-debug-on-error debug-on-error
			"出現錯誤時進入調試器" "出現 %s 錯誤時進行調試"
			"出現錯誤信號時進入 Lisp 調試器"))
(define-key menu-bar-options-menu [debugger-separator]
  '("--"))

(define-key menu-bar-options-menu [blink-cursor-mode]
  (menu-bar-make-mm-toggle blink-cursor-mode
			   "光標閃爍"
			   "Whether the cursor blinks (Blink Cursor mode)"))
(define-key menu-bar-options-menu [cursor-separator]
  '("--"))

(define-key menu-bar-options-menu [save-place]
  (menu-bar-make-toggle toggle-save-place-globally save-place
			"保存會話的檔案位置"
			"保存檔案 %s 的位置"
			"Visit files of previous session when restarting Emacs"
                        (require 'saveplace)
                        ;; Do it by name, to avoid a free-variable
                        ;; warning during byte compilation.
                        (set-default
                         'save-place (not (symbol-value 'save-place)))))

(define-key menu-bar-options-menu [uniquify]
  (menu-bar-make-toggle toggle-uniquify-buffer-names uniquify-buffer-name-style
			"在緩沖區使用目錄名"
			"緩沖區名稱中的目錄名(重名) %s"
			"Uniquify buffer names by adding parent directory names"
			(require 'uniquify)
			(setq uniquify-buffer-name-style
			      (if (not uniquify-buffer-name-style)
				  'forward))))

(define-key menu-bar-options-menu [edit-options-separator]
  '("--"))
(define-key menu-bar-options-menu [cua-mode]
  (menu-bar-make-mm-toggle cua-mode
			   "C-x/C-c/C-v 剪切和粘貼(CUA)"
			   "Use C-z/C-x/C-c/C-v keys for undo/cut/copy/paste"
			   (:visible (or (not (boundp 'cua-enable-cua-keys))
					 cua-enable-cua-keys))))

(define-key menu-bar-options-menu [cua-emulation-mode]
  (menu-bar-make-mm-toggle cua-mode
			   "按住 Shift 鍵并移動光標來選定區域(CUA)"
			   "Use shifted movement keys to set and extend the region."
			   (:visible (and (boundp 'cua-enable-cua-keys)
					  (not cua-enable-cua-keys)))))

(define-key menu-bar-options-menu [case-fold-search]
  (menu-bar-make-toggle toggle-case-fold-search case-fold-search
	    "區分字母大小寫來搜索"
	    "區分字母大小寫來搜索 %s"
	    "Globally ignore letter-case in search"))

(defun menu-bar-text-mode-auto-fill ()
  (interactive)
  (toggle-text-mode-auto-fill)
  ;; This is somewhat questionable, as `text-mode-hook'
  ;; might have changed outside customize.
  ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
  (customize-mark-as-set 'text-mode-hook))

(define-key menu-bar-options-menu [auto-fill-mode]
  '(menu-item "文本模式時單詞換行"
              menu-bar-text-mode-auto-fill
	      :help "Automatically fill text between left and right margins (Auto Fill)"
              :button (:toggle . (if (listp text-mode-hook)
				     (member 'turn-on-auto-fill text-mode-hook)
				   (eq 'turn-on-auto-fill text-mode-hook)))))
(define-key menu-bar-options-menu [truncate-lines]
  '(menu-item "對當前緩沖區的長行換行"
	      toggle-truncate-lines
	      :help "Truncate long lines on the screen"
	      :button (:toggle . truncate-lines)
	      :enable (menu-bar-menu-frame-live-and-visible-p)))

(define-key menu-bar-options-menu [highlight-separator]
  '("--"))
(define-key menu-bar-options-menu [highlight-paren-mode]
  (menu-bar-make-mm-toggle show-paren-mode
			   "高亮配對的括號"
			   "Highlight matching/mismatched parentheses at cursor (Show Paren mode)"))
(define-key menu-bar-options-menu [transient-mark-mode]
  (menu-bar-make-mm-toggle transient-mark-mode
			   "高亮選區"
			   "Make text in active region stand out in color (Transient Mark mode)"
			   (:enable (not cua-mode))))


;; The "Tools" menu items

(defun send-mail-item-name ()
  (let* ((known-send-mail-commands '((sendmail-user-agent . "sendmail")
				     (mh-e-user-agent . "MH")
				     (message-user-agent . "Gnus 迅息")
				     (gnus-user-agent . "Gnus")))
	 (name (assq mail-user-agent known-send-mail-commands)))
    (if name
	(setq name (cdr name))
      (setq name (symbol-name mail-user-agent))
      (if (string-match "\\(.+\\)-user-agent" name)
	  (setq name (match-string 1 name))))
    name))

(defun read-mail-item-name ()
  (let* ((known-rmail-commands '((rmail . "RMAIL")
				 (mh-rmail . "MH")
				 (gnus . "Gnus")))
	 (known (assq read-mail-command known-rmail-commands)))
    (if known (cdr known) (symbol-name read-mail-command))))

(defvar menu-bar-games-menu (make-sparse-keymap "游戲"))

(define-key menu-bar-tools-menu [games]
  (list 'menu-item "游戲" menu-bar-games-menu))

(define-key menu-bar-tools-menu [separator-games]
  '("--"))

(define-key menu-bar-games-menu [zone]
  '(menu-item "花屏(zone)"  zone
	      :help "Play tricks with Emacs display when Emacs is idle"))
(define-key menu-bar-games-menu [tetris]
  '(menu-item "堆積木(tetris)"  tetris))
(define-key menu-bar-games-menu [solitaire]
  '(menu-item "寶石棋(solitaire)"  solitaire))
(define-key menu-bar-games-menu [snake]
  '(menu-item "貪食蛇(snake)"  snake
	      :help "Move snake around avoiding collisions"))
(define-key menu-bar-games-menu [mult]
  '(menu-item "乘法謎(mpuz)"  mpuz
	      :help "Exercise brain with multiplication"))
(define-key menu-bar-games-menu [life]
  '(menu-item "生命(life)"  life
	      :help "Watch how John Conway's cellular automaton evolves"))
(define-key menu-bar-games-menu [hanoi]
  '(menu-item "漢諾塔(hanoi)" hanoi
	      :help "Watch Towers-of-Hanoi puzzle solved by Emacs"))
(define-key menu-bar-games-menu [gomoku]
  '(menu-item "五子棋(gomoku)"  gomoku
	      :help "Mark 5 contiguous squares (like tic-tac-toe)"))
(define-key menu-bar-games-menu [black-box]
  '(menu-item "黑箱子(blackbox)"  blackbox
	      :help "Find balls in a black box by shooting rays"))
(define-key menu-bar-games-menu [adventure]
  '(menu-item "冒險(dunnet)"  dunnet
	      :help "Dunnet, a text Adventure game for Emacs"))
(define-key menu-bar-games-menu [5x5]
  '(menu-item "5x5" 5x5
	      :help "Fill in all the squares on a 5x5 board"))

(define-key menu-bar-tools-menu [simple-calculator]
  '(menu-item "簡單計算器(calculator)" calculator
	      :help "Invoke the Emacs built-in quick calculator"))
(define-key menu-bar-tools-menu [calc]
  '(menu-item "高級計算器(calc)" calc
	      :help "Invoke the Emacs built-in full scientific calculator"))
(define-key menu-bar-tools-menu [calendar]
  '(menu-item "日歷(calendar)" calendar
	      :help "Invoke the Emacs built-in calendar"))

(define-key menu-bar-tools-menu [separator-net]
  '("--"))

(define-key menu-bar-tools-menu [directory-search]
  '(menu-item "目錄搜索" eudc-tools-menu))
(define-key menu-bar-tools-menu [compose-mail]
  (list
   'menu-item `(format "發送電郵(用 %s)" (send-mail-item-name))
   'compose-mail
   :visible `(and mail-user-agent (not (eq mail-user-agent 'ignore)))
   :help "發送電郵迅息"))
(define-key menu-bar-tools-menu [rmail]
  (list
   'menu-item `(format "查看電郵(用 %s)" (read-mail-item-name))
   'menu-bar-read-mail
   :visible `(and read-mail-command (not (eq read-mail-command 'ignore)))
   :help "Read your mail and reply to it"))

(defun menu-bar-read-mail ()
  "Read mail using `read-mail-command'."
  (interactive)
  (call-interactively read-mail-command))

(define-key menu-bar-tools-menu [gnus]
  '(menu-item "查看網絡新聞(Gnus)" gnus
	      :help "Read network news groups"))

(define-key menu-bar-tools-menu [separator-vc]
  '("--"))

(define-key menu-bar-tools-menu [pcl-cvs]
  '(menu-item "PCL-CVS" cvs-global-menu))
(define-key menu-bar-tools-menu [vc] nil) ;Create the place for the VC menu.

(define-key menu-bar-tools-menu [separator-compare]
  '("--"))

(define-key menu-bar-tools-menu [ediff-misc]
  '(menu-item "Ediff 雜項" menu-bar-ediff-misc-menu))
(define-key menu-bar-tools-menu [epatch]
  '(menu-item "應用補丁" menu-bar-epatch-menu))
(define-key menu-bar-tools-menu [ediff-merge]
  '(menu-item "合并" menu-bar-ediff-merge-menu))
(define-key menu-bar-tools-menu [compare]
  '(menu-item "比對(Ediff)" menu-bar-ediff-menu))

(define-key menu-bar-tools-menu [separator-spell]
  '("--"))

(define-key menu-bar-tools-menu [spell]
  '(menu-item "拼寫檢查" ispell-menu-map))

(define-key menu-bar-tools-menu [separator-prog]
  '("--"))

(define-key menu-bar-tools-menu [gdb]
  '(menu-item "調試器(GDB)..." gdb
	      :help "Debug a program from within Emacs with GDB"))
(define-key menu-bar-tools-menu [shell-on-region]
  '(menu-item "選區內的 Shell 命令..." shell-command-on-region
	      :enable mark-active
	      :help "Pass marked region to a shell command"))
(define-key menu-bar-tools-menu [shell]
  '(menu-item "Shell 命令..." shell-command
	      :help "Invoke a shell command and catch its output"))
(define-key menu-bar-tools-menu [compile]
  '(menu-item "編譯..." compile
	      :help "Invoke compiler or Make, view compilation errors"))
(define-key menu-bar-tools-menu [grep]
  '(menu-item "搜索檔案(Grep)..." grep
	      :help "Search files for strings or regexps (with Grep)"))


;; The "Help" menu items

(defvar menu-bar-describe-menu (make-sparse-keymap "說明"))

(define-key menu-bar-describe-menu [mule-diag]
  '(menu-item "顯示全部多國語言(Mule)狀態" mule-diag
	      :visible default-enable-multibyte-characters
	      :help "Display multilingual environment settings"))
(define-key menu-bar-describe-menu [describe-coding-system-briefly]
  '(menu-item "文字編碼說明(簡述)"
              describe-current-coding-system-briefly
              :visible default-enable-multibyte-characters))
(define-key menu-bar-describe-menu [describe-coding-system]
  '(menu-item "文字編碼說明..." describe-coding-system
	      :visible default-enable-multibyte-characters))
(define-key menu-bar-describe-menu [describe-input-method]
  '(menu-item "輸入法說明..." describe-input-method
	      :visible default-enable-multibyte-characters
	      :help "Keyboard layout for specific input method"))
(define-key menu-bar-describe-menu [describe-language-environment]
  (list 'menu-item "語言環境說明"
	describe-language-environment-map))

(define-key menu-bar-describe-menu [separator-desc-mule]
  '("--"))

(define-key menu-bar-describe-menu [list-keybindings]
  '(menu-item "列出熱鍵綁定" describe-bindings
	      :help "Display all current keybindings (keyboard shortcuts)"))
(define-key menu-bar-describe-menu [describe-current-display-table]
  '(menu-item "顯示表說明" describe-current-display-table
	      :help "Describe the current display table"))
(define-key menu-bar-describe-menu [describe-face]
  '(menu-item "外觀說明..." describe-face
              :help "Display the properties of a face"))
(define-key menu-bar-describe-menu [describe-variable]
  '(menu-item "變量說明..." describe-variable
	      :help "Display documentation of variable/option"))
(define-key menu-bar-describe-menu [describe-function]
  '(menu-item "函數說明..." describe-function
	      :help "Display documentation of function/command"))
(define-key menu-bar-describe-menu [describe-key-1]
  '(menu-item "按鍵和滑鼠操作說明..." describe-key
	      ;; Users typically don't identify keys and menu items...
	      :help "Display documentation of command bound to a \
key, a click, or a menu-item"))
(define-key menu-bar-describe-menu [describe-mode]
  '(menu-item "緩沖區模式說明" describe-mode
	      :help "Describe this buffer's major and minor mode"))

(defvar menu-bar-search-documentation-menu
  (make-sparse-keymap "搜索文檔"))
(defun menu-bar-read-lispref ()
  "Display the Emacs Lisp Reference manual in Info mode."
  (interactive)
  (info "elisp"))

(defun menu-bar-read-lispintro ()
  "Display the Introduction to Emacs Lisp Programming in Info mode."
  (interactive)
  (info "eintr"))

(defun search-emacs-glossary ()
  "Display the Glossary node of the Emacs manual in Info mode."
  (interactive)
  (info "Emacs 詞匯表"))

(defun emacs-index-search (topic)
  "Look up TOPIC in the indices of the Emacs User Manual."
  (interactive "尋找 sSubject 主題")
  (info "emacs")
  (Info-index topic))

(defun elisp-index-search (topic)
  "Look up TOPIC in the indices of the Emacs Lisp Reference Manual."
  (interactive "尋找 sSubject 主題")
  (info "elisp")
  (Info-index topic))

(define-key menu-bar-search-documentation-menu [search-documentation-strings]
  '(menu-item "搜索文檔字符串..." apropos-documentation
              :help
	      "Find functions and variables whose doc strings match a regexp"))
(define-key menu-bar-search-documentation-menu [find-any-object-by-name]
  '(menu-item "按名稱查找某個主題"  apropos
              :help "Find symbols of any kind whose names match a regexp"))
(define-key menu-bar-search-documentation-menu [find-option-by-value]
  '(menu-item "按值查找選項..." apropos-value
              :help "Find variables whose values match a regexp"))
(define-key menu-bar-search-documentation-menu [find-options-by-name]
  '(menu-item "按名稱查找選項..." apropos-variable
	      :help "Find variables whose names match a regexp"))
(define-key menu-bar-search-documentation-menu [find-commands-by-name]
  '(menu-item "按名稱查找指令..." apropos-command
	      :help "Find commands whose names match a regexp"))
(define-key menu-bar-search-documentation-menu [sep1]
  '("--"))
(define-key menu-bar-search-documentation-menu [lookup-command-in-manual]
  '(menu-item "在用戶手冊中尋找指令..." Info-goto-emacs-command-node
	      :help "Display manual section that describes a command"))
(define-key menu-bar-search-documentation-menu [lookup-key-in-manual]
  '(menu-item "在用戶手冊中尋找關鍵詞..." Info-goto-emacs-key-command-node
	      :help "Display manual section that describes a key"))
(define-key menu-bar-search-documentation-menu [lookup-subject-in-elisp-manual]
  '(menu-item "在 ELisp 手冊中尋找主題..." elisp-index-search
	      :help "Find description of a subject in Emacs Lisp manual"))
(define-key menu-bar-search-documentation-menu [lookup-subject-in-emacs-manual]
  '(menu-item "在用戶手冊中尋找主題..." emacs-index-search
	      :help "Find description of a subject in Emacs User manual"))
(define-key menu-bar-search-documentation-menu [emacs-terminology]
  '(menu-item "Emacs 術語" search-emacs-glossary
	      :help "Display the Glossary section of the Emacs manual"))

(defvar menu-bar-manuals-menu (make-sparse-keymap "更多手冊"))

(define-key menu-bar-manuals-menu [man]
  '(menu-item "查找 Man 手冊頁..." manual-entry
	      :help "Man-page docs for external commands and libraries"))
(define-key menu-bar-manuals-menu [sep2]
  '("--"))
(define-key menu-bar-manuals-menu [order-emacs-manuals]
  '(menu-item "訂購手冊" view-order-manuals
	      :help "How to order manuals from the Free Software Foundation"))
(define-key menu-bar-manuals-menu [lookup-subject-in-all-manuals]
  '(menu-item "在全部手冊中尋找主題..." info-apropos
	      :help "Find description of a subject in all installed manuals"))
(define-key menu-bar-manuals-menu [other-manuals]
  '(menu-item "其它全部 Info 手冊" Info-directory
	      :help "Read any of the installed manuals"))
(define-key menu-bar-manuals-menu [emacs-lisp-reference]
  '(menu-item "Emacs Lisp 參考手冊" menu-bar-read-lispref
	      :help "Read the Emacs Lisp Reference manual"))
(define-key menu-bar-manuals-menu [emac-lisp-intro]
  '(menu-item "Emacs Lisp 介紹" menu-bar-read-lispintro
	      :help "Read the Introduction to Emacs Lisp Programming"))

(define-key menu-bar-help-menu [about-gnu-project]
  '(menu-item "關于 GNU" describe-gnu-project
	      :help "About the GNU System, GNU Project, and GNU/Linux"))
(define-key menu-bar-help-menu [about-emacs]
  '(menu-item "關于 Emacs" about-emacs
	      :help "Display version number, copyright info, and basic help"))
(define-key menu-bar-help-menu [sep4]
  '("--"))
(define-key menu-bar-help-menu [describe-no-warranty]
  '(menu-item "免責聲明" describe-no-warranty
	      :help "Explain that Emacs has NO WARRANTY"))
(define-key menu-bar-help-menu [describe-copying]
  '(menu-item "版權條款" describe-copying
	      :help "Show the Emacs license (GPL)"))
(define-key menu-bar-help-menu [getting-new-versions]
  '(menu-item "獲取新版本" describe-distribution
	      :help "How to get latest versions of Emacs"))
(defun menu-bar-help-extra-packages ()
  "Display help about some additional packages available for Emacs."
  (interactive)
  (let (enable-local-variables)
    (view-file (expand-file-name "MORE.STUFF"
				 data-directory))
    (goto-address)))
(define-key menu-bar-help-menu [sep2]
  '("--"))
(define-key menu-bar-help-menu [external-packages]
  '(menu-item "擴展軟體包" menu-bar-help-extra-packages
	      :help "Lisp packages distributed separately for use in Emacs"))
(define-key menu-bar-help-menu [find-emacs-packages]
  '(menu-item "查找 Emacs 軟體包" finder-by-keyword
	      :help "Find packages and features by keyword"))
(define-key menu-bar-help-menu [more-manuals]
  (list 'menu-item "更多手冊" menu-bar-manuals-menu))
(define-key menu-bar-help-menu [emacs-manual]
  '(menu-item "查看 Emacs 手冊" info-emacs-manual
	      :help "Full documentation of Emacs features"))
(define-key menu-bar-help-menu [describe]
  (list 'menu-item "說明" menu-bar-describe-menu))
(define-key menu-bar-help-menu [search-documentation]
  (list 'menu-item "搜索文檔" menu-bar-search-documentation-menu))
(define-key menu-bar-help-menu [sep1]
  '("--"))
(define-key menu-bar-help-menu [emacs-psychotherapist]
  '(menu-item "Emacs 心理醫生(doctor)" doctor
	      :help "Our doctor will help you feel better"))
(define-key menu-bar-help-menu [send-emacs-bug-report]
  '(menu-item "發送缺陷報告..." report-emacs-bug
	      :help "Send e-mail to Emacs maintainers"))
(define-key menu-bar-help-menu [emacs-known-problems]
  '(menu-item "Emacs 已知問題" view-emacs-problems
	      :help "Read about known problems with Emacs"))
(define-key menu-bar-help-menu [emacs-news]
  '(menu-item "Emacs 新聞" view-emacs-news
	      :help "New features of this version"))
(define-key menu-bar-help-menu [emacs-faq]
  '(menu-item "Emacs 常見問題 (FAQ)" view-emacs-FAQ
	      :help "Frequently asked (and answered) questions about Emacs"))

(defun help-with-tutorial-spec-language ()
  "Use the Emacs tutorial, specifying which language you want."
  (interactive)
  (help-with-tutorial t))

(define-key menu-bar-help-menu [emacs-tutorial-language-specific]
  '(menu-item "Emacs 教程(選擇語言)..."
	      help-with-tutorial-spec-language
	      :help "Learn how to use Emacs (choose a language)"))
(define-key menu-bar-help-menu [emacs-tutorial]
  '(menu-item "Emacs 教程" help-with-tutorial
	      :help "Learn how to use Emacs"))

(defun menu-bar-menu-frame-live-and-visible-p ()
  "Return non-nil if the menu frame is alive and visible.
The menu frame is the frame for which we are updating the menu."
  (let ((menu-frame (or menu-updating-frame (selected-frame))))
    (and (frame-live-p menu-frame)
	 (frame-visible-p menu-frame))))

(defun menu-bar-non-minibuffer-window-p ()
  "Return non-nil if selected window of the menu frame is not a minibuf window.

See the documentation of `menu-bar-menu-frame-live-and-visible-p'
for the definition of the menu frame."
  (let ((menu-frame (or menu-updating-frame (selected-frame))))
    (not (window-minibuffer-p (frame-selected-window menu-frame)))))

(defun kill-this-buffer ()	; for the menu bar
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun kill-this-buffer-enabled-p ()
  (let ((count 0)
	(buffers (buffer-list)))
    (while buffers
      (or (string-match "^ " (buffer-name (car buffers)))
	  (setq count (1+ count)))
      (setq buffers (cdr buffers)))
    (and (menu-bar-non-minibuffer-window-p)
	 (> count 1))))

(put 'dired 'menu-enable '(menu-bar-non-minibuffer-window-p))

;; Permit deleting frame if it would leave a visible or iconified frame.
(defun delete-frame-enabled-p ()
  "Return non-nil if `delete-frame' should be enabled in the menu bar."
  (let ((frames (frame-list))
	(count 0))
    (while frames
      (if (frame-visible-p (car frames))
	  (setq count (1+ count)))
      (setq frames (cdr frames)))
    (> count 1)))

(defcustom yank-menu-length 20
  "*Maximum length to display in the yank-menu."
  :type 'integer
  :group 'mouse)

(defun menu-bar-update-yank-menu (string old)
  (let ((front (car (cdr yank-menu)))
	(menu-string (if (<= (length string) yank-menu-length)
			 string
		       (concat
			(substring string 0 (/ yank-menu-length 2))
			"..."
			(substring string (- (/ yank-menu-length 2)))))))
    ;; Don't let the menu string be all dashes
    ;; because that has a special meaning in a menu.
    (if (string-match "\\`-+\\'" menu-string)
	(setq menu-string (concat menu-string " ")))
    ;; If we're supposed to be extending an existing string, and that
    ;; string really is at the front of the menu, then update it in place.
    (if (and old (or (eq old (car front))
		     (string= old (car front))))
	(progn
	  (setcar front string)
	  (setcar (cdr front) menu-string))
      (setcdr yank-menu
	      (cons
	       (cons string (cons menu-string 'menu-bar-select-yank))
	       (cdr yank-menu)))))
  (if (> (length (cdr yank-menu)) kill-ring-max)
      (setcdr (nthcdr kill-ring-max yank-menu) nil)))

(put 'menu-bar-select-yank 'apropos-inhibit t)
(defun menu-bar-select-yank ()
  "Insert the stretch of previously-killed text selected from menu.
The menu shows all the killed text sequences stored in `kill-ring'."
  (interactive "*")
  (push-mark (point))
  (insert last-command-event))


(defcustom buffers-menu-show-directories 'unless-uniquify
  "If non-nil, show directories in the Buffers menu for buffers that have them.
The special value `unless-uniquify' means that directories will be shown
unless `uniquify-buffer-name-style' is non-nil (in which case, buffer
names should include enough of a buffer's directory to distinguish it
from other buffers).

Setting this variable directly does not take effect until next time the
Buffers menu is regenerated."
  :set (lambda (symbol value)
	 (set symbol value)
	 (menu-bar-update-buffers t))
  :initialize 'custom-initialize-default
  :type '(choice (const :tag "從不" nil)
		 (const :tag "除非已啟用重名模式" unless-uniquify)
		 (const :tag "總是" t))
  :group 'menu)

(defcustom buffers-menu-show-status t
  "If non-nil, show modified/read-only status of buffers in the Buffers menu.
Setting this variable directly does not take effect until next time the
Buffers menu is regenerated."
  :set (lambda (symbol value)
	 (set symbol value)
	 (menu-bar-update-buffers t))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'menu)

(defvar list-buffers-directory nil)

(defvar menu-bar-update-buffers-maxbuf)

(defun menu-bar-select-buffer ()
  (interactive)
  (switch-to-buffer last-command-event))

(defun menu-bar-select-frame ()
  (interactive)
  (let (frame)
    (dolist (f (frame-list))
      (when (equal last-command-event (frame-parameter f 'name))
	(setq frame f)))
    ;; FRAME can be nil when user specifies the selected frame.
    (setq frame (or frame (selected-frame)))
    (make-frame-visible frame)
    (raise-frame frame)
    (select-frame frame)))

(defun menu-bar-update-buffers-1 (elt)
  (let* ((buf (car elt))
	 (file
	  (and (if (eq buffers-menu-show-directories 'unless-uniquify)
		   (or (not (boundp 'uniquify-buffer-name-style))
		       (null uniquify-buffer-name-style))
		 buffers-menu-show-directories)
	       (or (buffer-file-name buf)
		   (buffer-local-value 'list-buffers-directory buf)))))
    (when file
      (setq file (file-name-directory file)))
    (when (and file (> (length file) 20))
      (setq file (concat "..." (substring file -17))))
    (cons (if buffers-menu-show-status
	      (let ((mod (if (buffer-modified-p buf) "*" ""))
		    (ro (if (buffer-local-value 'buffer-read-only buf) "%" "")))
		(if file
		    (format "%s  %s%s  --  %s" (cdr elt) mod ro file)
		  (format "%s  %s%s" (cdr elt) mod ro)))
	    (if file
		(format "%s  --  %s"  (cdr elt) file)
	      (cdr elt)))
	  buf)))

;; Used to cache the menu entries for commands in the Buffers menu
(defvar menu-bar-buffers-menu-command-entries nil)

(defun menu-bar-update-buffers (&optional force)
  ;; If user discards the Buffers item, play along.
  (and (lookup-key (current-global-map) [menu-bar buffer])
       (or force (frame-or-buffer-changed-p))
       (let ((buffers (buffer-list))
	     (frames (frame-list))
	     buffers-menu)
	 ;; If requested, list only the N most recently selected buffers.
	 (if (and (integerp buffers-menu-max-size)
		  (> buffers-menu-max-size 1))
	     (if (> (length buffers) buffers-menu-max-size)
		 (setcdr (nthcdr buffers-menu-max-size buffers) nil)))

	 ;; Make the menu of buffers proper.
	 (setq buffers-menu
	       (let* ((buffer-list
		       (mapcar 'list buffers))
		      (menu-bar-update-buffers-maxbuf 0)
		      alist)
		 ;; Put into each element of buffer-list
		 ;; the name for actual display,
		 ;; perhaps truncated in the middle.
		 (dolist (buf buffer-list)
		   (let ((name (buffer-name (car buf))))
		     (setcdr buf
			     (if (> (length name) 27)
				 (concat (substring name 0 12)
					 "..."
					 (substring name -12))
			       name))))
		 ;; Compute the maximum length of any name.
		 (dolist (buf buffer-list)
		   (unless (eq ?\s (aref (cdr buf) 0))
		     (setq menu-bar-update-buffers-maxbuf
			   (max menu-bar-update-buffers-maxbuf
				(length (cdr buf))))))
		 ;; Set ALIST to an alist of the form
		 ;; ITEM-STRING . BUFFER
		 (dolist (buf buffer-list)
		   (unless (eq ?\s (aref (cdr buf) 0))
		     (push (menu-bar-update-buffers-1 buf) alist)))
		 ;; Now make the actual list of items, and add
		 ;; some miscellaneous buffer commands to the end.
		 (mapcar (lambda (pair)
			   ;; This is somewhat risque, to use
			   ;; the buffer name itself as the event
			   ;; type to define, but it works.
			   ;; It would not work to use the buffer
			   ;; since a buffer as an event has its
			   ;; own meaning.
			   (nconc (list (buffer-name (cdr pair))
					(car pair)
					(cons nil nil))
				  'menu-bar-select-buffer))
			 (nreverse alist))))

	 ;; Make a Frames menu if we have more than one frame.
	 (when (cdr frames)
	   (let ((frames-menu
		  (cons 'keymap
			(cons "選擇框架"
			      (mapcar
			       (lambda (frame)
				 (nconc
				  (list (frame-parameter frame 'name)
					(frame-parameter frame 'name)
					(cons nil nil))
				  'menu-bar-select-frame))
			       frames)))))
	     ;; Put it after the normal buffers
	     (setq buffers-menu
		   (nconc buffers-menu
			  `((frames-separator "--")
			    (frames menu-item "框架" ,frames-menu))))))

	 ;; Add in some normal commands at the end of the menu.  We use
	 ;; the copy cached in `menu-bar-buffers-menu-command-entries'
	 ;; if it's been set already.  Note that we can't use constant
	 ;; lists for the menu-entries, because the low-level menu-code
	 ;; modifies them.
	 (unless menu-bar-buffers-menu-command-entries
	   (setq menu-bar-buffers-menu-command-entries
		 (list '(command-separator "--")
		       (list 'next-buffer
			     'menu-item
			     "下一個緩沖區"
			     'next-buffer
			     :help "Switch to the \"next\" buffer in a cyclic order")
		       (list 'previous-buffer
			     'menu-item
			     "上一個緩沖區"
			     'previous-buffer
			     :help "Switch to the \"previous\" buffer in a cyclic order")
		       (list 'select-named-buffer
			     'menu-item
			     "選擇已命名的緩沖區..."
			     'switch-to-buffer
			     :help "Prompt for a buffer name, and select that buffer in the current window")
		       (list 'list-all-buffers
			     'menu-item
			     "列出全部緩沖區"
			     'list-buffers
			     :help "Pop up a window listing all Emacs buffers"
			     ))))
	 (setq buffers-menu
	       (nconc buffers-menu menu-bar-buffers-menu-command-entries))

         ;; We used to "(define-key (current-global-map) [menu-bar buffer]"
         ;; but that did not do the right thing when the [menu-bar buffer]
         ;; entry above had been moved (e.g. to a parent keymap).
	 (setcdr global-buffers-menu-map (cons "選擇緩沖區" buffers-menu)))))

(add-hook 'menu-bar-update-hook 'menu-bar-update-buffers)

(menu-bar-update-buffers)

;; this version is too slow
;;(defun format-buffers-menu-line (buffer)
;;  "Returns a string to represent the given buffer in the Buffer menu.
;;nil means the buffer shouldn't be listed.  You can redefine this."
;;  (if (string-match "\\` " (buffer-name buffer))
;;      nil
;;    (save-excursion
;;     (set-buffer buffer)
;;     (let ((size (buffer-size)))
;;       (format "%s%s %-19s %6s %-15s %s"
;;	       (if (buffer-modified-p) "*" " ")
;;	       (if buffer-read-only "%" " ")
;;	       (buffer-name)
;;	       size
;;	       mode-name
;;	       (or (buffer-file-name) ""))))))

;;; Set up a menu bar menu for the minibuffer.

(dolist (map (list minibuffer-local-map
		   ;; This shouldn't be necessary, but there's a funny
		   ;; bug in keymap.c that I don't understand yet.  -stef
		   minibuffer-local-completion-map))
  (define-key map [menu-bar minibuf]
    (cons "小緩沖" (make-sparse-keymap "小緩沖"))))

(let ((map minibuffer-local-completion-map))
  (define-key map [menu-bar minibuf ?\?]
    (list 'menu-item "列出補齊" 'minibuffer-completion-help
	  :help "Display all possible completions"))
  (define-key map [menu-bar minibuf space]
    (list 'menu-item "單詞補齊" 'minibuffer-complete-word
	  :help "Complete at most one word"))
  (define-key map [menu-bar minibuf tab]
    (list 'menu-item "補齊" 'minibuffer-complete
	  :help "Complete as far as possible")))

(let ((map minibuffer-local-map))
  (define-key map [menu-bar minibuf quit]
    (list 'menu-item "退出" 'keyboard-escape-quit
	  :help "Abort input and exit minibuffer"))
  (define-key map [menu-bar minibuf return]
    (list 'menu-item "進入" 'exit-minibuffer
	  :help "Terminate input and exit minibuffer")))

;;;###autoload
;; This comment is taken from tool-bar.el near
;; (put 'tool-bar-mode ...)
;; We want to pretend the menu bar by standard is on, as this will make
;; customize consider disabling the menu bar a customization, and save
;; that.  We could do this for real by setting :init-value below, but
;; that would overwrite disabling the tool bar from X resources.
(put 'menu-bar-mode 'standard-value '(t))

;;;###autoload
(define-minor-mode menu-bar-mode
  "Toggle display of a menu bar on each frame.
This command applies to all frames that exist and frames to be
created in the future.
With a numeric argument, if the argument is positive,
turn on menu bars; otherwise, turn off menu bars."
  :init-value nil
  :global t
  :group 'frames
  ;; Make menu-bar-mode and default-frame-alist consistent.
  (let ((lines (if menu-bar-mode 1 0)))
    ;; Alter existing frames...
    (mapc (lambda (frame)
	    (modify-frame-parameters frame
				     (list (cons 'menu-bar-lines lines))))
	  (frame-list))
    ;; ...and future ones.
    (let ((elt (assq 'menu-bar-lines default-frame-alist)))
      (if elt
	  (setcdr elt lines)
	(add-to-list 'default-frame-alist (cons 'menu-bar-lines lines)))))

  ;; Make the message appear when Emacs is idle.  We can not call message
  ;; directly.  The minor-mode message "Menu-bar mode disabled" comes
  ;; after this function returns, overwriting any message we do here.
  (when (and (interactive-p) (not menu-bar-mode))
    (run-with-idle-timer 0 nil 'message
			 "Menu-bar mode disabled.  Use M-x menu-bar-mode to make the menu bar appear."))
  menu-bar-mode)

(provide 'menu-bar)

;; arch-tag: 6e6a3c22-4ec4-4d3d-8190-583f8ef94ced
;;; menu-bar.el ends here
