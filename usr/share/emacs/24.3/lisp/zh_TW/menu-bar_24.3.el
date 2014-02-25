;;; Traditional Chinese(繁體中文) (zh_TW) localization resources for Emacs. ; -*- coding: utf-8 -*-
;;; translate script: version 2.0
;;; 2013-05-19 22:10:14 CST translated
;;; translator: Careone <emacs-locale@qq.com>, 2013
;;; menu-bar.el --- define a default menu bar

;; Copyright (C) 1993-1995, 2000-2013 Free Software Foundation, Inc.

;; Author: RMS
;; Maintainer: FSF
;; Keywords: internal, mouse
;; Package: emacs

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

;; Avishai Yacobi suggested some menu rearrangements.

;;; Commentary:

;;; Code:

;; This is referenced by some code below; it is defined in uniquify.el
(defvar uniquify-buffer-name-style)

;; From emulation/cua-base.el; used below
(defvar cua-enable-cua-keys)

;; Don't clobber an existing menu-bar keymap, to preserve any menu-bar key
;; definitions made in loaddefs.el.
(or (lookup-key global-map [menu-bar])
    (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))

(if (not (featurep 'ns))
    ;; Force Help item to come last, after the major mode's own items.
    ;; The symbol used to be called `help', but that gets confused with the
    ;; help key.
    (setq menu-bar-final-items '(help-menu))
  (if (eq system-type 'darwin)
      (setq menu-bar-final-items '(buffer services help-menu))
    (setq menu-bar-final-items '(buffer services hide-app quit))
    ;; Add standard top-level items to GNUstep menu.
    (bindings--define-key global-map [menu-bar quit]
      '(menu-item "退出" save-buffers-kill-emacs
                   :help "Save unsaved buffers, then exit"))
    (bindings--define-key global-map [menu-bar hide-app]
      '(menu-item "隱藏" ns-do-hide-emacs
                  :help "Hide Emacs")))
  (bindings--define-key global-map [menu-bar services] ; Set-up in ns-win.
    (cons "服務" (make-sparse-keymap "服務"))))

;; This definition is just to show what this looks like.
;; It gets modified in place when menu-bar-update-buffers is called.
(defvar global-buffers-menu-map (make-sparse-keymap "緩沖區"))

;; Only declared obsolete (and only made a proper alias) in 23.3.
(define-obsolete-variable-alias
  'menu-bar-files-menu 'menu-bar-file-menu "22.1")
(defvar menu-bar-file-menu
  (let ((menu (make-sparse-keymap "檔案")))

    ;; The "檔案" menu items
    (bindings--define-key menu [exit-emacs]
      '(menu-item "退出" save-buffers-kill-terminal
                  :help "Save unsaved buffers, then exit"))

    (bindings--define-key menu [separator-exit]
      menu-bar-separator)

    ;; Don't use delete-frame as event name because that is a special
    ;; event.
    (bindings--define-key menu [delete-this-frame]
      '(menu-item "刪除框架" delete-frame
                  :visible (fboundp 'delete-frame)
                  :enable (delete-frame-enabled-p)
                  :help "Delete currently selected frame"))
    (bindings--define-key menu [make-frame-on-display]
      '(menu-item "在其它控製台新建框架..." make-frame-on-display
                  :visible (fboundp 'make-frame-on-display)
                  :help "Open a new frame on another display"))
    (bindings--define-key menu [make-frame]
      '(menu-item "新建框架" make-frame-command
                  :visible (fboundp 'make-frame-command)
                  :help "Open a new frame"))

    (bindings--define-key menu [separator-frame]
      menu-bar-separator)

    (bindings--define-key menu [one-window]
      '(menu-item "移除其它窗格" delete-other-windows
                  :enable (not (one-window-p t nil))
                  :help "Make selected window fill whole frame"))

    (bindings--define-key menu [new-window-on-right]
      '(menu-item "在右側新建窗格" split-window-right
                  :enable (and (menu-bar-menu-frame-live-and-visible-p)
                               (menu-bar-non-minibuffer-window-p))
                  :help "Make new window on right of selected one"))

    (bindings--define-key menu [new-window-below]
      '(menu-item "在下方新建窗格" split-window-below
                  :enable (and (menu-bar-menu-frame-live-and-visible-p)
                               (menu-bar-non-minibuffer-window-p))
                  :help "Make new window below selected one"))

    (bindings--define-key menu [separator-window]
      menu-bar-separator)

    (bindings--define-key menu [ps-print-region]
      '(menu-item "PostScript 列印選區(黑白)" ps-print-region
                  :enable mark-active
                  :help "Pretty-print marked region in black and white to PostScript printer"))
    (bindings--define-key menu [ps-print-buffer]
      '(menu-item "PostScript 列印緩沖區(黑白)" ps-print-buffer
                  :enable (menu-bar-menu-frame-live-and-visible-p)
                  :help "Pretty-print current buffer in black and white to PostScript printer"))
    (bindings--define-key menu [ps-print-region-faces]
      '(menu-item "PostScript 列印選區"
                  ps-print-region-with-faces
                  :enable mark-active
                  :help "Pretty-print marked region to PostScript printer"))
    (bindings--define-key menu [ps-print-buffer-faces]
      '(menu-item "PostScript 列印緩沖區"
                  ps-print-buffer-with-faces
                  :enable (menu-bar-menu-frame-live-and-visible-p)
                  :help "Pretty-print current buffer to PostScript printer"))
    (bindings--define-key menu [print-region]
      '(menu-item "列印選區" print-region
                  :enable mark-active
                  :help "Print region between mark and current position"))
    (bindings--define-key menu [print-buffer]
      '(menu-item "列印緩沖區" print-buffer
                  :enable (menu-bar-menu-frame-live-and-visible-p)
                  :help "Print current buffer with page headings"))

    (bindings--define-key menu [separator-print]
      menu-bar-separator)

    (bindings--define-key menu [recover-session]
      '(menu-item "恢復崩潰的會話" recover-session
                  :enable
                  (and auto-save-list-file-prefix
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
    (bindings--define-key menu [revert-buffer]
      '(menu-item "重讀緩沖區" revert-buffer
                  :enable (or revert-buffer-function
                              revert-buffer-insert-file-contents-function
                              (and buffer-file-number
                                   (or (buffer-modified-p)
                                       (not (verify-visited-file-modtime
                                             (current-buffer))))))
                  :help "Re-read current buffer from its file"))
    (bindings--define-key menu [write-file]
      '(menu-item "另存為..." write-file
                  :enable (and (menu-bar-menu-frame-live-and-visible-p)
                               (menu-bar-non-minibuffer-window-p))
                  :help "Write current buffer to another file"))
    (bindings--define-key menu [save-buffer]
      '(menu-item "保存" save-buffer
                  :enable (and (buffer-modified-p)
                               (buffer-file-name)
                               (menu-bar-non-minibuffer-window-p))
                  :help "Save current buffer to its file"))

    (bindings--define-key menu [separator-save]
      menu-bar-separator)

    (bindings--define-key menu [kill-buffer]
      '(menu-item "關閉" kill-this-buffer
                  :enable (kill-this-buffer-enabled-p)
                  :help "Discard (kill) current buffer"))
    (bindings--define-key menu [insert-file]
      '(menu-item "插入檔案..." insert-file
                  :enable (menu-bar-non-minibuffer-window-p)
                  :help "Insert another file into current buffer"))
    (bindings--define-key menu [dired]
      '(menu-item "打開目錄..." dired
                  :enable (menu-bar-non-minibuffer-window-p)
                  :help "Read a directory, to operate on its files"))
    (bindings--define-key menu [open-file]
      '(menu-item "打開檔案..." menu-find-file-existing
                  :enable (menu-bar-non-minibuffer-window-p)
                  :help "Read an existing file into an Emacs buffer"))
    (bindings--define-key menu [new-file]
      '(menu-item "訪問新檔案..." find-file
                  :enable (menu-bar-non-minibuffer-window-p)
                  :help "Specify a new file's name, to edit the file"))

    menu))

(defun menu-find-file-existing ()
  "編輯已有的某個檔案。"
  (interactive)
  (let* ((mustmatch (not (and (fboundp 'x-uses-old-gtk-dialog)
			      (x-uses-old-gtk-dialog))))
	 (filename (car (find-file-read-args "查找檔案: " mustmatch))))
    (if mustmatch
	(find-file-existing filename)
      (find-file filename))))

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

;; The Edit->Search->Incremental Search menu
(defvar menu-bar-i-search-menu
  (let ((menu (make-sparse-keymap "漸進式搜索")))
    (bindings--define-key menu [isearch-backward-regexp]
      '(menu-item "後退搜索正則表達式..." isearch-backward-regexp
        :help "Search backwards for a regular expression as you type it"))
    (bindings--define-key menu [isearch-forward-regexp]
      '(menu-item "向前搜索正則表達式..." isearch-forward-regexp
        :help "Search forward for a regular expression as you type it"))
    (bindings--define-key menu [isearch-backward]
      '(menu-item "後退搜索字符串..." isearch-backward
        :help "Search backwards for a string as you type it"))
    (bindings--define-key menu [isearch-forward]
      '(menu-item "向前搜索字符串..." isearch-forward
        :help "Search forward for a string as you type it"))
    menu))

(defvar menu-bar-search-menu
  (let ((menu (make-sparse-keymap "搜索")))

    (bindings--define-key menu [i-search]
      `(menu-item "漸進式搜索" ,menu-bar-i-search-menu))
    (bindings--define-key menu [separator-tag-isearch]
      menu-bar-separator)

    (bindings--define-key menu [tags-continue]
      '(menu-item "繼續搜索 Tag 標識" tags-loop-continue
                  :help "Continue last tags search operation"))
    (bindings--define-key menu [tags-srch]
      '(menu-item "搜索帶 Tag 標識的檔案..." tags-search
                  :help "Search for a regexp in all tagged files"))
    (bindings--define-key menu [separator-tag-search] menu-bar-separator)

    (bindings--define-key menu [repeat-search-back]
      '(menu-item "繼續後退"
                  nonincremental-repeat-search-backward
                  :enable (or (and (eq menu-bar-last-search-type 'string)
                                   search-ring)
                              (and (eq menu-bar-last-search-type 'regexp)
                                   regexp-search-ring))
                  :help "Repeat last search backwards"))
    (bindings--define-key menu [repeat-search-fwd]
      '(menu-item "繼續向前"
                  nonincremental-repeat-search-forward
                  :enable (or (and (eq menu-bar-last-search-type 'string)
                                   search-ring)
                              (and (eq menu-bar-last-search-type 'regexp)
                                   regexp-search-ring))
                  :help "Repeat last search forward"))
    (bindings--define-key menu [separator-repeat-search]
      menu-bar-separator)

    (bindings--define-key menu [re-search-backward]
      '(menu-item "後退搜索正則表達式..."
                  nonincremental-re-search-backward
                  :help "Search backwards for a regular expression"))
    (bindings--define-key menu [re-search-forward]
      '(menu-item "向前搜索正則表達式..."
                  nonincremental-re-search-forward
                  :help "Search forward for a regular expression"))

    (bindings--define-key menu [search-backward]
      '(menu-item "後退搜索字符串..."
                  nonincremental-search-backward
                  :help "Search backwards for a string"))
    (bindings--define-key menu [search-forward]
      '(menu-item "向前搜索字符串..." nonincremental-search-forward
                  :help "Search forward for a string"))
    menu))

;; The Edit->Replace submenu

(defvar menu-bar-replace-menu
  (let ((menu (make-sparse-keymap "替換")))
    (bindings--define-key menu [tags-repl-continue]
      '(menu-item "繼續替換" tags-loop-continue
                  :help "Continue last tags replace operation"))
    (bindings--define-key menu [tags-repl]
      '(menu-item "在帶 Tag 標識的檔案中進行替換..." tags-query-replace
        :help "Interactively replace a regexp in all tagged files"))
    (bindings--define-key menu [separator-replace-tags]
      menu-bar-separator)

    (bindings--define-key menu [query-replace-regexp]
      '(menu-item "替換正則表達式..." query-replace-regexp
                  :enable (not buffer-read-only)
                  :help "Replace regular expression interactively, ask about each occurrence"))
    (bindings--define-key menu [query-replace]
      '(menu-item "替換字符串..." query-replace
        :enable (not buffer-read-only)
        :help "Replace string interactively, ask about each occurrence"))
    menu))

;;; Assemble the top-level Edit menu items.
(defvar menu-bar-goto-menu
  (let ((menu (make-sparse-keymap "轉到")))

    (bindings--define-key menu [set-tags-name]
      '(menu-item "設置 Tag 標識檔案名..." visit-tags-table
                  :help "Tell Tags commands which tag table file to use"))

    (bindings--define-key menu [separator-tag-file]
      menu-bar-separator)

    (bindings--define-key menu [apropos-tags]
      '(menu-item "相關的 Tag 標識..." tags-apropos
                  :help "Find function/variables whose names match regexp"))
    (bindings--define-key menu [next-tag-otherw]
      '(menu-item "其它窗格的下一個 Tag 標識"
                  menu-bar-next-tag-other-window
                  :enable (and (boundp 'tags-location-ring)
                               (not (ring-empty-p tags-location-ring)))
                  :help "Find next function/variable matching last tag name in another window"))

    (bindings--define-key menu [next-tag]
      '(menu-item "查找下一個 Tag 標識"
                  menu-bar-next-tag
                  :enable (and (boundp 'tags-location-ring)
                               (not (ring-empty-p tags-location-ring)))
                  :help "Find next function/variable matching last tag name"))
    (bindings--define-key menu [find-tag-otherw]
      '(menu-item "在其它窗格中查找 Tag 標識..." find-tag-other-window
                  :help "Find function/variable definition in another window"))
    (bindings--define-key menu [find-tag]
      '(menu-item "查找 Tag 標識..." find-tag
                  :help "Find definition of function or variable"))

    (bindings--define-key menu [separator-tags]
      menu-bar-separator)

    (bindings--define-key menu [end-of-buf]
      '(menu-item "轉到緩沖區末尾" end-of-buffer))
    (bindings--define-key menu [beg-of-buf]
      '(menu-item "轉到緩沖區開頭" beginning-of-buffer))
    (bindings--define-key menu [go-to-pos]
      '(menu-item "轉到緩沖區位置..." goto-char
                  :help "Read a number N and go to buffer position N"))
    (bindings--define-key menu [go-to-line]
      '(menu-item "轉到某行..." goto-line
                  :help "Read a line number and go to that line"))
    menu))

(defvar yank-menu (cons (purecopy "選擇性粘貼(即召回)") nil))
(fset 'yank-menu (cons 'keymap yank-menu))

(defvar menu-bar-edit-menu
  (let ((menu (make-sparse-keymap "編輯")))

    (bindings--define-key menu [props]
      `(menu-item "文本屬性" facemenu-menu))

    ;; ns-win.el said: Add spell for platform consistency.
    (if (featurep 'ns)
        (bindings--define-key menu [spell]
          `(menu-item "拼寫" ispell-menu-map)))

    (bindings--define-key menu [fill]
      `(menu-item "調整" fill-region
                  :enable (and mark-active (not buffer-read-only))
                  :help
                  "Fill text in region to fit between left and right margin"))

    (bindings--define-key menu [separator-bookmark]
      menu-bar-separator)

    (bindings--define-key menu [bookmark]
      `(menu-item "書簽" menu-bar-bookmark-map))

    (bindings--define-key menu [goto]
      `(menu-item "轉到" ,menu-bar-goto-menu))

    (bindings--define-key menu [replace]
      `(menu-item "替換" ,menu-bar-replace-menu))

    (bindings--define-key menu [search]
      `(menu-item "搜索" ,menu-bar-search-menu))

    (bindings--define-key menu [separator-search]
      menu-bar-separator)

    (bindings--define-key menu [mark-whole-buffer]
      '(menu-item "全選" mark-whole-buffer
                  :help "Mark the whole buffer for a subsequent cut/copy"))
    (bindings--define-key menu [clear]
      '(menu-item "清除" delete-region
                  :enable (and mark-active
                               (not buffer-read-only))
                  :help
                  "Delete the text in region between mark and current position"))

    (bindings--define-key menu (if (featurep 'ns) [select-paste]
                       [paste-from-menu])
      ;; ns-win.el said: Change text to be more consistent with
      ;; surrounding menu items `paste', etc."
      `(menu-item ,(if (featurep 'ns) "選擇并粘貼"
                     "從剪貼板(即 Kill 菜單)進行粘貼") yank-menu
                  :enable (and (cdr yank-menu) (not buffer-read-only))
                  :help "Choose a string from the kill ring and paste it"))
    (bindings--define-key menu [paste]
      '(menu-item "粘貼" yank
                  :enable (and (or
                                ;; Emacs compiled --without-x (or --with-ns)
                                ;; doesn't have x-selection-exists-p.
                                (and (fboundp 'x-selection-exists-p)
                                     (x-selection-exists-p 'CLIPBOARD))
                                (if (featurep 'ns) ; like paste-from-menu
                                    (cdr yank-menu)
                                  kill-ring))
                               (not buffer-read-only))
                  :help "Paste (yank) text most recently cut/copied"))
    (bindings--define-key menu [copy]
      ;; ns-win.el said: Substitute a Copy function that works better
      ;; under X (for GNUstep).
      `(menu-item "復製" ,(if (featurep 'ns)
                              'ns-copy-including-secondary
                            'kill-ring-save)
                  :enable mark-active
                  :help "Copy text in region between mark and current position"
                  :keys ,(if (featurep 'ns)
                             "\\[ns-copy-including-secondary]"
                           "\\[kill-ring-save]")))
    (bindings--define-key menu [cut]
      '(menu-item "剪切" kill-region
                  :enable (and mark-active (not buffer-read-only))
                  :help
                  "Cut (kill) text in region between mark and current position"))
    ;; ns-win.el said: Separate undo from cut/paste section.
    (if (featurep 'ns)
        (bindings--define-key menu [separator-undo] menu-bar-separator))

    (bindings--define-key menu [undo]
      '(menu-item "撤消" undo
                  :enable (and (not buffer-read-only)
                               (not (eq t buffer-undo-list))
                               (if (eq last-command 'undo)
                                   (listp pending-undo-list)
                                 (consp buffer-undo-list)))
                  :help "Undo last operation"))

    menu))

(defun menu-bar-next-tag-other-window ()
  "Find the next definition of the tag already specified."
  (interactive)
  (find-tag-other-window nil t))

(defun menu-bar-next-tag ()
  "Find the next definition of the tag already specified."
  (interactive)
  (find-tag nil t))

(define-obsolete-function-alias
  'menu-bar-kill-ring-save 'kill-ring-save "24.1")

;; These are alternative definitions for the cut, paste and copy
;; menu items.  Use them if your system expects these to use the clipboard.

(put 'clipboard-kill-region 'menu-enable
     '(and mark-active (not buffer-read-only)))
(put 'clipboard-kill-ring-save 'menu-enable 'mark-active)
(put 'clipboard-yank 'menu-enable
     '(and (or (not (fboundp 'x-selection-exists-p))
	       (x-selection-exists-p)
	       (x-selection-exists-p 'CLIPBOARD))
 	   (not buffer-read-only)))

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

(defvar menu-bar-custom-menu
  (let ((menu (make-sparse-keymap "自定義")))

    (bindings--define-key menu [customize-apropos-faces]
      '(menu-item "符合的外觀..." customize-apropos-faces
                  :help "Browse faces matching a regexp or word list"))
    (bindings--define-key menu [customize-apropos-options]
      '(menu-item "符合的選項..." customize-apropos-options
                  :help "Browse options matching a regexp or word list"))
    (bindings--define-key menu [customize-apropos]
      '(menu-item "符合條件的全部設置..." customize-apropos
                  :help "Browse customizable settings matching a regexp or word list"))
    (bindings--define-key menu [separator-1]
      menu-bar-separator)
    (bindings--define-key menu [customize-group]
      '(menu-item "指定分組..." customize-group
                  :help "Customize settings of specific group"))
    (bindings--define-key menu [customize-face]
      '(menu-item "指定外觀..." customize-face
                  :help "Customize attributes of specific face"))
    (bindings--define-key menu [customize-option]
      '(menu-item "指定選項..." customize-option
                  :help "Customize value of specific option"))
    (bindings--define-key menu [separator-2]
      menu-bar-separator)
    (bindings--define-key menu [customize-changed-options]
      '(menu-item "新建選項..." customize-changed-options
                  :help "Options added or changed in recent Emacs versions"))
    (bindings--define-key menu [customize-saved]
      '(menu-item "已保存的選項" customize-saved
                  :help "Customize previously saved options"))
    (bindings--define-key menu [separator-3]
      menu-bar-separator)
    (bindings--define-key menu [customize-browse]
      '(menu-item "瀏覽自定義分組" customize-browse
                  :help "Browse all customization groups"))
    (bindings--define-key menu [customize]
      '(menu-item "頂級自定義分組" customize
                  :help "The master group called `Emacs'"))
    (bindings--define-key menu [customize-themes]
      '(menu-item "自定義主題" customize-themes
                  :help "Choose a pre-defined customization theme"))
    menu))
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

;; Function for setting/saving default font.

(defun menu-set-font ()
  "Interactively select a font and make it the default."
  (interactive)
  (set-frame-font (if (fboundp 'x-select-font)
		      (x-select-font)
		    (mouse-select-font))
		  nil t))

(defun menu-bar-options-save ()
  "Save current values of Options menu items using Custom."
  (interactive)
  (let ((need-save nil))
    ;; These are set with menu-bar-make-mm-toggle, which does not
    ;; put on a customized-value property.
    (dolist (elt '(line-number-mode column-number-mode size-indication-mode
		   cua-mode show-paren-mode transient-mark-mode
		   blink-cursor-mode display-time-mode display-battery-mode
		   ;; These are set by other functions that don't set
		   ;; the customized state.  Having them here has the
		   ;; side-effect that turning them off via X
		   ;; resources acts like having customized them, but
		   ;; that seems harmless.
		   menu-bar-mode tool-bar-mode))
      ;; FIXME ? It's a little annoying that running this command
      ;; always loads cua-base, paren, time, and battery, even if they
      ;; have not been customized in any way.  (Due to custom-load-symbol.)
      (and (customize-mark-to-save elt)
	   (setq need-save t)))
    ;; These are set with `customize-set-variable'.
    (dolist (elt '(scroll-bar-mode
		   debug-on-quit debug-on-error
		   ;; Somehow this works, when tool-bar and menu-bar don't.
		   tooltip-mode
		   save-place uniquify-buffer-name-style fringe-mode
		   indicate-empty-lines indicate-buffer-boundaries
		   case-fold-search font-use-system-font
		   current-language-environment default-input-method
		   ;; Saving `text-mode-hook' is somewhat questionable,
		   ;; as we might get more than we bargain for, if
		   ;; other code may has added hooks as well.
		   ;; Nonetheless, not saving it would like be confuse
		   ;; more often.
		   ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
		   text-mode-hook tool-bar-position))
      (and (get elt 'customized-value)
	   (customize-mark-to-save elt)
	   (setq need-save t)))
    (when (get 'default 'customized-face)
      (put 'default 'saved-face (get 'default 'customized-face))
      (put 'default 'customized-face nil)
      (setq need-save t))
    ;; Save if we changed anything.
    (when need-save
      (custom-save-all))))

;;; Assemble all the top-level items of the "Options" menu

;; The "Show/Hide" submenu of menu "Options"

(defun menu-bar-showhide-fringe-ind-customize ()
  "Show customization buffer for `indicate-buffer-boundaries'."
  (interactive)
  (customize-variable 'indicate-buffer-boundaries))

(defun menu-bar-showhide-fringe-ind-mixed ()
  "Display top and bottom indicators in opposite fringes, arrows in right."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries
			  '((t . right) (top . left))))

(defun menu-bar-showhide-fringe-ind-box ()
  "Display top and bottom indicators in opposite fringes."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries
			  '((top . left) (bottom . right))))

(defun menu-bar-showhide-fringe-ind-right ()
  "Display buffer boundaries and arrows in the right fringe."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries 'right))

(defun menu-bar-showhide-fringe-ind-left ()
  "Display buffer boundaries and arrows in the left fringe."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries 'left))

(defun menu-bar-showhide-fringe-ind-none ()
  "Do not display any buffer boundary indicators."
  (interactive)
  (customize-set-variable 'indicate-buffer-boundaries nil))

(defvar menu-bar-showhide-fringe-ind-menu
  (let ((menu (make-sparse-keymap "緩沖區包邊")))

    (bindings--define-key menu [customize]
      '(menu-item "其它(自定義) [Other (Customize)]"
                  menu-bar-showhide-fringe-ind-customize
                  :help "Additional choices available through Custom buffer"
                  :visible (display-graphic-p)
                  :button (:radio . (not (member indicate-buffer-boundaries
                                                 '(nil left right
                                                   ((top . left) (bottom . right))
                                                   ((t . right) (top . left))))))))

    (bindings--define-key menu [mixed]
      '(menu-item "反方向-右箭頭 (Opposite, Arrows Right)" menu-bar-showhide-fringe-ind-mixed
                  :help
                  "Show top/bottom indicators in opposite fringes, arrows in right"
                  :visible (display-graphic-p)
                  :button (:radio . (equal indicate-buffer-boundaries
                                           '((t . right) (top . left))))))

    (bindings--define-key menu [box]
      '(menu-item "反方向-無箭頭 (Opposite, No Arrows)" menu-bar-showhide-fringe-ind-box
                  :help "Show top/bottom indicators in opposite fringes, no arrows"
                  :visible (display-graphic-p)
                  :button (:radio . (equal indicate-buffer-boundaries
                                           '((top . left) (bottom . right))))))

    (bindings--define-key menu [right]
      '(menu-item "在右側包邊內 (In Right Fringe)" menu-bar-showhide-fringe-ind-right
                  :help "Show buffer boundaries and arrows in right fringe"
                  :visible (display-graphic-p)
                  :button (:radio . (eq indicate-buffer-boundaries 'right))))

    (bindings--define-key menu [left]
      '(menu-item "在左側包邊內 (In Left Fringe)" menu-bar-showhide-fringe-ind-left
                  :help "Show buffer boundaries and arrows in left fringe"
                  :visible (display-graphic-p)
                  :button (:radio . (eq indicate-buffer-boundaries 'left))))

    (bindings--define-key menu [none]
      '(menu-item "不提示 (No Indicators)" menu-bar-showhide-fringe-ind-none
                  :help "Hide all buffer boundary indicators and arrows"
                  :visible (display-graphic-p)
                  :button (:radio . (eq indicate-buffer-boundaries nil))))
    menu))

(defun menu-bar-showhide-fringe-menu-customize ()
  "Show customization buffer for `fringe-mode'."
  (interactive)
  (customize-variable 'fringe-mode))

(defun menu-bar-showhide-fringe-menu-customize-reset ()
  "Reset the fringe mode: display fringes on both sides of a window."
  (interactive)
  (customize-set-variable 'fringe-mode nil))

(defun menu-bar-showhide-fringe-menu-customize-right ()
  "Display fringes only on the right of each window."
  (interactive)
  (require 'fringe)
  (customize-set-variable 'fringe-mode '(0 . nil)))

(defun menu-bar-showhide-fringe-menu-customize-left ()
  "Display fringes only on the left of each window."
  (interactive)
  (require 'fringe)
  (customize-set-variable 'fringe-mode '(nil . 0)))

(defun menu-bar-showhide-fringe-menu-customize-disable ()
  "Do not display window fringes."
  (interactive)
  (require 'fringe)
  (customize-set-variable 'fringe-mode 0))

(defvar menu-bar-showhide-fringe-menu
  (let ((menu (make-sparse-keymap "包邊")))

    (bindings--define-key menu [showhide-fringe-ind]
      `(menu-item "緩沖區邊界" ,menu-bar-showhide-fringe-ind-menu
                  :visible (display-graphic-p)
                  :help "Indicate buffer boundaries in fringe"))

    (bindings--define-key menu [indicate-empty-lines]
      (menu-bar-make-toggle toggle-indicate-empty-lines indicate-empty-lines
                            "指示空行"
                            "提示空行 %s"
                            "Indicate trailing empty lines in fringe, globally"))

    (bindings--define-key menu [customize]
      '(menu-item "自定義包邊" menu-bar-showhide-fringe-menu-customize
                  :help "Detailed customization of fringe"
                  :visible (display-graphic-p)))

    (bindings--define-key menu [default]
      '(menu-item "默認值 (Default)" menu-bar-showhide-fringe-menu-customize-reset
                  :help "Default width fringe on both left and right side"
                  :visible (display-graphic-p)
                  :button (:radio . (eq fringe-mode nil))))

    (bindings--define-key menu [right]
      '(menu-item "在右側 (On the Right)" menu-bar-showhide-fringe-menu-customize-right
                  :help "Fringe only on the right side"
                  :visible (display-graphic-p)
                  :button (:radio . (equal fringe-mode '(0 . nil)))))

    (bindings--define-key menu [left]
      '(menu-item "在左側 (On the Left)" menu-bar-showhide-fringe-menu-customize-left
                  :help "Fringe only on the left side"
                  :visible (display-graphic-p)
                  :button (:radio . (equal fringe-mode '(nil . 0)))))

    (bindings--define-key menu [none]
      '(menu-item "無 (None)" menu-bar-showhide-fringe-menu-customize-disable
                  :help "關閉包邊 (Turn off fringe)"
                  :visible (display-graphic-p)
                  :button (:radio . (eq fringe-mode 0))))
    menu))

(defun menu-bar-right-scroll-bar ()
  "Display scroll bars on the right of each window."
  (interactive)
  (customize-set-variable 'scroll-bar-mode 'right))

(defun menu-bar-left-scroll-bar ()
  "Display scroll bars on the left of each window."
  (interactive)
  (customize-set-variable 'scroll-bar-mode 'left))

(defun menu-bar-no-scroll-bar ()
  "關閉滑塊。"
  (interactive)
  (customize-set-variable 'scroll-bar-mode nil))

(defvar menu-bar-showhide-scroll-bar-menu
  (let ((menu (make-sparse-keymap "滑塊")))

    (bindings--define-key menu [right]
      '(menu-item "在右側 (On the Right)"
                  menu-bar-right-scroll-bar
                  :help "Scroll-bar on the right side"
                  :visible (display-graphic-p)
                  :button (:radio . (eq (cdr (assq 'vertical-scroll-bars
                                                   (frame-parameters))) 'right))))

    (bindings--define-key menu [left]
      '(menu-item "在左側 (On the Left)"
                  menu-bar-left-scroll-bar
                  :help "Scroll-bar on the left side"
                  :visible (display-graphic-p)
                  :button (:radio . (eq (cdr (assq 'vertical-scroll-bars
                                                   (frame-parameters))) 'left))))

    (bindings--define-key menu [none]
      '(menu-item "無 (None)"
                  menu-bar-no-scroll-bar
                  :help "關閉滑塊 (Turn off scroll-bar)"
                  :visible (display-graphic-p)
                  :button (:radio . (eq (cdr (assq 'vertical-scroll-bars
                                                   (frame-parameters))) nil))))
    menu))

(defun menu-bar-frame-for-menubar ()
  "Return the frame suitable for updating the menu bar."
  (or (and (framep menu-updating-frame)
	   menu-updating-frame)
      (selected-frame)))

(defun menu-bar-positive-p (val)
  "Return non-nil iff VAL is a positive number."
  (and (numberp val)
       (> val 0)))

(defun menu-bar-set-tool-bar-position (position)
  (customize-set-variable 'tool-bar-mode t)
  (customize-set-variable 'tool-bar-position position))
(defun menu-bar-showhide-tool-bar-menu-customize-disable ()
  "不關閉工具列。"
  (interactive)
  (customize-set-variable 'tool-bar-mode nil))
(defun menu-bar-showhide-tool-bar-menu-customize-enable-left ()
  "Display tool bars on the left side."
  (interactive)
  (menu-bar-set-tool-bar-position 'left))
(defun menu-bar-showhide-tool-bar-menu-customize-enable-right ()
  "Display tool bars on the right side."
  (interactive)
  (menu-bar-set-tool-bar-position 'right))
(defun menu-bar-showhide-tool-bar-menu-customize-enable-top ()
  "Display tool bars on the top side."
  (interactive)
  (menu-bar-set-tool-bar-position 'top))
(defun menu-bar-showhide-tool-bar-menu-customize-enable-bottom ()
  "Display tool bars on the bottom side."
  (interactive)
  (menu-bar-set-tool-bar-position 'bottom))

(when (featurep 'move-toolbar)
  (defvar menu-bar-showhide-tool-bar-menu
    (let ((menu (make-sparse-keymap "工具列")))

      (bindings--define-key menu [showhide-tool-bar-left]
        '(menu-item "在左側 (On the Left)"
                    menu-bar-showhide-tool-bar-menu-customize-enable-left
                    :help "Tool-bar at the left side"
                    :visible (display-graphic-p)
                    :button
                    (:radio . (and tool-bar-mode
                                   (eq (frame-parameter
                                        (menu-bar-frame-for-menubar)
                                        'tool-bar-position)
                                       'left)))))

      (bindings--define-key menu [showhide-tool-bar-right]
        '(menu-item "在右側 (On the Right)"
                    menu-bar-showhide-tool-bar-menu-customize-enable-right
                    :help "Tool-bar at the right side"
                    :visible (display-graphic-p)
                    :button
                    (:radio . (and tool-bar-mode
                                   (eq (frame-parameter
                                        (menu-bar-frame-for-menubar)
                                        'tool-bar-position)
                                       'right)))))

      (bindings--define-key menu [showhide-tool-bar-bottom]
        '(menu-item "在底部"
                    menu-bar-showhide-tool-bar-menu-customize-enable-bottom
                    :help "Tool-bar at the bottom"
                    :visible (display-graphic-p)
                    :button
                    (:radio . (and tool-bar-mode
                                   (eq (frame-parameter
                                        (menu-bar-frame-for-menubar)
                                        'tool-bar-position)
                                       'bottom)))))

      (bindings--define-key menu [showhide-tool-bar-top]
        '(menu-item "在頂部"
                    menu-bar-showhide-tool-bar-menu-customize-enable-top
                    :help "Tool-bar at the top"
                    :visible (display-graphic-p)
                    :button
                    (:radio . (and tool-bar-mode
                                   (eq (frame-parameter
                                        (menu-bar-frame-for-menubar)
                                        'tool-bar-position)
                                       'top)))))

      (bindings--define-key menu [showhide-tool-bar-none]
        '(menu-item "無 (None)"
                    menu-bar-showhide-tool-bar-menu-customize-disable
                    :help "Turn tool-bar off"
                    :visible (display-graphic-p)
                    :button (:radio . (eq tool-bar-mode nil))))
      menu)))

(defvar menu-bar-showhide-menu
  (let ((menu (make-sparse-keymap "顯示/隱藏")))

    (bindings--define-key menu [column-number-mode]
      (menu-bar-make-mm-toggle column-number-mode
                               "列號"
                               "Show the current column number in the mode line"))

    (bindings--define-key menu [line-number-mode]
      (menu-bar-make-mm-toggle line-number-mode
                               "行號"
                               "Show the current line number in the mode line"))

    (bindings--define-key menu [size-indication-mode]
      (menu-bar-make-mm-toggle size-indication-mode
                               "顯示檔案大小"
                               "Show the size of the buffer in the mode line"))

    (bindings--define-key menu [linecolumn-separator]
      menu-bar-separator)

    (bindings--define-key menu [showhide-battery]
      (menu-bar-make-mm-toggle display-battery-mode
                               "電池狀態"
                               "Display battery status information in mode line"))

    (bindings--define-key menu [showhide-date-time]
      (menu-bar-make-mm-toggle display-time-mode
                               "時間-負載-郵件"
                               "Display time, system load averages and \
mail status in mode line"))

    (bindings--define-key menu [datetime-separator]
      menu-bar-separator)

    (bindings--define-key menu [showhide-speedbar]
      '(menu-item "快捷列" speedbar-frame-mode
                  :help "Display a Speedbar quick-navigation frame"
                  :button (:toggle
                           . (and (boundp 'speedbar-frame)
                                  (frame-live-p (symbol-value 'speedbar-frame))
                                  (frame-visible-p
                                   (symbol-value 'speedbar-frame))))))

    (bindings--define-key menu [showhide-fringe]
      `(menu-item "包邊" ,menu-bar-showhide-fringe-menu
                  :visible (display-graphic-p)))

    (bindings--define-key menu [showhide-scroll-bar]
      `(menu-item "滑塊" ,menu-bar-showhide-scroll-bar-menu
                  :visible (display-graphic-p)))

    (bindings--define-key menu [showhide-tooltip-mode]
      '(menu-item "工具列提示" tooltip-mode
                  :help "Turn tooltips on/off"
                  :visible (and (display-graphic-p) (fboundp 'x-show-tip))
                  :button (:toggle . tooltip-mode)))

    (bindings--define-key menu [menu-bar-mode]
      '(menu-item "菜單列" toggle-menu-bar-mode-from-frame
                  :help "打開/關閉菜單列"
                  :button
                  (:toggle . (menu-bar-positive-p
                              (frame-parameter (menu-bar-frame-for-menubar)
                                               'menu-bar-lines)))))

    (if (and (boundp 'menu-bar-showhide-tool-bar-menu)
             (keymapp menu-bar-showhide-tool-bar-menu))
        (bindings--define-key menu [showhide-tool-bar]
          `(menu-item "工具列" ,menu-bar-showhide-tool-bar-menu
                      :visible (display-graphic-p)))
      ;; else not tool bar that can move.
      (bindings--define-key menu [showhide-tool-bar]
        '(menu-item "工具列" toggle-tool-bar-mode-from-frame
                    :help "打開/關閉工具列"
                    :visible (display-graphic-p)
                    :button
                    (:toggle . (menu-bar-positive-p
                                (frame-parameter (menu-bar-frame-for-menubar)
                                                 'tool-bar-lines))))))
    menu))

(defun menu-bar-text-mode-auto-fill ()
  (interactive)
  (toggle-text-mode-auto-fill)
  ;; This is somewhat questionable, as `text-mode-hook'
  ;; might have changed outside customize.
  ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
  (customize-mark-as-set 'text-mode-hook))

(defvar menu-bar-line-wrapping-menu
  (let ((menu (make-sparse-keymap "換行")))

    (bindings--define-key menu [word-wrap]
      `(menu-item "單詞換行(可見行模式) [Word Wrap (Visual Line mode)]"
                  ,(lambda ()
                     (interactive)
                     (unless visual-line-mode
                       (visual-line-mode 1))
                     (message "啟用可見行模式 (Visual-Line mode enabled)"))
                  :help "Wrap long lines at word boundaries"
                  :button (:radio
                           . (and (null truncate-lines)
                                  (not (truncated-partial-width-window-p))
                                  word-wrap))
                  :visible (menu-bar-menu-frame-live-and-visible-p)))

    (bindings--define-key menu [truncate]
      `(menu-item "長行換行 (Truncate Long Lines)"
                  ,(lambda ()
                     (interactive)
                     (if visual-line-mode (visual-line-mode 0))
                     (setq word-wrap nil)
                     (toggle-truncate-lines 1))
                  :help "Truncate long lines at window edge"
                  :button (:radio . (or truncate-lines
                                        (truncated-partial-width-window-p)))
                  :visible (menu-bar-menu-frame-live-and-visible-p)
                  :enable (not (truncated-partial-width-window-p))))

    (bindings--define-key menu [window-wrap]
      `(menu-item "在窗格邊緣換行 (Wrap at Window Edge)"
                  ,(lambda () (interactive)
                     (if visual-line-mode (visual-line-mode 0))
                     (setq word-wrap nil)
                     (if truncate-lines (toggle-truncate-lines -1)))
                  :help "Wrap long lines at window edge"
                  :button (:radio
                           . (and (null truncate-lines)
                                  (not (truncated-partial-width-window-p))
                                  (not word-wrap)))
                  :visible (menu-bar-menu-frame-live-and-visible-p)
                  :enable (not (truncated-partial-width-window-p))))
    menu))

(defvar menu-bar-options-menu
  (let ((menu (make-sparse-keymap "選項")))
    (bindings--define-key menu [customize]
      `(menu-item "自定義 Emacs" ,menu-bar-custom-menu))

    (bindings--define-key menu [package]
      '(menu-item "管理 Emacs 軟體包" package-list-packages
        :help "Install or uninstall additional Emacs packages"))

    (bindings--define-key menu [save]
      '(menu-item "保存選項" menu-bar-options-save
                  :help "Save options set from the menu above"))

    (bindings--define-key menu [custom-separator]
      menu-bar-separator)

    (bindings--define-key menu [menu-set-font]
      '(menu-item "設置默認字體..." menu-set-font
                  :visible (display-multi-font-p)
                  :help "Select a default font"))

    (if (featurep 'system-font-setting)
        (bindings--define-key menu [menu-system-font]
          (menu-bar-make-toggle
           toggle-use-system-font font-use-system-font
           "使用系統字體"
           "使用系統字體: %s"
           "Use the monospaced font defined by the system")))

    (bindings--define-key menu [showhide]
      `(menu-item "顯示/隱藏" ,menu-bar-showhide-menu))

    (bindings--define-key menu [showhide-separator]
      menu-bar-separator)

    (bindings--define-key menu [mule]
      ;; It is better not to use backquote here,
      ;; because that makes a bootstrapping problem
      ;; if you need to recompile all the Lisp files using interpreted code.
      `(menu-item "多國語言環境(Mule)" ,mule-menu-keymap
                  ;; Most of the MULE menu actually does make sense in
                  ;; unibyte mode, e.g. language selection.
                  ;; :visible '(default-value 'enable-multibyte-characters)
                  ))
    ;;(setq menu-bar-final-items (cons 'mule menu-bar-final-items))
    ;;(bindings--define-key menu [preferences]
    ;;  `(menu-item "參數" ,menu-bar-preferences-menu
    ;;	      :help "切換重要的全局選項"))

    (bindings--define-key menu [mule-separator]
      menu-bar-separator)

    (bindings--define-key menu [debug-on-quit]
      (menu-bar-make-toggle toggle-debug-on-quit debug-on-quit
                            "退出時 (或按 C-g 鍵) 進入調試器" "退出 %s 時進行調試"
                            "按 C-g 鍵進入 Lisp 調試器"))
    (bindings--define-key menu [debug-on-error]
      (menu-bar-make-toggle toggle-debug-on-error debug-on-error
                            "出現錯誤時進入調試器" "出現 %s 錯誤時進行調試"
                            "出現錯誤信號時進入 Lisp 調試器"))
    (bindings--define-key menu [debugger-separator]
      menu-bar-separator)

    (bindings--define-key menu [blink-cursor-mode]
      (menu-bar-make-mm-toggle
       blink-cursor-mode
       "光標閃爍"
       "Whether the cursor blinks (Blink Cursor mode)"))
    (bindings--define-key menu [cursor-separator]
      menu-bar-separator)

    (bindings--define-key menu [save-place]
      (menu-bar-make-toggle
       toggle-save-place-globally save-place
       "保存會話的檔案位置"
       "保存檔案 %s 的位置"
       "Visit files of previous session when restarting Emacs"
       (require 'saveplace)
       ;; Do it by name, to avoid a free-variable
       ;; warning during byte compilation.
       (set-default
	'save-place (not (symbol-value 'save-place)))))

    (bindings--define-key menu [uniquify]
      (menu-bar-make-toggle
       toggle-uniquify-buffer-names uniquify-buffer-name-style
       "在緩沖區使用目錄名"
       "緩沖區名稱中的目錄名(重名) %s"
       "Uniquify buffer names by adding parent directory names"
       (require 'uniquify)
       (setq uniquify-buffer-name-style
	     (if (not uniquify-buffer-name-style)
		 'forward))))

    (bindings--define-key menu [edit-options-separator]
      menu-bar-separator)
    (bindings--define-key menu [cua-mode]
      (menu-bar-make-mm-toggle
       cua-mode
       "使用 CUA 按鍵(使用 C-x/C-c/C-v 進行剪切/粘貼)"
       "Use C-z/C-x/C-c/C-v keys for undo/cut/copy/paste"
       (:visible (or (not (boundp 'cua-enable-cua-keys))
		     cua-enable-cua-keys))))

    (bindings--define-key menu [cua-emulation-mode]
      (menu-bar-make-mm-toggle
       cua-mode
       "按住 Shift 鍵并移動光標來選定區域(CUA)"
       "Use shifted movement keys to set and extend the region"
       (:visible (and (boundp 'cua-enable-cua-keys)
		      (not cua-enable-cua-keys)))))

    (bindings--define-key menu [case-fold-search]
      (menu-bar-make-toggle
       toggle-case-fold-search case-fold-search
       "搜索時不區分字母大小寫"
       "區分字母大小寫來搜索 %s"
       "Ignore letter-case in search commands"))

    (bindings--define-key menu [auto-fill-mode]
      '(menu-item
 "文本模式時自動調整"
	menu-bar-text-mode-auto-fill
	:help "Automatically fill text while typing (Auto Fill mode)"
	:button (:toggle . (if (listp text-mode-hook)
			       (member 'turn-on-auto-fill text-mode-hook)
			     (eq 'turn-on-auto-fill text-mode-hook)))))

    (bindings--define-key menu [line-wrapping]
      `(menu-item "在當前緩沖區換行"
		  ,menu-bar-line-wrapping-menu))

    (bindings--define-key menu [highlight-separator]
      menu-bar-separator)
    (bindings--define-key menu [highlight-paren-mode]
      (menu-bar-make-mm-toggle
       show-paren-mode
       "高亮配對的括號"
       "Highlight matching/mismatched parentheses at cursor (Show Paren mode)"))
    (bindings--define-key menu [transient-mark-mode]
      (menu-bar-make-mm-toggle
       transient-mark-mode
       "高亮選區"
       "Make text in active region stand out in color (Transient Mark mode)"
       (:enable (not cua-mode))))
    menu))


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

(defvar menu-bar-games-menu
  (let ((menu (make-sparse-keymap "游戲")))

    (bindings--define-key menu [zone]
      '(menu-item "花屏(zone)" zone
                  :help "Play tricks with Emacs display when Emacs is idle"))
    (bindings--define-key menu [tetris]
      '(menu-item "堆積木(tetris)" tetris
                  :help "Falling blocks game"))
    (bindings--define-key menu [solitaire]
      '(menu-item "寶石棋(solitaire)" solitaire
                  :help "Get rid of all the stones"))
    (bindings--define-key menu [snake]
      '(menu-item "貪食蛇(snake)" snake
                  :help "Move snake around avoiding collisions"))
    (bindings--define-key menu [pong]
      '(menu-item "打磚塊(pong)" pong
                  :help "Bounce the ball to your opponent"))
    (bindings--define-key menu [mult]
      '(menu-item "乘法謎(mpuz)"  mpuz
                  :help "Exercise brain with multiplication"))
    (bindings--define-key menu [life]
      '(menu-item "生命(life)"  life
                  :help "Watch how John Conway's cellular automaton evolves"))
    (bindings--define-key menu [land]
      '(menu-item "地標(landmark)" landmark
                  :help "Watch a neural-network robot learn landmarks"))
    (bindings--define-key menu [hanoi]
      '(menu-item "漢諾塔(hanoi)" hanoi
                  :help "Watch Towers-of-Hanoi puzzle solved by Emacs"))
    (bindings--define-key menu [gomoku]
      '(menu-item "五子棋(gomoku)"  gomoku
                  :help "Mark 5 contiguous squares (like tic-tac-toe)"))
    (bindings--define-key menu [bubbles]
      '(menu-item "同色棋(bubbles)" bubbles
                  :help "Remove all bubbles using the fewest moves"))
    (bindings--define-key menu [black-box]
      '(menu-item "黑箱子(blackbox)"  blackbox
                  :help "Find balls in a black box by shooting rays"))
    (bindings--define-key menu [adventure]
      '(menu-item "冒險(dunnet)"  dunnet
                  :help "Dunnet, a text Adventure game for Emacs"))
    (bindings--define-key menu [5x5]
      '(menu-item "5x5" 5x5
                  :help "Fill in all the squares on a 5x5 board"))
    menu))

(defvar menu-bar-encryption-decryption-menu
  (let ((menu (make-sparse-keymap "加密/解密")))
    (bindings--define-key menu [insert-keys]
      '(menu-item "插入密鑰" epa-insert-keys
                  :help "Insert public keys after the current point"))

    (bindings--define-key menu [export-keys]
      '(menu-item "匯出密鑰" epa-export-keys
                  :help "Export public keys to a file"))

    (bindings--define-key menu [import-keys-region]
      '(menu-item "從區域導入密鑰" epa-import-keys-region
                  :help "Import public keys from the current region"))

    (bindings--define-key menu [import-keys]
      '(menu-item "從檔案導入密鑰..." epa-import-keys
                  :help "Import public keys from a file"))

    (bindings--define-key menu [list-keys]
      '(menu-item "列出密鑰" epa-list-keys
                  :help "Browse your public keyring"))

    (bindings--define-key menu [separator-keys]
      menu-bar-separator)

    (bindings--define-key menu [sign-region]
      '(menu-item "對區域進行簽名" epa-sign-region
                  :help "Create digital signature of the current region"))

    (bindings--define-key menu [verify-region]
      '(menu-item "驗証區域" epa-verify-region
                  :help "Verify digital signature of the current region"))

    (bindings--define-key menu [encrypt-region]
      '(menu-item "加密區域" epa-encrypt-region
                  :help "Encrypt the current region"))

    (bindings--define-key menu [decrypt-region]
      '(menu-item "解密區域" epa-decrypt-region
                  :help "Decrypt the current region"))

    (bindings--define-key menu [separator-file]
      menu-bar-separator)

    (bindings--define-key menu [sign-file]
      '(menu-item "對檔案進行簽名..." epa-sign-file
                  :help "Create digital signature of a file"))

    (bindings--define-key menu [verify-file]
      '(menu-item "驗証檔案..." epa-verify-file
                  :help "Verify digital signature of a file"))

    (bindings--define-key menu [encrypt-file]
      '(menu-item "加密檔案..." epa-encrypt-file
                  :help "Encrypt a file"))

    (bindings--define-key menu [decrypt-file]
      '(menu-item "解密檔案..." epa-decrypt-file
                  :help "Decrypt a file"))

    menu))

(defun menu-bar-read-mail ()
  "Read mail using `read-mail-command'."
  (interactive)
  (call-interactively read-mail-command))

(defvar menu-bar-tools-menu
  (let ((menu (make-sparse-keymap "工具")))

    (bindings--define-key menu [games]
      `(menu-item "游戲" ,menu-bar-games-menu))

    (bindings--define-key menu [separator-games]
      menu-bar-separator)

    (bindings--define-key menu [encryption-decryption]
      `(menu-item "加密/解密"
                  ,menu-bar-encryption-decryption-menu))

    (bindings--define-key menu [separator-encryption-decryption]
      menu-bar-separator)

    (bindings--define-key menu [simple-calculator]
      '(menu-item "簡單計算器(calculator)" calculator
                  :help "Invoke the Emacs built-in quick calculator"))
    (bindings--define-key menu [calc]
      '(menu-item "高級計算器(calc)" calc
                  :help "Invoke the Emacs built-in full scientific calculator"))
    (bindings--define-key menu [calendar]
      '(menu-item "日歷(calendar)" calendar
                  :help "Invoke the Emacs built-in calendar"))

    (bindings--define-key menu [separator-net]
      menu-bar-separator)

    (bindings--define-key menu [directory-search]
      '(menu-item "目錄搜索" eudc-tools-menu))
    (bindings--define-key menu [compose-mail]
      '(menu-item (format "發送電郵(用 %s)" (send-mail-item-name)) compose-mail
                  :visible (and mail-user-agent (not (eq mail-user-agent 'ignore)))
                  :help "發送電郵迅息"))
    (bindings--define-key menu [rmail]
      '(menu-item (format "查看電郵(用 %s)" (read-mail-item-name))
                  menu-bar-read-mail
                  :visible (and read-mail-command
                                (not (eq read-mail-command 'ignore)))
                  :help "Read your mail and reply to it"))

    (bindings--define-key menu [gnus]
      '(menu-item "查看網絡新聞(Gnus)" gnus
                  :help "Read network news groups"))

    (bindings--define-key menu [separator-vc]
      menu-bar-separator)

    (bindings--define-key menu [vc] nil) ;Create the place for the VC menu.

    (bindings--define-key menu [separator-compare]
      menu-bar-separator)

    (bindings--define-key menu [epatch]
      '(menu-item "應用補丁" menu-bar-epatch-menu))
    (bindings--define-key menu [ediff-merge]
      '(menu-item "合并" menu-bar-ediff-merge-menu))
    (bindings--define-key menu [compare]
      '(menu-item "比對(Ediff)" menu-bar-ediff-menu))

    (bindings--define-key menu [separator-spell]
      menu-bar-separator)

    (bindings--define-key menu [spell]
      '(menu-item "拼寫檢查" ispell-menu-map))

    (bindings--define-key menu [separator-prog]
      menu-bar-separator)

    (bindings--define-key menu [semantic]
      '(menu-item "源碼解析器(語法)"
                  semantic-mode
                  :help "Toggle automatic parsing in source code buffers (Semantic mode)"
                  :button (:toggle . (bound-and-true-p semantic-mode))))

    (bindings--define-key menu [ede]
      '(menu-item "項目支持(EDE)"
                  global-ede-mode
                  :help "Toggle the Emacs Development Environment (Global EDE mode)"
                  :button (:toggle . (bound-and-true-p global-ede-mode))))

    (bindings--define-key menu [gdb]
      '(menu-item "調試器(GDB)..." gdb
                  :help "Debug a program from within Emacs with GDB"))
    (bindings--define-key menu [shell-on-region]
      '(menu-item "選區內的 Shell 命令..." shell-command-on-region
                  :enable mark-active
                  :help "Pass marked region to a shell command"))
    (bindings--define-key menu [shell]
      '(menu-item "Shell 命令..." shell-command
                  :help "Invoke a shell command and catch its output"))
    (bindings--define-key menu [compile]
      '(menu-item "編譯..." compile
                  :help "Invoke compiler or Make, view compilation errors"))
    (bindings--define-key menu [grep]
      '(menu-item "搜索檔案(Grep)..." grep
                  :help "Search files for strings or regexps (with Grep)"))
    menu))

;; The "Help" menu items

(defvar menu-bar-describe-menu
  (let ((menu (make-sparse-keymap "說明")))

    (bindings--define-key menu [mule-diag]
      '(menu-item "顯示全部多國語言(Mule)狀態" mule-diag
                  :visible (default-value 'enable-multibyte-characters)
                  :help "Display multilingual environment settings"))
    (bindings--define-key menu [describe-coding-system-briefly]
      '(menu-item "文字編碼說明(簡述)"
                  describe-current-coding-system-briefly
                  :visible (default-value 'enable-multibyte-characters)))
    (bindings--define-key menu [describe-coding-system]
      '(menu-item "文字編碼說明..." describe-coding-system
                  :visible (default-value 'enable-multibyte-characters)))
    (bindings--define-key menu [describe-input-method]
      '(menu-item "輸入法說明..." describe-input-method
                  :visible (default-value 'enable-multibyte-characters)
                  :help "Keyboard layout for specific input method"))
    (bindings--define-key menu [describe-language-environment]
      `(menu-item "語言環境說明"
                  ,describe-language-environment-map))

    (bindings--define-key menu [separator-desc-mule]
      menu-bar-separator)

    (bindings--define-key menu [list-keybindings]
      '(menu-item "列出熱鍵綁定" describe-bindings
                  :help "Display all current key bindings (keyboard shortcuts)"))
    (bindings--define-key menu [describe-current-display-table]
      '(menu-item "顯示表說明" describe-current-display-table
                  :help "Describe the current display table"))
    (bindings--define-key menu [describe-package]
      '(menu-item "軟體包說明..." describe-package
                  :help "Display documentation of a Lisp package"))
    (bindings--define-key menu [describe-face]
      '(menu-item "外觀說明..." describe-face
                  :help "Display the properties of a face"))
    (bindings--define-key menu [describe-variable]
      '(menu-item "變量說明..." describe-variable
                  :help "Display documentation of variable/option"))
    (bindings--define-key menu [describe-function]
      '(menu-item "函數說明..." describe-function
                  :help "Display documentation of function/command"))
    (bindings--define-key menu [describe-key-1]
      '(menu-item "按鍵和滑鼠操作說明..." describe-key
                  ;; Users typically don't identify keys and menu items...
                  :help "Display documentation of command bound to a \
key, a click, or a menu-item"))
    (bindings--define-key menu [describe-mode]
      '(menu-item "緩沖區模式說明" describe-mode
                  :help "Describe this buffer's major and minor mode"))
    menu))

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

(defvar menu-bar-search-documentation-menu
  (let ((menu (make-sparse-keymap "搜索文檔")))

    (bindings--define-key menu [search-documentation-strings]
      '(menu-item "搜索文檔字符串..." apropos-documentation
                  :help
                  "Find functions and variables whose doc strings match a regexp"))
    (bindings--define-key menu [find-any-object-by-name]
      '(menu-item "按名稱查找某個主題" apropos
                  :help "Find symbols of any kind whose names match a regexp"))
    (bindings--define-key menu [find-option-by-value]
      '(menu-item "按值查找選項..." apropos-value
                  :help "Find variables whose values match a regexp"))
    (bindings--define-key menu [find-options-by-name]
      '(menu-item "按名稱查找選項..." apropos-variable
                  :help "Find variables whose names match a regexp"))
    (bindings--define-key menu [find-commands-by-name]
      '(menu-item "按名稱查找指令..." apropos-command
                  :help "Find commands whose names match a regexp"))
    (bindings--define-key menu [sep1]
      menu-bar-separator)
    (bindings--define-key menu [lookup-command-in-manual]
      '(menu-item "在用戶手冊中尋找指令..." Info-goto-emacs-command-node
                  :help "Display manual section that describes a command"))
    (bindings--define-key menu [lookup-key-in-manual]
      '(menu-item "在用戶手冊中尋找關鍵詞..." Info-goto-emacs-key-command-node
                  :help "Display manual section that describes a key"))
    (bindings--define-key menu [lookup-subject-in-elisp-manual]
      '(menu-item "在 ELisp 手冊中尋找主題..." elisp-index-search
                  :help "Find description of a subject in Emacs Lisp manual"))
    (bindings--define-key menu [lookup-subject-in-emacs-manual]
      '(menu-item "在用戶手冊中尋找主題..." emacs-index-search
                  :help "Find description of a subject in Emacs User manual"))
    (bindings--define-key menu [emacs-terminology]
      '(menu-item "Emacs 術語" search-emacs-glossary
                  :help "Display the Glossary section of the Emacs manual"))
    menu))

(defvar menu-bar-manuals-menu
  (let ((menu (make-sparse-keymap "更多手冊")))

    (bindings--define-key menu [man]
      '(menu-item "查找 Man 手冊頁..." manual-entry
                  :help "Man-page docs for external commands and libraries"))
    (bindings--define-key menu [sep2]
      menu-bar-separator)
    (bindings--define-key menu [order-emacs-manuals]
      '(menu-item "訂購手冊" view-order-manuals
                  :help "How to order manuals from the Free Software Foundation"))
    (bindings--define-key menu [lookup-subject-in-all-manuals]
      '(menu-item "在全部手冊中尋找主題..." info-apropos
                  :help "Find description of a subject in all installed manuals"))
    (bindings--define-key menu [other-manuals]
      '(menu-item "其它全部 Info 手冊" Info-directory
                  :help "Read any of the installed manuals"))
    (bindings--define-key menu [emacs-lisp-reference]
      '(menu-item "Emacs Lisp 參考手冊" menu-bar-read-lispref
                  :help "Read the Emacs Lisp Reference manual"))
    (bindings--define-key menu [emacs-lisp-intro]
      '(menu-item "Emacs Lisp 介紹" menu-bar-read-lispintro
                  :help "Read the Introduction to Emacs Lisp Programming"))
    menu))

(defun menu-bar-help-extra-packages ()
  "Display help about some additional packages available for Emacs."
  (interactive)
  (let (enable-local-variables)
    (view-file (expand-file-name "MORE.STUFF"
				 data-directory))
    (goto-address-mode 1)))

(defun help-with-tutorial-spec-language ()
  "Use the Emacs tutorial, specifying which language you want."
  (interactive)
  (help-with-tutorial t))

(defvar menu-bar-help-menu
  (let ((menu (make-sparse-keymap "幫助")))
    (bindings--define-key menu [about-gnu-project]
      '(menu-item "關于 GNU" describe-gnu-project
                  :help "About the GNU System, GNU Project, and GNU/Linux"))
    (bindings--define-key menu [about-emacs]
      '(menu-item "關于 Emacs" about-emacs
                  :help "Display version number, copyright info, and basic help"))
    (bindings--define-key menu [sep4]
      menu-bar-separator)
    (bindings--define-key menu [describe-no-warranty]
      '(menu-item "免責聲明" describe-no-warranty
                  :help "Explain that Emacs has NO WARRANTY"))
    (bindings--define-key menu [describe-copying]
      '(menu-item "版權條款" describe-copying
                  :help "Show the Emacs license (GPL)"))
    (bindings--define-key menu [getting-new-versions]
      '(menu-item "獲取新版本" describe-distribution
                  :help "How to get the latest version of Emacs"))
    (bindings--define-key menu [sep2]
      menu-bar-separator)
    (bindings--define-key menu [external-packages]
      '(menu-item "查找額外的軟體包" menu-bar-help-extra-packages
                  :help "Lisp packages distributed separately for use in Emacs"))
    (bindings--define-key menu [find-emacs-packages]
      '(menu-item "搜索集成的軟體包" finder-by-keyword
                  :help "Find built-in packages and features by keyword"))
    (bindings--define-key menu [more-manuals]
      `(menu-item "更多手冊" ,menu-bar-manuals-menu))
    (bindings--define-key menu [emacs-manual]
      '(menu-item "查看 Emacs 手冊" info-emacs-manual
                  :help "Full documentation of Emacs features"))
    (bindings--define-key menu [describe]
      `(menu-item "說明" ,menu-bar-describe-menu))
    (bindings--define-key menu [search-documentation]
      `(menu-item "搜索文檔" ,menu-bar-search-documentation-menu))
    (bindings--define-key menu [sep1]
      menu-bar-separator)
    (bindings--define-key menu [emacs-psychotherapist]
      '(menu-item "Emacs 心理醫生(doctor)" doctor
                  :help "Our doctor will help you feel better"))
    (bindings--define-key menu [send-emacs-bug-report]
      '(menu-item "發送缺陷報告..." report-emacs-bug
                  :help "Send e-mail to Emacs maintainers"))
    (bindings--define-key menu [emacs-manual-bug]
      '(menu-item "How to Report a Bug" info-emacs-bug
                  :help "Read about how to report an Emacs bug"))
    (bindings--define-key menu [emacs-known-problems]
      '(menu-item "Emacs 已知問題" view-emacs-problems
                  :help "Read about known problems with Emacs"))
    (bindings--define-key menu [emacs-news]
      '(menu-item "Emacs 新聞" view-emacs-news
                  :help "New features of this version"))
    (bindings--define-key menu [emacs-faq]
      '(menu-item "Emacs 常見問題 (FAQ)" view-emacs-FAQ
                  :help "Frequently asked (and answered) questions about Emacs"))

    (bindings--define-key menu [emacs-tutorial-language-specific]
      '(menu-item "Emacs 教程(選擇語言)..."
                  help-with-tutorial-spec-language
                  :help "Learn how to use Emacs (choose a language)"))
    (bindings--define-key menu [emacs-tutorial]
      '(menu-item "Emacs 教程" help-with-tutorial
                  :help "Learn how to use Emacs"))

    ;; In OS X it's in the app menu already.
    ;; FIXME? There already is an "關于 Emacs" (sans ...) entry in the Help menu.
    (and (featurep 'ns)
         (not (eq system-type 'darwin))
         (bindings--define-key menu [info-panel]
           '(menu-item "關于 Emacs..." ns-do-emacs-info-panel)))
    menu))

(bindings--define-key global-map [menu-bar tools]
  (cons "工具" menu-bar-tools-menu))
(bindings--define-key global-map [menu-bar buffer]
  (cons "緩沖區" global-buffers-menu-map))
(bindings--define-key global-map [menu-bar options]
  (cons "選項" menu-bar-options-menu))
(bindings--define-key global-map [menu-bar edit]
  (cons "編輯" menu-bar-edit-menu))
(bindings--define-key global-map [menu-bar file]
  (cons "檔案" menu-bar-file-menu))

;; Put "Help" menu at the end, or Info at the front.
;; If running under GNUstep, "Help" is moved and renamed "Info" (see below).
(if (and (featurep 'ns)
         (not (eq system-type 'darwin)))
    (bindings--define-key global-map [menu-bar help-menu]
      (cons "迅息" menu-bar-help-menu))
  (define-key-after global-map [menu-bar help-menu]
    (cons (purecopy "幫助") menu-bar-help-menu)))

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
  "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'."
  (interactive)
  (cond
   ;; Don't do anything when `menu-frame' is not alive or visible
   ;; (Bug#8184).
   ((not (menu-bar-menu-frame-live-and-visible-p)))
   ((menu-bar-non-minibuffer-window-p)
    (kill-buffer (current-buffer)))
   (t
    (abort-recursive-edit))))

(defun kill-this-buffer-enabled-p ()
  "Return non-nil if the `kill-this-buffer' menu item should be enabled."
  (or (not (menu-bar-non-minibuffer-window-p))
      (let (found-1)
	;; Instead of looping over entire buffer list, stop once we've
	;; found two "killable" buffers (Bug#8184).
	(catch 'found-2
	  (dolist (buffer (buffer-list))
	    (unless (string-match-p "^ " (buffer-name buffer))
	      (if (not found-1)
		  (setq found-1 t)
		(throw 'found-2 t))))))))

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
  "Maximum length to display in the yank-menu."
  :type 'integer
  :group 'menu)

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


;;; Buffers Menu

(defcustom buffers-menu-max-size 10
  "Maximum number of entries which may appear on the Buffers menu.
If this is 10, then only the ten most-recently-selected buffers are shown.
If this is nil, then all buffers are shown.
A large number or nil slows down menu responsiveness."
  :type '(choice integer
		 (const :tag "全部" nil))
  :group 'menu)

(defcustom buffers-menu-buffer-name-length 30
  "Maximum length of the buffer name on the Buffers menu.
If this is a number, then buffer names are truncated to this length.
If this is nil, then buffer names are shown in full.
A large number or nil makes the menu too wide."
  :type '(choice integer
		 (const :tag "Full length" nil))
  :group 'menu)

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

(defvar list-buffers-directory nil
  "String to display in buffer listings for buffers not visiting a file.")
(make-variable-buffer-local 'list-buffers-directory)

(defun menu-bar-select-buffer ()
  (interactive)
  (switch-to-buffer last-command-event))

(defun menu-bar-select-frame (frame)
  (make-frame-visible frame)
  (raise-frame frame)
  (select-frame frame))

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

(defvar menu-bar-select-buffer-function 'switch-to-buffer
  "Function to select the buffer chosen from the `Buffers' menu-bar menu.
It must accept a buffer as its only required argument.")

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
	       (let (alist)
		 ;; Put into each element of buffer-list
		 ;; the name for actual display,
		 ;; perhaps truncated in the middle.
		 (dolist (buf buffers)
		   (let ((name (buffer-name buf)))
                     (unless (eq ?\s (aref name 0))
                       (push (menu-bar-update-buffers-1
                              (cons buf
				    (if (and (integerp buffers-menu-buffer-name-length)
					     (> (length name) buffers-menu-buffer-name-length))
					(concat
					 (substring
					  name 0 (/ buffers-menu-buffer-name-length 2))
					 "..."
					 (substring
					  name (- (/ buffers-menu-buffer-name-length 2))))
				      name)
                                    ))
                             alist))))
		 ;; Now make the actual list of items.
                 (let ((buffers-vec (make-vector (length alist) nil))
                       (i (length alist)))
                   (dolist (pair alist)
                     (setq i (1- i))
                     (aset buffers-vec i
			   (nconc (list (car pair)
					(cons nil nil))
				  `(lambda ()
                                     (interactive)
                                     (funcall menu-bar-select-buffer-function ,(cdr pair))))))
                   (list buffers-vec))))

	 ;; Make a Frames menu if we have more than one frame.
	 (when (cdr frames)
	   (let* ((frames-vec (make-vector (length frames) nil))
                  (frames-menu
                   (cons 'keymap
                         (list "選擇框架" frames-vec)))
                  (i 0))
             (dolist (frame frames)
               (aset frames-vec i
                     (nconc
                      (list
                       (frame-parameter frame 'name)
                       (cons nil nil))
                      `(lambda ()
                         (interactive) (menu-bar-select-frame ,frame))))
               (setq i (1+ i)))
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
;;    (with-current-buffer buffer
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
  (bindings--define-key map [menu-bar minibuf]
    (cons "小緩沖" (make-sparse-keymap "小緩沖"))))

(let ((map minibuffer-local-completion-map))
  (bindings--define-key map [menu-bar minibuf ?\?]
    '(menu-item "列出補齊" minibuffer-completion-help
		:help "Display all possible completions"))
  (bindings--define-key map [menu-bar minibuf space]
    '(menu-item "單詞補齊" minibuffer-complete-word
		:help "Complete at most one word"))
  (bindings--define-key map [menu-bar minibuf tab]
    '(menu-item "補齊" minibuffer-complete
		:help "Complete as far as possible")))

(let ((map minibuffer-local-map))
  (bindings--define-key map [menu-bar minibuf quit]
    '(menu-item "退出" abort-recursive-edit
		:help "Abort input and exit minibuffer"))
  (bindings--define-key map [menu-bar minibuf return]
    '(menu-item "進入" exit-minibuffer
		:key-sequence "\r"
		:help "Terminate input and exit minibuffer"))
  (bindings--define-key map [menu-bar minibuf isearch-forward]
    '(menu-item "漸進式向前搜索歷史" isearch-forward
		:help "Incrementally search minibuffer history forward"))
  (bindings--define-key map [menu-bar minibuf isearch-backward]
    '(menu-item "漸進式後退搜索歷史" isearch-backward
		:help "Incrementally search minibuffer history backward"))
  (bindings--define-key map [menu-bar minibuf next]
    '(menu-item "下一個歷史記錄" next-history-element
		:help "Put next minibuffer history element in the minibuffer"))
  (bindings--define-key map [menu-bar minibuf previous]
    '(menu-item "上一個歷史記錄" previous-history-element
		:help "Put previous minibuffer history element in the minibuffer")))

(define-minor-mode menu-bar-mode
  "Toggle display of a menu bar on each frame (Menu Bar mode).
With a prefix argument ARG, enable Menu Bar mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Menu Bar mode if ARG is omitted or nil.

This command applies to all frames that exist and frames to be
created in the future."
  :init-value t
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable menu-bar-mode

  ;; Turn the menu-bars on all frames on or off.
  (let ((val (if menu-bar-mode 1 0)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'menu-bar-lines val))
    ;; If the user has given `default-frame-alist' a `menu-bar-lines'
    ;; parameter, replace it.
    (if (assq 'menu-bar-lines default-frame-alist)
	(setq default-frame-alist
	      (cons (cons 'menu-bar-lines val)
		    (assq-delete-all 'menu-bar-lines
				     default-frame-alist)))))
  ;; Make the message appear when Emacs is idle.  We can not call message
  ;; directly.  The minor-mode message "Menu-bar mode disabled" comes
  ;; after this function returns, overwriting any message we do here.
  (when (and (called-interactively-p 'interactive) (not menu-bar-mode))
    (run-with-idle-timer 0 nil 'message
			 "Menu-bar mode disabled.  Use M-x menu-bar-mode to make the menu bar appear.")))

;;;###autoload
;; (This does not work right unless it comes after the above definition.)
;; This comment is taken from tool-bar.el near
;; (put 'tool-bar-mode ...)
;; We want to pretend the menu bar by standard is on, as this will make
;; customize consider disabling the menu bar a customization, and save
;; that.  We could do this for real by setting :init-value above, but
;; that would overwrite disabling the menu bar from X resources.
(put 'menu-bar-mode 'standard-value '(t))

(defun toggle-menu-bar-mode-from-frame (&optional arg)
  "Toggle menu bar on or off, based on the status of the current frame.
See `menu-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (menu-bar-mode
       (if (menu-bar-positive-p
	    (frame-parameter (menu-bar-frame-for-menubar) 'menu-bar-lines))
	    0 1))
    (menu-bar-mode arg)))

(declare-function x-menu-bar-open "終端/x-win" (&optional frame))
(declare-function w32-menu-bar-open "終端/w32-win" (&optional frame))

(defun menu-bar-open (&optional frame)
  "Start key navigation of the menu bar in FRAME.

This function decides which method to use to access the menu
depending on FRAME's terminal device.  On X displays, it calls
`x-menu-bar-open'; on Windows, `w32-menu-bar-open' otherwise it
calls `tmm-menubar'.

If FRAME is nil or not given, use the selected frame."
  (interactive)
  (let ((type (framep (or frame (selected-frame)))))
    (cond
     ((eq type 'x) (x-menu-bar-open frame))
     ((eq type 'w32) (w32-menu-bar-open frame))
     (t (with-selected-frame (or frame (selected-frame))
          (tmm-menubar))))))

(global-set-key [f10] 'menu-bar-open)

(provide 'menu-bar)

;;; menu-bar.el ends here
