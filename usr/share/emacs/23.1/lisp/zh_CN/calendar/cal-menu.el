;;; cal-menu.el --- calendar functions for menu bar and popup menu support

;; Copyright (C) 1994, 1995, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;;         Lara Rios <lrios@coewl.cen.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, popup menus, menu bar

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

;; See calendar.el.

;;; Code:

(require 'calendar)

(defconst cal-menu-sunmoon-menu
  '("太阳/月亮"
    ["月相" calendar-lunar-phases]
    ["光标处日期的日出/日落" calendar-sunrise-sunset]
    ["光标处月份的日出/日落" calendar-sunrise-sunset-month])
  "Key map for \"Sun/Moon\" menu in the calendar.")

(defconst cal-menu-diary-menu
  '("行程"
    ["其它文件" diary-view-other-diary-entries]
    ["光标处日期" diary-view-entries]
    ["标记全部" diary-mark-entries]
    ["显示全部" diary-show-all-entries]
    ["插入行程事项" diary-insert-entry]
    ["插入周事项" diary-insert-weekly-entry]
    ["插入月事项" diary-insert-monthly-entry]
    ["插入年事项" diary-insert-yearly-entry]
    ["插入周年纪念日" diary-insert-anniversary-entry]
    ["插入块" diary-insert-block-entry]
    ["插入" diary-insert-cyclic-entry]
    ("插入巴海派"
     ["一次" diary-bahai-insert-entry]
     ["月周期" diary-bahai-insert-monthly-entry]
     ["年周期" diary-bahai-insert-yearly-entry])
    ("插入伊斯兰教"
     ["一次" diary-islamic-insert-entry]
     ["月周期" diary-islamic-insert-monthly-entry]
     ["年周期" diary-islamic-insert-yearly-entry])
    ("插入希伯来"
     ["一次" diary-hebrew-insert-entry]
     ["月周期" diary-hebrew-insert-monthly-entry]
     ["年周期" diary-hebrew-insert-yearly-entry]))
    "Key map for \"Diary\" menu in the calendar.")

(defun cal-menu-holiday-window-suffix ()
  "Return a string suffix for the \"Window\" entry in `cal-menu-holidays-menu'."
  (let ((my1 (calendar-increment-month-cons -1))
        (my2 (calendar-increment-month-cons 1)))
    ;; Mon1-Mon2, Year  or  Mon1, Year1-Mon2, Year2.
    (format "%s%s-%s, %d"
            (calendar-month-name (car my1) 'abbrev)
            (if (= (cdr my1) (cdr my2))
                ""
              (format ", %d" (cdr my1)))
            (calendar-month-name (car my2) 'abbrev)
            (cdr my2))))

(defvar displayed-year)                 ; from calendar-generate

(defconst cal-menu-holidays-menu
  `("节假日"
    ["针对光标处日期 -" calendar-cursor-holidays
     :suffix (calendar-date-string (calendar-cursor-to-date) t t)
     :visible (calendar-cursor-to-date)]
    ["针对窗格 -" calendar-list-holidays
     :suffix (cal-menu-holiday-window-suffix)]
    ["针对今天 -" (calendar-cursor-holidays (calendar-current-date))
     :suffix (calendar-date-string (calendar-current-date) t t)]
    "--"
    ,@(let ((l ()))
        ;; Show 11 years--5 before, 5 after year of middle month.
        ;; We used to use :suffix rather than :label and bumped into
        ;; an easymenu bug:
        ;; http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01813.html
        ;; The bug has since been fixed.
        (dotimes (i 11)
          (push (vector (format "hol-year-%d" i)
                        `(lambda ()
                           (interactive)
                           (holiday-list (+ displayed-year ,(- i 5))))
                        :label `(format "%d年"
                                       (+ displayed-year ,(- i 5))))
                l))
        (nreverse l))
    "--"
    ["取消标记日历" calendar-unmark]
    ["标记节假日" calendar-mark-holidays])
  "Key map for \"Holidays\" menu in the calendar.")

(defconst cal-menu-goto-menu
  '("转到"
    ["今天" calendar-goto-today]
    ["每周开始" calendar-beginning-of-week]
    ["周末" calendar-end-of-week]
    ["月头" calendar-beginning-of-month]
    ["月尾" calendar-end-of-month]
    ["年头" calendar-beginning-of-year]
    ["年尾" calendar-end-of-year]
    ["其它日期" calendar-goto-date]
    ["365 天" calendar-goto-day-of-year]
    ["ISO 周" calendar-iso-goto-week]
    ["ISO 日期" calendar-iso-goto-date]
    ["天文学日期" calendar-astro-goto-day-number]
    ["希伯来日期" calendar-hebrew-goto-date]
    ["波斯日期" calendar-persian-goto-date]
    ["巴海派日期" calendar-bahai-goto-date]
    ["伊斯兰教日期" calendar-islamic-goto-date]
    ["恺撒日期" calendar-julian-goto-date]
    ["中国日期" calendar-chinese-goto-date]
    ["古埃及日期" calendar-coptic-goto-date]
    ["埃塞俄比亚日期" calendar-ethiopic-goto-date]
    ("玛雅日期"
     ["下一个 Tzolkin" calendar-mayan-next-tzolkin-date]
     ["上一个 Tzolkin" calendar-mayan-previous-tzolkin-date]
     ["下一个 Haab" calendar-mayan-next-haab-date]
     ["上一个 Haab" calendar-mayan-previous-haab-date]
     ["下一轮" calendar-mayan-next-round-date]
     ["上一轮" calendar-mayan-previous-round-date])
    ["法国日期" calendar-french-goto-date])
  "Key map for \"Goto\" menu in the calendar.")

(defconst cal-menu-scroll-menu
  '("翻页"
    ["向前1个月" calendar-scroll-left]
    ["向前3个月" calendar-scroll-left-three-months]
    ["向前1年" (calendar-scroll-left 12) :keys "4 C-v"]
    ["后退1个月" calendar-scroll-right]
    ["后退3个月" calendar-scroll-right-three-months]
    ["后退1年" (calendar-scroll-right 12) :keys "4 M-v"])
  "Key map for \"Scroll\" menu in the calendar.")

(declare-function x-popup-menu "xmenu.c" (position menu))

(defmacro cal-menu-x-popup-menu (event title &rest body)
  "Call `x-popup-menu' at position EVENT, with TITLE and contents BODY.
Signals an error if popups are unavailable."
  (declare (indent 2))
  `(if (display-popup-menus-p)
       (x-popup-menu ,event (list ,title (append (list ,title) ,@body)))
     (error "Popup menus are not available on this system")))

(autoload 'diary-list-entries "diary-lib")
;; Autoloaded in diary-lib.
(declare-function calendar-check-holidays "holidays" (date))

(defun calendar-mouse-view-diary-entries (&optional date diary event)
  "Pop up menu of diary entries for mouse-selected date.
Use optional DATE and alternative file DIARY.  EVENT is the event
that invoked this command.  Shows holidays if `diary-show-holidays-flag'
is non-nil."
  (interactive "i\ni\ne")
  (let* ((date (or date (calendar-cursor-to-date nil event)))
         (diary-file (or diary diary-file))
         (diary-list-include-blanks nil)
         (diary-entries (mapcar (lambda (x) (split-string (cadr x) "\n"))
                                (diary-list-entries date 1 'list-only)))
         (holidays (if diary-show-holidays-flag
                       (calendar-check-holidays date)))
         (title (format "行程事项 %s, 事由: %s"
                        (if diary (format " from %s" diary) "")
                        (calendar-date-string date)))
         (selection (cal-menu-x-popup-menu event title
                      (mapcar (lambda (x) (list (concat "     " x))) holidays)
                      (if holidays
                          (list "--shadow-etched-in" "--shadow-etched-in"))
                      (if diary-entries
                          (mapcar 'list (apply 'append diary-entries))
                        '("无")))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-view-other-diary-entries (&optional event)
  "Pop up menu of diary entries from alternative file on mouse-selected date."
  (interactive "e")
  (calendar-mouse-view-diary-entries
   (calendar-cursor-to-date nil event)
   (read-file-name "输入行程文件名: " default-directory nil t)
   event))

(defun cal-menu-set-date-title (menu)
  "Convert date of last event to title suitable for MENU."
  (easy-menu-filter-return
   menu (calendar-date-string (calendar-cursor-to-date t last-input-event)
                              t nil)))

(easy-menu-define cal-menu-context-mouse-menu nil
  "Pop up menu for Mouse-2 for selected date in the calendar window."
  '("cal-menu-mouse2" :filter cal-menu-set-date-title
    "--"
    ["节假日" calendar-cursor-holidays]
    ["标记日期" calendar-set-mark]
    ["日出/日落" calendar-sunrise-sunset]
    ["其它日历" calendar-print-other-dates]
    ;; FIXME there is a bug (#447) with last-nonmenu-event and submenus.
    ;; These currently don't work if called without calendar window selected.
    ("准备 LaTex 缓冲区"
     ["每天 (1页)" cal-tex-cursor-day]
     ["每周 (1页)" cal-tex-cursor-week]
     ["每周 (2页)" cal-tex-cursor-week2]
     ["每周(其它风格; 1页)" cal-tex-cursor-week-iso]
     ["每周(另外一种风格; 1页)" cal-tex-cursor-week-monday]
     ["月周期" cal-tex-cursor-month]
     ["月周期 (横向)" cal-tex-cursor-month-landscape]
     ["年周期" cal-tex-cursor-year]
     ["年周期 (横向)" cal-tex-cursor-year-landscape]
     ("Filofax 活页式风格"
      ["Filofax 活页式日事项(每天一页)" cal-tex-cursor-filofax-daily]
      ["Filofax 活页式周事项(2周简略图)" cal-tex-cursor-filofax-2week]
      ["Filofax 活页式周事项(1周简略图)" cal-tex-cursor-filofax-week]
      ["Filofax 活页式年事项" cal-tex-cursor-filofax-year]))
    ("保存到 HTML 日历"
     ["针对选中的月份" cal-html-cursor-month]
     ["针对选中的年份" cal-html-cursor-year])
    ["行程事项" calendar-mouse-view-diary-entries :keys "d"]
    ["插入行程事项" diary-insert-entry]
    ["其它行程文件事项" calendar-mouse-view-other-diary-entries
     :keys "D"]))

(easy-menu-define cal-menu-global-mouse-menu nil
  "Menu bound to a mouse event, not specific to the mouse-click location."
  '("日历"
    ["向前翻页" calendar-scroll-left-three-months]
    ["往回翻页" calendar-scroll-right-three-months]
    ["标记行程事项" diary-mark-entries]
    ["列出节假日" calendar-list-holidays]
    ["标记节假日" calendar-mark-holidays]
    ["取消标记" calendar-unmark]
    ["月相" calendar-lunar-phases]
    ["某个月份的日出时间" calendar-sunrise-sunset-month]
    ["显示行程" diary-show-all-entries]
    ["离开日历" calendar-exit]))

;; Undocumented and probably useless.
(defvar cal-menu-load-hook nil
  "Hook run on loading of the `cal-menu' package.")
(make-obsolete-variable 'cal-menu-load-hook
                        "以后会被移除." "23.1")

(run-hooks 'cal-menu-load-hook)

(provide 'cal-menu)

;; arch-tag: aa81cf73-ce89-48a4-97ec-9ef861e87fe9
;;; cal-menu.el ends here
;; Simplified Chinese (zh_CN) localization resources for Emacs.
;; translated by Careone <careone@wo.com.cn>, 20130106

