;;; cal-menu.el --- calendar functions for menu bar and popup menu support

;; Copyright (C) 1994, 1995, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;;	Lara Rios <lrios@coewl.cen.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, popup menus, menu bar

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

;; This collection of functions implements menu bar and popup menu support for
;; calendar.el.

;;; Code:

(defvar displayed-month)
(defvar displayed-year)

;; Don't require calendar because calendar requires us.
;; (eval-when-compile (require 'calendar))
(defvar calendar-mode-map)

(define-key calendar-mode-map [menu-bar edit] 'undefined)
(define-key calendar-mode-map [menu-bar search] 'undefined)

(define-key calendar-mode-map [down-mouse-2] 'calendar-mouse-2-date-menu)
(define-key calendar-mode-map [mouse-2] 'ignore)

(defvar calendar-mouse-3-map (make-sparse-keymap "日历"))
(define-key calendar-mode-map [down-mouse-3] calendar-mouse-3-map)

(define-key calendar-mode-map [menu-bar moon]
  (cons "月亮" (make-sparse-keymap "月亮")))

(define-key calendar-mode-map [menu-bar moon moon]
  '("月相" . calendar-phases-of-moon))

(define-key calendar-mode-map [menu-bar diary]
  (cons "行程" (make-sparse-keymap "行程")))

(define-key calendar-mode-map [menu-bar diary heb]
  '("插入希伯来" . calendar-mouse-insert-hebrew-diary-entry))
(define-key calendar-mode-map [menu-bar diary isl]
  '("插入伊斯兰教" . calendar-mouse-insert-islamic-diary-entry))
(define-key calendar-mode-map [menu-bar diary baha]
  '("插入巴海派" . calendar-mouse-insert-bahai-diary-entry))
(define-key calendar-mode-map [menu-bar diary cyc]
  '("插入" . insert-cyclic-diary-entry))
(define-key calendar-mode-map [menu-bar diary blk]
  '("插入块" . insert-block-diary-entry))
(define-key calendar-mode-map [menu-bar diary ann]
  '("插入周年纪念日" . insert-anniversary-diary-entry))
(define-key calendar-mode-map [menu-bar diary yr]
  '("插入年事项" . insert-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar diary mon]
  '("插入月事项" . insert-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar diary wk]
  '("插入周事项" . insert-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar diary ent]
  '("插入行程事项" . insert-diary-entry))
(define-key calendar-mode-map [menu-bar diary all]
  '("显示全部" . diary-show-all-entries))
(define-key calendar-mode-map [menu-bar diary mark]
 '("标记全部" . mark-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("光标处日期" . diary-view-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("其它文件" . view-other-diary-entries))

(define-key calendar-mode-map [menu-bar Holidays]
  (cons "节假日" (make-sparse-keymap "节假日")))

(define-key calendar-mode-map [menu-bar goto]
  (cons "转到" (make-sparse-keymap "转到")))

(define-key calendar-mode-map [menu-bar goto french]
  '("法国日期" . calendar-goto-french-date))
(define-key calendar-mode-map [menu-bar goto mayan]
  (cons "玛雅日期" (make-sparse-keymap "Mayan")))
(define-key calendar-mode-map [menu-bar goto ethiopic]
  '("埃塞俄比亚日期" . calendar-goto-ethiopic-date))
(define-key calendar-mode-map [menu-bar goto coptic]
  '("古埃及日期" . calendar-goto-coptic-date))
(define-key calendar-mode-map [menu-bar goto chinese]
  '("中国日期" . calendar-goto-chinese-date))
(define-key calendar-mode-map [menu-bar goto julian]
  '("恺撒日期" . calendar-goto-julian-date))
(define-key calendar-mode-map [menu-bar goto islamic]
  '("伊斯兰教日期" . calendar-goto-islamic-date))
(define-key calendar-mode-map [menu-bar goto persian]
  '("巴海派日期" . calendar-goto-bahai-date))
(define-key calendar-mode-map [menu-bar goto persian]
  '("波斯日期" . calendar-goto-persian-date))
(define-key calendar-mode-map [menu-bar goto hebrew]
  '("希伯来日期" . calendar-goto-hebrew-date))
(define-key calendar-mode-map [menu-bar goto astro]
  '("天文学日期" . calendar-goto-astro-day-number))
(define-key calendar-mode-map [menu-bar goto iso]
  '("ISO 日期" . calendar-goto-iso-date))
(define-key calendar-mode-map [menu-bar goto iso-week]
  '("ISO 周" . calendar-goto-iso-week))
(define-key calendar-mode-map [menu-bar goto day-of-year]
  '("365 天" . calendar-goto-day-of-year))
(define-key calendar-mode-map [menu-bar goto gregorian]
  '("其它日期" . calendar-goto-date))
(define-key calendar-mode-map [menu-bar goto end-of-year]
  '("年尾" . calendar-end-of-year))
(define-key calendar-mode-map [menu-bar goto beginning-of-year]
  '("年头" . calendar-beginning-of-year))
(define-key calendar-mode-map [menu-bar goto end-of-month]
  '("月尾" . calendar-end-of-month))
(define-key calendar-mode-map [menu-bar goto beginning-of-month]
  '("月头" . calendar-beginning-of-month))
(define-key calendar-mode-map [menu-bar goto end-of-week]
  '("周末" . calendar-end-of-week))
(define-key calendar-mode-map [menu-bar goto beginning-of-week]
  '("每周开始" . calendar-beginning-of-week))
(define-key calendar-mode-map [menu-bar goto today]
  '("今天" . calendar-goto-today))


(define-key calendar-mode-map [menu-bar goto mayan prev-rnd]
  '("上一轮" . calendar-previous-calendar-round-date))
(define-key calendar-mode-map [menu-bar goto mayan nxt-rnd]
  '("下一轮" . calendar-next-calendar-round-date))
(define-key calendar-mode-map [menu-bar goto mayan prev-haab]
  '("上一个 Haab" . calendar-previous-haab-date))
(define-key calendar-mode-map [menu-bar goto mayan next-haab]
  '("下一个 Haab" . calendar-next-haab-date))
(define-key calendar-mode-map [menu-bar goto mayan prev-tzol]
  '("上一个 Tzolkin" . calendar-previous-tzolkin-date))
(define-key calendar-mode-map [menu-bar goto mayan next-tzol]
  '("下一个 Tzolkin" . calendar-next-tzolkin-date))

(define-key calendar-mode-map [menu-bar scroll]
  (cons "翻页" (make-sparse-keymap "翻页")))

(define-key calendar-mode-map [menu-bar scroll bk-12]
  '("后退1年" . "4\ev"))
(define-key calendar-mode-map [menu-bar scroll bk-3]
  '("后退3个月" . scroll-calendar-right-three-months))
(define-key calendar-mode-map [menu-bar scroll bk-1]
  '("后退1个月" . scroll-calendar-right))
(define-key calendar-mode-map [menu-bar scroll fwd-12]
  '("向前1年" . "4\C-v"))
(define-key calendar-mode-map [menu-bar scroll fwd-3]
  '("向前3个月" . scroll-calendar-left-three-months))
(define-key calendar-mode-map [menu-bar scroll fwd-1]
  '("向前1个月" . scroll-calendar-left))

(defun calendar-flatten (list)
  "Flatten LIST eliminating sublists structure; result is a list of atoms.
This is the same as the preorder list of leaves in a rooted forest."
  (if (atom list)
      (list list)
    (if (cdr list)
        (append (calendar-flatten (car list)) (calendar-flatten (cdr list)))
      (calendar-flatten (car list)))))

(defun cal-menu-x-popup-menu (position menu)
  "Like `x-popup-menu', but prints an error message if popup menus are
not available."
  (if (display-popup-menus-p)
      (x-popup-menu position menu)
    (error "Popup menus are not available on this system")))

(defun cal-menu-list-holidays-year ()
  "Display a list of the holidays of the selected date's year."
  (interactive)
  (let ((year (extract-calendar-year (calendar-cursor-to-date))))
    (list-holidays year year)))

(defun cal-menu-list-holidays-following-year ()
  "Display a list of the holidays of the following year."
  (interactive)
  (let ((year (1+ (extract-calendar-year (calendar-cursor-to-date)))))
    (list-holidays year year)))

(defun cal-menu-list-holidays-previous-year ()
  "Display a list of the holidays of the previous year."
  (interactive)
  (let ((year (1- (extract-calendar-year (calendar-cursor-to-date)))))
    (list-holidays year year)))

(defun cal-menu-update ()
  ;; Update the holiday part of calendar menu bar for the current display.
  (condition-case nil
      (if (eq major-mode 'calendar-mode)
          (let ((l))
            ;; Show 11 years--5 before, 5 after year of middle month
            (dotimes (i 11)
              (let ((y (+ displayed-year -5 i)))
                (push (vector (format "%s年" y)
                              (list (list 'lambda 'nil '(interactive)
                                          (list 'list-holidays y y)))
                              t)
                      l)))
            (setq l (cons ["标记节假日" mark-calendar-holidays t]
                          (cons ["取消标记日历" calendar-unmark t]
                                (cons "--" l))))
            (define-key calendar-mode-map [menu-bar Holidays]
	      (cons "节假日" (easy-menu-create-menu "节假日" (nreverse l))))
            (define-key calendar-mode-map [menu-bar Holidays separator]
              '("--"))
            (define-key calendar-mode-map [menu-bar Holidays today]
                `(,(format "针对今天 -"
                           (calendar-date-string (calendar-current-date) t t))
                  . cal-menu-today-holidays))
            (let ((title
                   (let ((my1 (calendar-increment-month -1))
                         (my2 (calendar-increment-month 1)))
                     (if (= (cdr my1) (cdr my2))
                         (format "%s-%s, %d"
                                 (calendar-month-name (car my1) 'abbrev)
                                 (calendar-month-name (car my2) 'abbrev)
                                 (cdr my2))
                       (format "%s, %d-%s, %d"
                               (calendar-month-name (car my1) 'abbrev)
                               (cdr my1)
                               (calendar-month-name (car my2) 'abbrev)
                               (cdr my2))))))
              (define-key  calendar-mode-map [menu-bar Holidays 3-month]
                `(,(format "针对窗格 -" title)
                  . list-calendar-holidays)))
            (let ((date (calendar-cursor-to-date)))
              (if date
                  (define-key calendar-mode-map [menu-bar Holidays 1-day]
                    `(,(format "针对光标处日期 -"
                               (calendar-date-string date t t))
                      . calendar-cursor-holidays))))))
    ;; Try to avoid entering infinite beep mode in case of errors.
    (error (ding))))

(defun calendar-event-to-date (&optional error)
  "Date of last event.
If event is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil."
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-start last-input-event))))
    (goto-char (posn-point (event-start last-input-event)))
    (calendar-cursor-to-date error)))

(defun calendar-mouse-insert-hebrew-diary-entry (event)
  "Pop up menu to insert a Hebrew-date diary entry."
  (interactive "e")
  (let ((hebrew-selection
         (cal-menu-x-popup-menu
          event
          (list "希伯来插入菜单"
                (list (calendar-hebrew-date-string (calendar-cursor-to-date))
                      '("一次" . insert-hebrew-diary-entry)
                      '("月周期" . insert-monthly-hebrew-diary-entry)
                      '("年周期" . insert-yearly-hebrew-diary-entry))))))
    (and hebrew-selection (call-interactively hebrew-selection))))

(defun calendar-mouse-insert-islamic-diary-entry (event)
  "Pop up menu to insert an Islamic-date diary entry."
  (interactive "e")
  (let ((islamic-selection
         (cal-menu-x-popup-menu
          event
          (list "伊斯兰教插入菜单"
                (list (calendar-islamic-date-string (calendar-cursor-to-date))
                      '("一次" . insert-islamic-diary-entry)
                      '("月周期" . insert-monthly-islamic-diary-entry)
                      '("年周期" . insert-yearly-islamic-diary-entry))))))
    (and islamic-selection (call-interactively islamic-selection))))

(defun calendar-mouse-insert-bahai-diary-entry (event)
  "Pop up menu to insert an Baha'i-date diary entry."
  (interactive "e")
  (let ((bahai-selection
         (x-popup-menu
          event
          (list "巴海派插入菜单"
                (list (calendar-bahai-date-string (calendar-cursor-to-date))
                      '("一次" . insert-bahai-diary-entry)
                      '("月周期" . insert-monthly-bahai-diary-entry)
                      '("年周期" . insert-yearly-bahai-diary-entry))))))
    (and bahai-selection (call-interactively bahai-selection))))

(defun calendar-mouse-sunrise/sunset ()
  "Show sunrise/sunset times for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (calendar-sunrise-sunset)))

(defun cal-menu-today-holidays ()
  "显示今天对应的节假日."
  (interactive)
  (save-excursion
    (calendar-cursor-to-date (calendar-current-date))
    (calendar-cursor-holidays)))

(autoload 'check-calendar-holidays "holidays")
(autoload 'diary-list-entries "diary-lib")

(defun calendar-mouse-holidays (&optional event)
  "Pop up menu of holidays for mouse selected date."
  (interactive "e")
  (let* ((date (calendar-event-to-date))
         (l (mapcar 'list (check-calendar-holidays date)))
         (selection
          (cal-menu-x-popup-menu
           event
           (list
            (format "节假日: %s" (calendar-date-string date))
            (append
             (list (format "节假日: %s" (calendar-date-string date)))
             (if l l '("无")))))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-view-diary-entries (&optional date diary event)
  "Pop up menu of diary entries for mouse-selected date.
Use optional DATE and alternative file DIARY.

Any holidays are shown if `holidays-in-diary-buffer' is t."
  (interactive "i\ni\ne")
  (let* ((date (if date date (calendar-event-to-date)))
         (diary-file (if diary diary diary-file))
         (diary-list-include-blanks nil)
         (diary-display-hook 'ignore)
         (diary-entries
          (mapcar (lambda (x) (split-string (car (cdr x)) "\^M\\|\n"))
                  (diary-list-entries date 1 'list-only)))
         (holidays (if holidays-in-diary-buffer
                       (check-calendar-holidays date)))
         (title (concat "行程事项 "
                        (if diary (format "from %s " diary) "")
                        "for "
                        (calendar-date-string date)))
         (selection
          (cal-menu-x-popup-menu
           event
           (list title
                 (append
                  (list title)
                  (mapcar (lambda (x) (list (concat "     " x))) holidays)
                  (if holidays
                      (list "--shadow-etched-in" "--shadow-etched-in"))
                  (if diary-entries
                      (mapcar 'list (calendar-flatten diary-entries))
                    '("无")))))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-view-other-diary-entries ()
  "Pop up menu of diary entries from alternative file on mouse-selected date."
  (interactive)
  (calendar-mouse-view-diary-entries
   (calendar-event-to-date)
   (read-file-name "输入行程文件名: " default-directory nil t)))

(defun calendar-mouse-insert-diary-entry ()
  "Insert diary entry for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (insert-diary-entry nil)))

(defun calendar-mouse-set-mark ()
  "Mark the date under the cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (calendar-set-mark nil)))

(defun cal-tex-mouse-day ()
  "Make a buffer with LaTeX commands for the day mouse is on."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-day nil)))

(defun cal-tex-mouse-week ()
  "One page calendar for week indicated by cursor.
Holidays are included if `cal-tex-holidays' is t."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-week nil)))

(defun cal-tex-mouse-week2 ()
  "Make a buffer with LaTeX commands for the week cursor is on.
The printed output will be on two pages."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-week2 nil)))

(defun cal-tex-mouse-week-iso ()
  "One page calendar for week indicated by cursor.
Holidays are included if `cal-tex-holidays' is t."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-week-iso nil)))

(defun cal-tex-mouse-week-monday ()
  "One page calendar for week indicated by cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-week-monday nil)))

(defun cal-tex-mouse-filofax-daily ()
  "Day-per-page Filofax calendar for week indicated by cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-filofax-daily nil)))

(defun cal-tex-mouse-filofax-2week ()
  "One page Filofax calendar for week indicated by cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-filofax-2week nil)))

(defun cal-tex-mouse-filofax-week ()
  "Two page Filofax calendar for week indicated by cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-filofax-week nil)))

(defun cal-tex-mouse-month ()
  "Make a buffer with LaTeX commands for the month cursor is on.
Calendar is condensed onto one page."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-month nil)))

(defun cal-tex-mouse-month-landscape ()
  "Make a buffer with LaTeX commands for the month cursor is on.
The output is in landscape format, one month to a page."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-month-landscape nil)))

(defun cal-tex-mouse-year ()
  "Make a buffer with LaTeX commands for the year cursor is on."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-year nil)))

(defun cal-tex-mouse-filofax-year ()
  "Make a buffer with LaTeX commands for Filofax calendar of year cursor is on."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-filofax-year nil)))

(defun cal-tex-mouse-year-landscape ()
  "Make a buffer with LaTeX commands for the year cursor is on."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-year-landscape nil)))

(defun calendar-mouse-print-dates (&optional event)
  "Pop up menu of equivalent dates to mouse selected date."
  (interactive "e")
  (let* ((date (calendar-event-to-date))
        (selection
         (cal-menu-x-popup-menu
          event
          (list
           (concat (calendar-date-string date) " (格里高历)")
           (append
            (list
             (concat (calendar-date-string date) " (格里高历)")
             (list (calendar-day-of-year-string date))
             (list (format "ISO 日期: %s" (calendar-iso-date-string date)))
             (list (format "恺撒日期: %s"
                           (calendar-julian-date-string date)))
             (list
              (format "天文学(恺撒)日(在 UTC 中午): $s.0"
                           (calendar-astro-date-string date)))
             (list
              (format "固定的(RD)日期: %s"
                      (calendar-absolute-from-gregorian date)))
             (list (format "希伯来日期(日落之前): %s"
                           (calendar-hebrew-date-string date)))
             (list (format "波斯日期: %s"
                           (calendar-persian-date-string date)))
             (list (format "巴海派日期(日落之前): %s"
                           (calendar-bahai-date-string date))))
            (let ((i (calendar-islamic-date-string date)))
              (if (not (string-equal i ""))
                  (list (list (format "伊斯兰教日期(日落之前): %s" i)))))
            (list
             (list (format "中国日期: %s"
                           (calendar-chinese-date-string date))))
            ;; (list '("Chinese date (select to echo Chinese date)"
            ;;         . calendar-mouse-chinese-date))
            (let ((c (calendar-coptic-date-string date)))
              (if (not (string-equal c ""))
                  (list (list (format "古埃及日期: %s" c)))))
            (let ((e (calendar-ethiopic-date-string date)))
              (if (not (string-equal e ""))
                  (list (list (format "埃塞俄比亚日期: %s" e)))))
            (let ((f (calendar-french-date-string date)))
              (if (not (string-equal f ""))
                  (list (list (format "法国大革命时期的日期: %s" f)))))
            (list
             (list
              (format "玛雅日期: %s"
                      (calendar-mayan-date-string date)))))))))
        (and selection (call-interactively selection))))

(defun calendar-mouse-chinese-date ()
  "Show Chinese equivalent for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (calendar-print-chinese-date)))

(defun calendar-mouse-goto-date (date)
  (set-buffer (window-buffer (posn-window (event-start last-input-event))))
  (calendar-goto-date date))

(defun calendar-mouse-2-date-menu (event)
  "Pop up menu for Mouse-2 for selected date in the calendar window."
  (interactive "e")
  (let* ((date (calendar-event-to-date t))
         (selection
          (cal-menu-x-popup-menu
           event
           (list (calendar-date-string date t nil)
                 (list
                  ""
                  '("节假日" . calendar-mouse-holidays)
                  '("标记日期" . calendar-mouse-set-mark)
                  '("日出/日落" . calendar-mouse-sunrise/sunset)
                  '("其它日历" . calendar-mouse-print-dates)
                  '("准备 LaTex 缓冲区" . calendar-mouse-cal-tex-menu)
                  '("行程事项" . calendar-mouse-view-diary-entries)
                  '("插入行程事项" . calendar-mouse-insert-diary-entry)
                  '("其它行程文件事项"
                    . calendar-mouse-view-other-diary-entries)
                  )))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-cal-tex-menu (event)
  "Pop up submenu for Mouse-2 for cal-tex commands for selected date in the calendar window."
  (interactive "e")
  (let* ((selection
          (cal-menu-x-popup-menu
           event
           (list (calendar-date-string (calendar-event-to-date t) t nil)
                 (list
                  ""
                  '("每天 (1页)" . cal-tex-mouse-day)
                  '("每周 (1页)" . cal-tex-mouse-week)
                  '("每周 (2页)" . cal-tex-mouse-week2)
                  '("每周(其它风格; 1页)" . cal-tex-mouse-week-iso)
                  '("每周(另外一种风格; 1页)" .
                    cal-tex-mouse-week-monday)
                  '("月周期" . cal-tex-mouse-month)
                  '("月周期 (横向)" . cal-tex-mouse-month-landscape)
                  '("年周期" . cal-tex-mouse-year)
                  '("年周期 (横向)" . cal-tex-mouse-year-landscape)
                  '("Filofax 活页式风格" . cal-tex-mouse-filofax)
                  )))))
    (and selection (call-interactively selection))))

(defun cal-tex-mouse-filofax (event)
  "Pop up sub-submenu for Mouse-2 for Filofax cal-tex commands for selected date."
  (interactive "e")
  (let* ((selection
          (cal-menu-x-popup-menu
           event
           (list (calendar-date-string (calendar-event-to-date t) t nil)
                 (list
                  ""
                  '("Filofax 活页式日事项(每天一页)" .
                    cal-tex-mouse-filofax-daily)
                  '("Filofax 活页式周事项(2周简略图)" .
                    cal-tex-mouse-filofax-2week)
                  '("Filofax 活页式周事项(1周简略图)" .
                    cal-tex-mouse-filofax-week)
                  '("Filofax 活页式年事项" . cal-tex-mouse-filofax-year)
                  )))))
    (and selection (call-interactively selection))))

(define-key calendar-mouse-3-map [exit-calendar]
  '("离开日历" . exit-calendar))
(define-key calendar-mouse-3-map [show-diary]
  '("显示行程" . diary-show-all-entries))
(define-key calendar-mouse-3-map [lunar-phases]
  '("月相" . calendar-phases-of-moon))
(define-key calendar-mouse-3-map [unmark]
  '("取消标记" . calendar-unmark))
(define-key calendar-mouse-3-map [mark-holidays]
  '("标记节假日" . mark-calendar-holidays))
(define-key calendar-mouse-3-map [list-holidays]
  '("列出节假日" . list-calendar-holidays))
(define-key calendar-mouse-3-map [mark-diary-entries]
  '("标记行程事项" . mark-diary-entries))
(define-key calendar-mouse-3-map [scroll-backward]
  '("往回翻页" . scroll-calendar-right-three-months))
(define-key calendar-mouse-3-map [scroll-forward]
  '("向前翻页" . scroll-calendar-left-three-months))

(run-hooks 'cal-menu-load-hook)

(provide 'cal-menu)

;; arch-tag: aa81cf73-ce89-48a4-97ec-9ef861e87fe9
;;; cal-menu.el ends here
;; Simplified Chinese (zh_CN) localization resources for Emacs.
;; translated by Careone <careone@wo.com.cn>, 20130106

