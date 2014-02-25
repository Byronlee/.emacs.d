;;; loadlang-zh-cn.el -- set Emacs menu language to Simplified Chinese (zh_CN) ;;; -*- coding: utf-8 -*-
;;; 2013-11-02 updated, by Careone <emacslocale@126.com>

;;; version 15.0

;; enable special menu lanuage by load/autoload translated lisp files
;
;; for Emacs 22.3 and any other old versions for Windows, should use special
;; translated menu-bar_windows.el for show top menu in English better than
;; bad Chinese coding. (and translated menu-bar.el for Linux show Chinese ok);
;; translated bookmark.el not work, for unknown reason, so disable it for now;

;;

(load "zh_CN/menu-bar")
(load "zh_CN/international/mule-cmds")
;
;;(load "zh_CN/bookmark")	;;not work and disabled
;
(load "zh_CN/calendar/cal-china")
(load "zh_CN/calendar/cal-menu")
(load "zh_CN/calendar/lunar")
(load "zh_CN/calendar/solar")
;
(autoload 'dired "zh_CN/dired" "" t nil)
;
(load "zh_CN/facemenu")
;
(load "zh_CN/language/chinese")
;
(load "zh_CN/net/eudc")
;
(autoload 'org "zh_CN/org/org" "" t nil)
;
(load "zh_CN/play/animate")
;
(load "zh_CN/speedbar")
;
(load "zh_CN/textmodes/ispell")
;
(load "zh_CN/vc/ediff-hook")

(provide 'loadlang-zh-cn)

;; arch-tag: 
;;; loadlang-zh-cn.el ends here
