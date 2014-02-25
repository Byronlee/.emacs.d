;;; loadlang-zh-tw.el -- set Emacs menu language to Traditional Chinese (zh_TW) ;;; -*- coding: utf-8 -*-
;;; 2013-11-02 updated, by Careone <emacslocale@126.com>

;;; version 15.0

;; enable special menu lanuage by load/autoload translated lisp files
;
;; for Emacs 22.3 and any other old versions for Windows, should use special
;; translated menu-bar_windows.el for show top menu in English better than
;; bad Chinese coding. (and translated menu-bar.el for Linux show Chinese ok);
;; translated bookmark.el not work, for unknown reason, so disable it for now;

;;

(load "zh_TW/menu-bar")
(load "zh_TW/international/mule-cmds")

;;; TODO
;;(load "zh_TW/bookmark")	;;not work and disabled

;(autoload 'dired "zh_TW/dired" "" t nil)

;(load "zh_TW/facemenu")

;
;(load "zh_TW/language/chinese")

;(load "zh_TW/net/eudc")

;(autoload 'org "zh_TW/org/org" "" t nil)
;(autoload 'speedbar "zh_TW/speedbar" "" t nil)

;(load "zh_TW/textmodes/ispell")
;(load "zh_TW/vc/ediff-hook")
;;; TODO END

(provide 'loadlang-zh-tw)

;; arch-tag: 
;;; loadlang-zh-tw.el ends here
