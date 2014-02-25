;;; chinese.el --- support for Chinese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Chinese

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

;; For Chinese, three character sets GB2312, BIG5, and CNS11643 are
;; supported.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese (general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-coding-system 'iso-2022-cn
  "基于 ISO 2022 的7位文字编码, 适用于简体中文 GB 和繁体中文 CNS (MIME:ISO-2022-CN)。"
  :coding-type 'iso-2022
  :mnemonic ?C
  :charset-list '(ascii chinese-gb2312 chinese-cns11643-1 chinese-cns11643-2)
  :designation [ascii
		(nil chinese-gb2312 chinese-cns11643-1)
		(nil chinese-cns11643-2)
		nil]
  :flags '(ascii-at-eol ascii-at-cntl 7-bit
			designation locking-shift single-shift init-at-bol)
  :mime-charset 'iso-2022-cn
  :suitable-for-keyboard t)

(define-coding-system-alias 'chinese-iso-7bit 'iso-2022-cn)

(define-coding-system 'iso-2022-cn-ext
  "基于 ISO 2022 的7位文字编码, 适用于简体中文 GB 和繁体中文 CNS (MIME:ISO-2022-CN-EXT)。"
  :coding-type 'iso-2022
  :mnemonic ?C
  :charset-list '(ascii
		  chinese-gb2312 chinese-cns11643-1
		  chinese-cns11643-2 chinese-cns11643-3 chinese-cns11643-4
		  chinese-cns11643-5 chinese-cns11643-6 chinese-cns11643-7)
  :designation '[ascii
		 (nil chinese-gb2312 chinese-cns11643-1)
		 (nil chinese-cns11643-2)
		 (nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
		      chinese-cns11643-6 chinese-cns11643-7)]
  :flags '(ascii-at-eol ascii-at-cntl 7-bit
			designation locking-shift single-shift init-at-bol)
  :mime-charset 'iso-2022-cn-ext
  :suitable-for-keyboard t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese GB2312 (simplified)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-coding-system 'chinese-iso-8bit
  "基于 ISO 2022 的台湾 EUC 文字编码, 适用于简体中文 GB2312 (MIME:GB2312)。"
  :coding-type 'iso-2022
  :mnemonic ?c
  :charset-list '(ascii chinese-gb2312)
  :designation [ascii chinese-gb2312 nil nil]
  :mime-charset 'gb2312)

(define-coding-system-alias 'cn-gb-2312 'chinese-iso-8bit)
(define-coding-system-alias 'euc-china 'chinese-iso-8bit)
(define-coding-system-alias 'euc-cn 'chinese-iso-8bit)
(define-coding-system-alias 'cn-gb 'chinese-iso-8bit)
(define-coding-system-alias 'gb2312 'chinese-iso-8bit)

(define-coding-system 'chinese-hz
  "简体中文 GB2312 的 Hz/ZW 7位字符编码 (MIME:HZ-GB-2312)。"
  :coding-type 'utf-8
  :mnemonic ?z
  :charset-list '(ascii chinese-gb2312)
  :mime-charset 'hz-gb-2312
  :post-read-conversion 'post-read-decode-hz
  :pre-write-conversion 'pre-write-encode-hz)

(define-coding-system-alias 'hz-gb-2312 'chinese-hz)
(define-coding-system-alias 'hz 'chinese-hz)

(set-language-info-alist
 "简体中文 Chinese-GB" '((charset chinese-gb2312 chinese-sisheng)
		(iso639-language . zh)
		(coding-system chinese-iso-8bit iso-2022-cn chinese-hz)
		(coding-priority chinese-iso-8bit chinese-big5 iso-2022-cn)
		(input-method . "chinese-py-punct")
		(features china-util)
		(sample-text . "Chinese ($AVPND(B,$AFUM(;0(B,$A::So(B)	$ADc:C(B")
		(documentation . "对简体中文 GB2312 字符集的支持。")
		(tutorial . "TUTORIAL.cn"))
 '("Chinese"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese BIG5 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-coding-system 'chinese-big5
  "繁体中文 BIG5 的8位文字编码 (MIME:Big5)。"
  :coding-type 'big5
  :mnemonic ?B 
  :charset-list '(ascii big5)
  :mime-charset 'big5)

(define-coding-system-alias 'big5 'chinese-big5)
(define-coding-system-alias 'cn-big5 'chinese-big5)
(define-coding-system-alias 'cp950 'chinese-big5)

(set-language-info-alist
 "繁体中文 Chinese-BIG5" '((charset chinese-big5-1 chinese-big5-2)
		  (iso639-language . zh)
		  (coding-system chinese-big5 chinese-iso-7bit)
		  (coding-priority chinese-big5 iso-2022-cn chinese-iso-8bit)
		  (input-method . "chinese-py-punct-b5")
		  (ctext-non-standard-encodings "big5-0")
		  (features china-util)
		  (sample-text . "Cantonese ($(0GnM$(B,$(0N]0*Hd(B)	$(0*/=((B, $(0+$)p(B")
		  (documentation . "对繁体中文 Big5 字符集的支持。")
		  (tutorial . "TUTORIAL.zh"))
 '("Chinese"))

(define-coding-system 'chinese-big5-hkscs
  "中文 BIG5-HKSCS 8位文字编码，香港提供 (MIME:Big5-HKSCS)"
  :coding-type 'charset
  :mnemonic ?B 
  :charset-list '(ascii big5-hkscs)
  :mime-charset 'big5-hkscs)
(define-coding-system-alias 'big5-hkscs 'chinese-big5-hkscs)
(define-coding-system-alias 'cn-big5-hkscs 'chinese-big5-hkscs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese CNS11643 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-coding-system 'euc-tw
  "基于 ISO 2022 的台湾 EUC 文字编码, 适用于繁体中文 CNS11643。"
  :coding-type 'iso-2022
  :mnemonic ?Z
  :charset-list '(ascii
		  chinese-cns11643-1
		  chinese-cns11643-2
		  chinese-cns11643-3
		  chinese-cns11643-4
		  chinese-cns11643-5
		  chinese-cns11643-6
		  chinese-cns11643-7)
  :designation [ascii chinese-cns11643-1 (chinese-cns11643-1
					  chinese-cns11643-2
					  chinese-cns11643-3
					  chinese-cns11643-4
					  chinese-cns11643-5
					  chinese-cns11643-6
					  chinese-cns11643-7) nil]
  :mime-charset 'euc-tw)

(define-coding-system-alias 'euc-taiwan 'euc-tw)

(set-language-info-alist
 "繁体中文 Chinese-CNS" '((charset chinese-cns11643-1 chinese-cns11643-2
			  chinese-cns11643-3 chinese-cns11643-4
			  chinese-cns11643-5 chinese-cns11643-6
			  chinese-cns11643-7)
		 (iso639-language . zh)
		 (coding-system iso-2022-cn euc-tw)
		 (coding-priority iso-2022-cn euc-tw chinese-big5
				  chinese-iso-8bit)
		 (features china-util)
		 (input-method . "chinese-cns-quick")
		 ;; Fixme: presumably it won't accept big5 now.
		 (documentation . "\
Support for Chinese CNS character sets.  Note that the EUC-TW coding system
accepts Big5 for input also (which is then converted to CNS)."))
 '("Chinese"))

(set-language-info-alist
 "繁体中文 Chinese-EUC-TW" '((charset chinese-cns11643-1 chinese-cns11643-2
			     chinese-cns11643-3 chinese-cns11643-4
			     chinese-cns11643-5 chinese-cns11643-6
			     chinese-cns11643-7 chinese-big5-1 chinese-big5-2)
		    (iso639-language . zh)
		    (coding-system euc-tw iso-2022-cn)
		    (coding-priority euc-tw chinese-big5 iso-2022-cn
				     chinese-iso-8bit)
		    (features china-util)
		    (input-method . "chinese-cns-quick")
		    (documentation . "\
Support for Chinese, prefering the EUC-TW character set.  Note that
the EUC-TW coding system accepts Big5 for input also (which is then
converted to CNS)."))
 '("Chinese"))


;;; Chinese GBK

(define-coding-system 'chinese-gbk
  "中文 GBK 编码 (MIME:GBK)。"
  :coding-type 'charset
  :mnemonic ?c
  :charset-list '(ascii chinese-gbk)
  :mime-charset 'gbk)
(define-coding-system-alias 'gbk 'chinese-gbk)
(define-coding-system-alias 'cp936 'chinese-gbk)
(define-coding-system-alias 'windows-936 'chinese-gbk)

(set-language-info-alist
 "简体中文 Chinese-GBK" '((charset chinese-gbk)
		 (iso639-language . zh)
		 (coding-system chinese-gbk)
		 (coding-priority gbk iso-2022-cn chinese-big5
				  chinese-iso-8bit) ; fixme?
		 (ctext-non-standard-encodings "gbk-0")		 
		 (input-method . "chinese-py-punct") ; fixme?
		 (sample-text . "Chinese ($BCfJ8(B,$BIaDL$A;0(B,$A::So(B) $(D95$B9%(B")
		 (features china-util)
		 (documentation . "对中文 GBK 字符集的支持。")
		 (tutorial . "TUTORIAL.cn"))
 '("Chinese"))

;;; Chinese GB18030

(define-coding-system 'chinese-gb18030
  "中文 GB18030 文字编码 (MIME:GB18030)。"
  :coding-type 'charset
  :mnemonic ?c
  :charset-list '(ascii gb18030-2-byte
			gb18030-4-byte-bmp gb18030-4-byte-smp
			gb18030-4-byte-ext-1 gb18030-4-byte-ext-2)
  :mime-charset 'gb18030)

(define-coding-system-alias 'gb18030 'chinese-gb18030)

(set-language-info-alist
 "简体中文 Chinese-GB18030" '((charset gb18030)
		     (iso639-language . zh)
		     (coding-system chinese-gb18030)
		     (coding-priority gb18030 gbk iso-2022-cn chinese-big5
				      chinese-iso-8bit) ; fixme?
		     (input-method . "chinese-py-punct") ; fixme?
		     (sample-text . "Chinese ($BCfJ8(B,$BIaDL$A;0(B,$A::So(B) $(D0_$B9%(B")
		     (features china-util)
		     (documentation
		      . "对中文 GB18030 字符集的支持。")
		     (tutorial . "TUTORIAL.cn"))
 '("Chinese"))

;; Fixme: add HKSCS

(provide 'chinese)

;; arch-tag: b82fcf7a-84f6-4e0b-b38c-1742dac0e09f
;;; chinese.el ends here
;; Simplified Chinese (zh_CN) localization resources for Emacs.
;; translated by Careone <careone@wo.com.cn>, 20130106
