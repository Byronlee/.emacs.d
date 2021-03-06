;; -*- coding: utf-8 -*-
;;; animate.el --- make text dance

;; Copyright (C) 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008 Free Software Foundation, Inc.

;; Maintainer: Richard Stallman <rms@gnu.org>
;; Keywords: games

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

;; (animate-string STRING VPOS &optional HPOS)
;; makes the string STRING appear starting at VPOS, HPOS
;; by having each letter swoop into place from random starting position.

;; animate-birthday-present was the first application of this program.

;;; Code:

;;; STRING is the string to be displayed,
;;; and DEST-X, DEST-Y say where on the screen
;;; it should end up.

;;; This function returns a list describing
;;; all the characters and the paths they should take.
;;; Each element has the form
;;;  (CHAR START-Y START-X DEST-Y DEST-X).

;;; The start position of each character is chosen randomly.
;;; The destination is chosen to put it in the right place
;;; in the string when the whole string finally reaches its
;;; specified position.

(defun animate-initialize (string vpos hpos)
  (let ((characters nil))
    (dotimes (i (length string))
      (setq characters
	    (cons (list (aref string i)
			;; Random starting positions.
			(random (window-height))
			(random (1- (window-width)))
			;; All the chars should end up
			;; on the specified line.
			vpos
			;; The Ith character in the string
			;; needs to end up I positions later.
			(+ hpos i))
		  characters)))
    characters))

;;; Display the characters in CHARACTERS,
;;; each one FRACTION of the way from its start to its destination.
;;; If FRACTION is 0, the characters appear in their starting positions.
;;; If FRACTION is 1, the characters appear in their destinations.

(defun animate-step (characters fraction)
  (let ((remains (- 1 fraction)))
    (dolist (item characters)
      (let ((vpos (+ (* remains (nth 1 item))
		     (* fraction (nth 3 item))))
	    (hpos (+ (* remains (nth 2 item))
		     (* fraction (nth 4 item)))))
	(animate-place-char (car item) vpos hpos)))))

;;; Place the character CHAR at position VPOS, HPOS in the current buffer.
(defun animate-place-char (char vpos hpos)
  (goto-char (window-start))
  (let (abbrev-mode)
    (dotimes (i vpos)
      (end-of-line)
      (if (= (forward-line 1) 1)
	  (insert "\n"))))
  (beginning-of-line)
  (move-to-column (floor hpos) t)
  (unless (eolp) (delete-char 1))
  (insert-char char 1))

(defvar animate-n-steps 10
  "Number of steps to use `animate-string'.")

;;;###autoload
(defun animate-string (string vpos &optional hpos)
  "Display STRING starting at position VPOS, HPOS, using animation.
The characters start at randomly chosen places,
and all slide in parallel to their final positions,
passing through `animate-n-steps' positions before the final ones.
If HPOS is nil (or omitted), center the string horizontally
in the current window."
  (let ((characters
	 (animate-initialize string vpos
			     (or hpos
				 ;; HPOS unspecified, so compute
				 ;; it so as to center the string.
				 (max 0 (/ (- (window-width) (length string)) 2))))))
    (dotimes (i animate-n-steps)
      ;; Bind buffer-undo-list so it will be unchanged when we are done.
      ;; (We're going to undo all our changes anyway.)
      (let (buffer-undo-list
	    list-to-undo)
	;; Display the characters at the Ith position.
	;; This inserts them in the buffer.
	(animate-step characters (/ i 1.0 animate-n-steps))
	;; Make sure buffer is displayed starting at the beginning.
	(set-window-start nil 1)
	;; Display it, and wait just a little while.
	(sit-for .05)
	;; Now undo the changes we made in the buffer.
	(setq list-to-undo buffer-undo-list)
	(while list-to-undo
	  (let ((undo-in-progress t))
	    (setq list-to-undo (primitive-undo 1 list-to-undo))))))
    ;; Insert the characters in their final positions.
    (animate-step characters 1)
    ;; Put the cursor at the end of the text on the line.
    (end-of-line)
    ;; Redisplay so they appear on the screen there.
    (sit-for 0)
    ;; This is so that the undo command, used afterwards,
    ;; will undo the "animate" calls one by one.
    (undo-boundary)))

;;;###autoload
(defun animate-sequence (list-of-strings space)
  "Display strings from LIST-OF-STRING with animation in a new buffer.
Strings will be separated from each other by SPACE lines."
  (let ((vpos (/ (- (window-height)
		    1 ;; For the mode-line
		    (* (1- (length list-of-strings)) space)
		    (length list-of-strings))
		 2)))
    (switch-to-buffer (get-buffer-create "*动画*"))
    (erase-buffer)
    (sit-for 0)
    (setq indent-tabs-mode nil)
    (while list-of-strings
      (animate-string (car list-of-strings) vpos)
      (setq vpos (+ vpos space 1))
      (setq list-of-strings (cdr list-of-strings)))))

;;;###autoload
(defun animate-birthday-present (&optional name)
  "Display one's birthday present in a new buffer.
You can specify the one's name by NAME; the default value is \"Sarah\"."
  (interactive (list (read-string "姓名(默认叫“亲”): "
				  nil nil "亲")))
  ;; Make a suitable buffer to display the birthday present in.
  (switch-to-buffer (get-buffer-create (format "*%s*" name)))
  (erase-buffer)
  ;; Display the empty buffer.
  (sit-for 0)
  ;; Make sure indentation does not use tabs.
  ;; They would confuse things.
  (setq indent-tabs-mode nil)

  (animate-string "生日快乐, " 8 30)
  (animate-string (format "%s！" name) 8 40)

  (sit-for 1)

  (animate-string "你是我的阳光" 10 30)
  (sit-for .5)
  (animate-string "我唯一的阳光！" 11 30)
  (sit-for .5)
  (animate-string "从始至终" 12 30)
  (sit-for .5)
  (animate-string "从未改变！" 13 30)
  (sit-for .5)
  (animate-string "让我们认真谈一谈" 15 30)
  (sit-for .5)
  (animate-string "爱再深一点！" 16 30)
  (sit-for .5)
  (animate-string "记得把我的真情" 18 30)
  (animate-string "好好珍藏" 19 30)
  (animate-string "----直到永远！" 20 30))

(random t)

(provide 'animate)

;;; arch-tag: 275289a3-6ac4-41da-b527-a1147045392f
;;; animate.el ends here
;; Simplified Chinese (zh_CN) localization resources for Emacs.
