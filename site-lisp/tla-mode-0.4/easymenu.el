;;; easymenu.el - Easy menu support for FSF and Lucid Emacs 19.
;; 
;; $Id: easymenu.el,v 5.18 1994/04/24 23:07:50 amanda Exp $
;;
;; LCD Archive Entry:
;; easymenu|Per Abrahamsen|abraham@iesd.auc.dk|
;; Easy menu support for FSF and Lucid Emacs 19|
;; $Date: 1994/04/24 23:07:50 $|$Revision: 5.18 $|~/misc/easymenu.el.gz|

;; Copyright (C) 1993, 1994 Per Abrahamsen <abraham@iesd.auc.dk>
;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; The code to add and remove menus for Lucid Emacs was originally
;; from Alastair Burt <burt@dfki.uni-kl.de>.  The function
;; `easy-menu-create-keymaps' is derived from code from the file
;; `lemacs.el' in the FSF Emacs 19.15 distribution.

;; Commentary:
;;
;; Easymenu allows you to define menus for both FSF and Lucid Emacs
;; 19.  The advantages of using easymenu are:
;;
;; - Easier to use than either the FSF or Lucid menu syntax.
;;
;; - Common interface for Emacs 18, FSF Emacs 19, and Lucid Emacs.  
;;   (The code does nothing when run under Emacs 18).
;;
;; The public functions are:
;; 
;; - Function: easy-menu-define SYMBOL MAPS DOC MENU
;;     SYMBOL is the name of the variable that holds the menu. 
;;     MAPS is a list of keymaps where the menu should appear.
;;     DOC is the documentation string for the variable.
;;     MENU is a Lucid style menu description.  
;;
;;     A lucid style menu is a list where the first element is
;;     a string with the name of the menu, and the remaining elements
;;     are the menu items.  Each item can be either a
;;     - Menu: for nested menus.
;;     - String: for menu items that can not be selected.
;;     - Vector: for normal items.  It has three elements:
;;       1. A string with the name of the menu item.
;;       2. The function to be executed when the item is selected.
;;          This can be either a function name or a lisp expression.
;;       3. Normally `t', but it can be a string which is used in FSF
;;          19 as a keyboard shortcut, or a symbol which must be
;;          evaluate to non-nil to keep the item active.
;;
;; - Function: easy-menu-change PATH NAME ITEMS
;;     Change an existing menu.
;;     The menu must already exist an be visible on the menu bar.
;;     PATH is a list of strings used for locating the menu on the menu bar. 
;;     NAME is the name of the menu.  
;;     ITEMS is a list of menu items, as defined in `easy-menu-define'.
;;
;; - Function: easy-menu-add MENU [ MAP ]
;;     Add MENU to the current menubar in MAP.
;;
;; - Function: easy-menu-remove MENU
;;     Remove MENU from the current menubar.
;;
;; FSF Emacs 19 never uses `easy-menu-add' or `easy-menu-remove',
;; menus automatically appear and disappear when the keymaps
;; specified by the MAPS argument to `easy-menu-define' are
;; activated.
;;
;; Lucid Emacs will bind the map to button3 in each MAPS, but you must
;; explicitly call `easy-menu-add' and `easy-menu-remove' to add and
;; remove menus from the menu bar.

;;; Code:

(cond 

;;; Emacs 18

((< (string-to-int emacs-version) 19)

(defmacro easy-menu-define (symbol maps doc menu))

(defmacro easy-menu-remove (menu))

(defmacro easy-menu-add (menu &optional map))

(defmacro easy-menu-change (path name items))

)					;Emacs 18

;;; Lucid Emacs

((string-match "Lucid" emacs-version)

(defmacro easy-menu-define (symbol maps doc menu)
  "Define SYMBOL to be a menu for keymaps MAPS.
DOC is the documentation string, and MENU is a Lucid style menu."
  (` (progn
       (defvar (, symbol) (, menu) (, doc))
       (defun (, symbol) (e)
	 (, doc)
	 (interactive "@e")
	 (setq zmacs-region-stays 't)
	 (popup-menu (, symbol)))
       (let ((maps (, maps)))
	 (mapcar (function (lambda (map) 
		   (define-key map 'button3 '(, symbol))))
		 (if (keymapp maps) (list maps) maps))))))		 

(fset 'easy-menu-change (symbol-function 'add-menu))

(defun easy-menu-add (menu &optional map)
  "Add MENU to the current menu bar."
  (if current-menubar
      (if (assoc (car menu) current-menubar)
	  nil
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil (car menu) (cdr menu)))))

(defun easy-menu-remove (menu)
  "Remove MENU from the current menu bar."
	  (if current-menubar
	      (if (assoc (car menu) current-menubar)
		  (delete-menu-item (list (car menu))))))

)					;Lucid Emacs

;;; FSF Emacs 19

(t

(defun easy-menu-create-keymaps (menu-name menu-items)
  (let ((menu (make-sparse-keymap menu-name))
	(loop menu-items))
    ;; Process items in reverse order,
    ;; since the define-key loop reverses them again.
    (setq menu-items (reverse menu-items))

    (while menu-items
      (let* ((item (car menu-items))
	     (callback (if (vectorp item) (aref item 1)))
	     command name spaces
	     (desc ""))
	(cond ((stringp item)
	       (setq command nil)
	       (setq name item))
	      ((consp item)
	       (setq command
		     (easy-menu-create-keymaps (car item) (cdr item)))
	       (setq name (concat (car item))))
	      ((vectorp item)
	       (let ((enable (aref item 2)))
		 (setq command
		       (if (symbolp callback)
			   callback
			 (list 'lambda () '(interactive) callback)))
		 (and (symbolp enable)
		      (not (memq enable '(nil t)))
		      (progn
			(put enable 'menu-enable enable)
			(fset enable command)
			(setq command enable)))
		 (setq name (aref item 0))
		 (if (stringp enable)
		     (setq desc
			   (concat "  (" enable ")"))))))
	(if name 
	    (define-key menu (vector (intern name))
	      (cons (concat name desc) command))))
      (setq menu-items (cdr menu-items)))
    menu))

(defmacro easy-menu-define (symbol maps doc menu)
  "Define SYMBOL to be a menu for keymaps MAPS.
DOC is the documentation string, and MENU is a Lucid style menu."
  (` (let* ((maps (, maps))
	    (menu (, menu))
	    (keymap (easy-menu-create-keymaps (car menu) (cdr menu))))
       (mapcar (function (lambda (map) 
		 (define-key map (vector 'menu-bar (intern (car menu)))
		   (cons (car menu) keymap))))
	       (if (keymapp maps) (list maps) maps)))))

(defun easy-menu-change (path name items)
  "Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu containing NAME in the
menu bar.  ITEMS is a list of Lucid style menu items.

Call this from 'activate-menubar-hook' to implement dynamic menus."
  (let ((map (key-binding (apply 'vector
				 'menu-bar
				 (mapcar 'intern (append path (list name)))))))
    (if (keymapp map)
	(setcdr map (cdr (easy-menu-create-keymaps name items)))
      (error "Trying to change a non-menu."))))


(defmacro easy-menu-remove (menu))

(defmacro easy-menu-add (menu &optional map))

)					;FSF Emacs 19

)					;cond

(provide 'easymenu)

;;; easymenu.el ends here
