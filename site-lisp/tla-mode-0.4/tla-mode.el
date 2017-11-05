;;; tla-mode.el --- major mode for editing TLA+ specifications

;; Copyright (c) 1994 Frank Wegmann
;; Author: Frank Wegmann <wegmann@linguistics.ruhr-uni-bochum.de>
;; Maintainer: Frank Wegmann <wegmann@linguistics.ruhr-uni-bochum.de>
;; Created: 8 Sep 1993
;; Version: 0.4
;; Keywords: specification, TLA

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is a major mode for editing and working with file written in the
;; specification language TLA+ or its other variants.

;;; Code:

;; RCS

(defconst tla-mode-revision  (substring "$Revision: 1.27 $" 11 -2)
  "$Id: tla-mode.el,v 1.27 1994/09/03 12:25:35 wegmann Exp wegmann $")

(defconst tla-mode-version 
  (concat "0.4 Rev " tla-mode-revision))

;; load site-specific settings
;(require 'tla-site)

(require 'easymenu)
(require 'outline)

(defvar tla-mode-abbrev-table nil
  "Abbrev table in use in TLA mode.")

(define-abbrev-table 'tla-mode-abbrev-table ())

(defvar tla-mode-syntax-table nil
  "Syntax table in use in TLA-mode buffers.")

(if tla-mode-syntax-table
    ()
  (setq tla-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()1" tla-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" tla-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4" tla-mode-syntax-table)
  (modify-syntax-entry ?+ "." tla-mode-syntax-table)
  (modify-syntax-entry ?< "." tla-mode-syntax-table)
  (modify-syntax-entry ?> "." tla-mode-syntax-table)
  (modify-syntax-entry ?- "." tla-mode-syntax-table)
  (modify-syntax-entry ?= "." tla-mode-syntax-table))


(defvar tla-mode-map nil
  "Keymap used in TLA mode.")

(if tla-mode-map
    nil
  (setq tla-mode-map (make-sparse-keymap))
  (define-key tla-mode-map "\C-m" 'tla-newline)
  (define-key tla-mode-map "\177" 'backward-delete-char-untabify)
  (define-key tla-mode-map "\t" 'tla-indent-line)
  (define-key tla-mode-map "\C-c:" 'tla-un-comment-region)
  (define-key tla-mode-map "\C-c;" 'tla-comment-region)
  (define-key tla-mode-map "\C-c%" 'tla-comment-symdef)
  (define-key tla-mode-map "\C-c\C-h" 'tla-header)
  (define-key tla-mode-map "\C-c\C-k" 'tla-keyword)
  (define-key tla-mode-map "\C-c\C-d" 'tla-save-specification)
  (define-key tla-mode-map "\C-c\C-r" 'tla-command-region)
  (define-key tla-mode-map "\C-c\C-b" 'tla-command-buffer)
  (define-key tla-mode-map "\C-c\C-c" 'tla-command-master)
  (define-key tla-mode-map "\C-c\C-k" 'tla-kill-job)
  (define-key tla-mode-map "\C-c\C-l" 'tla-recenter-output-buffer)
  (define-key tla-mode-map "\C-c\C-i" 'tla-goto-info-page)
  (define-key tla-mode-map "\C-c^" 'tla-home-buffer)
  (define-key tla-mode-map "\C-c`" 'tla-next-error))

;; Variables and constants

(setq indent-tabs-mode nil)

(defconst tla-indent 2
  "*Indentation of TLA symbol definitions.")

(defconst tla-inner-indent 4
  "*Indentation inside symbol definitions.")

(defconst bug-tla-mode "tla-mode-bugs@linguistics.ruhr-uni-bochum.de"
  "Address for TLA mode bugs.")

(defconst tla-tab-always-indent t
  "*Non-nil means TAB in TLA mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defvar tla-startup-message t
  "*Non-nil displays a startup message when TLA mode is first called.")

(defvar tla-use-vc t
  "*Non-nil automatically inserts modified header for version control.")

(defvar tla-use-font-lock t
  "*Non-nil automatically fontifies buffer when entering TLA mode.")

(defvar tla-bullet-lorand t
  "*Non-nil bullets expressions conjunctions and disjunctions.")

(defvar tla-auto-untabify t
  "*Non-nil means always convert tabs to spaces when saving the file.")

;; these contain the necessary TeX information for spec92
(defconst tla-spec92-header
  (concat "\\documentstyle[spec92]{article}\n\\begin{document}\n"
	  "\\pagestyle{empty}\n\\begin{spec}\n")
  "Header for region files to be LaTeXed.")
	  
(defconst tla-spec92-trailer
  "------**\n\\end{spec}\n\\end{document}\n\\bye"
  "Trailer for region files to be LaTeXed.")

;;;
;;; TLA+ variants
;;;

(defvar tla-variant-list
  '((list "TLA+" nil nil t)
    (list "tTLA+"  nil t nil)
    (list "eTLA+" t t nil)
    (list "cTLA+" nil t nil))
  "*List containing the different variants of TLA+ that are supported.
Each element is a list whose first element is the name of the variant that
will also be the entry in the menu bar.

The following elements will each indicate presence or absence of a
particular language features:

The second element denotes typing. If non-nil, typing is mandatory
and thus identifiers followed by bracketed expressions will signal
an error. If nil, typing is optional and unqualified identifiers 
are allowed. 

The third element denotes the use of the END keyword. If non-nil,
insertion of a header automatically takes care of a final END keyword.

The fourth element denotes mixfix operators. If non-nil, mixfix operators
are allowed.

The fourth element denotes postfix operators. If non-nil, postfix operators
are allowed.

The list is to be continued.")

(defvar tla-variant-default "tTLA+"
  "Variable indicating the default variant of TLA.")

(defvar tla-current-variant tla-variant-default)
(make-variable-buffer-local 'tla-current-variant)

;;(defun tla-variant ()
;;  "Read new variant from minibuffer and change menu."
;;  (interactive)
;;  (let (ctr (found nil))
;;    (setq ctr 1)
;;    (variant (read-string "Choose TLA+ variant: "))
;;    (while ((and (not found)
;;		 (< ctr (length tla-variant-list)))
;;	    (if (string-equal variant
;;			      (car (cdr (nth ctr tla-variant-list))))
;;		(setq found t)
;;	      (1+ ctr))))
;;    (if (not found)
;;	(error "Unknown TLA+ variant")
;;      (easy-menu-remove (tla-mode-menu))
;;      (setq tla-current-variant variant)
;;      (easy-menu-add (tla-mode-meu tla-mode-map)))))
      
;;
;; New error symbols
;;
(put 'tla-error 'error-conditions '(tla-error tla-warning error))
(put 'tla-error 'error-message (concat tla-current-variant " Error"))
(put 'tla-warning 'error-message (concat tla-current-variant " Warning"))


;;;
;;; Paths for spec files
;;;

;; Change this to point to the place where specifications are stored
;; at your site.
(defvar tla-specfiles-global
  '("/usr/local/lib/specfiles/")
  "*Directories containing the site's spec files.

The directory names *must* end with a slash.")


(defun tla-split-string (char string)
  "Returns a list of strings. given REGEXP the STRING is split into 
sections which in string was seperated by REGEXP.

Examples:

      (tla-split-string \"\:\" \"abc:def:ghi\")
          -> (\"abc\" \"def\" \"ghi\")

      (tla-split-string \" *\" \"dvips -Plw -p3 -c4 testfile.dvi\")

          -> (\"dvips\" \"-Plw\" \"-p3\" \"-c4\" \"testfile.dvi\")

If CHAR is nil, or \"\", an error will occur."

  (let ((regexp char)
        (start 0)
        (result '()))
    (while (string-match regexp string start)
      (let ((match (string-match regexp string start)))
        (setq result (cons (substring string start match) result))
        (setq start (match-end 0))))
    (setq result (cons (substring string start nil) result))
    (nreverse result)))

(defun tla-parse-path (env)
  ;; Return a list if private TLA directories found in environment
  ;; variable ENV.  
  (let* ((value (getenv env))
	 (entries (and value (tla-split-string ":" value)))
	 entry
	 answers) 
    (while entries
      (setq entry (car entries))
      (setq entries (cdr entries))
      (or (string-match "/$" entry)
	  (setq entry (concat entry "/")))
      (or (not (string-match "^/" entry))
	  (member entry tla-specfiles-global)
	  (string-equal "/" entry)
	  (setq answers (cons entry answers))))
    answers))

(defvar tla-specfiles-private (append (tla-parse-path "TLASRC")
				      (tla-parse-path "SPECFILES"))
  "*Directories where you store your personal spec files.
Each must end with a slash.")

;; This is the major configuration variable.  Most sites only
;; want a list for one variant. Here we have different tools for
;; different variants, but some can be used for all variants. When
;; changing this list, look out for the second string in each
;; entry, which is the name of a command to send to the shell.
;; See tla-expand-list for a description of the % escapes.

(defvar tla-command-list
  (list (list "LaTeX Spec92" "latex '\\nonstopmode\\input %t'" 'tla-run-TeX nil t)
        (if (string-equal "eTLA+" tla-current-variant)
            (progn
              (list "VisTLA" "visTLA %t" 'tla-run-command nil nil)
              (list "Coarsener" "tco %t" 'tla-run-command nil nil)))
        (if (string-equal "tTLA+" tla-current-variant)
	    (list "TOAST" "toast %t" 'tla-run-background nil nil))
	(list "Check" "tpp %t" 'tla-run-command nil nil)
	(list "Browse" "tTLA+Browser %t" 'tla-run-discard nil nil))
"*List of commands to execute on the current spec file.

Each element is a list, whose first element is the name of the command
as it will be presented to the user.  

The second element is the string handed to the shell after being
expanded. The expansion is done using the information found in
tla-expand-list. 

The third element is the function which actually start the process.
Several such hooks has been defined:

tla-run-command: Start up the process and show the output in a
separate buffer.  Check that there is not two commands running for the
same file.  Return the process object. 

tla-run-format: As tla-run-command, but assume the output is created
by a TeX macro package.  Return the process object. 

tla-run-TeX: For TeX output.

tla-run-compile: Use `compile' to run the process.  

tla-run-shell: Use `shell-command' to run the process.

tla-run-discard: Start the process in the background, discarding its
output.

tla-run-background: Start the process in the background, show output
in other window.

To create your own hook, define a function taking three arguments: The
name of the command, the command string, and the name of the file to
process.  It might be useful to use tla-run-command in order to
create an asynchronous process.

If the fourth element is non-nil, the user will get a chance to
modify the expanded string.

If the fifth element is non-nil, the tla-region file will be rebuilt
before the command is started.")

;; This is the list of expansion for the commands in
;; tla-command-list.  Not likely to be changed, but you may e.g. want
;; to handle .ps files. 

(defvar tla-expand-list 
  (list (list "%p" 'tla-printer-query)	;%p must be the first entry
	(list "%q" (function (lambda ()
			       (tla-printer-query tla-queue-command 2))))
	(list "%s" 'file)
	;;	(list "%t" 'file 't) ; orig
	(list "%t" 'file "t")
	(list "%d" 'file "dvi")
	(list "%f" 'file "ps"))
  "*List of expansion strings for tla command names.

Each entry is a list with two or more elements.  The first element is
the string to be expanded.  The second element is the name of a
function returning the expanded string when called with the remaining
elements as arguments.  The special value `file' will be expanded to
the name of the file being processed, with an optional extension.")

(defvar tla-command-Show "Browse"
  "*The default command to show a module.
Must be the car of an entry in tla-command-list.")

(make-variable-buffer-local 'tla-command-Show)

(defvar tla-command-Print "Print"
  "The name of the Print entry in tla-command-Print.")

(defvar tla-command-Queue "Queue"
  "The name of the Queue entry in tla-command-Queue.")


(defun tla-mode ()
  "Major mode for editing TLA+ specifications.
It supports editing and working with spec files written
in TLA+ or its variants.

Syntactic control behaves according to the tTLA+ grammar.
From here, various other TLA+ related tools can be triggered.

Disclaimer:
This is still an alpha release! Use it at your own risk!
There is no warranty.

Comments are delimited by \(* ... *\).
Delete converts tabs to spaces as it moves back.
Saving the buffer converts all tabs to spaces.

\\{tla-mode-map}

Variables:

 tla-tab-always-indent
    Non-nil means TAB in TLA mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.

 tla-indent
    Indentation of TLA statements.  (default 2)

 tla-startup-message
    Set to nil to inhibit message first time TLA mode is used.

 tla-use-vc
    Non-nil inserts RCS Id strings into header. (default t)

 tla-use-font-lock
    Non nil means fontify buffer when entering TLA mode. (default t)

 tla-bullet-lorand
    Non-nil means that expression lists bulleted by conjunctions and dis-
    junctions will be aligned with a corresponding con- or disjunction.
    (default t)

Turning on TLA mode calls the value of the variable `tla-mode-hook' with no
args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (boundp 'tla-current-variant)
      ()
    (setq tla-current-variant tla-variant-default))
  (use-local-map tla-mode-map)
  (easy-menu-add tla-mode-menu tla-mode-map)
  (setq major-mode 'tla-mode)
  (setq mode-name "TLA+")
  (setq local-abbrev-table tla-mode-abbrev-table)
  (set-syntax-table tla-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'tla-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'tla-indent-region)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "\(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *\)")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 75)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (setq max-lisp-eval-depth 1000)
  (make-local-variable 'outline-level)
  (setq outline-level 'tla-outline-level)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp (tla-outline-regexp))
  (make-local-variable 'compile-command)
  (if (boundp 'compile-command)
      ()
    (setq compile-command "make"))
  (if (boundp 'local-write-file-hooks)
      (add-hook 'local-write-file-hooks 'tla-safe-write)
    (add-hook 'write-file-hooks 'tla-safe-write))
  (run-hooks 'tla-mode-hooks)
  (and tla-use-font-lock
       (tla-fontify))
  (if tla-startup-message
      (message "Emacs TLA+ mode %s.  Bugs to %s"
	       tla-mode-version bug-tla-mode))
  (setq tla-startup-message nil))


;;
;; Create menus
;;

(defvar tla-command-force nil)
;; If non-nil, tla-command-query will return the value of this
;; variable instead of quering the user. 

(defun tla-command-menu (name file)
  ;; Execute tla-command-list NAME on FILE from a menu.
  (let ((tla-command-force name))
    (funcall file)))

(defun tla-command-menu-print (printer command name file)
  ;; On PRINTER print FILE from a menu.
  (let ((tla-printer-default printer)
	(tla-printer-list nil)
	(tla-print-command command))
    (tla-command-menu name file)))

(defun tla-command-menu-printer-entry (entry)
  ;; Return tla-printer-list ENTRY as a menu item.
  (vector (nth 0 entry)
	  (list 'tla-command-menu-print
		(nth 0 entry)
		(or (nth lookup entry) command)
		name
		(list 'quote file))
	  t))

(defun tla-command-menu-entry (entry)
  ;; Return tla-command-list ENTRY as a menu item.
  (let ((name (car entry)))
    (cond ((and (string-equal name tla-command-Print)
		tla-printer-list)
	   (let ((command tla-print-command)
		 (lookup 1))
	     (append (list tla-command-Print)
		     (mapcar 'tla-command-menu-printer-entry tla-printer-list))))
	  ((and (string-equal name tla-command-Queue)
		tla-printer-list)
	   (let ((command tla-queue-command)
		 (lookup 2))
	     (append (list tla-command-Queue)
		     (mapcar 'tla-command-menu-printer-entry tla-printer-list))))
	  (t
	   (vector name (list 'tla-command-menu name (list 'quote file)) t)))))

(defun tla-command-create-menu (name file)
  ;; Create menu NAME for each entry tla-command-list for FILE.
  (append (list name)
	  (mapcar 'tla-command-menu-entry tla-command-list)))

(defun tla-header-menu-entry (entry)
  ;; Return tla-header-list ENTRY as a menu item.
  (vector (nth 0 entry)
	  (list 'tla-command-menu-print
		(nth 0 entry)
		(or (nth lookup entry) command)
		name
		(list 'quote file))
	  t))

(defun tla-header-create-menu (name entry)
  ;; Create menu NAME for each entry tla-header-list for ENTRY.
  (append (list name)
	  (mapcar 'tla-header-menu-entry tla-header-list)))

(defvar tla-header-menu-name 
  (concat "Insert " tla-current-variant " Module Header"))

(defvar tla-keyword-menu-name
  (concat "Insert " tla-current-variant " Keyword"))


(defun tla-keyword-menu-entry (entry)
  ;; Create an entry for the keyword menu.
  (vector (car entry) (list 'tla-keyword-menu (car entry)) t))

(defun tla-keyword-menu-create ()
  ;; Create a menu over TLA+ keywords
  (append '(tla-keyword-menu-name)
	  (mapcar 'tla-keyword-menu-entry tla-const-keyword-list)))

(easy-menu-define tla-mode-menu
    tla-mode-map
    "Menu used in TLA+ mode."
  (list tla-current-variant
	;; (tla-header-create-menu tla-header-menu-name 'tla-header)
	;; preliminary solution
	(list tla-header-menu-name
	      ["untyped "	(tla-header nil ?\C-s)	"C-c C-h C-s"] 
	      ["CONSTANT"	(tla-header nil ?\C-c)	"C-c C-h C-c"] 
	      ["TEMPORAL"	(tla-header nil ?\C-t)	"C-c C-h C-t"])
	(list tla-keyword-menu-name
	      ["IMPORT              " (tla-keyword nil ?\C-i) "C-c C-k C-i"]
	      ["EXPORT              " (tla-keyword nil ?\C-x) "C-c C-k C-x"]
	      ["INCLUDE             " (tla-keyword nil ?\C-l) "C-c C-k C-l"]
	      ["PARAMETERS          " (tla-keyword nil ?\C-p) "C-c C-k C-p"]
	      ["CONSTANTS           " (tla-keyword nil ?\C-c) "C-c C-k C-c"]
	      ["BOOLEANS            " (tla-keyword nil ?\C-b) "C-c C-k C-b"]
	      ["CONST ASSUMPTIONS   " (tla-keyword nil ?\C-j) "C-c C-k C-j"]
	      ["CONST THEOREMS      " (tla-keyword nil ?\C-k) "C-c C-k C-k"]
	      "----"
	      ["PREDICATES          " (tla-keyword nil ?\C-r) "C-c C-k C-r"]
	      ["STATE FUNCTIONS     " (tla-keyword nil ?\C-s) "C-c C-k C-s"]
	      ["ACTIONS             " (tla-keyword nil ?\C-a) "C-c C-k C-a"]
	      ["TRANSITION FUNCTIONS" (tla-keyword nil ?\C-f) "C-c C-k C-f"]
	      ["TEMPORALS           " (tla-keyword nil ?\C-t) "C-c C-k C-t"]
	      ["ACTION ASSUMPTIONS  " (tla-keyword nil ?\C-o) "C-c C-k C-o"]
	      ["ACTION THEOREMS     " (tla-keyword nil ?\C-n) "C-c C-k C-n"]
	      ["TEMPORAL THEOREMS   " (tla-keyword nil ?\C-h) "C-c C-k C-h"])
	["Save Specification"	tla-save-specification	t]
	["Switch to Top Module" tla-home-buffer		t]
	(tla-command-create-menu "Command on Top Module (C-c C-c)"
				 'tla-command-master)
	(tla-command-create-menu "Command on Buffer (C-c C-b)"
				 'tla-command-buffer)
	;; this one still has to be improved before public release
	;;(tla-command-create-menu "Command on Region (C-c C-r)"
	;;			 'tla-command-region)
	["Next Error" tla-next-error t]
	(list "Command Output"
	      ["Kill Job" tla-kill-job t]
	      ["Switch to original file" tla-home-buffer t]
	      ["Recenter Output Buffer" tla-recenter-output-buffer t])
	"----"
	["Toggle Outlining"	outline-minor-mode	t]
	["Uncomment Region"	tla-un-comment-region	t]
	["Comment Region"	tla-comment-region	t]
	["Comment Symbol Definition"	tla-comment-symdef	t]
	["Documentation"	tla-goto-info-page	t]
	["Submit bug report"	tla-submit-bug-report	t]))

;;;
;;; Error handling
;;;

(defun tla-parse-error (msg)
  (signal 'tla-error (list (format "%s" msg))))

(defun tla-parse-warning (msg)
  (signal 'tla-warning (list (format "%s" msg))))

;;;
;;; Save hook
;;;

(defun tla-safe-write ()
  ;; call tla-write-file safely
  (condition-case name
      (and tla-auto-untabify
	   (tla-write-file))
    (error nil))
  ;; continue with other write file hooks
  nil)

(defun tla-write-file ()
  (if tla-auto-untabify
      (untabify (point-min) (point-max))))


;;; we probably don't need the next two defs
(defvar tla-trailer-start nil
  "Regular expression delimiting start of trailer in a TLA file.")

(make-variable-buffer-local 'tla-trailer-start)

(defvar tla-header-end nil
  "Regular expression delimiting end of header in a TLA spec file.")

(make-variable-buffer-local 'tla-header-end)

(defvar tla-command-default nil
  "The default command for tla-command.")

(make-variable-buffer-local 'tla-command-default)

(defvar tla-active-specfiles nil)
(make-variable-buffer-local 'tla-active-specfiles)

;;;
;;; Top Module
;;;

;;; this code is taken from the excellent AUC TeX package
;;; and adapted to fit into the TLA framework.
;;; All credits of code that is unaltered except naming scheme
;;; thus go to Per Abrahamsen, the maintainer. To be precise:
;;; Parts of this code relating to the idea of using a master
;;; file are under GPL with the following copyrights:
;;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;;; Copyright (C) 1987 Lars Peter Fischer
;;; Copyright (C) 1991 Kresten Krab Thorup
;;; Copyright (C) 1993, 1994 Per Abrahamsen 

;;;

(defvar tla-one-master "\\.t$"
  "*Regular expression matching ordinary TLA+ spec files.

You should set this variable to match the name of all files, where
automatically adding a file variable with the name of the top module
file is a good idea.  When TLA+ mode adds the name of the top module
file as a file variable, it does not need to ask next time when you
edit the file.  

If you dislike TLA mode automatically modifying your files, you can set
this variable to \"<none>\".")


(defun tla-master-file (&optional extension)
  "Return the name of the master file for the current document.

If optional argument EXTENSION is non-nil, add that file extension to
the name.  Special value `t' means use `tla-default-extension'."

  (if (null extension)
      (setq extension tla-default-extension))
  (let ((my-name (if (buffer-file-name)
                     (tla-strip-extension nil (list tla-default-extension) t)
                   "<none>")))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(cond
	 ;; Special value 't means it is own top module.
	 ((equal tla-master my-name) 
	  (setq tla-master t))

	 ;; For files shared between many documents.
	 ((eq 'shared tla-master)
	  (setq tla-master
		(tla-strip-extension
		 (read-file-name "Top module: (default this file) "
				 nil "///")
		 (list tla-default-extension)
		 t))
	  (if (or (string-equal tla-master "///")
		  (string-equal tla-master ""))
	      (setq tla-master t)))

	 ;; We might already know the name.
	 (tla-master)

	 ;; Is this a top module file?
	 ;; for that we should perhaps check whether buffer name
	 ;; matches directory name. Or is there any other criterion?
	 ;;((and tla-header-end
	 ;;       (re-search-forward tla-header-end 10000 t))
	 ;; (setq tla-master my-name))

	 ;; Ask the user (but add it as a local variable).
	 (t
	  (setq tla-master
		(tla-strip-extension
		 (condition-case name
		     (read-file-name "Top module: (default this file) "
				     nil "<default>")
		   (quit "<quit>"))
		 (list tla-default-extension)
		 t))
	  (cond ((string-equal tla-master "<quit>")
		 (setq tla-master t))
		((or (string-equal tla-master "<default>")
		     (string-equal tla-master ""))
		 (setq tla-master t)
		 (tla-add-local-master))
		(t
		 (tla-add-local-master)))))))
  
    (let ((name (if (eq tla-master t)
		    my-name
		  tla-master)))
      
      (if (tla-match-extension name)
      ;; If it already has an extension...
	  (if (equal extension tla-default-extension)
	      ;; Use instead of the default extension
	      (setq extension nil)
	    ;; Otherwise drop it.
	    (setq name (tla-strip-extension name))))

      (if extension
	  (concat name "." extension)
	name))))

(defvar tla-master t
  "*The top module file associated with the current buffer.
If the file being edited is actually included from another file, you
can determine the name of the top module file by setting this variable.
If there are multiple levels of nesting, specify the top level file. 

If this variable is nil, TLA mode will query you for the name.

If the variable is t, TLA mode will assume the file is a top module
file itself.

If the variable is 'shared, TLA mode will query for the name, but not
change the file.  

It is suggested that you use the File Variables (see the info node in
the Emacs manual) to set this variable permanently for each file.")

(make-variable-buffer-local 'tla-master)

(defun tla-add-local-master ()
  "Add local variable for the top module."
  (interactive)
  (if (and (buffer-file-name)
           (string-match tla-one-master
                         (file-name-nondirectory (buffer-file-name)))
           (not buffer-read-only))
      (progn
        (goto-char (point-max))
        (if (re-search-backward (concat "^\\([^\n]+\\)Local " "Variables:")
                                (- (point-max) 3000) t)
            (let ((prefix (tla-match-buffer 1)))
              (re-search-forward (regexp-quote (concat prefix
						       "End:")))
              (beginning-of-line 1)
              (insert prefix "tla-master: " (prin1-to-string tla-master) " " comment-end "\n"))
          (insert "\n" comment-start "Local Variables: " comment-end "\n"
                  comment-start "tla-current-variant: " tla-current-variant comment-end "\n"
                  comment-start "tla-master: " (prin1-to-string tla-master) 
		  comment-end "\n" comment-start "End: " comment-end "\n")))))

(defun tla-specfile-list ()
  ;; return a list with spec files in current directory
  (let ((specdir (file-name-directory (buffer-file-name (current-buffer)))))
    (directory-files specdir nil "\\.t$" nil)))

;;;
;;; Paths
;;;

(defvar tla-check-path (append (list "./") tla-specfiles-private tla-specfiles-global)
  "*Directory path to search for dependencies.

If nil, just check the current file.
Used when checking if any files have changed.")

;;;
;;; Commands
;;;

;;; this code is taken from the excellent AUC TeX package (9.1f)
;;; and adapted to fit into the TLA framework.
;;; All credits of code that is unaltered except naming scheme
;;; thus go to Per Abrahamsen, the maintainer. To be precise:
;;; Parts of this code relating to the idea of using a master
;;; file are under GPL with the following copyrights:
;;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;;; Copyright (C) 1987 Lars Peter Fischer
;;; Copyright (C) 1991 Kresten Krab Thorup
;;; Copyright (C) 1993, 1994 Per Abrahamsen 

;; taken from tex-buf.el

(defvar tla-process-asynchronous (not (eq system-type 'ms-dos))
  "*Use asynchronous processes.")

(defvar tla-shell
  (if (memq system-type '(ms-dos emx))
      shell-file-name
    "/bin/sh")
  "Name of shell used to parse  commands.")

(defvar tla-shell-command-option
  (cond ((eq system-type 'ms-dos) 
	 (if (boundp 'shell-command-option)
	     shell-command-option
	   "/c"))
	(t				;Unix & EMX (Emacs 19 port to OS/2)
	 "-c"))
  "Shell argument indicating that next argument is the command.")

;;; Interactive Commands
;;
;; The general idea is, that there is one process and process buffer
;; associated with each master file, and one process and process buffer
;; for running TeX on a region.   Thus, if you have N master files, you
;; can run N + 1 processes simultaneously.  
;;
;; Some user commands operates on ``the'' process.  The following
;; algorithm determine what ``the'' process is.
;;
;; IF   last process started was on a region 
;; THEN ``the'' process is the region process
;; ELSE ``the'' process is the master file (of the current buffer) process

(defun tla-save-specification (name)
  "Save all files that belong to the current specification
and are currently visited and modified."
  ;; Return non-nil if document need to be re-TeX'ed.
  (interactive (list (tla-master-file)))
  (if (string-equal name "")
      (setq name (tla-master-file)))
  (let ((specfiles (tla-specfile-list)))
    (if (not (member name specfiles))
	(cons name specfiles))
    (tla-check-files specfiles)))

(defun tla-command-master ()
  "Run command on the current document."
  (interactive)
  (tla-command (tla-command-query (tla-master-file)) 'tla-master-file))

(defvar tla-command-region-begin nil)
(defvar tla-command-region-end nil)
;; Used for marking the last region.

(make-variable-buffer-local 'tla-command-region-begin)
(make-variable-buffer-local 'tla-command-region-end)

(defun tla-command-region (&optional old regionname)
  "Run a command on the current region.

Query the user for a command to run on the temporary file specified by
the variable tla-region.  If the chosen command is so marked in
tla-command-list, and no argument (or nil) is given to the command,
the region file file be recreated with the current region.  If mark is
not active, the new text in the previous used region will be used."

;;If the master file for the document has a header, it is written to the
;;temporary file before the region itself.  The document's header is all
;;text after tla-header-end.

;;If the master file for the document has a trailer, it is written to
;;the temporary file before the region itself.  The document's trailer is
;;all text after TeX-trailer-start."
  (interactive "P")
  (let (command regionfilename specinsert)
    (if regionname
	(progn
	  (setq command (tla-command-query (buffer-name)))
	  (setq regionfilename (buffer-name)))
      (setq command (tla-command-query (tla-region-file)))
      (setq regionfilename (tla-region-file tla-default-extension)))
    ;;((command (tla-command-query (tla-region-file))))
    (if (null (nth 4 (assoc command tla-command-list)))
	()
      (if (string= command "LaTeX Spec92")
	  (progn
	    (setq specinsert t)
	    (setq regionfilename
		  (concat (tla-strip-extension (buffer-name)) ".ltx")))
	(setq specinsert nil))
      (if (and (tla-mark-active) (not old))
	  (let ((begin (min (point) (mark)))
		(end (max (point) (mark))))
	    (if tla-command-region-begin
		()
	      (setq tla-command-region-begin (make-marker)
		    tla-command-region-end (make-marker)))
	    (set-marker tla-command-region-begin begin)
	    (set-marker tla-command-region-end end)))
      (if (null tla-command-region-begin)
	  (error "Mark not set."))
      (let ((begin (marker-position tla-command-region-begin))
	    (end (marker-position tla-command-region-end)))
	;;(tla-region-create regionfilename
	;;		   (buffer-substring begin end)
	;;		   (file-name-nondirectory (buffer-file-name))
	;;		   (count-lines (save-restriction (widen) (point-min)) begin))))
	(tla-region-create regionfilename
			   (buffer-substring begin end)
			   (file-name-nondirectory (buffer-file-name))
			   specinsert)))
    (cond ((and regionname specinsert)
	   (tla-command command 'tla-spec92-file))
	  (regionname
	   (tla-command command 'tla-buffer-file))
	  (t
	   (tla-command command 'tla-region-file)))))
;;(if regionname
;;   (tla-command command 'tla-buffer-file)
;;(tla-command command 'tla-region-file))))

(defun tla-command-buffer ()
  "Run a command on the current buffer.

Query the user for a command to run on the temporary file specified by
the variable tla-region.  The region file file be recreated from the
visible part of the buffer."
  (interactive)
  (let ((tla-command-region-begin (point-min-marker))
	(tla-command-region-end (point-max-marker)))
    (tla-command-region t (buffer-name))))

(defun tla-recenter-output-buffer (line)
  "Redisplay buffer of job output so that most recent output can be seen.
The last line of the buffer is displayed on line LINE of the window, or
at bottom if LINE is nil." 
  (interactive "P")
  (let ((buffer (tla-active-buffer)))
    (if buffer
	(let ((old-buffer (current-buffer)))
	  (pop-to-buffer buffer t)
	  (bury-buffer buffer)
	  (goto-char (point-max))
	  (recenter (if line
			(prefix-numeric-value line)
		      (/ (window-height) 2)))
	  (pop-to-buffer old-buffer))
      (message "No process for this document."))))

(defun tla-kill-job ()
  "Kill the currently running job."
  (interactive)
  (let ((process (tla-active-process)))
    (if process
	(kill-process process)
      ;; Should test for background process here.
      (error "No process to kill."))))

(defun tla-home-buffer (arg)
  "Go to the buffer where you last issued a command.  
If there is no such buffer, or you already are in that buffer, find
the master file."
  (interactive "P")
  (if (or (null tla-command-buffer)
	  (eq tla-command-buffer (current-buffer)))
      (find-file (tla-master-file tla-default-extension))
    (switch-to-buffer tla-command-buffer)))

(defun tla-next-error (reparse)
  "Find the next error in the output buffer.
Prefix by C-u to start from the beginning of the errors."
  (interactive "P")
  (if (null (tla-active-buffer))
      (error "No output buffer.")
    (funcall (tla-process-get-variable (tla-active-master) 'tla-parse-function)
	     reparse)))


;;; Command Query

(defun tla-command (name file)
  "Run command NAME on the file you get by calling FILE.

FILE is a function returning a file name.  It has one optional argument,
the extension to use on the file.

Use the information in tla-command-list to determine how to run the
command."

  (setq tla-current-process-buffer-p (or (eq file 'tla-buffer-file)
					 (eq file 'tla-spec92-file)))
  (setq tla-current-process-region-p (eq file 'tla-region-file))
  (let ((command (tla-command-expand (nth 1 (assoc name tla-command-list))
				     file))
	(hook (nth 2 (assoc name tla-command-list)))
	(confirm (nth 3 (assoc name tla-command-list))))

    ;; Verify the expanded command
    (if confirm
	(setq command
	      (read-from-minibuffer (concat name " command: ") command)))
    
    ;; Now start the process
    (tla-process-set-variable name 'tla-command-next tla-command-Show)
    (apply hook name command (apply file nil) nil)))

(defun tla-command-expand (command file &optional list)
  "Expand COMMAND for FILE as described in LIST.
LIST default to tla-expand-list."
  (if (null list)
      (setq list tla-expand-list))
  (while list
    (let ((case-fold-search nil) ; Do not ignore case.
	  (string (car (car list)))	;First element
	  (expansion (car (cdr (car list)))) ;Second element
	  (arguments (cdr (cdr (car list))))) ;Remaining elements
      (while (string-match string command)
	(let ((prefix (substring command 0 (match-beginning 0)))
	      (postfix (substring command (match-end 0))))
	  (setq command (concat prefix
				(cond ((tla-function-p expansion)
				       (apply expansion arguments))
				      ((boundp expansion)
				       (apply (eval expansion) arguments))
				      (t
				       (error "Nonexpansion %s." expansion)))
				postfix)))))
    (setq list (cdr list)))
  command)

(defun tla-check-files (specfiles)
  "Compare SPECFILES list against current buffers
and save modified buffers of current specification."
  ;; get list of modified buffers
  (let* ((buffer-names (mapcar (function
			       (lambda (current-buffer)
				 (if (buffer-modified-p current-buffer)
				     (buffer-name current-buffer))))
			      (buffer-list)))
	(buffers-to-save 
	 (delete nil 
		 (mapcar (function
			  (lambda (file)
			    (if (and (member file buffer-names)
				     ;; (tla-match-extension file))
				     (> (length file) 2)
				     (string= (substring file -2) ".t"))
				file)))
			 specfiles))))
    ;; save buffers-to-save
    (while buffers-to-save
      (let* ((buffer (get-buffer (car buffers-to-save)))
	     (name (buffer-file-name buffer)))
	(setq buffers-to-save (cdr buffers-to-save))
	(if (or (not tla-save-query)
		(y-or-n-p (concat "Save file " name "? ")))
	    (save-excursion (set-buffer buffer) (save-buffer)))))))
	     
(defvar tla-save-query t
  "*If non-nil, ask user for permission to save files before starting command.")

(defun tla-command-query (name)
  "Query the user for a what command to use."
  (or tla-command-force
      (let* ((default (cond ((and (not (string-equal name tla-region))
				  (tla-save-specification (tla-master-file)))
			     tla-command-default)
			    ((tla-process-get-variable name
						       'tla-command-next
						       tla-command-Show))
			    (tla-command-Show)))
	     (completion-ignore-case t)
	     (answer (completing-read (concat "Command: (default " default  ") ")
				      tla-command-list nil t)))
	;; If the answer "latex" it will not be expanded to "LaTeX"
	;; (setq answer (car-safe (tla-assoc answer tla-command-list)))
	(setq answer (car-safe (assoc answer tla-command-list)))
	(if (and answer
		 (not (string-equal answer "")))
	    answer
	  default))))

(defvar tla-command-next nil
  "The default command next time tla-command is invoked.")

 (make-variable-buffer-local 'tla-command-next)

;; this will probably vanish in a future release. If we need this for
;; a TeX file we could switch to that file and use the *real* AUC TeX
;; mechanism for that.

(defun tla-printer-query (&optional command element)
  "Query the user for a printer name.
COMMAND is the default command to use if the entry for the printer in
tla-printer-list does not itself have it specified in the ELEMENT'th
entry." 
  (or command (setq command tla-print-command))
  (or element (setq element 1))
  (let ((printer (if tla-printer-list
		     (let ((completion-ignore-case t))
		       (completing-read (concat "Printer: (default "
						tla-printer-default ") ")
					tla-printer-list))
		   "")))
    
    (setq printer (or (car-safe (tla-assoc printer tla-printer-list))
		      printer))
    (if (or (null printer) (string-equal "" printer))
	(setq printer tla-printer-default)
      (setq tla-printer-default printer))

    (let ((expansion (let ((entry (assoc printer tla-printer-list)))
		       (if (and entry (nth element entry))
			   (nth element entry)
			 command))))
      (if (string-match "%p" printer)
	  (error "Don't use %s in printer names." "%p"))
      (while (string-match "%p" expansion)
	(setq expansion (concat (substring expansion 0 (match-beginning 0))
				printer
				(substring expansion (match-end 0)))))
      expansion)))

;;;
;;; Command Hooks
;;;

(defvar tla-show-compilation nil
  "*If non-nil, show output of compilation in other window.")

(defun tla-run-command (name command file)
  "Create a process for NAME using COMMAND to process FILE.
Return the new process."
  (let ((default tla-command-default)
	(buffer (tla-process-buffer-name file)))
    (tla-process-check file)		; Check that no process is running
    (setq tla-command-buffer (current-buffer))
    (get-buffer-create buffer)
    (set-buffer buffer)
    (erase-buffer)
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    ;; (setq mode-name name)
    (if tla-show-compilation
	(display-buffer buffer)
      (message "Type `C-c C-l' to display results of compilation."))
    (if (string= name "Check")
	    (setq tla-parse-function 'tla-parse-tpp)
      (setq tla-parse-function 'tla-parse-command))
    (setq tla-command-default default)
    (setq tla-sentinel-function
	  (function (lambda (process name)
		      (message (concat name ": done.")))))
    (if tla-process-asynchronous
	(let ((process (start-process name buffer tla-shell
				      tla-shell-command-option command)))
	  ;; (TeX-command-mode-line process)
	  (set-process-filter process 'tla-command-filter)
	  (set-process-sentinel process 'tla-command-sentinel)
	  (set-marker (process-mark process) (point-max))
	  (setq compilation-in-progress (cons process compilation-in-progress))
	  process)
      ;; (setq mode-line-process ": run")
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0)				; redisplay
      (call-process tla-shell nil buffer nil
		    tla-shell-command-option command))))

(defun tla-run-format (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (let ((process (tla-run-command name command file)))
    ;; Hook to TeX debuger.
    ;;(TeX-parse-reset)
    ;; (setq TeX-parse-function 'TeX-parse-TeX)
    (setq TeX-sentinel-function 'tla-TeX-sentinel)
    ;;    (if tla-process-asynchronous
    ;;	(progn
    ;;	  ;; Updating the mode line.
    ;;	  (setq TeX-current-page "[0]")
    ;;	  (TeX-format-mode-line process)
    ;;	  (set-process-filter process 'TeX-format-filter)))
    process))

(defun tla-run-TeX (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (let ((process (tla-run-format name command file)))
    (if tla-process-asynchronous
	process
      (tla-synchronous-sentinel name file process))))

(defun tla-run-compile (name command file)
  "Ignore first and third argument, start compile with second argument."
  (compile command))

(defun tla-run-shell (name command file)
  "Ignore first and third argument, start shell-command with second argument."
  (shell-command command)
  (if (eq system-type 'ms-dos)
      (redraw-display)))

(defun tla-run-discard (name command file)
  "Start process with second argument, discarding its output."
  (process-kill-without-query (start-process (concat name " discard")
					     nil tla-shell
					     tla-shell-command-option
					     command)))

(defun tla-run-background (name command file)
  "Start process with second argument, show output when and if it arrives."
  (let ((process (start-process (concat name " background")
				nil tla-shell
				tla-shell-command-option command)))
    (set-process-filter process 'tla-background-filter)
    (process-kill-without-query process)))

;;;
;;; Command Sentinels
;;;

(defun tla-synchronous-sentinel (name file result)
  "Process command output buffer after the process dies."
  (let* ((buffer (tla-process-buffer file)))
    (save-excursion
      (set-buffer buffer)
      
      ;; Append post-mortem information to the buffer
      (goto-char (point-max))
      (insert "\n" mode-name (if (and result (zerop result))
				 " finished" " exited") " at "
	      (substring (current-time-string) 0 -5))
      (setq mode-line-process ": exit")
      
      ;; Do command specific actions.
      (setq tla-command-next tla-command-Show)
      (goto-char (point-min))
      (apply tla-sentinel-function nil name nil)
      
      ;; Force mode line redisplay soon
      (set-buffer-modified-p (buffer-modified-p)))))

(defun tla-command-sentinel (process msg)
  "Process command output buffer after the process dies."
  (let* ((buffer (process-buffer process))
	 (name (process-name process)))
    (cond ((null (buffer-name buffer))	; buffer killed
	   (set-process-buffer process nil)
	   (set-process-sentinel process nil))
	  ((memq (process-status process) '(signal exit))
	   (save-excursion
	     (set-buffer buffer)
	     
	     ;; Append post-mortem information to the buffer
	     (goto-char (point-max))
	     (insert "\n" mode-name " " msg)
	     (forward-char -1)
	     (insert " at "
		     (substring (current-time-string) 0 -5))
	     (forward-char 1)
	     
	     ;; Do command specific actions.
	     (setq tla-command-next tla-command-Show)
	     (goto-char (point-min))
	     (apply tla-sentinel-function process name nil)
	     
	     
	     ;; If buffer and mode line will show that the process
	     ;; is dead, we can delete it now.  Otherwise it
	     ;; will stay around until M-x list-processes.
	     (delete-process process)
	     
	     ;; Force mode line redisplay soon
	     (set-buffer-modified-p (buffer-modified-p))))))
  (setq compilation-in-progress (delq process compilation-in-progress)))


(defvar tla-sentinel-function (function (lambda (process name)))
  "Hook to cleanup command buffer after termination of PROCESS.
NAME is the name of the process.")

  (make-variable-buffer-local 'tla-sentinel-function)

(defun tla-TeX-sentinel (process name)
  "Cleanup TeX output buffer after running TeX.
Return nil ifs no errors were found."
  ;; (if process (TeX-format-mode-line process))
  (if (re-search-forward "^! " nil t)
      (progn
	(message (concat name " errors in `" (buffer-name)
			 "'. Use C-c ` to display."))
	(setq tla-command-next tla-command-default)
	t)
    (setq tla-command-next tla-command-Show)
    nil))

;;;
;;; Process Control
;;;

;; This variable is shared with `compile.el'.
(defvar compilation-in-progress nil
  "List of compilation processes now running.")

(or (assq 'compilation-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-in-progress " Compiling")
				 minor-mode-alist)))

(defun tla-process-get-variable (name symbol &optional default)
  "Return the value in the process buffer for NAME of SYMBOL.

Return DEFAULT if the process buffer does not exist or SYMBOL is not
defined."
  (let ((buffer (tla-process-buffer name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (if (boundp symbol)
	      (eval symbol)
	    default))
      default)))

(defun tla-process-set-variable (name symbol value)
  "Set the variable SYMBOL in the process buffer to VALUE.
Return nil iff no process buffer exist."
  (let ((buffer (tla-process-buffer name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (set symbol value)
	  t)
      nil)))

(defun tla-process-check (name)
  "Check if a process for the tla document NAME already exist.
If so, give the user the choice of aborting the process or the current
command."
  (let ((process (tla-process name)))
    (cond ((null process))
	  ((not (eq (process-status process) 'run)))
	  ((yes-or-no-p (concat "Process `"
				(process-name process)
				"' for document `"
				name
				"' running, kill it? "))
	   (delete-process process))
	  (t
	   (error "Cannot have two processes for the same document.")))))

(defun tla-process-buffer-name (name)
  "Return name of TLA buffer associated with the document NAME."
  (concat "*" (abbreviate-file-name (expand-file-name name)) " output*"))

(defun tla-process-buffer (name)
  "Return the TLA buffer associated with the document NAME."
  (get-buffer (tla-process-buffer-name name)))

(defun tla-process (name)
  "Return TLA process associated with the document NAME."
  (and tla-process-asynchronous
       (get-buffer-process (tla-process-buffer name))))

;;;
;;; Process Filters
;;;

(defun tla-command-filter (process string)
  "Filter to process normal output."
  (save-excursion
    (set-buffer (process-buffer process))
    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers string)
      (set-marker (process-mark process) (point)))))

(defvar tla-parse-function nil
  "Function to call to parse content of command output buffer.")
 (make-variable-buffer-local 'tla-parse-function)

(defun tla-background-filter (process string)
  "Filter to process background output."
  (let ((old-window (selected-window))
	(pop-up-windows t))
    (pop-to-buffer "*TeX background*")
    (insert string)
    (select-window old-window)))

;;;
;;; Active Process
;;;

(defvar tla-current-process-region-p nil
  "This variable is set to t iff the last command is on a region.")

(defvar tla-current-process-buffer-p nil
  "This variable is set to t iff the last command is on a buffer.")

(defun tla-active-process ()
  "Return the active process for the current buffer."
  (cond (tla-current-process-buffer-p
	 (tla-process (tla-buffer-file)))
	(tla-current-process-region-p
	 (tla-process (tla-region-file)))
	(t
	 (tla-process (tla-master-file)))))

(defun tla-active-buffer ()
  "Return the buffer of the active process for this buffer."
  (cond (tla-current-process-buffer-p
	 (tla-process-buffer (tla-buffer-file)))
	(tla-current-process-region-p
	 (tla-process-buffer (tla-region-file)))
	(t
	 (tla-process-buffer (tla-master-file)))))

(defun tla-active-master (&optional extension)
  "The master file currently being compiled."
  (cond (tla-current-process-buffer-p
	 (tla-buffer-file extension))
	(tla-current-process-region-p
	 (tla-region-file extension))
	(t
	 (tla-master-file extension))))

(defvar tla-command-buffer nil
  "The buffer from where the last TeX command was issued.")

;;;
;;; Region File
;;;

(defun tla-region-create (file region original specinsert)
  "Create a new file named FILE with the string REGION
The region is taken from ORIGINAL starting at line OFFSET.

The current buffer and master file is searched, in order to ensure
that the TeX header and trailer information is also included.

SPECINSERT indicates whether header and trailer must be inserted
in order to use the spec92 LaTeX style."
  ;;The OFFSET is used to provide the debugger with information about the
  ;;original file."
  (let* (;; We shift buffer a lot, so we must keep track of the buffer
	 ;; local variables.  
	 (header-end tla-header-end)
	 (trailer-start tla-trailer-start)
	 
	 ;; We search for header and trailer in the master file.
	 (master-name (tla-master-file tla-default-extension))
	 (master-buffer (find-file-noselect master-name))
	 
	 ;; And insert them into the FILE buffer.
	 (file-buffer (find-file-noselect file))

	 ;; Header contains document information for LaTeX spec92 style
	 (header (if (not specinsert)
		     ""
		   (concat tla-spec92-header "-------| MODULE "
			   (tla-strip-extension file (cons "ltx" tla-file-extensions))
			   " |------\n")))
	 
	 (trailer (if (not specinsert)
		      ""
		    (concat "\n" tla-spec92-trailer))))

	 ;; We search for the header from the master file, if it is
	 ;; not present in the region.
	 ;;(header (if (string-match header-end region)
	 ;;	     ""
	 ;;	   (save-excursion
	 ;;	     (save-restriction
	 ;;	       (set-buffer master-buffer)
	 ;;	       (widen)
	 ;;	       (goto-char (point-min))
	 ;;	       ;; NOTE: We use the local value of
	 ;;	       ;; tla-header-end from the master file.
	 ;;	       (if (not (re-search-forward tla-header-end nil t))
	 ;;		   ""
	 ;;		 (re-search-forward "[\r\n]" nil t)
	 ;;		 (buffer-substring (point-min) (point)))))))
	 
	 ;; We search for the trailer from the master file, if it is
	 ;; not present in the region.
	 ;;(trailer-offset 0)
	 ;;(trailer (if (string-match trailer-start region)
	 ;;	      ""
	 ;;	    (save-excursion
	 ;;	      (save-restriction
	 ;;		(set-buffer master-buffer)
	 ;;		(widen)
	 ;;		(goto-char (point-max))
	 ;;		;; NOTE: We use the local value of
	 ;;		;; TeX-trailer-start from the master file.
	 ;;		(if (not (re-search-backward tla-trailer-start nil t))
	 ;;		    ""
	 ;;		  (beginning-of-line 1)
	 ;;		  (setq trailer-offset
	 ;;			(count-lines (point-min) (point)))
	 ;;		  (buffer-substring (point) (point-max))))))))
    (save-excursion
      (set-buffer file-buffer)
      (erase-buffer)
      (insert header region trailer)
      ;; some minor changes
      (goto-char (point-min))
      (while (re-search-forward "\\$" nil t)
	(replace-match "\\$"))
      ;;(insert "\\message{ !name(" master-name ")}"
      ;;      header
      ;;     "\n\\message{ !name(" original ") !offset(")
      ;;(insert (int-to-string (- offset
      ;;				(count-lines (point-min) (point))))
      ;;	      ") }\n"
      ;;	      region
      ;;	      "\n\\message{ !name("  master-name ") !offset(")
      ;;     (insert (int-to-string (- trailer-offset
      ;;			(count-lines (point-min) (point))))
      ;;     ") }\n"
      ;;    trailer)
      (save-buffer 0))))

(defun tla-region-file (&optional extension)
  "Return tla-region file name with EXTENSION."
  (cond ((eq extension t)
	 (concat tla-region "." tla-default-extension))
	(extension
	 (concat tla-region "." extension))
	(t
	 tla-region)))

(defvar tla-region "_region_"
  "*Base name for temporary file for use with tla-region.")

(defun tla-buffer-file (&optional extension)
  "Return buffer file name with EXTENSION."
  (buffer-name))

(defun tla-spec92-file (&optional extension)
  "Return buffer file name with LaTeX extension."
  (concat (tla-strip-extension (buffer-name)) ".ltx"))

;; end of former AUC TeX code

;;;
;;; Parsing of TPP output
;;;

;;; - Global Parser Variables

(defvar tla-error-point 1
  "How far we have parsed until now.")

 (make-variable-buffer-local 'tla-error-point)

(defvar tla-error-file nil
  "Current file in which errors have occured")

 (make-variable-buffer-local 'tla-error-file)

(defvar tla-error-offset nil
  "Add this to any line numbers from tpp.")

 (make-variable-buffer-local 'tla-error-offset)

(defun tla-parse-reset ()
  "Reset all variables used for parsing TPP output."
  (setq tla-error-point (point-min))
  (setq tla-error-offset nil)
  (setq tla-error-file nil))

;;; - Parsers Hooks

(defun tla-parse-command (reparse)
  "We can't parse anything but TPP."
  (error "I cannot parse %s output, sorry."
	 (if (tla-active-process)
	     (process-name (tla-active-process))
	   "this")))

(defun tla-parse-tpp (reparse)
  "Find the next error produced by running TPP.
Prefix by C-u to start from the beginning of the errors.

If the file occurs in an included file, the file is loaded (if not
already in an Emacs buffer) and the cursor is placed at the error."

  (let ((old-buffer (current-buffer)))
    (pop-to-buffer (tla-active-buffer))
    (if reparse
	(tla-parse-reset))
    (goto-char tla-error-point)
    (tla-parse-tpp-error old-buffer)))

;;; Parsing TPP

(defun tla-parse-tpp-error (old)
  "Goto next error.  Pop to OLD buffer if no more errors are found."
  (while
      (progn
	(re-search-forward (concat "\\("
				   "^at line \\|"
				   "\\.\\.\\.\'"
				   "\\)") nil t)
	(let ((string (tla-match-buffer 1)))

	  (cond ((string= string "at line ")
		 (tla-tpp-error)
		 nil)
		;; New file
		((string= string "...'")
		 (re-search-forward "[A-Za-z0-9]*")
		 (setq tla-error-file (tla-match-buffer 0))
		 (setq tla-error-offset 0)
		 t)
		;; Hook to change line numbers
		((string-match "!offset(\\([---0-9]*\\))" string)
		 (setq tla-error-offset
			 (string-to-int (substring string
						   (match-beginning 1)
						   (match-end 1))))
		 t)
		;; Hook to change file name
		((string-match "!name(\\([^)]*\\))" string)
		 (setq tla-error-file (substring string
						   (match-beginning 1)
						   (match-end 1)))
		 t)
		;; No more errors.
		(t
		 (message "No more errors.")
		 (beep)
		 (pop-to-buffer old)
		 nil))))))

(defun tla-tpp-error ()
  "Display an error."
  (let* (;; We need the line number to position the cursor.
	 (line (if (re-search-forward "\\([0-9]+\\)" nil t)
		   (string-to-int (tla-match-buffer 1))
		 1))


	 ;; We need the error message to show the user.
	 (error (progn
		  (if (looking-at ": ")
		      (skip-chars-forward ":\\ *"))
		  (re-search-forward "\\(.*\\)")
		  (tla-match-buffer 1)))

	 ;; We may use these in another buffer.
	 (offset tla-error-offset)
	 ;; Here we should also look for other allowed extensions
	 (file (concat tla-error-file "." tla-default-extension)))
	 
    ;; Remember where we were.
    (setq tla-error-point (point))

    ;; Find the error.
    (if (null file)
	(error "Error occured after last file closed."))
    
    (find-file-other-window file)
    (goto-line (+ offset line))
    (if (not (string= string " "))
	(search-forward string nil t))))


;;;
;;; Regexps
;;;

;;; probably this has to be carefully reviewed by someone else

(defvar tla-indent-column tla-indent)

;; this one is currently not used
(defvar tla-keyword-header-regexp-list
  '(("IMPORT\\|\\EXPORT\\|OPAQUE\\|INCLUDE")	; module header keywords
    ("PARAMETERS\\|BOOLEANS\\|CONSTANTS\\|CONST\\s-\\ASSUMPTIONS") ; constants keywords
    ("==\\|/\\\\\\|\\\\\\/\\|::")))	; tla-sym-{eqdef,land,lor,eosf}

(defconst tla-lorand-regexp "\\(/\\\\\\|\\\\\/\\)"
  "Regexp describing the logical symbols used for conjunctions
and disjunctions.")

(defconst tla-quantifier-regexp
  "\\(:A:\\|:E:\\|:AA:\\|:EE:\\)"
  "Regexp describing all allowed quantifier in TLA+.")

(defconst tla-normid-regexp
  "[A-Za-z][A-Za-z0-9]*"
  "Regexp describing a normal identifier in TLA+.")

;(defconst tla-deflhs-regexp
;  (concat "\\(\\(" tla-normid-regexp "\\|INFIX\\|INFIXR\\)_"
;	  tla-normid-regexp "_\\|" tla-normid-regexp "\\ *\(.*\)\\)\\ *==")
;  "Regexp describing the left-hand side of a symbol definition.")

;(defconst tla-deflhs-regexp
;  (concat "\\(" 
;	  "\\(" tla-normid-regexp "\\|INFIX\\|INFIXR\\)_*_"
;	  "\\|" tla-normid-regexp "\\ *\(.*\)"
;	  "\\)\\ *==")
;  "Regexp describing the left-hand side of a symbol definition.")

(defconst tla-formal-paramdecl-regexp
  (concat tla-normid-regexp "\\(\\ *(.*)\\|\\ *\\)"
	  "\\(\\ *\\|\\ *:\\ *" tla-normid-regexp "\\)\\ *")
  "Regexp describing the formal parameter declaration.")

(defconst tla-deflhs-regexp
  (concat tla-formal-paramdecl-regexp ".*==")
  "Regexp describing the left-hand side of a symbol definition.")

(defconst tla-module-header-keyw-regexp
  "IMPORT\\|EXPORT\\|INCLUDE"
  "Regexp describing keywords used in a module header.")

;(defconst tla-const-module-keyw-regexp
;  (concat tla-module-header-keyw-regexp "\\|"
;	  "PARAMETERS\\|BOOLEANS\\|CONSTANTS\\|ASSUMPTIONS\\|CONSTANTS\\s \\(ASSUMPTIONS\\|THEOREMS\\)")
;  "Regexp describing keywords used in CONSTANT modules.")

(defconst tla-const-module-keyw-regexp
  (concat tla-module-header-keyw-regexp "\\|"
	  "PARAMETERS\\|BOOLEANS\\|CONSTANTS\\|ASSUMPTIONS\\|CONSTANTS\\s ASSUMPTIONS\\|CONSTANTS\\s THEOREMS")
  "Regexp describing keywords used in CONSTANT modules.")

(defconst tla-temp-module-only-keyw-regexp
  "PREDICATES\\|STATE\\ FUNCTIONS\\|TRANSITION\\ FUNCTIONS\\|ACTIONS\\|ACTION\\ ASSUMPTIONS\\|ACTION\\ THEOREMS\\|TEMPORALS\\|TEMPORAL\\ THEOREMS"
  "Regexp describing keywords that may only be used in TEMPORAL modules.")

(defconst tla-temp-module-keyw-regexp
  (concat tla-const-module-keyw-regexp "\\|" tla-temp-module-only-keyw-regexp)
  "Regexp describing keywords used in TEMPORAL modules.")

(defconst tla-prefix-keyw-regexp
  "IF\\|ELSE\\|CASE\\|LET\\|IN"
  "Regexp describing keywords that can introduce a formula line.")

(defconst tla-sorts-regexp
  "CONSTANT\\|BOOLEAN\\|PREDICATE\\|ACTION\\|\\(STATE\\|TRANSITION\\)\\ F\\(CN\\|UNCTION\\)\\|TEMPORAL"
  "Regexp describing sorts in TLA+.")

;;;
;;; Indentation
;;;

(defun tla-indent-line ()
  "Indent current line respecting tTLA+ conventions."
  (interactive)
  (setq indent-tabs-mode nil)
  (let ((indent (calculate-tla-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward "\\ ")		; to first non-ws char
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	nil
      (delete-region beg (point))
      (indent-to indent))
    ;; If initial point was within line's indentation,
    ;; position after the indentation. Else stay at same point.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

(defun calculate-tla-indent (&optional parse-start)
  "Return appropriate indentation for current line as tTLA+ spec
code. Normally returns an integer: the column to indent to."
  (save-excursion
    (beginning-of-line)
    (setq case-fold-search nil)
    (if parse-start
	(goto-char parse-start))
    ;; First look at the stuff in the current line
    ;; perhaps we can already decide where to indent to
    (let ((case-fold-search nil) (beg (point)))
      (cond ((looking-at "\\ *$")
	     (tla-empty-line-indent))
	    ((looking-at (concat "\\ *\\(END\\|" 
				 "\\(CONSTANT \\|TEMPORAL \\)*MODULE\\|"
				 tla-module-header-keyw-regexp
				 tla-temp-module-keyw-regexp "\\)"))
	     ;; 1: keyword introducing class of symdefs or empty line
	     ;;         line must begin at left margin
	     left-margin)
	    ((looking-at "^\\ *(\\*")
	     ;; 2: potentially indented comment
	     ;;    use a special defun
	     (tla-comment-indent))
	    ((and (looking-at (concat "\\ *" tla-deflhs-regexp))
		  (or (tla-empty-line -1 t)
		      (tla-looking-at -1 tla-temp-module-keyw-regexp t)))
	     ;; 3: LHS of symbol definition
	     ;;    must have a previous line that is empty
	     ;;    or has a proper keyword
	     tla-indent)
	    ((and (looking-at (concat "\\ *" tla-deflhs-regexp))
		  (re-search-backward "LET" nil t))
	     ;; 4: symbol definition of LET statement found
	     ;;    return indentation of LET
	     (+ 4 (current-column)))
	    ((looking-at "\\ *;")
	     ;; 5: semicolon closing symdefn
	     (if (< 0 (car (tla-in-formula)))
		 tla-indent
	       (tla-parse-error "Missing LHS of formula or definition")))
	    ((looking-at "\\ *\\(\}\\|)\\|\\]\\).*$")
	     ;; 6: closing of sets, records, groupings
	     ;;    return indentation of corresponding opener
	     (let (grop)
	       (setq grop (car (cdr (parse-partial-sexp (tla-bof) (point)))))
	       (if (natnump grop)
		   (progn
		     (goto-char grop)
		     (current-column))
		 (tla-parse-error "Corresponding opener of grouping missing."))))
	    ((looking-at "\\ *ELSE")
	     ;; 7: ELSE branch
	     ;;    return indentation of beginning of THEN
	     ;;    otherwise complain about missing THEN branch
	     (re-search-backward "THEN" (tla-bof) t)
	     (if (looking-at "THEN")
		 (current-column)
	       (tla-parse-error "Missing THEN branch")))
	    ((looking-at "\\ *IN")
	     ;; 8: where there is an IN there must also be a LET
	     (if (re-search-backward "LET" (tla-bof) t)
		 (current-column)
	       (tla-parse-error "No corresponding LET found")))
	    ((looking-at ".*\\(WITH.*<-\\|<-\\).*$")
	     ;; 9: substitution part of INCLUDE statement
	     (let (withpart beg-include grop)
	       (save-excursion
		 (if (looking-at ".*WITH")
		     (setq withpart t))
		 (if (re-search-backward "INCLUDE\\ " nil t)
		     (progn
		       (setq beg-include (match-beginning 0))
		       (setq grop (car (cdr (parse-partial-sexp beg-include (point)))))
		       (cond ((natnump grop)
			      (goto-char grop)
			      (forward-char)
			      (skip-chars-forward "\\ ")
			      (current-column))
			     (withpart
			      (re-search-forward "INCLUDE\\ *" nil t)
			      (current-column))
			     (t
			      (re-search-forward "WITH\\ *" nil t)
			      (current-column))))
		   (tla-parse-error "INCLUDE keyword missing")))))
	    ;;		     (progn
	    ;;		       (if withpart
	    ;;			   (progn			 
	    ;;			     (re-search-forward "INCLUDE\\ *" nil t)
	    ;;			     (current-column))
	    ;;			 (re-search-forward "WITH\\ *" nil t)
	    ;;			 (current-column)))
	    ;;     (tla-parse-error "INCLUDE keyword missing")))))
	    ((and (looking-at "\\ *OPAQUE") (tla-looking-at -1 "EXPORT"))
	     ;; 10: OPAQUE as continued EXPORT statement
	     (forward-line -1)
	     (re-search-forward "EXPORT\\ *" nil t)
	     (current-column))
	    ((tla-looking-at -1 (concat "PARAMETERS\\ *" tla-formal-paramdecl-regexp))
	     ;; 11: PARAMETERS statement containing param declaration
	     (forward-line -1)
	     (re-search-forward "PARAMETERS\\ *" nil t)
	     (current-column))
	    (t
	     ;; so we're very probably inside a symbol definition
	     ;; let's first determine its beginning
	     (let (beginning-of-symdef logop lorand-indent indent-offset openparen)
	       (save-excursion
		 ;; (re-search-backward tla-deflhs-regexp nil t)
		 (setq beginning-of-symdef (tla-bof)))
	       ;; before further examining check out for bulleted lorand
	       ;; structures
	       (if (and tla-bullet-lorand
			(looking-at (concat "\\ *" tla-lorand-regexp ".*$")))
		   ;;	(or (looking-at (concat "\\ *" tla-lorand-regexp ".*$"))
		   ;;	    (and (natnump beginning-of-symdef)
		   ;;	 (re-search-backward tla-lorand-regexp beginning-of-symdef t))))
		   (progn
		     ;; which one?
		     ;;(setq logop (buffer-substring (match-beginning 0)
		     ;;				   (1+ (match-beginning 0))))
		     ;;   (if (looking-at (concat "\\ *" tla-lorand-regexp))
		     (if (looking-at "\\ */\\\\")
			 (setq logop "/\\\\")
		       (setq logop "\\\\/"))
		     (setq lorand-indent
			   (tla-lorand-indent logop beginning-of-symdef (point)
					      (- (re-search-forward tla-lorand-regexp nil) 2) t))))
	       ;; at this point we should now look for every type of structure
	       ;; that we might be in, because we have to account correctly
	       ;; for open braces, parens etc.
	       (if (natnump beginning-of-symdef)
		   (setq openparen (car (cdr (parse-partial-sexp beginning-of-symdef (point))))))
	       (if (natnump openparen)
		   (progn
		     (save-excursion
		       (goto-char openparen)
		       (if (looking-at "\\ *[^\\ ].*$")
			   (progn
			     (forward-char)
			     (skip-chars-forward "\\ ")))
		       (if (and (natnump lorand-indent)
				(< lorand-indent (current-column)))
			   (setq lorand-indent (current-column))
			 (if (not (natnump lorand-indent))
			     (setq lorand-indent (current-column)))))))
		 
	       ;; if that fails we could look at the previous line
	       (progn
		 ;; 5: now we have to look at previous line
		 ;; must be replaced by a defun that looks thru the whole symdef
		 (forward-line -1)
		 ;; skip empty lines
		 ;; this should be avoided by convention
		 (while (or (eolp) (looking-at "\\ *$"))
		   (forward-line -1))
		 (beginning-of-line)
		 (cond ((looking-at ".*[|].*$")
			;; 5a: open set description
			;;     return pos. of vertical bar
			(re-search-forward "|" nil)
			(if (not (looking-at ".*}.*$"))
			    (+ 1 (current-column))
			  (if (not (looking-at ".*;.*$"))
			      (+ tla-indent tla-inner-indent)
			    ;; (tla-parse-error "Expression invalid outside definition"))))
			    tla-indent)))
		       ((looking-at ".*::")
			;; 5b: end of scope
			;;     return pos. of eoscope
			(re-search-forward "::" nil)
			(+ 1 (current-column)))
		       ((looking-at (concat "\\ *.*" tla-quantifier-regexp))
			;; 5c: quantifier
			;;     return pos. of sec.char of quantifier
			(re-search-forward ":[AE]" nil)
			(if (looking-at ".*::.*$")
			    ;; if there is still a scope, go for it
			    (progn
			      (re-search-forward "::" nil)
			      (1+ (current-column)))
			  (- (current-column) 1)))
		       ((looking-at (concat "\\ *" tla-prefix-keyw-regexp))
			;; 5e: keyword introducing building block
			(re-search-forward tla-prefix-keyw-regexp nil)
			(if (and (natnump lorand-indent)
				 (> lorand-indent (current-column)))
			    lorand-indent
			  (+ 1 (current-column))))
		       ((looking-at "\\ *THEN")
			;; 5g: keyword THEN
			(re-search-forward "THEN" nil)
			(match-beginning 0))
		       ((looking-at (concat "\\ *" tla-deflhs-regexp))
			;; 5h: start of definition
			(if (looking-at (concat "\\ *" tla-deflhs-regexp "\\ *"
						tla-lorand-regexp ".*$"))
			    lorand-indent
			  (+ tla-indent tla-inner-indent)))
		       ;; otherwise return default indentation
		       (t
			(if (natnump lorand-indent)
			    ;; at least there is a bulleted lorand
			    lorand-indent
			  (if (natnump beginning-of-symdef)
			      (progn
				(goto-char beg)
				(forward-line -1)
				(if (> (current-indentation) (+ tla-indent tla-inner-indent))
				    (current-indentation)
				  (+ tla-indent tla-inner-indent)))
			    ;; no criterium found, return default
			    tla-indent)))))))))))

;; currently not used
(defun tla-symdef-indent (bof current)
  "Return point of last open parentheses structure."
  ;; Look at the kind of structure that we're in and pay
  ;; attention to the current open structure
  (save-excursion
    (let ((parse-sexp-ignore-comments nil))
      (goto-char bof)
      (parse-partial-sexp bof current))))


(defun tla-empty-line-indent ()
  "Return the indentation for the current empty line.
This is normally the left margin, but it might make sense
to automatically indent to the position of a lorand if that
is on the line above. Same holds possibly for comments."
  ;; 0: current empty line, indent to left-margin
  ;;    except if the line above contains bulleted lorand
  ;;    cond will then eval to nil and the tla-bullet-lorand
  ;;    condition further down will handle that
  (if (not (and tla-bullet-lorand
		(tla-looking-at -1 (concat ".*" tla-lorand-regexp))))
      (if (tla-looking-at -1 (concat "\\ *" (regexp-quote comment-start)))
	  ;; previous line is comment
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (re-search-forward (regexp-quote comment-start))
	      (goto-char (match-beginning 0))
	      (current-column)))
	left-margin)
    ;; so there is a bulleted lorand somewhere in the line
    ;; above, let's get the necessary input for tla-bullet-lorand.
    (let (beginning-of-symdef logop bolorand lorand-indent beg)
      (setq beg (point))
      (save-excursion
	(re-search-backward tla-deflhs-regexp nil t)
	(setq beginning-of-symdef (match-beginning 0)))
      ;; we know for sure that there is a lorand the line above
      ;; so we can quickly access its location and its type
      (forward-line -1)
      ;; it doesn't matter which lorand to feed tla-bullet-lorand
      (setq logop "/\\\\")
      (setq bolorand (- (re-search-forward tla-lorand-regexp nil) 2))
      (setq lorand-indent
	    (tla-lorand-indent logop beginning-of-symdef beg bolorand nil))
      ;; return indentation
      lorand-indent)))

(defun tla-lorand-indent (logop bosymdef bocurrline bolorand logop-match)
  "Return the indentation of the innermost bulleted lorand if
that matches LOGOP. The lorand is searched within BOSYMDEF and
BOCURRLINE. BOLORAND marks the beginning of the LOGOP on the
current line. If LOGOP-MATCH is nil, there is no matching thus
it definitely returns the indentation of the innermost lorand.
Return nil if no LOGOP could be found."

  ;; current line might perhaps start with a bulleted lorand
  ;; this is nearly guaranteed to be aligned at the last same rightmost
  ;; lorand that is found previously in the definition.
  ;; This method can fail when there are two different bulleted
  ;; lorand exprs, cos there is no way to determine whether a bulleted
  ;; lorand expr is finished or not. The only way out of this dilemma
  ;; is to write spec code in a way that avoids this situation.
  ;; Return the indentation of that very innermost lorand that is
  ;; outside a closed grouping

  (let (indent-to candidate (lorandsleft t))
    (save-excursion
      (while (and (< bosymdef bocurrline) lorandsleft)
	(goto-char bosymdef)
	(if (re-search-forward tla-lorand-regexp bocurrline t)
	    ;; yes there is one, check if it is the same one
	    (progn
	      (if (or (not logop-match)
		      (string-equal (buffer-substring bolorand (1+ bolorand))
				    (buffer-substring (match-beginning 0)
						      (1+ (match-beginning 0)))))
		  ;; yes, we found a matching pair, store column
		  ;; but first make sure that this doesn't match a lorand
		  ;; which is inside a grouping that has already been closed
		  (progn
		    (setq candidate (match-beginning 0))
		    (if (not (natnump (car (cdr (parse-partial-sexp bosymdef candidate)))))
			(progn
			  (goto-char candidate)
			  (setq indent-to (current-column)))))) ; matches 2nd inner if
	      ;; nope, found the other one, but hey, perhaps there's more
	      (setq bosymdef (match-end 0)))
	  ;; nope, we didn't find any (more) occurrence of lorand so
	  ;; let's get out of here
	  (setq lorandsleft nil))))
    ;; now indent-to contains the desired lorand or it is still void
    (if (not (boundp 'indent-to))
	(setq indent-to -1))
    indent-to))


(defun tla-comment-indent ()
  "Return indentation for comment line according to context."
  (save-excursion
    (cond ((tla-looking-at 1 (concat "\\ *" tla-deflhs-regexp))
	   ;; next line symbol definition
	   (save-excursion
	     (forward-line 1)
	     (re-search-forward tla-deflhs-regexp nil)
	     (goto-char (match-beginning 0))
	     (current-column)))
	  ((tla-looking-at -1 (concat "\\ *" (regexp-quote comment-start)))
	   ;; previous line comment
	   (save-excursion
	     (forward-line -1)
	     (re-search-forward (regexp-quote comment-start) nil)
	     (goto-char (match-beginning 0))
	     (current-column)))
	  ((tla-looking-at -1 tla-temp-module-keyw-regexp)
	   tla-indent)
	  ((tla-looking-at -1 (concat "\\ *" tla-deflhs-regexp))
	   ;; inside symbol definition
	   (save-excursion
	     (forward-line -1)
	     (re-search-forward "==" nil)
	     (1+ (current-column))))
	  ((/= 0 (car (tla-in-formula)))
	   (+ tla-indent tla-inner-indent))
	  (t left-margin))))

;; this doesn't yet work
(defun tla-indent-region (start end)
  "Indent the current region from start to end."
  (save-excursion
    (goto-char start)
    (and (bolp) (not (eolp))
	 (tla-indent-line))))
;    (let ((endmark (copy-marker end)))
;      (indent-sexp endmark)
;      (set-marker endmark nil))))

(defun tla-newline ()
  "Start new line and indent to current tab stop."
  (interactive)
  (let ((tla-cc (current-indentation)))
    (newline)
    (indent-to tla-cc)))

;;;
;;; Outlining
;;;

(defun tla-outline-regexp ()
  "Return regexp for matching TLA+ symbol definitions and formulas."
  ;; this fails for LET definitions inside other defs
  ;; who can provide a regexp that matches any BUT "LET"??
  ;;  (concat "\\ *" tla-normid-regexp "\\ *==\\|"
  ;;	  "\\ *" tla-normid-regexp "\\ *\(.*\)\\ *=="))
  (concat "\\(" tla-temp-module-keyw-regexp "\\|"
	  "\\ *" tla-normid-regexp "\\ *==\\|"
	  "\\ *" tla-normid-regexp "\\ *\(.*\)\\ *=="
	  "\\)"))

(defun tla-outline-level ()
  "Find the level of current outline heading in a tTLA+ spec."
  (cond ((looking-at (concat "\\ *" tla-temp-module-keyw-regexp)) 1)
	((looking-at (concat "\\ *" tla-deflhs-regexp)) 2)
	(t
	 (if (or (looking-at (concat "\\ *MODULE"))
		 (looking-at (concat "\\ *END")))
	     1
	   (tla-parse-error "Unrecognized heading.")))))

;;;
;;; Font Locking
;;;

(defconst tla-font-lock-keywords
  (list
   '("(\\*.*\\*)" . font-lock-comment-face)
;;   '("[Hh][Rr][Ee][Ff]=\"\\([^\"]*\\)\"" 1 font-lock-string-face t)
  ;; '("[Ss][Rr][Cc]=\"\\([^\"]*\\)\"" 1 font-lock-string-face t))
  "Patterns to highlight in TLA buffers."))

(defun tla-fontify ()
  (font-lock-mode 1)
  (make-local-variable 'font-lock-keywords) 
  (setq font-lock-keywords tla-font-lock-keywords)
  (font-lock-fontify-keywords-region (point-min) (point-max)))
;  (font-lock-hack-keywords (point-min) (point-max))) ; durant
; rename: https://emacsformacosx.com/emacs-bzr/trunk/lisp/ChangeLog.6

;;
;; Ops
;;

(defconst tla-sym-eqdef "==")		; equals by definition
(defconst tla-sym-land "/\\")		; logical and
(defconst tla-sym-lor "\\/")		; logical or
(defconst tla-sym-eosf "\;")	; end of spec formula
(defconst tla-math-exists ":EE")
(defconst tla-math-forall ":A")
(defconst tla-math-inset ":in:")

;;;
;;; Templates / Keywords
;;;

(defvar tla-header-list '((?\C-s "untyped" "")
			  (?\C-c "CONSTANT" "CONSTANT ")
			  (?\C-t "TEMPORAL" "TEMPORAL "))
  "*List of module header types used by tla-header.

Each entry is a list with three elements.  The first element is the
key indicating the type of header.  The second element is the entry
appearing on the menu, and the third element is the string to insert
just in front of the keyword MODULE.")

(defvar tla-insert-header-p t)

(defvar tla-delete-header-p nil)

(defun tla-header (dummy which)
  "Insert TLA+ module header."
  (interactive "*P\nc")
  (setq case-fold-search nil)
  (setq tla-insert-header-p t)
  (save-excursion
    ;; check for existing header
    (goto-char (point-min))
    (if (re-search-forward "MODULE" nil t)
        (progn
          (setq tla-delete-header-p
		(y-or-n-p "Do you want to delete existing header? "))
          ;; don't do anything if existing header should not be deleted
          (if (not tla-delete-header-p)
              (setq tla-insert-header-p nil)
            (tla-delete-header)))))  ;; check for existing END keyword
  (if (not (re-search-forward "END" nil t))
      (save-excursion
	(goto-char (point-max))
	(insert "\nEND\n")))
  (if tla-insert-header-p
      (progn
        ;; okay, insert full header
        (goto-char (point-min))
        (insert (nth 2 (assoc which tla-header-list)) "MODULE "
                (tla-strip-extension (buffer-name) tla-file-extensions))
        (insert "\n" comment-start (buffer-name) " --- " comment-end)
        ;; (indent-to end-comment-column)
        (insert "\n" comment-start "Created:\t" (current-time-string))
	(tla-pad-comments)
        (insert "\n" comment-start "Author: \t" (user-full-name)
                " <" (user-login-name) "@" (system-name) "> ")
	(tla-pad-comments)
        (if tla-use-vc (insert "\n" comment-start "$Id$"))
	(tla-pad-comments)
        (insert "\n\n")
        (goto-char (point-min))
        (re-search-forward "--- " nil))))

(defun tla-pad-comments ()
  (indent-to end-comment-column)
  (insert comment-end))


(defun tla-delete-header ()
  "Delete module header of current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; why on earth does  "\s>\n\S<" not work? (GRR)
    ;; RMS thinks that it might be useful to extend it for two-char
    ;; comments, but he doesn't "want to do this now."
    (re-search-forward "\*\)\n[^\(]" (point-max) nil)
    ;; delete region containing header
    (delete-region 1 (point))))

(defvar tla-keyword-list '((?\C-i "IMPORT"		"IMPORT")
			   (?\C-x "EXPORT"		"EXPORT")
			   (?\C-l "INCLUDE"		"INCLUDE")
			   (?\C-p "PARAMETERS"		"PARAMETERS")
			   (?\C-b "BOOLEANS"		"BOOLEANS")
			   (?\C-c "CONSTANTS"		"CONSTANTS")
			   (?\C-j "CONST ASSUMPTIONS"	"CONSTANTS ASSUMPTIONS")
			   (?\C-k "CONST THEOREMS"	"CONSTANTS THEOREMS")
			   (?\C-r "PREDICATES"		"PREDICATES")
			   (?\C-s "STATE FUNCTIONS"    	"STATE FUNCTIONS")
			   (?\C-a "ACTIONS"		"ACTIONS")
			   (?\C-f "TRANSITION FUNCTIONS" "TRANSITION FUNCTIONS")
			   (?\C-t "TEMPORALS"		"TEMPORALS")
			   (?\C-o "ACTION ASSUMPTIONS"	"ACTION ASSUMPTIONS")
			   (?\C-n "ACTION THEOREMS"	"ACTION THEOREMS")
			   (?\C-h "TEMPORAL THEOREMS"	"TEMPORAL THEOREMS"))
  "*List of TLA+ keywords used by tla-keyword.

Each entry is a list with three elements.  The first element is the
key inserting the appropriate keyword.  The second element is the entry
appearing on the menu, and the third element is the string that will be
inserted on a line by its own.")

(defun tla-keyword (dummy which)
  "Insert TLA+ keyword template WHICH"
  (interactive "*P\nc")
  (setq case-fold-search nil)
  (let ((beg (point))
	(menuentry (nth 1 (assoc which tla-keyword-list)))
	(template (nth 2 (assoc which tla-keyword-list))))
    ;; first check if template is allowed at all
    (if (tla-template-allowed template)
	(progn
	  (tla-insert-keyword beg template)
	  (if (tla-template-present-p template)
	      ;; issue warning if template is already present
	      (tla-parse-warning (concat "Template " template " already used")))))))

(defun tla-insert-keyword (insert-pos template)
  "Insert TEMPLATE at INSERT-POS."
  ;; first check if we're inside a body of statements
  (let ((formula-range (tla-in-formula)))
    (if (/= 0 (car formula-range))
	;; yes, we're inside a formula, out of here
	(setq insert-pos (car (cdr formula-range))))
    (if (not (string-match "IMPORT\\|EXPORT\\|INCLUDE\\|PARAMETERS" template))
       (progn
	 (save-excursion
	   (goto-char insert-pos)
	   (if (not (eolp)) (end-of-line))
	   (insert "\n" template "\n")
	   (tla-indent-line)))
      (goto-char insert-pos)
      (if (not (eolp)) (end-of-line))
      (insert "\n" template " ;\n")
      (re-search-backward (concat template " ") nil)
      (goto-char (match-end 0)))))

(defun tla-template-allowed (template)
  "Check TEMPLATE if it is allowed to insert it in the current
module and variant. Return non-nil if it is, otherwise signal
an error."

  ;; This is also the right place to check for idiosyncrasies of
  ;; a certain variant of TLA+. Tell the user what's wrong!
  (save-excursion
    ;; is this a CONSTANT module?
    (goto-char (point-min))
    (if (looking-at "CONSTANT MODULE")
	(if (string-match tla-const-module-keyw-regexp
			  template)
	    t
	  ;; just don't use it in here
	  (tla-parse-error "Template not allowed in CONSTANT MODULE"))
      t)))

(defun tla-template-present-p (template)
  "Check TEMPLATE if it is already present in the current module.
Return nil if not present otherwise return t."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward 
	 (concat "\\(" template "\\|" menuentry "\\)") (point-max) t)
	;; template is already present, so user gets warning signaled
	t)))
    
;;;
;;; Commenting
;;;

(defun tla-end-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to end-comment-column))
  (insert comment-end))

;;(fset 'tla-comment-region 'comment-region)

(defun tla-comment-region (level)
  "Inserts LEVEL \(*'s at the beginning and *\)'s at the end
of every line in the current region."
  (interactive "*p")
  (save-excursion
    (let ((beg (mark)) (end (point)) (comment-free nil))
      (if (< beg end)
	  (goto-char beg)
	(goto-char end))
      (beginning-of-line)
      (while (and (< (point) (max beg end))
		  (not comment-free))
	(if (not (looking-at "^\\ *(\\*"))
	    (setq comment-free t))
	(forward-line 1))
      (if comment-free
	  (comment-region beg end level)))))
	

(defun tla-comment-paragraph (level)
  "Inserts LEVEL \(*'s at the beginning and *\)'s at the end
of every line in the current paragraph."
  (interactive "*p")
  (save-excursion
    (mark-paragraph)
    (comment-region (point) (mark) level)))

(defun tla-comment-symdef (level)
  "Inserts LEVEL \(*'s at the beginning and *\)'s at the end
of every line in the current paragraph."
  (interactive "*p")
  (save-excursion
    (setq formula-range (tla-in-formula))
    (if (= 0 (car formula-range))
	(tla-parse-error "Not within symbol definition")
      (comment-region (car formula-range) (car (cdr formula-range)) level))))

(defun tla-un-comment-region (start end level)
  "Remove up to LEVEL comment characters from each line in the region."
  (interactive "*r\np") 
  (comment-region start end (- level)))

;;;;;;;;;;;

(defvar tla-file-extensions '("t" "t+")
  "*File extensions used by manually generated TLA spec files.")

(defvar tla-all-extensions '("[^.\n]+")
  "All possible file extensions.")

(defvar tla-default-extension "t"
  "*Default extension for TLA spec files.")

(make-variable-buffer-local 'tla-default-extension)

(defun tla-match-extension (file &optional extensions)
  "Return non-nil if FILE has an one of EXTENSIONS.

If EXTENSIONS is not specified or nil, the value of
tla-file-extensions is used instead."

  (if (null extensions)
      (setq extensions tla-file-extensions))

  (let ((regexp (concat "\\.\\("
                        (mapconcat 'identity extensions "\\|")
                        "\\)$")))
    (string-match regexp file)))

(defun tla-strip-extension (&optional string extensions nodir nostrip)
  "Return STRING without any trailing extension in EXTENSIONS.
If NODIR is set, also remove directory part of STRING. 
If NOSTRIP is set, do not remove extension after all.
STRING defaults to the name of the current buffer.
EXTENSIONS defaults to tla-file-extensions."
  
  (if (null string)
      (setq string (or (buffer-file-name) "<none>")))
  
  (if (null extensions)
      (setq extensions tla-file-extensions))
  
  (let ((strip (if (and (not nostrip)
                        (tla-match-extension string extensions))
                   (substring string 0 (match-beginning 0))
                 string)))
    (if nodir
        (file-name-nondirectory strip)
      strip)))

(defun tla-function-p (arg)
  "Return non-nil if ARG is callable as a function."
  (or (and (fboundp 'byte-code-function-p)
	   (byte-code-function-p arg))
      (and (listp arg)
	   (eq (car arg) 'lambda))
      (and (symbolp arg)
	   (fboundp arg))))

(defun tla-member (elt list how)
  "Returns the member ELT in LIST.  Comparison done with HOW.

Return nil if ELT is not a member of LIST."
  (while (and list (not (funcall how elt (car list))))
    (setq list (cdr list)))
  (car-safe list))

(defun tla-assoc (elem list)
  "Like assoc, except case incentive."
  (let ((case-fold-search t))
    (tla-member elem list
		(function (lambda (a b)
		  (string-match (concat "^" (regexp-quote a) "$")
				(car b)))))))
(defun tla-match-buffer (n)
  "Return the substring corresponding to the n-th match.

See match-data for details."
  (buffer-substring (match-beginning n) (match-end n)))


(defvar tla-in-header-p nil
  "*If non-nil, point is inside a module header.")

;;;
;;; Determine range of symbol definitions
;;;

(defun tla-in-symdef-p ()
  "*If non-nil, point is inside a symbol definition delimited by
tla-op-eqdef and tla-op-eosf."
  (if (/= 0 (car (tla-in-formula)))
      t
    nil))

(defun tla-in-symdef-line-p ()
  "Return nil if not on a line that belongs to a symbol definition.
Otherwise retur the point of the beginning of line."
  (let ((excl-deflhs-regexp (concat "\\(^\\ *\n\\|^\\ *(\\*\\|^\\ *==\\|"
				    "^\\ *\\(" tla-temp-module-keyw-regexp "\\)\\|END\\)"))
	(incl-deflhs-regexp (concat "^\\ *" tla-deflhs-regexp))
	(search-deflhs-regexp (concat "\\(^\\ *\n\\|^\\ *(\\*\\|==\\|"
				      "^\\ *[A-Z][A-Z][A-Z]\\)")))
    (save-excursion
      (beginning-of-line)
      ;; next three conditions definitely don't introduce a symbol definition
      (cond ((looking-at excl-deflhs-regexp)
	     nil)
	    ((looking-at incl-deflhs-regexp)
	     (point))
	    ;; we are right at the beginning of a symbol definition
	    ;; t)
	    ((bobp) nil)
	    (t
	     ;; this regexp search is critical so we use a regexp
	     ;; that is very simple in contrast to the exact regexp
	     (goto-char (re-search-backward search-deflhs-regexp nil))
	     (tla-in-symdef-line-p))))))

(defun tla-eof ()
  "Return nil if not in symbol definition else return point
of the last character belonging to the current symbol definition."
  ;; This function is independent from empty lines between two
  ;; symbol definitions.
  (let (eof bof)
    (save-excursion
      (setq eof (search-forward ";"))
      (skip-chars-forward " \t\n")
      (skip-chars-backward " \t")
      (setq bof (tla-bof))
      (if (or (not bof)
	      (= bof (point)))
	  eof
	(tla-eof)))))

(defun tla-bof ()
  "Return nil if not in symbol definition else return point
of the first character of the line which starts the definition."
  ;; This function is not independent from empty lines between
  ;; two symbol definitions.
  (save-excursion
    (let ((possbeg (tla-in-symdef-line-p)))
      (if possbeg
	  (progn
	    (while (and possbeg
			(progn
			  (goto-char possbeg)
			  (forward-line -1)
			  (setq possbeg (tla-in-symdef-line-p)))))
	    (forward-line 1)
	    (beginning-of-line)
	    (point))))))

(defun tla-in-formula ()
  "Return a list containing BEG and END indicating the range of
the current symbol definition. BEG is 0 if point is not in inside
a symbol definition."
  (save-excursion
    (let ((beg (tla-bof)) end)
      (if beg
	  ;; if t then we already have beginning of symbol definition
	  (list beg (tla-eof))
	(list 0 nil)))))
	
(defun tla-empty-line (skiplines &optional skipcomments)
  ;; Return t if the line, which is relatively addressed as
  ;; current line + skiplines, is empty (nothing, ws or tab)
  ;; Return nil otherwise
  (save-excursion
    (beginning-of-line)
    (if skipcomments
	(tla-skip-comments skiplines)
      (forward-line skiplines))
    (if (looking-at "^\\ *$")
	t
      nil)))

(defun tla-looking-at (skiplines regexp &optional skipcomments)
  ;; Return t if looking at the regexp in line skiplines
  ;; is true. skiplines is the number of lines to go from
  ;; the current line
  (save-excursion
    (beginning-of-line)
    (if skipcomments
	(tla-skip-comments skiplines)
      (forward-line skiplines))
    (if (looking-at regexp)
	t
      nil)))

(defun tla-skip-comments (skiplines)
  ;; skip over comment lines
  ;; excursion should have been saved previously
  (let (ctr direction)
    (setq ctr (abs skiplines))
    (if (natnump skiplines)
	(setq direction 1)
      (setq direction -1))
    (while (> ctr 0)
      (forward-line direction)
      (while (looking-at "^\\ *(\\*")
	(forward-line direction))
      (setq ctr (- ctr 1)))))

;;;
;; Emacs 19 special

(defun tla-mark-active ()
  ;; In FSF 19 mark-active indicates if mark is active.
  mark-active)

(defun tla-active-mark ()
  (and transient-mark-mode mark-active))


;;;;

(defun tla-goto-info-page ()
  "Read documentation for TLA mode in the info system."
  (interactive)
  (require 'info)
  (Info-goto-node "(tla-mode)"))

(defun tla-submit-bug-report ()
  "Use mail to submit a bug report on TLA+ mode"
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   bug-tla-mode
   (concat "TLA+ " tla-mode-version)
   (list 'tla-current-variant
	 'tla-use-vc
	 'tla-bullet-lorand
	 'tla-command-list
	 )
   nil nil
   "Remember to cover the basics, i.e. what you expected to happen and
what in fact did happen."))

(provide 'tla-mode)

;;; tla-mode.el ends here
