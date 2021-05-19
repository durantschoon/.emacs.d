;;; hl7-mode.el --- Major mode for editing HL7v2 messages

;; Copyright (C) 2015  Bartłomiej Kruczyk

;; Author: bkruczyk <bartlomiej.kruczyk@gmail.com>
;; Keywords: HL7
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/bkruczyk/hl7-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing HL7v2 messages

;;; Code:

(defvar hl7-mode-font-lock-keywords
  (list
   '("^\\(...\\)|" (1 font-lock-keyword-face))
   '("\\^\\||\\|~\\|\\&\\|\\\\" . font-lock-constant-face)))

(defun hl7-mode-forward-node ()
  "Go to next HL7 field, component or subcomponent."
  (interactive)
  (goto-char (re-search-forward "[|^&]")))

(defun hl7-mode-backward-node ()
  "Go to previous HL7 field, component or subcomponent."
  (interactive)
  (goto-char (re-search-backward "[|^&]")))

(defvar hl7-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'hl7-mode-forward-node)
    (define-key map [backtab] 'hl7-mode-backward-node)
    map))

(defun hl7-mode-tag-thing-at-point ()
  "Get the tag of thing at point."
  (interactive)
  (save-excursion
    (let* ((end (point))
           (start (re-search-backward "^"))
           (i start)
           (c (char-after start))
           (field 0)
           (comp 1)
           (subcomp 1)
           (header (buffer-substring-no-properties start (+ start 3))))

      (while (< i end)
        (cond ((char-equal ?| c)
               (progn
                 (setq field (1+ field))
                 (setq comp 1)
                 (setq subcomp 1)))
              ((char-equal ?^ c)
               (progn
                 (setq comp (1+ comp))
                 (setq subcomp 1)))
              ((char-equal ?& c)
               (setq subcomp (1+ subcomp))))
        (setq i (1+ i))
        (setq c (char-after i)))

      (if (< field 1)
          nil
          (format "%s-%s.%s.%s" header field comp subcomp)))))

;;;###autoload
(define-derived-mode hl7-mode fundamental-mode "HL7"
  "Major mode for editing HL7v2 messages.

\\{hl7-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(hl7-mode-font-lock-keywords))
  (set (make-local-variable 'eldoc-documentation-function) 'hl7-mode-tag-thing-at-point)
  (eldoc-add-command "hl7-mode-forward-node" "hl7-mode-backward-node")
  (eldoc-mode +1))

(provide 'hl7-mode)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hl7-mode.el ends here
