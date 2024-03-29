;;; org-bibtex-extras --- extras for working with org-bibtex entries

;; Copyright (C) 2008-2013 Free Software Foundation, Inc.

;; Author: Eric Schulte <eric dot schulte at gmx dot com>
;; Keywords: outlines, hypermedia, bibtex, d3
;; Homepage: http://orgmode.org
;; Version: 0.01

;; This file is not yet part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Warning: This should certainly be considered EXPERIMENTAL and still
;;          in development, feedback is welcome, but don't expect it
;;          to work.

;; This file add some extra functionality to your bibtex entries which
;; are stored as Org-mode headlines using org-bibtex.el.  Most
;; features expect that you keep all of your reading notes in a single
;; file, set the `obe-bibtex-file' variable to the path to this file.
;;
;; - d3 view :: d3 is a Javascript library which supports interactive
;;              display of graphs.  To view your citations as a d3
;;              graph, execute the following which will create a .json
;;              export of your references file, then grab a copy of
;;              d3, edit examples/force/force.js to replace
;;
;;                var source`"miserables.json";
;;
;;              with
;;
;;                var source`"your-references.json";
;;
;;              then view examples/force/force.html in your browser.
;;
;; - HTML export :: Customize the `obe-html-link-base' variable so
;;                  that it points to an html export of your
;;                  references, then add the following to your html
;;                  export hook, and citations will be resolved during
;;                  html export.
;;
;;	 (add-hook 'org-export-first-hook
;;	 	  (lambda ()
;;	 	    (when (equal org-export-current-backend 'html)
;;	 	      (obe-html-export-citations))))

;;; Code:
(require 'org-bibtex)

(defcustom obe-bibtex-file nil "File holding bibtex entries.")

(defcustom obe-html-link-base nil
  "Base of citation links.
For example, to point to your `obe-bibtex-file' use the following.

  (setq obe-html-link-base (format \"file:%s\" obe-bibtex-file))
")

(defvar obe-citations nil)
(defun obe-citations ()
  "Return all citations from `obe-bibtex-file'."
  (or obe-citations
      (save-window-excursion
	(find-file obe-bibtex-file)
	(goto-char (point-min))
	(while (re-search-forward "  :CUSTOM_ID: \\(.+\\)$" nil t)
	  (push (org-no-properties (match-string 1))
		obe-citations))
	obe-citations)))

(defun obe-goto-citation (&optional citation)
  "Visit a citation given its ID."
  (interactive)
  (let ((citation (or citation
		      (org-icompleting-read "Citation: "
					    (obe-citations)))))
    (find-file obe-bibtex-file)
    (goto-char (point-min))
    (when (re-search-forward (format "  :CUSTOM_ID: %s" citation) nil t)
      (outline-previous-visible-heading 1)
      t)))

(defun obe-html-export-citations ()
  "Convert all \\cite{...} citations in the current file into HTML links."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\cite{\\([^\000}]+\\)}" nil t)
      (replace-match
       (save-match-data
	 (mapconcat (lambda (c) (format "[[%s#%s][%s]]" obe-html-link-base c c))
		    (mapcar #'org-babel-trim
			    (split-string (match-string 1) ",")) ", "))))))

(defun obe-get-meta-data (citation)
  "Collect meta-data for CITATION."
  (save-excursion
    (when (obe-goto-citation citation)
      (let ((pt (point)))
	`((:authors . ,(split-string (org-entry-get pt "AUTHOR") " and " t))
	  (:title   . ,(org-no-properties (org-get-heading 1 1)))
	  (:journal . ,(org-entry-get pt "JOURNAL")))))))

(defun obe-meta-to-json (meta &optional fields)
  "Turn a list of META data from citations into a string of json."
  (let ((counter 1) nodes links)
    (flet ((id (it) (position it nodes :test #'string= :key #'car))
	   (col (k) (mapcar (lambda (r) (cdr (assoc k r))) meta))
	   (add (lst)
		(dolist (el lst) (push (cons el counter) nodes))
		(incf counter)))
      ;; build the nodes of the graph
      (add (col :title))
      (add (remove-if (lambda (author) (string-match "others" author))
		      (remove-duplicates (apply #'append (col :authors))
					 :test #'string=)))
      (dolist (field fields)
	(add (remove-duplicates (col field) :test #'string=)))
      ;; build the links in the graph
      (dolist (citation meta)
        (let ((dest (id (cdr (assoc :title citation)))))
          (dolist (author (mapcar #'id (cdr (assoc :authors citation))))
            (when author (push (cons author dest) links)))
          (let ((jid (id (cdr (assoc :journal citation)))))
            (when jid (push (cons jid dest) links)))
          (let ((cid (id (cdr (assoc :category citation)))))
            (when cid (push (cons cid dest) links)))))
      ;; build the json string
      (format "{\"nodes\":[%s],\"links\":[%s]}"
	      (mapconcat
	       (lambda (pair)
		 (format "{\"name\":%S,\"group\":%d}"
			 (car pair) (cdr pair)))
	       nodes ",")
	      (mapconcat
	       (lambda (link)
		 (format "{\"source\":%d,\"target\":%d,\"value\":1}"
			 (car link) (cdr link)))
	       (meta-to-links meta nodes) ",")))))

(provide 'org-bibtex-extras)
;;; org-bibtex-extras ends here
