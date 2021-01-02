;;; pk-bl.el --- Elisp code for the pk-bl Python package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Peter Klenner

;; Author: Peter Klenner <peterklenner@gmx.de>
;; Version: 0.0.1
;; Keywords: Manual Backlinking for org-files
;; Homepage: 

;; Package-Requires: ((emacs "25") (org "9") (ox-json "0.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun pk-bl--find-all-org-files ()
  (let ((dir (projectile-project-root)))
    (directory-files-recursively dir "\\.org$")))

(defun pk-bl--process-link ()
  (let ((id (eos/org-custom-id-get))
        (header (org-get-heading t t))
	(filename (buffer-file-name)))
    (format "- [[%s::#%s][%s]]" filename id header)))

(defun pk-bl ()
  "Insert backlinks to the current headline under a new subheadline."
  (interactive)
  (let ((id (org-entry-get (point) "custom_id")))
    (when id
      (org-insert-subheading 1)
      (insert "Backlinks\n")
      (insert
       (mapconcat
	#'identity
	(org-ql-select
	  'pk-bl--find-all-org-files
	  `(link ,id)
	  :action 'pk-bl--process-link)
	"\n")))))

(provide 'pk-bl)
