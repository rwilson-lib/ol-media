;;; ol-media.el --- Org-mode support for media link   -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ramus Jabee Lloyd Wilson

;; Author: Ramus Jabee Lloyd Wilson
;; URL: "https://github.com/rwilson-lib/ol-media"

;; Keywords: files, outlines, hypermedia
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (mpv "0.35.1"))

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

;; org-mode links to media files

;;; Code:

(require 'ol)
(eval-when-compile
  (require 'subr-x))

(require 'mpv)

(defvar org-media-link-keymap (make-sparse-keymap))
(define-key org-media-link-keymap (kbd "C->") 'mpv-seek-forward)
(define-key org-media-link-keymap (kbd "C-<") 'mpv-seek-backward)
(define-key org-media-link-keymap (kbd "C-SPC") 'mpv-pause)

(org-link-set-parameters "media"
			 :follow #'org-media-open
			 :export #'org-media-export
			 :keymap (append org-media-link-keymap org-mouse-map))



(defcustom ol-media-supported-exts
  '(("mp3" . ("audio" "" ""))
    ("wav" . ("audio" "" ""))
    ("ogg" . ("audio" "" ""))
    ("mp4" . ("video" "" ""))
    ("webm" . ("video" "" "")))
  "Alist of supported link file extensions and their types.

Each element in the alist has the format
`(EXTENSION . (TYPE SUBTYPE PARAMETERS))`,where:
- `EXTENSION` is a string representing the file extension
`(e.g., `mp3`, `mp4`, etc.)`
- `TYPE` is a string representing the link type
`(values, `audio`, `video`)`
- `PROG` is a string representing the program used to open link
`(e.g., `mpv`, `vlc`, etc.)`
- `PARAMETERS` is an optional string representing additional parameters
for the link type (e.g., codecs, etc.)

This variable is used to configure media-related functionality in the `ol-media` module."
  :type '(alist :key-type symbol :value-type (string string string))
  :group 'ol-media)

(defun org-media-keymap ()
  "Define a keymap for mylink."
  (define-key org-media-link-map (kbd "C->") 'mpv-seek-forward)
  (define-key org-media-link-map (kbd "C-<") 'mpv-seek-backward))


(defun org-media-open (path _)
  "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the man command."
  ;; (funcall org-media-command path))
  (if (null (mpv--url-p path))
      (mpv-play path)
    (mpv-play-url path)))

  
(defun org-media-export (link description format _)
  "Export a link file linked from Org files."
    (pcase format
      (`html (org-media--prepare-html-link link description))
      (`latex (format "\\href{%s}{%s}" link description))
      (`texinfo (format "@uref{%s,%s}" link description))
      (`ascii (format "%s (%s)" description link))
      (t link)))

(defun org-media--prepare-html-link (link description)
  "Docstr."
  (let* ((ext (file-name-extension link))
	 (type (car (cdr (assoc ext ol-media-supported-exts)))))
    (pcase type
      ("audio" (format "<audio controls title=\"%s\">
  <source src=\"%s\" type=\"audio/%s\">
  Your browser does not support the audio element.
</audio>" description link ext))

      ("video" (format "<video controls title=\"%s\">
  <source src=\"%s\" type=\"video/%s\">
  Your browser does not support the video tag.
</video>" description link type))

      (t (format "<a target=\"_blank\" href=\"%s\">%s</a>" link description)))))

(provide ol-media)
;;; ol-media.el ends here

;; (setq mpv-default-options '())

;; (mpv--position-insert-as-org-item "01:10")


;; --start=01:10:00


;; (add-to-list 'mpv-default-options "--geometry=80%x70%")
;; (add-to-list 'mpv-default-options "--screenshot-directory=~/tmp/screenshoot")
;; (add-to-list 'mpv-default-options "--screenshot-template=mpv-shot-%F~%P~%03n")

;; (expand-file-name "mpv-" (temporary-file-directory)


;; (mpv-run-command "screenshot")
;; (directory-files "~/tmp/screenshoot/" t "mpv")

;; (mpv-get-playback-position)
