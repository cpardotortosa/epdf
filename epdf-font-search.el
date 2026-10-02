;;; epdf-font-search.el --- Find the file of a font for epdf  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `epdf-find-file-for-font' finds the file, and the face in it for a
;; font collection, of the font Emacs knows by a family name.

;;; Code:

(require 'cl-lib)
(require 'epdf-ttf)

(defvar epdf--family-names-cache (make-hash-table :test #'equal)
  "Family names in each font file, as `epdf-ttf-extract-family-names'
returns them, keyed by file name.  The search in a font directory reads
every file until it finds the font; with this, each file is read once,
and looking for another font does not read them all again.  A file that
could not be read maps to nil.  `epdf-begin' empties it, so fonts
installed or removed since the last document are seen.")

(defun epdf--cached-family-names (file)
  "Return the family names in the font file FILE.
The first time, FILE is read with `epdf-ttf-extract-family-names', and
the result kept in `epdf--family-names-cache'.  If reading FILE signals
an error, nil is kept, so it is not read again, and the error goes on."
  (let ((cached (gethash file epdf--family-names-cache 'missing)))
    (if (not (eq cached 'missing))
        cached
      (puthash file nil epdf--family-names-cache)
      (puthash file (epdf-ttf-extract-family-names file)
               epdf--family-names-cache))))

(defun epdf--find-file-for-font-in-directory (directory font-name)
  "Looks for a font with the given name in the given
directory. Returns (file . font-index). font-index will be nil for
normal TrueType files, a zero-based index for collections"

  (message "Looking for font '%s' in [%s]" font-name directory)
  (let ((files (directory-files directory t))
        (case-fold-search t)
        return )
    ;; Delete elements that don't have a TrueType extension.
    (setq files (cl-remove-if (lambda (f)
                                (and
                                 (not (string-match-p "\\.ttc$" f))
                                 (not (string-match-p "\\.otc$" f))
                                 (not (string-match-p "\\.ttf$" f))
                                 (not (string-match-p "\\.otf$" f))))
                              files))
    (setq font-name (upcase font-name))
    (dolist (f files)
      (unless return
        (condition-case nil
            (let* ((family-names (epdf--cached-family-names f)))
              (if (stringp family-names) ;; single name
                  (when (string= font-name (upcase family-names))
                    (setq return (cons f nil)))
                (setq family-names (mapcar #'upcase family-names))
                (let ((index (cl-position font-name family-names :test #'equal)))
                  (when index
                    (setq return (cons f index))))))
          
          ((debug error) (message "%s FAILED" f))
          
          )
      ))
  return))

(defun epdf--font-file-from-emacs (finfo)
  "Return (FILE . INDEX) for FONT-NAME if the font backend knows them.
Return nil otherwise, as happens on MS-Windows, where fonts are resolved
by name and no file is ever involved.

FINFO is the vector returned by `font-info' for FONT-NAME.  The FreeType
backends keep the file and the face index in the `:font-entity' property
of the font entity, which is what fontconfig reports.  Other backends may
still report a file in `font-info', and then the index is 0."
  (let* ((entity (ignore-errors
                   (find-font (font-spec :name (aref finfo 0)))))
         (fe (and entity (font-get entity :font-entity))))
    (cond ((and (consp fe)
                (stringp (car fe))
                (integerp (cdr fe)))
           fe)
          ((stringp (aref finfo 12))
           (cons (aref finfo 12) nil)))))

(defun epdf--find-file-for-font-in-windows (font-name)
  "Return (FILE . 0) for FONT-NAME, looking in the MS-Windows font directories.
Return nil if it is in neither of them.  The index is always 0 because
this search does not look inside collections."
  (or (epdf--find-file-for-font-in-directory
       (expand-file-name "Fonts" (getenv "windir"))
       font-name)
      (epdf--find-file-for-font-in-directory
       (expand-file-name "AppData/Local/Microsoft/Windows/Fonts"
                         (getenv "USERPROFILE"))
       font-name)))

(defun epdf-find-file-for-font (font-name)
  "Return (FILE . INDEX) for the font named FONT-NAME.
FILE is the font file and INDEX the face index inside it, which is 0
unless FILE is a font collection.

Where the font backend deals with files, as on GNU/Linux, both come from
Emacs itself.  On MS-Windows nothing does, so the font directories are
searched instead."
  (let ((finfo (font-info font-name)))
    (unless finfo
      (error "epdf: font `%s' not found" font-name))
    (or (epdf--font-file-from-emacs finfo)
        (and (eq system-type 'windows-nt)
             (epdf--find-file-for-font-in-windows font-name))
        (error "epdf: no font file found for `%s'" font-name))))

(provide 'epdf-font-search)

;;; epdf-font-search.el ends here
