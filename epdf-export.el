;;; epdf-export.el --- Export Emacs buffers to PDF with epdf  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `epdf-export-buffer' writes the current buffer to a PDF document,
;; drawing every character with the font Emacs itself would use to
;; display it.

;;; Code:

(require 'cl-lib)
(require 'epdf)

(defun epdf--font-family-for-char (ch)
  "Return the family name of the font Emacs uses to display CH.
Return nil if no font covers CH.  The default face is used, so text
properties are ignored."
  (let ((fc (internal-char-font nil ch)))
    (when (consp fc)
      (let ((family (font-get (car fc) :family)))
        (cond ((stringp family) family)
              ((symbolp family) (symbol-name family)))))))

(defun epdf--default-font-family ()
  "Return the family name of the font of the default face."
  (let ((family (font-get (face-attribute 'default :font) :family)))
    (cond ((stringp family) family)
          ((symbolp family) (symbol-name family)))))

(defun epdf--run-extents (glyphs font size)
  "Return (ASCENT . DESCENT) in points of the glyphs in the run GLYPHS.
FONT is the font object the run was shaped with, and SIZE the size in
points it is drawn at.  The glyph metrics are in pixels of FONT."
  (let ((scale (/ size (float (or (font-get font :size) size))))
        (ascent 0)
        (descent 0))
    (dolist (g (epdf-glyph-run-glyphs glyphs) (cons ascent descent))
      (setq ascent (max ascent (* scale (or (epdf-glyph-ascent g) 0)))
            descent (max descent (* scale (or (epdf-glyph-descent g) 0)))))))

(defun epdf--line-extents (shaped size leading)
  "Return (ASCENT . DESCENT) in points for a line made of SHAPED runs.
SHAPED is a list of (FONT-INDEX GLYPHS ADVANCE GLYPH-EXTENTS), where
GLYPH-EXTENTS is what `epdf--run-extents' returns for the run.

Every font used in the line counts with its own ascent and descent, as
in Emacs, where a line is as tall as all of its fonts need.  Except, as
Emacs does too (see FONT_TOO_HIGH), for fonts whose ascent and descent
add up to more than three times the size: for those, the extents of the
glyphs actually drawn count instead.  LEADING, split four to one, is the
minimum, so lines of ordinary text keep that spacing."
  (let ((ascent (* 0.8 leading))
        (descent (* 0.2 leading)))
    (dolist (s shaped (cons ascent descent))
      (let* ((m (nth (nth 0 s) epdf-font-line-metrics))
             (e (if (> (+ (car m) (cdr m)) 3000)
                    (nth 3 s)
                  (cons (/ (* (car m) size) 1000.0)
                        (/ (* (cdr m) size) 1000.0)))))
        (setq ascent (max ascent (car e))
              descent (max descent (cdr e)))))))

(defun epdf--fonts-for-text (text)
  "Embed a font for every character in TEXT.
Return a hash table mapping each character to its font index.  Fonts are
embedded here, before any page exists, because the Resources dictionary
of a page is built when the page is opened."
  (let ((by-char (make-hash-table))
        (by-family (make-hash-table :test #'equal)))
    (dolist (ch (append text nil) by-char)
      (unless (gethash ch by-char)
        (let ((family (or (epdf--font-family-for-char ch)
                          ;; No font covers CH.  Draw it with the font
                          ;; of the default face, which has no glyph for
                          ;; it, so shaping gives glyph 0, the .notdef
                          ;; box.  ToUnicode still maps it to CH.
                          (epdf--default-font-family))))
          (puthash ch
                   (or (gethash family by-family)
                       (puthash family (epdf-embed-ttf family) by-family))
                   by-char))))))

(defun epdf--line-runs (line fonts)
  "Split LINE into a list of (FONT-INDEX . SUBSTRING).
FONTS is a hash table as returned by `epdf--fonts-for-text'.  Consecutive
characters drawn with the same font end up in the same run."
  (let ((len (length line))
        (i 0)
        (start 0)
        (current nil)
        (runs nil))
    (while (< i len)
      (let ((index (gethash (aref line i) fonts)))
        (cond ((null current)
               (setq current index
                     start i))
              ((/= index current)
               (push (cons current (substring line start i)) runs)
               (setq current index
                     start i))))
      (cl-incf i))
    (when current
      (push (cons current (substring line start)) runs))
    (nreverse runs)))

(defun epdf-export-buffer (&optional size margin leading)
  "Export the current buffer to a PDF document in `epdf-buffer'.

This just uses the default face, no properties.
Does font substitution to print all characters that emacs can print.


SIZE is the font size in points, 10 by default.  MARGIN is the page
margin in points, 50 by default.  LEADING is the minimum distance
between baselines, by default SIZE plus a fifth.  As Emacs does, a line
grows to fit the ascent and descent of every font used in it.

An uncovered character no font covers is drawn as glyph 0 of the default face's
font, the .notdef glyph.

Signal an error if a line does not fit in the width of the page."
  (interactive)
  (let* ((size (or size 10))
         (margin (or margin 50))
         (leading (or leading (round (* size 1.2))))
         (lines (split-string (buffer-substring-no-properties
                                       (point-min) (point-max))
                                      "\n"))
         (fonts nil)
         (page-height 0)
         (usable-width 0)
         (y 0)
         (prev-descent 0)
         (last-font nil)
         (open nil))
    (epdf-begin)
    (setq fonts (epdf--fonts-for-text (apply #'concat lines)))
    (setq page-height (nth 1 epdf-paper-size))
    (setq usable-width (- (nth 0 epdf-paper-size) (* 2 margin)))
    (dolist (line lines)
      (let* ((runs (epdf--line-runs line fonts))
             (shaped
              (mapcar (lambda (run)
                        (let* ((index (car run))
                               (font (epdf--font-object-for-index
                                      index epdf--shaping-size))
                               (glyphs (epdf-shape-string font (cdr run))))
                          (list index
                                glyphs
                                (epdf--run-advance glyphs index size)
                                (epdf--run-extents glyphs font size))))
                      runs))
             (width (apply #'+ (mapcar (lambda (s) (nth 2 s)) shaped)))
             (extents (epdf--line-extents shaped size leading))
             (ascent (car extents))
             (descent (cdr extents))
             ;; Baseline of this line if it goes on the current page.
             (next-y (and open (- y prev-descent ascent))))
        (when (> width usable-width)
          (error "epdf: line does not fit (%.1f > %d points): %s"
                 width usable-width
                 (truncate-string-to-width line 40 nil nil t)))
        (if (and open (>= (- next-y descent) margin))
            ;; Td is relative. Emit the difference of the rounded
            ;; positions.
            (progn
              (epdf-text-xy (list 0 (- (round next-y) (round y))))
              (setq y next-y))
          (when open
            (epdf-text-end))
          (epdf-begin-page)
          (epdf-text-begin)
          (setq y (- page-height margin ascent))
          (epdf-text-xy (list margin (round y)))
          (setq last-font nil
                open t))
        (dolist (s shaped)
          (unless (eq (nth 0 s) last-font)
            (epdf-text-font (nth 0 s) size)
            (setq last-font (nth 0 s)))
          (epdf-text-glyph-run (nth 1 s)))
        (setq prev-descent descent)))
    (when open
      (epdf-text-end))
    (epdf-end)
    (message "epdf: %d lines, %d fonts" (length lines) (length epdf-fonts))
    epdf-buffer))

(provide 'epdf-export)

;;; epdf-export.el ends here
