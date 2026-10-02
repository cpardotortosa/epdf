;;; epdf-ttf.el --- Read and write TrueType fonts for epdf  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; epdf-ttf-*: Functions and variables to extract information from ttf
;; files. Not a general purpose library, as it only implements what is
;; needed for PDF generation in this library. It gives its results in
;; global variables, so only a file can be processed at a time.

;; There are also functions for writing TTF files, embedded into PDF files.

;; epdf-ttf--bits-*: Functions and variables to read binary data from
;; ttf file. Integers are big endian.

;;; Code:

(require 'cl-lib)

(defvar epdf-ttf--bits-data nil
  "The contents of a TTF file are loaded here for processing.")
(defvar epdf-ttf--bits-index nil
  "While processing a TTF file, this points to the next byte.")

(defun epdf-ttf--bits-u8 (&optional n)
  "Read a byte from the TTF file in process and returns it.
With N, skip N of them."
  (if n (cl-loop repeat n collect (epdf-ttf--bits-u8))
    (prog1 (logand #xff (aref epdf-ttf--bits-data epdf-ttf--bits-index))
      (cl-incf epdf-ttf--bits-index))))

(defun epdf-ttf--bits-u16(&optional n)
  "Read a 16 bit unsigned integer from the TTF file in process and return it.
With N, skip N of them."
  (if n (cl-loop repeat n collect (epdf-ttf--bits-u16))
    (logand #xFFFF
            (logior (ash (epdf-ttf--bits-u8) 8)
                    (epdf-ttf--bits-u8)))))

(defun epdf-ttf--bits-i16(&optional n)
  "Read a 16 bit signed integer from the TTF file in process and return it.
With N, skip N of them."
  (if n (cl-loop repeat n collect (epdf-ttf--bits-i16))
    (let ((u16 (epdf-ttf--bits-u16)))
      (if (>= u16 #x8000) (- u16 #x10000) u16))))

(defun epdf-ttf--bits-u32(&optional n)
  "Read a 32 bit unsigned integer from the TTF file in process and return it.
With N, skip N of them."
  (if n (cl-loop repeat n collect (epdf-ttf--bits-u32))
    (logand #xFFFFFFFF
            (logior (ash (epdf-ttf--bits-u8) 24)
                    (ash (epdf-ttf--bits-u8) 16)
                    (ash (epdf-ttf--bits-u8) 8)
                    (epdf-ttf--bits-u8)))))

(defun epdf-ttf--bits-u64(&optional n)
  "Read a 64 bit unsigned integer from the TTF file in process and return it.
With N, skip N of them."
  (if n (cl-loop repeat n collect (epdf-ttf--bits-u64))
    (logand #xFFFFFFFFFFFFFFFF
            (logior (ash (epdf-ttf--bits-u8) 56)
                    (ash (epdf-ttf--bits-u8) 48)
                    (ash (epdf-ttf--bits-u8) 40)
                    (ash (epdf-ttf--bits-u8) 32)
                    (ash (epdf-ttf--bits-u8) 24)
                    (ash (epdf-ttf--bits-u8) 16)
                    (ash (epdf-ttf--bits-u8) 8)
                    (epdf-ttf--bits-u8)))))

(defun epdf-ttf--bits-i32 (&optional n)
  "Read a 32 bit signed integer from the TTF file in process and return it.
With N, skip N of them."
  (if n
      (cl-loop repeat n collect (epdf-ttf--bits-i32))
    (let ((u32 (epdf-ttf--bits-u32)))
      (if (>= u32 #x80000000)
          (- u32 #x100000000)
        u32))))

(defun epdf-ttf--bits-fixed ()
  "Read a signed 16.16 fixed-point value and return a float."
  (/ (float (epdf-ttf--bits-i32)) 65536.0))

(defun epdf-ttf--bits-ascii(len)
  "Read LEN chars and return a string."
  (apply #'string (cl-loop repeat len collect (epdf-ttf--bits-u8))))

(defun epdf-ttf--bits-utf-16-be(len)
  "Read LEN UTF-16 chars and return a string."
  (decode-coding-string
   (apply #'unibyte-string (cl-loop repeat len collect (epdf-ttf--bits-u8)))
   'utf-16be))

;; These are for writing. Used to build a font we extract from a
;; collection to embed into the final PDF.
(defun epdf-ttf--write-u16 (v)
  "Insert V at point as an unsigned 16 bit integer."
  (insert (logand (ash v -8) #xFF)
          (logand v #xFF)))

(defun epdf-ttf--write-u32 (v)
  "Insert V at point as an unsigned 32 bit integer."
  (insert (logand (ash v -24) #xFF)
          (logand (ash v -16) #xFF)
          (logand (ash v -8) #xFF)
          (logand v #xFF)))

;; Global variables for parsing results
(defvar epdf-ttf-units-per-em nil "A parameter of the current TTF file.")
(defvar epdf-ttf-font-bbox nil "A parameter of the current TTF file.")
(defvar epdf-ttf-num-glyphs nil "A parameter of the current TTF file.")
(defvar epdf-ttf-italic-angle nil "A parameter of the current TTF file.")
(defvar epdf-ttf-number-of-hmetrics nil "A parameter of the current TTF file.")
(defvar epdf-ttf-widths nil "A parameter of the current TTF file.")

;; The following functions parse certain blocks of the TTF file.
(defun epdf-ttf--head (_table-offset)
  "Read the `head' table of a TTF file."
  (let* ((_major-version (epdf-ttf--bits-u16))
         (_minor-version (epdf-ttf--bits-u16))
         (_revision (epdf-ttf--bits-u32))
         (_checksum (epdf-ttf--bits-u32))
         (_magic (epdf-ttf--bits-u32))
         (_flags (epdf-ttf--bits-u16))
         (units-per-em (epdf-ttf--bits-u16))
         (_created (epdf-ttf--bits-u64))
         (_modified (epdf-ttf--bits-u64))
         (xmin (epdf-ttf--bits-i16))
         (ymin (epdf-ttf--bits-i16))
         (xmax (epdf-ttf--bits-i16))
         (ymax (epdf-ttf--bits-i16)))
    ;(message "   %d units per em, box: %d %d %d %d" units-per-em xmin ymin xmax ymax)
    (setq epdf-ttf-units-per-em units-per-em)
    (setq epdf-ttf-font-bbox (list xmin ymin xmax ymax))))

(defun epdf-ttf--maxp (_table-offset)
  "Read the `maxp' table of a TTF file."
  (let* ((_version (epdf-ttf--bits-u32))
         (num-glyphs (epdf-ttf--bits-u16)))
    ;(message "   %d glyphs" num-glyphs)
    (setq epdf-ttf-num-glyphs num-glyphs)))
  
(defun epdf-ttf--post (_table-offset)
  "Read the `post' table of a TTF file."
  (let* ((_format-type (epdf-ttf--bits-u32))
         (italic-angle (epdf-ttf--bits-fixed))
         (_underline-position (epdf-ttf--bits-i16))
         (_underline-thickness (epdf-ttf--bits-i16)))
    (setq epdf-ttf-italic-angle italic-angle)))

(defun epdf-ttf--hmtx (_table-offset)
  "Glyph advances and left side bearing. We only care about the advance width."
  ;; The number of hmetrics is in the hhea table, epdf-ttf-number-of-hmetrics

  (let (widths width) ;; The last applies to all remaining glyphs.
    (dotimes (_ epdf-ttf-number-of-hmetrics)
      (setq width (epdf-ttf--bits-u16))
      (epdf-ttf--bits-i16) ;; Skip Left Side Bearings
      (push width widths))
    (setq epdf-ttf-widths (nreverse widths))))

(defun epdf-ttf--os/2 (_table-offset)
  "Read the `os/2' table of a TTF file."
  ;; TODO: Check table size, as some old fonts may have less fields for version 0.
  
  (let* ((version (epdf-ttf--bits-u16))
         (_avg-char-width (epdf-ttf--bits-u16))
         (_weight-class (epdf-ttf--bits-u16))
         (_width-class (epdf-ttf--bits-u16))
         (_fstype (epdf-ttf--bits-u16))
         (_y-subscript-x-size (epdf-ttf--bits-i16))
         (_y-subscript-y-size (epdf-ttf--bits-i16))
         (_y-subscript-x-offset (epdf-ttf--bits-i16))
         (_y-subscript-y-offset (epdf-ttf--bits-i16))
         (_y-superscript-x-size (epdf-ttf--bits-i16))
         (_y-superscript-y-size (epdf-ttf--bits-i16))
         (_y-superscript-x-offset (epdf-ttf--bits-i16))
         (_y-superscript-y-offset (epdf-ttf--bits-i16))
         (_y-strikeout-size (epdf-ttf--bits-i16))
         (_y-strikeout-position (epdf-ttf--bits-i16))
         (_family-class (epdf-ttf--bits-i16))
         (_panose (epdf-ttf--bits-u8 10))
         (_unicode-range1 (epdf-ttf--bits-u32))
         (_unicode-range2 (epdf-ttf--bits-u32))
         (_unicode-range3 (epdf-ttf--bits-u32))
         (_unicode-range4 (epdf-ttf--bits-u32))
         (_vendor-id (epdf-ttf--bits-u32))
         (_selection (epdf-ttf--bits-u16))
         (_first-char-index (epdf-ttf--bits-u16))
         (_last-char-index (epdf-ttf--bits-u16))
         (typo-ascender (epdf-ttf--bits-i16))
         (typo-descender (epdf-ttf--bits-i16))
         (_typo-line-gap (epdf-ttf--bits-i16))
         (win-ascent (epdf-ttf--bits-u16))
         (win-descent (epdf-ttf--bits-u16))
         (_code-page-range1 (if (> version 0) (epdf-ttf--bits-u32) 0))
         (_code-page-range2 (if (> version 0) (epdf-ttf--bits-u32) 0))
         (_x-height (if (> version 1) (epdf-ttf--bits-i16) 0))
         (cap-height (if (> version 1) (epdf-ttf--bits-i16) 1000))
         (_default-char (if (> version 1) (epdf-ttf--bits-u16) 0))
         (_break-char (if (> version 1) (epdf-ttf--bits-u16) 32))
         (_max-context (if (> version 1) (epdf-ttf--bits-u16) 0)))
    (setq epdf-ttf-typo-ascender typo-ascender)
    (setq epdf-ttf-cap-height cap-height)
    (setq epdf-ttf-typo-descender typo-descender)
    (setq epdf-ttf-win-ascent win-ascent)
    (setq epdf-ttf-win-descent win-descent))
    ;;(message "   win ascent: %d  win descent: %d" win-ascent win-descent)
  )

(defun epdf-ttf--hhea (_table-offset)
  (let* ((_major-version (epdf-ttf--bits-u16)) 
         (_minor-version (epdf-ttf--bits-u16)) 
         (_ascender (epdf-ttf--bits-u16))
         (_descender (epdf-ttf--bits-u16))
         (_line-gap (epdf-ttf--bits-u16))
         (advance-width-max (epdf-ttf--bits-u16))
         (_min-left-side-bearing (epdf-ttf--bits-u16))
         (_min-right-side-bearing (epdf-ttf--bits-u16))
         (_x-max-extent (epdf-ttf--bits-u16))
         (_caret-slope-rise (epdf-ttf--bits-u16))
         (_caret-slope-run (epdf-ttf--bits-u16))
         (_caret-offset (epdf-ttf--bits-u16))
         (_ignored (epdf-ttf--bits-u16 4))
         (_metric-data-format (epdf-ttf--bits-u16))
         (number-of-h-metrics (epdf-ttf--bits-u16)))
;;    (message "   advance width max: %d   #hmetrics: %d" advance-width-max number-of-h-metrics)
    (setq epdf-ttf-advance-width-max advance-width-max)
    (setq epdf-ttf-number-of-hmetrics number-of-h-metrics)))

(defun epdf-ttf--name (table-offset)
  "Extract the font family and subfamily names from the name table"
  (let* ((_v (epdf-ttf--bits-u16)) ;; name table version
         (n (epdf-ttf--bits-u16)) ;; number of name records
         (storage-offset (epdf-ttf--bits-u16))) ;; from beginning of table (table-offset
    ;;(message "    %d name records" n)
    (dotimes (_ n)
      (let (platform-id language-id name-id length offset)
        (setq platform-id (epdf-ttf--bits-u16))
        (epdf-ttf--bits-u16) ;; skip encoding-id
        (setq language-id (epdf-ttf--bits-u16)
              name-id (epdf-ttf--bits-u16)
              length (epdf-ttf--bits-u16)
              offset (epdf-ttf--bits-u16))
        ;; TODO: platform 1 (macintosh) uses different encodings.
        ;; hopefully most fonts will have also 0 or 3 
        (unless (eq platform-id 1)
          (let* ((epdf-ttf--bits-index (+ table-offset storage-offset offset))
                 (name (epdf-ttf--bits-utf-16-be length)))
            (pcase name-id
              (1
               (if (eq platform-id 3)
                   (when (eq language-id 1033)
                     (setq epdf-ttf-font-family name))
                 (unless epdf-ttf-font-family
                   (setq epdf-ttf-font-family name))))
              
              (2
               ;; For platform id 3, use only language 1033 so we get Bold, Italic, etc.
               (if (eq platform-id 3)
                   (when (eq language-id 1033)
                     (setq epdf-ttf-font-subfamily name))
                 (unless epdf-ttf-font-subfamily
                   (setq epdf-ttf-font-subfamily name))))
              
              (6
               ;; PostScript name.  This is the best source for PDF /BaseFont and
               ;; /FontName.  Prefer Windows English if available, otherwise take the
               ;; first non-Mac Unicode-ish name we see.
               (if (eq platform-id 3)
                   (when (eq language-id 1033)
                     (setq epdf-ttf-postscript-name name))
                 (unless epdf-ttf-postscript-name
                   (setq epdf-ttf-postscript-name name)))))))))))


(defun epdf-ttf--read-table-directory (n)
  "Return hash table mapping TTF table tags to (OFFSET . LENGTH)."
  (let ((tables (make-hash-table :test 'equal)))
    (dotimes (_ n)
      (let ((tag (epdf-ttf--bits-ascii 4)))
        (epdf-ttf--bits-u32) ;; checksum
        (let ((offset (epdf-ttf--bits-u32))
              (length (epdf-ttf--bits-u32)))
          (puthash tag (cons offset length) tables))))
    tables))

(defun epdf-ttf--parse-table (tables tag parser &optional required)
  "Parse TAG from TABLES using PARSER.
PARSER is called with OFFSET, the start of the table.
If REQUIRED is non-nil, signal an error when TAG is missing."
  (let ((entry (gethash tag tables)))
    (cond
     (entry
      (let ((offset (car entry)))
        (let ((epdf-ttf--bits-index offset))
          (funcall parser offset))))
     (required
      (error "epdf: required TTF table %s missing" tag))
     (t
      nil))))

(defun epdf-ttf--load-file(font-file)
  (let ((bin (with-temp-buffer
               (let ((file-name-handler-alist nil)) ;; leave my file alone
                 (insert-file-contents-literally font-file))
               (buffer-substring-no-properties (point-min) (point-max)))))
    (setq epdf-ttf--bits-data bin)
    (setq epdf-ttf--bits-index 0)))

(defun epdf-ttf--program-tables (program)
  "Return the table directory of PROGRAM, a font program as a string.
It is a hash table from tag to (OFFSET . LENGTH), the same that
`epdf-ttf--read-table-directory' returns."
  (let* ((epdf-ttf--bits-data program)
         (epdf-ttf--bits-index 4) ; skip sfntVersion
         (n (epdf-ttf--bits-u16)))
    ;; searchRange, entrySelector and rangeShift are not needed to find
    ;; a table by tag.
    (epdf-ttf--bits-u16 3)
    (epdf-ttf--read-table-directory n)))

(defun epdf-ttf--program-subsettable-p (program)
  "Return non-nil if the font program PROGRAM can be subset.
That is, if it has TrueType outlines, in glyf and loca.  Fonts with CFF
outlines have neither, and are embedded whole."
  (let ((tables (epdf-ttf--program-tables program)))
    (and (gethash "glyf" tables) (gethash "loca" tables) t)))

(defun epdf-ttf--program-loca (program tables)
  "Return where each glyph of PROGRAM starts, as a vector.
TABLES is the table directory of PROGRAM, from `epdf-ttf--program-tables'.
The vector has numGlyphs + 1 elements: element N is the offset in glyf
where glyph N starts, and element N + 1 where it ends.  Both equal means
an empty glyph.  Short and long loca both come out as real offsets."
  ;; Fonts with CFF outlines (OpenType .otf) have no glyf or loca, and
  ;; compose glyphs differently; they are not handled.
  (unless (and (gethash "loca" tables) (gethash "glyf" tables))
    (error "epdf: font without glyf/loca tables, cannot subset"))
  (let* ((epdf-ttf--bits-data program)
         ;; maxp.numGlyphs, a uint16 at byte 4.
         (epdf-ttf--bits-index (+ (car (gethash "maxp" tables)) 4))
         (num-glyphs (epdf-ttf--bits-u16))
         ;; head.indexToLocFormat, an int16 at byte 50: 0 means short
         ;; loca, uint16 values that are half the offset; 1 means long
         ;; loca, the offsets themselves as uint32.
         (long (progn (setq epdf-ttf--bits-index
                            (+ (car (gethash "head" tables)) 50))
                      (= 1 (epdf-ttf--bits-i16))))
         (offsets (make-vector (1+ num-glyphs) 0)))
    (setq epdf-ttf--bits-index (car (gethash "loca" tables)))
    (dotimes (i (1+ num-glyphs))
      (aset offsets i (if long
                          (epdf-ttf--bits-u32)
                        (* 2 (epdf-ttf--bits-u16)))))
    offsets))

;; Composite glyphs and font subsets.
;;
;; In the TrueType "glyf" table a glyph is either simple, with its own
;; outlines, or composite: no outlines at all, just a list of references
;; to other glyphs by GID, each placed with an offset and maybe scaled.
;; Fonts use this to build accented letters from a base letter and an
;; accent: in Arial, GID 98 is made of GID 36 and GID 142.
;;
;; A page never draws those referenced glyphs by themselves, so they are
;; not among the GIDs of the CID table.  But a subset that keeps glyph 98
;; and empties 36 and 142 draws nothing for 98.  So before subsetting,
;; the set of glyphs to keep has to be closed over composites: add the
;; components of every composite in the set, and the components of
;; those, since a component can itself be composite (in Times New Roman,
;; GID 470 uses GID 99, which is made of 36 and 219).
;;
;; This comes from the TrueType format, not from PDF.  9.9.2 "Font
;; subsets" only asks for the name tag and for .notdef to be defined.
;;
;; The layout this relies on, from the OpenType specification:
;;
;; - head: indexToLocFormat, an int16 at byte 50, says how loca stores
;;   offsets.  0 means "short": uint16 values that are half the real
;;   offset.  1 means "long": uint32 offsets as they are.
;;
;; - maxp: numGlyphs, a uint16 at byte 4.
;;
;; - loca: numGlyphs + 1 offsets into glyf.  Glyph N occupies the bytes
;;   from loca[N] to loca[N+1].  If both are equal the glyph is empty
;;   (a space, for instance) and has no data at all, not even a header.
;;
;; - glyf, for each non-empty glyph, starts with a 10-byte header:
;;   numberOfContours (int16) and the bounding box xMin, yMin, xMax,
;;   yMax (int16 each).  A negative numberOfContours marks a composite.
;;   After the header of a composite come the component records:
;;
;;     flags        uint16
;;     glyphIndex   uint16   the GID of the component
;;     arguments    two values, uint8 or int16 each (see flags)
;;     transform    nothing, or 1, 2 or 4 F2DOT14 values (see flags)
;;
;;   The flags this function needs, to know where a record ends and
;;   whether another one follows:
;;
;;     #x0001  ARG_1_AND_2_ARE_WORDS     arguments are 2 bytes each
;;     #x0008  WE_HAVE_A_SCALE           one scale value, 2 bytes
;;     #x0020  MORE_COMPONENTS           another record follows
;;     #x0040  WE_HAVE_AN_X_AND_Y_SCALE  two scale values, 4 bytes
;;     #x0080  WE_HAVE_A_TWO_BY_TWO      a 2x2 matrix, 8 bytes
;;
;;   The records may be followed by instructions (WE_HAVE_INSTRUCTIONS),
;;   but those come after the last record, so they never need skipping.

(defun epdf-ttf--add-composite-components (program gids)
  "Add to GIDS the components of the composite glyphs in it.
PROGRAM is a TrueType font program, as a string.  GIDS is a hash table
whose keys are GIDs, as returned by `epdf--used-gids'.  A composite glyph
is drawn from other glyphs, which are not in GIDS unless some page draws
them too; this adds them, and theirs, until nothing new appears.
GIDS is modified in place, and returned."
  (let* ((tables (epdf-ttf--program-tables program))
         ;; Where each glyph starts in glyf, and numGlyphs, to ignore
         ;; GIDs the font does not have.
         (loca (epdf-ttf--program-loca program tables))
         (num-glyphs (1- (length loca)))
         (glyf (car (gethash "glyf" tables)))
         ;; Read PROGRAM with the usual binary readers.  Binding the
         ;; reader state here leaves whatever font is loaded untouched.
         (epdf-ttf--bits-data program)
         (epdf-ttf--bits-index 0)
         ;; Work list: GIDs whose glyph has not been looked at yet.
         (pending nil))
    ;; Where glyph GID starts, as an offset from the start of glyf.
    ;; Called with GID + 1 it gives where GID ends, since loca has one
    ;; entry more than there are glyphs.
    (cl-flet ((glyph-start (gid) (aref loca gid)))
      ;; Every glyph already in the set has to be looked at once.
      (maphash (lambda (gid _) (push gid pending)) gids)
      (while pending
        (let* ((gid (pop pending))
               ;; A GID past numGlyphs would read loca out of bounds;
               ;; treat it as having no glyph.
               (start (and (< gid num-glyphs) (glyph-start gid)))
               (end (and start (glyph-start (1+ gid)))))
          ;; Empty glyphs (start = end) have no header to read.
          (when (and start (< start end))
            (setq epdf-ttf--bits-index (+ glyf start))
            ;; numberOfContours.  Zero or positive means a simple glyph,
            ;; which references nothing: done with it.
            (when (< (epdf-ttf--bits-i16) 0)
              ;; Skip the bounding box, to reach the first record.
              (epdf-ttf--bits-i16 4)
              (let ((more t))
                (while more
                  (let ((flags (epdf-ttf--bits-u16))
                        (component (epdf-ttf--bits-u16)))
                    ;; A component seen for the first time joins the set,
                    ;; and the work list, as it may be composite itself.
                    ;; One already in the set was or will be looked at, so
                    ;; each glyph is read once, and a (broken) font with a
                    ;; glyph referring to itself cannot loop forever.
                    (unless (gethash component gids)
                      (puthash component t gids)
                      (push component pending))
                    ;; Skip the rest of the record, whose size depends on
                    ;; the flags.
                    (setq epdf-ttf--bits-index
                          (+ epdf-ttf--bits-index
                             ;; The two arguments (offsets or point
                             ;; numbers): 2 bytes each, or 1 byte each.
                             (if (/= 0 (logand flags #x0001)) 4 2)
                             ;; The transformation, in F2DOT14 values of 2
                             ;; bytes.  At most one of these flags is set.
                             (cond ((/= 0 (logand flags #x0008)) 2)  ; WE_HAVE_A_SCALE
                                   ((/= 0 (logand flags #x0040)) 4)  ; X_AND_Y_SCALE
                                   ((/= 0 (logand flags #x0080)) 8)  ; TWO_BY_TWO
                                   (t 0))))
                    ;; MORE_COMPONENTS: whether another record follows.
                    (setq more (/= 0 (logand flags #x0020))))))))))) ; MORE_COMPONENTS
    gids))

(defun epdf-ttf--subset-glyf (program gids)
  "Return new glyf, loca and head tables for PROGRAM, keeping only GIDS.
PROGRAM is a TrueType font program, as a string.  GIDS is a hash table
whose keys are the GIDs to keep, already closed over composite glyphs
with `epdf-ttf--add-composite-components'.

Every glyph keeps its GID: a glyph not in GIDS stays in the font, but
empty.  So the GIDs in the page content, the CIDToGIDMap and the widths
are still right, and only glyf gets smaller.  loca still has an entry
for every glyph, and is written in the long format, so head is copied
with indexToLocFormat set to 1.

The result is a list of (TAG BYTES nil), for `epdf-ttf--write-font'."
  (let* ((tables (epdf-ttf--program-tables program))
         (loca (epdf-ttf--program-loca program tables))
         (num-glyphs (1- (length loca)))
         (glyf (car (gethash "glyf" tables)))
         (head (gethash "head" tables))
         ;; Where each glyph starts in the new glyf, and where the last
         ;; one ends.
         (new-loca (make-vector (1+ num-glyphs) 0))
         new-glyf)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dotimes (gid num-glyphs)
        ;; The glyph starts where the new glyf is at this point.  For an
        ;; empty glyph, the next one starts at the same place.
        (aset new-loca gid (1- (point)))
        (let ((start (aref loca gid))
              (end (aref loca (1+ gid))))
          (when (and (gethash gid gids) (< start end))
            (insert (substring program (+ glyf start) (+ glyf end)))
            ;; The specification asks for glyph offsets to be multiples
            ;; of 4, so pad each glyph with zeros; rasterizers ignore
            ;; bytes after the glyph data.
            (dotimes (_ (% (- 4 (% (- end start) 4)) 4))
              (insert 0)))))
      (aset new-loca num-glyphs (1- (point)))
      (setq new-glyf (buffer-string)))
    (list
     (list "glyf" new-glyf nil)
     ;; Long loca: every offset as a uint32.
     (list "loca"
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (mapc #'epdf-ttf--write-u32 new-loca)
             (buffer-string))
           nil)
     ;; head, with indexToLocFormat (int16 at byte 50) set to 1.  The
     ;; writer sets checkSumAdjustment.
     (let ((bytes (substring program (car head) (+ (car head) (cdr head)))))
       (list "head"
             (concat (substring bytes 0 50)
                     (unibyte-string 0 1)
                     (substring bytes 52))
             nil)))))

(defun epdf-ttf--checksum (bytes)
  "Return the TrueType checksum of BYTES, a string.
It is the sum of the data as big-endian uint32, modulo 2^32, with zeros
added at the end up to a multiple of 4."
  (let ((sum 0)
        (len (length bytes))
        (i 0))
    (while (< i len)
      (setq sum (+ sum (ash (logand #xff (aref bytes i))
                            (* 8 (- 3 (% i 4))))))
      (cl-incf i))
    (logand sum #xFFFFFFFF)))

(defun epdf-ttf--write-font (sfnt-version tables)
  "Return a TrueType font program, as a unibyte string, made of TABLES.
SFNT-VERSION is the uint32 the font starts with: #x00010000 for fonts
with TrueType outlines.  TABLES is a list of (TAG BYTES CHECKSUM): TAG
is the 4-character table tag, BYTES a string with the table, and
CHECKSUM its checksum, or nil to compute it.  For a table copied
unchanged from another font, pass the checksum that font has for it,
which saves summing the whole table.

The layout is the one the OpenType specification describes: a header,
a table directory sorted by tag, and the tables, each one starting at a
multiple of 4 and padded with zeros.  head.checkSumAdjustment is set so
that the whole font sums to #xB1B0AFBA."
  (let* ((tables (sort (copy-sequence tables)
                       (lambda (a b) (string< (car a) (car b)))))
         (n (length tables))
         ;; The directory header helps a binary search over the table
         ;; records: searchRange is 16 times the largest power of 2 that
         ;; is not greater than N, entrySelector the log2 of that power,
         ;; and rangeShift what is left.
         (entry-selector (logb n))
         (search-range (* 16 (ash 1 entry-selector)))
         (range-shift (- (* 16 n) search-range))
         (offset (+ 12 (* 16 n)))
         (sum 0)
         (head-offset nil)
         records)
    ;; Work out where each table goes, and its checksum.
    (dolist (table tables)
      (let* ((tag (nth 0 table))
             (bytes (nth 1 table))
             (checksum (nth 2 table)))
        (when (string= tag "head")
          ;; checkSumAdjustment, at byte 8 of head, is set at the end.
          ;; head's own checksum is computed with it at zero, so a
          ;; checksum given for head cannot be trusted: compute it.
          (setq bytes (concat (substring bytes 0 8)
                              (unibyte-string 0 0 0 0)
                              (substring bytes 12))
                checksum nil
                head-offset offset))
        (unless checksum
          (setq checksum (epdf-ttf--checksum bytes)))
        (setq sum (+ sum checksum))
        (push (list tag bytes checksum offset) records)
        (setq offset (+ offset (* 4 (/ (+ (length bytes) 3) 4))))))
    (setq records (nreverse records))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; Header.
      (epdf-ttf--write-u32 sfnt-version)
      (epdf-ttf--write-u16 n)
      (epdf-ttf--write-u16 search-range)
      (epdf-ttf--write-u16 entry-selector)
      (epdf-ttf--write-u16 range-shift)
      ;; Table directory.
      (dolist (record records)
        (insert (nth 0 record))
        (epdf-ttf--write-u32 (nth 2 record))
        (epdf-ttf--write-u32 (nth 3 record))
        (epdf-ttf--write-u32 (length (nth 1 record))))
      ;; Every table is padded to a multiple of 4, so the checksum of the
      ;; whole font is the checksum of the header and directory plus the
      ;; checksums of the tables: no need to sum it all again.
      (setq sum (+ sum (epdf-ttf--checksum (buffer-string))))
      ;; Tables.
      (dolist (record records)
        (let ((bytes (nth 1 record)))
          (insert bytes)
          (dotimes (_ (% (- 4 (% (length bytes) 4)) 4))
            (insert 0))))
      ;; head.checkSumAdjustment.
      (when head-offset
        (goto-char (+ (point-min) head-offset 8))
        (delete-char 4)
        (epdf-ttf--write-u32 (logand (- #xB1B0AFBA sum) #xFFFFFFFF)))
      (buffer-string))))

(defun epdf-ttf--subset-font (program gids)
  "Return a subset of the TrueType font program PROGRAM, as a string.
GIDS is a hash table whose keys are the GIDs to keep, already closed
over composite glyphs with `epdf-ttf--add-composite-components'.  Every
glyph keeps its GID; those not in GIDS are left empty.

The subset has only the tables that 9.9 of the PDF specification asks
for in a TrueType program used by a CIDFont (FontFile2 in Table 124, and
the text after Table 125): glyf, head, hhea, hmtx, loca and maxp, and
cvt, fpgm and prep when the original has them, since the font
instructions may need them.  In particular there is no cmap, which a
CIDFont does not use, and which the specification says shall not be
present.  Nor the layout tables, GSUB, GPOS and GDEF: shaping is done
by the time a glyph reaches the PDF."
  (let ((tables (epdf-ttf--program-tables program))
        ;; New glyf, loca and head.
        (subset (epdf-ttf--subset-glyf program gids))
        ;; sfntVersion, to write the same one.
        (sfnt-version (let ((epdf-ttf--bits-data program)
                            (epdf-ttf--bits-index 0))
                        (epdf-ttf--bits-u32))))
    ;; The other tables go unchanged.  hmtx keeps an entry for every
    ;; glyph, and maxp its numGlyphs, which stays right because the
    ;; GIDs do not change.
    (dolist (tag '("hhea" "hmtx" "maxp" "cvt " "fpgm" "prep"))
      (let ((entry (gethash tag tables)))
        (when entry
          (push (list tag
                      (substring program (car entry)
                                 (+ (car entry) (cdr entry)))
                      nil)
                subset))))
    (epdf-ttf--write-font sfnt-version subset)))

(defun epdf-ttf--compose-font-from-collection (collection-index)
  "Replace the loaded font collection with a font made of one of its faces.
COLLECTION-INDEX is the zero-based index of the face.  It must be called
with the reader just after numFonts, at the table of offsets to the
faces.  Afterwards the reader holds the new font, just after
sfntVersion, as it would for a font read from a .ttf file."
  ;; Entry COLLECTION-INDEX of the table of offsets says where the table
  ;; directory of the face starts.
  (epdf-ttf--bits-u32 collection-index)
  (setq epdf-ttf--bits-index (epdf-ttf--bits-u32))
  (let ((sfnt-version (epdf-ttf--bits-u32))
        (n (epdf-ttf--bits-u16))
        tables)
    ;; searchRange, entrySelector, rangeShift: the writer computes them.
    (epdf-ttf--bits-u16 3)
    ;; The tables are copied unchanged, so their checksums hold.  Their
    ;; offsets are from the start of the collection, and some are shared
    ;; with other faces; the writer places them anew.
    (dotimes (_ n)
      (let ((tag (epdf-ttf--bits-ascii 4))
            (checksum (epdf-ttf--bits-u32))
            (offset (epdf-ttf--bits-u32))
            (length (epdf-ttf--bits-u32)))
        (push (list tag
                    (substring epdf-ttf--bits-data offset (+ offset length))
                    checksum)
              tables)))
    (setq epdf-ttf--bits-data (epdf-ttf--write-font sfnt-version tables))
    (setq epdf-ttf--bits-index 4)))

;; And this is the main entry point for the library.
(defun epdf-ttf-extract-info(font-file collection-index)
  "Read information from a TTF file, initializing the globals:

  epdf-ttf-font-family
  epdf-ttf-font-subfamily
  epdf-ttf-postscript-name
  epdf-ttf-font-bbox
  epdf-ttf-italic-angle
"
  (setq epdf-ttf-font-family nil)
  (setq epdf-ttf-font-subfamily nil)
  (setq epdf-ttf-postscript-name nil)
  (setq epdf-ttf-font-bbox nil)
  (setq epdf-ttf-italic-angle 0)
  (setq epdf-ttf-win-ascent nil)
  (setq epdf-ttf-win-descent nil)

  ;; (message "Reading font %s" font-file)
  (let (n num-fonts-in-collection)
    (epdf-ttf--load-file font-file)    
    (pcase (epdf-ttf--bits-u32)
      (#x00010000 ; (message "TrueType")
                  )
      (#x4F54544F ; (message "OpenType")
                  )
      (#x74746366 ; (message "Font Collection")
       (unless collection-index (error "epdf: nil index for collection %s" font-file))
       (epdf-ttf--bits-u16 2)        ;; skip version (2xu16)
       (setq num-fonts-in-collection (epdf-ttf--bits-u32))
       ;; (message "Font collection with %d fonts" num-fonts-in-collection)

       ;; We need to build a font file from the tables in collection,
       ;; load it into epdf-ttf--bits-data, and set
       ;; epdf-ttf--bits-index just after sfntVersion so the
       ;; following code works the same.
       (epdf-ttf--compose-font-from-collection collection-index))

      (other (error (format "Unidentified truetype file (%x)" other))))

    (setq n (epdf-ttf--bits-u16)) ;; number of tables
    ;;    (message "# tables: %d" n)
    (epdf-ttf--bits-u16 3) ; skip rest of header

    (let ((tables (epdf-ttf--read-table-directory n)))

      ;; Parse tables in dependency order.
      (epdf-ttf--parse-table tables "name" #'epdf-ttf--name t)

        ;; Independent core metrics.
      (epdf-ttf--parse-table tables "head" #'epdf-ttf--head t)
      (epdf-ttf--parse-table tables "maxp" #'epdf-ttf--maxp t)
      (epdf-ttf--parse-table tables "hhea" #'epdf-ttf--hhea t)
      
      ;; Optional-ish but very useful for PDF descriptors.
      (epdf-ttf--parse-table tables "OS/2" #'epdf-ttf--os/2 nil)
      (epdf-ttf--parse-table tables "post" #'epdf-ttf--post nil)
      
      ;; Depends on hhea's numberOfHMetrics.
      (epdf-ttf--parse-table tables "hmtx" #'epdf-ttf--hmtx t))
    t))

(defun epdf-ttf-extract-family-names(font-file)
  "Extract family names from a font file.
Returns a string for normal TTF files, and a list of strings for Font
Collection files. Used only on MS-Windows by epdf, when looking for a
font file to match a name."

  ;; (message "Reading font %s" font-file)
  (let (n num-fonts-in-collection)
    (epdf-ttf--load-file font-file)    

    (pcase (epdf-ttf--bits-u32) ;; sfntVersion
      (#x00010000) ;; truetype
      (#x4F54544F) ;; opentype
      ;; font collection
      (#x74746366 (epdf-ttf--bits-u16 2) ;; skip version (2xu16)
                  (setq num-fonts-in-collection (epdf-ttf--bits-u32)))
      (other (error (format "Unidentified truetype file (%x)" other))))

    (if num-fonts-in-collection
        ;; This is a font collection
        (progn
          ;; (message (format "Font collection with %d fonts" num-fonts-in-collection))
          ;; Now there is a table of offsets into the file for each font.
          ;; Read num-fonts-in-collection number of 32 bits
          (let ((offsets nil))
            (dotimes (_ num-fonts-in-collection)
              (push (epdf-ttf--bits-u32) offsets))
            (mapcar (lambda (offset)
                      (let ((epdf-ttf--bits-index offset))
                        (epdf-ttf--bits-u32) ; sfntVersion
                        (setq n (epdf-ttf--bits-u16))
                        (epdf-ttf--bits-u16 3)
                        (let ((tables (epdf-ttf--read-table-directory n)))
                          (epdf-ttf--parse-table tables "name" #'epdf-ttf--name t)
                          (concat epdf-ttf-font-family))))
                    (nreverse offsets))))
      ;; Normal font
      (setq n (epdf-ttf--bits-u16)) ;; number of tables
      (epdf-ttf--bits-u16 3) ; skip rest of header
      (let ((tables (epdf-ttf--read-table-directory n)))
        (epdf-ttf--parse-table tables "name" #'epdf-ttf--name t)
        (concat epdf-ttf-font-family ;;  " " epdf-ttf-font-subfamily
                      )))))

(provide 'epdf-ttf)

;;; epdf-ttf.el ends here
