;;; epdf-core.el --- Internals of epdf  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Core PDF function: PDF objects and the CID tables, ToUnicode,
;; widths, shaping.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'ps-print)
(require 'epdf-ttf)

(defun epdf--pdf-font-name ()
  "Return the PDF name for the TrueType font being parsed.
Its PostScript name, or FAMILY-SUBFAMILY, with illegal chars replaced by -"
  (intern (replace-regexp-in-string
           "[^A-Za-z0-9+_.-]" "-"
           (or epdf-ttf-postscript-name
               (concat epdf-ttf-font-family "-" epdf-ttf-font-subfamily)))))

(defun epdf--convert-font-units (sz)
  "Converts font sizes (such as glyph widths) to what pdf expects.
It uses the global variable epdf-ttf-units-per-em. PDF units are 1000
for an em."
  (/ (* 1000 sz) epdf-ttf-units-per-em))

(defun epdf--convert-font-bbox (bbox)
  "Convert a TTF BBOX from font units to PDF 1000-em units."
  (epdf--array
   (mapcar #'epdf--convert-font-units
           (or bbox '(0 0 0 0)))))

(defun epdf--get-paper-size(paper-size)
  (unless paper-size (setq paper-size ps-paper-type))
  (unless paper-size (setq paper-size 'a4))
  (when (symbolp paper-size)
    (setq paper-size
          (let ((ps (assoc paper-size ps-page-dimensions-database)))
            (list (cl-second ps) (cl-third ps)))))
  paper-size)

(defun epdf--font-obj-from-name (name size)
  (let* ((opened-name (aref (font-info name) 0))
        (fspec (font-spec :name opened-name :size size ))
        (ffont (find-font fspec)))
    (open-font ffont size)))

(defun epdf--font-name-for-index (font-index)
  (or (nth font-index epdf-font-names)
      (error "epdf: no font at index %d" font-index)))

(defconst epdf--shaping-size 1000
  "Pixel size of the font objects that text is shaped with.
Emacs gives the positions HarfBuzz computes in whole pixels of the font
object.  At this size a pixel is a 1em/1000, the unit PDF uses")

(defun epdf--font-object-for-index (font-index size)
  (epdf--font-obj-from-name
   (epdf--font-name-for-index font-index)
   size))

;; 14.3.2 Metadata streams
(defun epdf--xmp-metadata ()
  (let ((time (format-time-string "%FT%TZ" nil t))
        (producer (format "epdf on %s" (emacs-version))))
    (with-temp-buffer
      (dom-print 
       (dom-node
        "rdf:RDF"
        '(("xmlns:rdf" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
          ("xmlns:xmp" . "http://ns.adobe.com/xap/1.0/")
          ("xmlns:pdf" . "http://ns.adobe.com/pdf/1.3/"))
        
        (dom-node "rdf:Description" nil
                  (dom-node "xmp:CreateDate" nil time)
                  (dom-node "xmp:ModifyDate" nil time)
                  (dom-node "xmp:CreatorTool" nil producer)
                  (dom-node "pdf:Producer" nil producer)
                  
                  (dom-node "pdf:Title" nil "Title of the document")
                  (dom-node "pdf:Author" nil "Author" ) ;; (user-full-name))
                  (dom-node "pdf:Subject" nil "Subject of the document")))
       t t)
      (buffer-string))))

;; The following funtions generate the constructs of the PDF Format.
;; 7.3.7 Dictionary objects
(defun epdf--dict (data) (list 'dict data))

;; 7.3.6 Array objects
(defun epdf--array (data) (list 'array data))

;; 7.3.8 Stream objects
(defun epdf--stream (data &optional type subtype add-length-1)
  (list 'stream type subtype data add-length-1))

(defun epdf--byte-string (data)
  "Data needs to be a unibyte string"
  (list 'byte-string data))

;; 7.3.10 Indirect objects
(defun epdf--objref (id)
  (list 'ref id))

;; Functions for PDF production
(defun epdf--reserve-object ()
  "Reserve and return a new indirect object id."
  (prog1 epdf-running-id
    (cl-incf epdf-running-id)))

(defun epdf--record-object-offset (id offset)
  "Remember that object ID starts at byte OFFSET."
  (when (assq id epdf-objects)
    (error "epdf: object %d has already been written" id))
  (push (cons id offset) epdf-objects))

;; 7.3.10 Indirect objects
(defun epdf--insert-referenced-object-at (id obj)
  "Insert OBJ as indirect object ID, and return ID"
  (with-current-buffer epdf-buffer
    ;; Emacs buffer positions are 1-based.  PDF byte offsets are 0-based.
    (epdf--record-object-offset id (1- (point)))
    (insert (format "%d 0 obj\n" id))
    (epdf--insert-object obj)
    (insert "endobj\n")
    id))

(defun epdf--insert-referenced-object (obj)
  "Reserve a object id, write OBJ there, and return the id."
  (let ((id (epdf--reserve-object)))
    (epdf--insert-referenced-object-at id obj)
    id))

(defun epdf--insert-dictionary (dict)
  (insert "<<\n")
  (dolist (pair dict)
    (insert (format "/%s " (car pair)))
    (epdf--insert-object (cdr pair))
    (insert "\n"))
  (insert ">>\n"))

(defun epdf--escape-pdf-string (s)
  (with-temp-buffer
    (dolist (ch (string-to-list s))
      (pcase ch
        (?\\ (insert "\\\\"))
        (?\( (insert "\\("))
        (?\) (insert "\\)"))
        (?\n (insert "\\n"))
        (?\r (insert "\\r"))
        (?\t (insert "\\t"))
        (_   (insert-char ch))))
    (buffer-string)))

(defun epdf--insert-object (obj)
  (if (integerp obj) (insert (format "%d" obj))
    (if (floatp obj) (insert (format "%f" obj))
      (if (stringp obj) (insert (format "(%s)" (epdf--escape-pdf-string obj))) 
        (if (symbolp obj) (insert (format "/%s " obj))
          (if (null obj) (insert "null")
            (pcase (car obj)
              ('dict (epdf--insert-dictionary (cadr obj)))
              ('stream
               (let ((dictionary `(("Length" . ,(length (nth 3 obj))))))
                 (if (nth 4 obj)
                     (push (cons "Length1" (length (nth 3 obj))) dictionary))
                 (if (nth 1 obj)
                     (push (cons "Type" (nth 1 obj)) dictionary))
                 (if (nth 2 obj)
                     (push (cons "Subtype" (nth 2 obj)) dictionary))
                 (epdf--insert-dictionary
                  dictionary))
               (insert "stream\n")
               (insert (nth 3 obj))
               (insert "\nendstream\n"))
              ('array
               (insert "[ ")
               (dolist (item (cadr obj))
                 (epdf--insert-object item)
                 (insert " "))
               (insert "] "))
              ('ref (insert (format "%d 0 R" (cadr obj))))
              ('byte-string
               (insert "<")
               (dolist (byte (append (cadr obj) nil))
                 (insert (format "%02x" byte)))
               (insert ">"))
              (_
               (print obj)
               (error "Unknown object type")))))))))
    
(defun epdf--subset-tag (font-index)
  "Return the 'subset tag' for the font with index FONT-INDEX.
9.9.2 Font subsets: each subset of a font needs a unique 'tag', formed
by six uppercase letters. We use font-index as base 26 number with
letters."
  (let ((tag (make-string 6 ?A))
        (n font-index))
    (dotimes (i 6)
      (aset tag (- 5 i) (+ ?A (% n 26)))
      (setq n (/ n 26)))
    tag))

(defmacro epdf--append-font (list-var value)
  "Append VALUE at the end of LIST-VAR.
The per-font lists are indexed by the font index returned by
`epdf-embed-ttf', so they must stay in embedding order."
  `(setq ,list-var (append ,list-var (list ,value))))

;; 7.7.3.3 Page objects
(defun epdf--close-current-page ()
  "Internal function, not part of the public API."
  (push (list epdf-current-page-info epdf-current-page-content) epdf-pages))

(defun epdf--resources-dictionary ()
  ;; Each font reference is like (F20 . 3)
  ;; F20 is a name to be used in the page content.
  ;; 3 is the object id of the font
  ;; We number fonts from F20 to leave room for standard fonts from F1.
  (epdf--dict
   `((Font . ,(epdf--dict
               (cl-loop for index from 0
                        for obj-id in epdf-fonts
                        collect `(,(intern (format "F%d" (+ 20 index))) .
                                  ,(epdf--objref obj-id))))))))
           

(defun epdf--bytes-to-hex (bytes)
  "Return BYTES, a unibyte string, as lowercase hexadecimal."
  (mapconcat (lambda (b)
               (format "%02x" b))
             (append bytes nil)
             ""))

(defun epdf--unicode-string-to-utf16be-hex (string)
  "Return STRING encoded as UTF-16BE hexadecimal, without BOM."
  (epdf--bytes-to-hex
   (encode-coding-string string 'utf-16be t)))

(defun epdf--cid-to-hex (cid)
  "Return CID as a 4-digit hexadecimal code string."
  (unless (and (integerp cid)
               (<= 0 cid)
               (<= cid #xffff))
    (error "epdf: CID out of range: %S" cid))
  (format "%04X" cid))

(defun epdf--cid-table-sorted-unicode-mappings (table)
  "Return sorted (CID . UNICODE) mappings from TABLE.
Entries with empty Unicode strings are skipped."
  (let (items)
    (maphash
     (lambda (cid unicode)
       (when (and unicode
                  (> (length unicode) 0))
         (push (cons cid unicode) items)))
     (epdf-cid-table-cid-to-unicode table))
    (sort items (lambda (a b) (< (car a) (car b))))))

(defun epdf--take (n list)
  "Return the first N elements of LIST."
  (let (result)
    (dotimes (_ n)
      (when list
        (push (pop list) result)))
    (nreverse result)))

(defun epdf--drop (n list)
  "Return LIST without its first N elements."
  (dotimes (_ n)
    (when list
      (setq list (cdr list))))
  list)

(defun epdf--chunks (list n)
  "Split LIST into chunks of at most N elements."
  (let (chunks)
    (while list
      (push (epdf--take n list) chunks)
      (setq list (epdf--drop n list)))
    (nreverse chunks)))

(defun epdf--tounicode-bfchar-block (mappings)
  "Return a beginbfchar/endbfchar block for MAPPINGS.
MAPPINGS is a list of (CID . UNICODE-STRING)."
  (with-temp-buffer
    (insert (format "%d beginbfchar\n" (length mappings)))
    (dolist (mapping mappings)
      (let ((cid (car mapping))
            (unicode (cdr mapping)))
        (insert
         (format "<%s> <%s>\n"
                 (epdf--cid-to-hex cid)
                 (epdf--unicode-string-to-utf16be-hex unicode)))))
    (insert "endbfchar\n")
    (buffer-string)))

(defun epdf--to-unicode-cmap-data (table)
  "Return a ToUnicode CMap stream for TABLE."
  (let ((mappings (epdf--cid-table-sorted-unicode-mappings table)))
    (with-temp-buffer
      (set-buffer-multibyte nil)

      ;; PDF spec 9.10.3: ToUnicode CMaps map character codes to Unicode.
      ;; Our character codes are 2-byte CIDs because the Type0 font uses
      ;; Identity-H.
      (insert "/CIDInit /ProcSet findresource begin\n")
      (insert "12 dict begin\n")
      (insert "begincmap\n")
      (insert "/CIDSystemInfo <<\n")
      (insert "  /Registry (Adobe)\n")
      (insert "  /Ordering (UCS)\n")
      (insert "  /Supplement 0\n")
      (insert ">> def\n")
      (insert "/CMapName /EPDF-ToUnicode def\n")
      (insert "/CMapType 2 def\n")
      (insert "1 begincodespacerange\n")
      (insert "<0000> <FFFF>\n")
      (insert "endcodespacerange\n")

      (dolist (chunk (epdf--chunks mappings 100))
        (insert (epdf--tounicode-bfchar-block chunk)))

      (insert "endcmap\n")
      (insert "CMapName currentdict /CMap defineresource pop\n")
      (insert "end\n")
      (insert "end\n")

      (buffer-substring-no-properties (point-min) (point-max)))))


(defun epdf--write-late-font-objects ()
  "Write font objects that depend on glyphs used in page content.

This currently writes:
  - Font file stream (FontFile2)
  - ToUnicode CMAP stream
  - CIDToGIDMap stream
  - CIDFontType2 dictionary with CID-indexed /W array"
  (cl-loop
   for table in epdf-font-cid-tables
   for cidfont-id in epdf-font-cidfont-ids
   for cidtogidmap-id in epdf-font-cidtogidmap-ids
   for tounicode-id in epdf-font-tounicode-ids
   for descriptor-id in epdf-font-descriptor-ids
   for width-vector in epdf-font-width-vectors
   for pdf-font-name in epdf-font-pdf-names
   for fontfile-id in epdf-font-fontfile-ids
   for program in epdf-font-programs
   do

   ;; Font file stream.
   ;; 9.9 Embedded font programs
   ;; Only the glyphs in use, and those they are made of.  Every
   ;; glyph keeps its GID, so the CIDToGIDMap and /W work
   ;; hold. If the font cannot be subset, embed in in full.
   (epdf--insert-referenced-object-at
    fontfile-id
    (epdf--stream
     (if (epdf-ttf--program-subsettable-p program)
         (epdf-ttf--subset-font
          program
          (epdf-ttf--add-composite-components program
                                              (epdf--used-gids table)))
       program)
     nil nil t))

   ;; ToUnicode CMap.  This maps the PDF CIDs emitted in page content
   ;; back to Unicode text for copy/paste/search/extraction.
   (epdf--insert-referenced-object-at
    tounicode-id
    (epdf--stream
     (epdf--to-unicode-cmap-data table) 'CMap))
   
   ;; CIDToGIDMap stream.
   (epdf--insert-referenced-object-at
    cidtogidmap-id
    (epdf--stream
     (epdf--cid-to-gid-map-data table)))

   ;; CIDFontType2 descendant font.
   (epdf--insert-referenced-object-at
    cidfont-id
    (epdf--dict
     `((Type . Font)
       (Subtype . CIDFontType2)
       (BaseFont . ,pdf-font-name)

       (CIDToGIDMap . ,(epdf--objref cidtogidmap-id))

       ;; 9.7.3 CIDSystemInfo dictionaries
       (CIDSystemInfo . ,(epdf--dict
                          `((Registry . "Adobe")
                            (Ordering . "Identity")
                            (Supplement . 0))))

       (FontDescriptor . ,(epdf--objref descriptor-id))

       (W . ,(epdf--cid-width-array table width-vector)))))))

(cl-defstruct epdf-glyph-run glyphs)

;; X-OFFSET and Y-OFFSET move the glyph alone, and ADVANCE is how far
;; the next glyph goes, as the shaper decided. ADVANCE nil means the
;; font's own width.
(cl-defstruct epdf-glyph
  cid gid unicode x-offset y-offset advance ascent descent)

(cl-defstruct epdf-cid-table
  next-cid       ;; Next PDF CID to allocate.
  by-key         ;; Maps (GID . UNICODE-STRING) -> CID.
  cid-to-gid     ;; Maps CID -> GID. Later used for /CIDToGIDMap.
  cid-to-unicode ;; Maps CID -> Unicode string. Later used for /ToUnicode.
  )

(defun epdf--make-cid-table ()
  (make-epdf-cid-table
   :next-cid 1
   :by-key (make-hash-table :test 'equal)
   :cid-to-gid (make-hash-table :test 'eql)
   :cid-to-unicode (make-hash-table :test 'eql)))

(defun epdf--cid-table-for-font (font-index)
  (or (nth font-index epdf-font-cid-tables)
      (error "epdf: no CID table for font index %d" font-index)))

(defun epdf--cid-table-max-cid (table)
  (let ((max-cid 0))
    (maphash (lambda (cid _gid)
               (setq max-cid (max max-cid cid)))
             (epdf-cid-table-cid-to-gid table))
    max-cid))

(defun epdf--used-gids (table)
  "Return the glyphs of a font that the document uses, as a hash table.
TABLE is the font's `epdf-cid-table'.  The keys are GIDs, each with value
t: the GID every emitted CID maps to, plus GID 0, the .notdef glyph,
which a TrueType font must always have."
  (let ((gids (make-hash-table)))
    (puthash 0 t gids)
    (maphash (lambda (_cid gid) (puthash gid t gids))
             (epdf-cid-table-cid-to-gid table))
    gids))

;; 9.7.4.2 Glyph selection in CIDFonts
(defun epdf--cid-to-gid-map-data (table)
  "Return raw unibyte CIDToGIDMap stream data for TABLE.
Entry N contains the big-endian GID for CID N.
Unassigned CIDs map to GID 0."
  (let ((max-cid (epdf--cid-table-max-cid table)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; Include CID 0.
      (dotimes (cid (1+ max-cid))
        (let ((gid (or (gethash cid
                                (epdf-cid-table-cid-to-gid table))
                       0)))
          (unless (and (integerp gid)
                       (<= 0 gid)
                       (<= gid #xffff))
            (error "epdf: GID out of CIDToGIDMap range: %S" gid))
          (insert-char (logand #xff (ash gid -8)))
          (insert-char (logand #xff gid))))
      (buffer-substring-no-properties (point-min) (point-max)))))


;; 9.7.4.3 Glyph metrics in CIDFonts
(defun epdf--cid-width-array (table width-vector)
  "Return a PDF /W array for TABLE using WIDTH-VECTOR indexed by GID.

Uses the simple form:
  c1 c2 w

for each assigned CID."
  (let (items)
    (maphash
     (lambda (cid gid)
       (let ((width (epdf--font-width-for-gid width-vector gid)))
         (setq items
               (append items
                       (list cid cid width)))))
     (epdf-cid-table-cid-to-gid table))
    (epdf--array items)))


(defun epdf--make-font-width-vector ()
  "Return a vector indexed by GID, containing PDF 1000-em widths."
  (let* ((hwidths epdf-ttf-widths)
         (hvec (vconcat hwidths))
         (hcount (length hvec))
         (glyph-count epdf-ttf-num-glyphs)
         (last-width (if (> hcount 0)
                         (aref hvec (1- hcount))
                       1000))
         (vec (make-vector glyph-count 0)))
    (dotimes (gid glyph-count)
      (aset vec gid
            (epdf--convert-font-units
             (if (< gid hcount)
                 (aref hvec gid)
               last-width))))
    vec))

(defun epdf--font-width-for-gid (width-vector gid)
  (if (and (integerp gid)
           (>= gid 0)
           (< gid (length width-vector)))
      (aref width-vector gid)
    1000))

(defun epdf--gstring->epdf-glyphs (gstring string scale)
  "Convert an Emacs GSTRING into a list of `epdf-glyph' structs.
SCALE converts pixels of the font object GSTRING was shaped with into
thousandths of an em.

The adjustment of a glyph, when there is one, is [XOFF YOFF WADJUST]:
offsets that move that glyph only, with Y down as on the screen, and the
whole advance to the next glyph, which replaces the font's width (see
how w32term.c draws them)."
  (cl-loop for i from 0 below (lgstring-glyph-len gstring)
           for glyph = (lgstring-glyph gstring i)
           while glyph
           collect
           (let* ((from (lglyph-from glyph))
                  (to (lglyph-to glyph))
                  (adj (lglyph-adjustment glyph)))
             (make-epdf-glyph
              :cid nil
              :gid (lglyph-code glyph)
              :unicode (substring string from (1+ to))
              :x-offset (if adj (* scale (aref adj 0)) 0)
              :y-offset (if adj (- (* scale (aref adj 1))) 0)
              :advance (and adj (* scale (aref adj 2)))
              :ascent (lglyph-ascent glyph)
              :descent (lglyph-descent glyph)))))

(defun epdf--grow-gstring (gstring factor)
  "Return a copy of GSTRING with FACTOR times as many glyph slots.
The extra slots are nil, which is where the shaper stops reading the
text, so only the room for its output grows."
  (let* ((len (lgstring-glyph-len gstring))
         (new (make-vector (1+ (* factor len)) nil)))
    (aset new 0 (lgstring-header gstring))
    (dotimes (i len)
      (aset new (1+ i) (lgstring-glyph gstring i)))
    new))


(defun epdf--cid-hex (cid)
  "Return CID as a 4-digit hexadecimal PDF text code."
  (unless (and (integerp cid)
               (<= 0 cid)
               (<= cid #xffff))
    (error "epdf: CID out of range for Identity-H text code: %S" cid))
  (format "%04X" cid))

(defun epdf--register-glyph-cid (font-index glyph)
  "Return the PDF CID to use for GLYPH in FONT-INDEX.
Also record:
  CID -> GID for /CIDToGIDMap
  CID -> Unicode for /ToUnicode

The key is (GID . UNICODE): the same glyph id can correspond to
different Unicode source strings."
  (let* ((table (epdf--cid-table-for-font font-index))
         (gid (epdf-glyph-gid glyph))
         (unicode (or (epdf-glyph-unicode glyph) ""))
         (key (cons gid unicode))
         (existing-cid
          (gethash key (epdf-cid-table-by-key table))))

    (or existing-cid
        (let ((cid (epdf-cid-table-next-cid table)))
          (when (> cid #xffff)
            (error "epdf: too many CIDs for one font: %d" cid))
          
          ;; Next allocation.
          (setf (epdf-cid-table-next-cid table) (1+ cid))
          
          ;; Main lookup: shaped glyph identity -> PDF CID.
          (puthash key cid (epdf-cid-table-by-key table))
          
          ;; /CIDToGIDMap data.
          (puthash cid gid (epdf-cid-table-cid-to-gid table))
          
          ;; /ToUnicode data.
          (puthash cid unicode (epdf-cid-table-cid-to-unicode table))
          
          (setf (epdf-glyph-cid glyph) cid)
          
          cid))))

(defun epdf--glyph-advance (glyph widths)
  "Return the advance of GLYPH in thousandths of an em."
  (or (epdf-glyph-advance glyph)
      (epdf--font-width-for-gid widths (epdf-glyph-gid glyph))))

(defun epdf--run-advance (run &optional font-index size)
  "Return the advance of RUN in text space units."
  (let ((widths (nth (or font-index epdf-text-font) epdf-font-width-vectors))
        (size (or size epdf-text-font-size))
        (total 0))
    (dolist (glyph (epdf-glyph-run-glyphs run) total)
      (cl-incf total (/ (* (epdf--glyph-advance glyph widths) size) 1000.0)))))

(provide 'epdf-core)

;;; epdf-core.el ends here
