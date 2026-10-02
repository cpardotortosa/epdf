;;; epdf.el --- Generation of pdf files.  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Section numbers in comments refer to ISO 32000-2:2020 (PDF 2.0).

(require 'cl-lib)
(require 'epdf-ttf)
(require 'epdf-core)
(require 'epdf-font-search)

;;; Code:

(defun epdf-embed-ttf (font-name)
  (let* ((found (epdf-find-file-for-font font-name))
         (font-file (car found))
         (face-index (cdr found)))
    ;; (print found)
    (epdf-ttf-extract-info font-file face-index)

    (let* ((type0-id      (epdf--reserve-object))
           (cidfont-id    (epdf--reserve-object))
           (descriptor-id (epdf--reserve-object))
           (fontfile-id   (epdf--reserve-object))
           (cidtogidmap-id  (epdf--reserve-object))
           (tounicode-id    (epdf--reserve-object))             
           (width-vector    (epdf--make-font-width-vector))
           ;; A font that will be subset gets the subset tag in front of
           ;; its name (9.9.2 Font subsets).  Whether it will is known
           ;; now: the same test decides it when the font is written.
           (pdf-font-name
            (if (epdf-ttf--program-subsettable-p epdf-ttf--bits-data)
                (intern (concat (epdf--subset-tag (length epdf-fonts))
                                "+"
                                (symbol-name (epdf--pdf-font-name))))
              (epdf--pdf-font-name))))
      
      ;; Type0 font.
      ;; PDF spec 9.7.6: Type 0 fonts are composite fonts.  The
      ;; DescendantFonts array contains exactly one CIDFont dictionary for
      ;; the simple Identity-H use case here.        
      (epdf--insert-referenced-object-at
       type0-id
       (epdf--dict
        `((Type . Font)
          (Subtype . Type0)
          ;; This name should be same for the descendant font.
          (BaseFont . ,pdf-font-name)
          ;; 9.7.5.2 Predefined CMaps
          (Encoding . Identity-H)
          (ToUnicode . ,(epdf--objref tounicode-id))
          ;; This is always an array of 1            
          (DescendantFonts . ,(epdf--array
                               (list (epdf--objref cidfont-id)))))))
      
      ;; 9.8 Font descriptors
      ;; A font descriptor, which references a font file that comes next.
      ;; Only required fields are included.
      (epdf--insert-referenced-object-at
       descriptor-id
       (epdf--dict
        `((Type . FontDescriptor)
          (FontName . ,pdf-font-name)
          (Flags . 4) ;; 9.8.2 Font descriptor flags.
          (FontBBox . ,(epdf--convert-font-bbox epdf-ttf-font-bbox))            
          (ItalicAngle . ,epdf-ttf-italic-angle)
          (Ascent . ,(epdf--convert-font-units epdf-ttf-typo-ascender))
          (Descent . ,(epdf--convert-font-units epdf-ttf-typo-descender))
          (CapHeight . ,(epdf--convert-font-units epdf-ttf-cap-height))
          (StemV . 0) 
          (FontFile2 . ,(epdf--objref fontfile-id)))))
      
      ;; The font file stream is written at the end, by
      ;; `epdf--write-late-font-objects', when the glyphs used are known.
      ;; Keep the font program until then: the whole file for a normal
      ;; font, or the face rebuilt from a collection.  Either way,
      ;; `epdf-ttf-extract-info' has left it in `epdf-ttf--bits-data'.
      (epdf--append-font epdf-font-fontfile-ids fontfile-id)
      (epdf--append-font epdf-font-programs epdf-ttf--bits-data)

      (epdf--append-font epdf-font-names font-name)
      (epdf--append-font epdf-font-pdf-names pdf-font-name)        
      (epdf--append-font epdf-font-cid-tables (epdf--make-cid-table))
      (epdf--append-font epdf-font-cidfont-ids cidfont-id)
      (epdf--append-font epdf-font-cidtogidmap-ids cidtogidmap-id)
      (epdf--append-font epdf-font-tounicode-ids tounicode-id)
      (epdf--append-font epdf-font-descriptor-ids descriptor-id)
      (epdf--append-font epdf-font-width-vectors width-vector)
      ;; Ascent and descent that decide the height of a line, in PDF
      ;; glyph space units.  These are the ones Windows uses; without an
      ;; OS/2 table, take something reasonable.
      (epdf--append-font epdf-font-line-metrics
                         (if epdf-ttf-win-ascent
                             (cons (epdf--convert-font-units epdf-ttf-win-ascent)
                                   (epdf--convert-font-units epdf-ttf-win-descent))
                           (cons 800 200)))

      (epdf--append-font epdf-fonts type0-id)
      ;; Return font index.
      (1- (length epdf-fonts)))))

(defun epdf-begin(&optional paper-size)
  "Starts building PDF document.
PAPER-SIZE is the default size for new pages. It can be a list (width
height), specified in points.  It can be a symbol, as in
`ps-paper-type'.  If nil, take the value of `ps-paper-type'.  If that is
also nil, use A4."
 
  (setq epdf-start-of-xref 0)
  (setq epdf-running-id 1)
  (setq epdf-objects nil)
  (setq epdf-pages-object-id nil)
  (setq epdf-pages nil)
  (setq epdf-current-page-info nil)
  (setq epdf-current-page-content nil)

  (setq epdf-fonts nil)
  (setq epdf-font-names nil)
  (setq epdf-font-pdf-names nil)
  (setq epdf-font-cid-tables nil)
  (setq epdf-font-cidfont-ids nil)
  (setq epdf-font-cidtogidmap-ids nil)
  (setq epdf-font-descriptor-ids nil)
  (setq epdf-font-width-vectors nil)
  (setq epdf-font-line-metrics nil)
  (setq epdf-font-fontfile-ids nil)
  (setq epdf-font-programs nil)
  (setq epdf-font-tounicode-ids nil)
  ;; Font files are read again for each document, to see fonts
  ;; installed or removed since the last one.
  (clrhash epdf--family-names-cache)

  (setq epdf-paper-size (epdf--get-paper-size paper-size))
  (message "epdf: start document")
  (message "epdf: default paper size %s" epdf-paper-size)
            
  (setq epdf-buffer (get-buffer-create "*mkpdf*"))
  
  (with-current-buffer epdf-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (erase-buffer)
    ;; 7.5.2 File Header
    (insert "%PDF-2.0\n")
    ;; Comment with binary data so no one thinks this is a text file.
    (insert "%\x81\x82\x83\x84 000004\n") 
    ;; 7.5.3 File body
    ;; Metadata (object 1)
    (epdf--insert-referenced-object
     (epdf--stream (epdf--xmp-metadata) 'Metadata 'XML))))

(defun epdf-end ()
  (when epdf-current-page-info
    (epdf--close-current-page))

  (with-current-buffer epdf-buffer
    (let* (;; Pages are pushed, so reverse them back to document order.
           (pages (nreverse epdf-pages))

           ;; Reserve all ids first.
           (page-ids
            (cl-loop repeat (length pages)
                     collect (epdf--reserve-object)))
           (content-ids
            (cl-loop repeat (length pages)
                     collect (epdf--reserve-object)))
           (pages-id   (epdf--reserve-object))
           (catalog-id (epdf--reserve-object)))

      (setq epdf-pages-object-id pages-id)

      ;; Write page dictionaries and content streams.
      (cl-loop for page in pages
               for page-id in page-ids
               for content-id in content-ids
               do
               (let ((descriptor (copy-sequence (car page)))
                     (content    (cadr page)))

                 (push `(Parent . ,(epdf--objref pages-id)) descriptor)
                 (push `(Contents . ,(epdf--objref content-id)) descriptor)

                 (epdf--insert-referenced-object-at
                  page-id
                  (epdf--dict descriptor))

                 (epdf--insert-referenced-object-at
                  content-id
                  (epdf--stream content))))

      ;; Write Pages object.
      ;; 7.7.3.2 Page tree nodes
      (epdf--insert-referenced-object-at
       pages-id
       (epdf--dict
        `((Type . Pages)
          (Count . ,(length pages))
          (Kids . ,(epdf--array
                    (mapcar #'epdf--objref page-ids))))))

      ;; Write Catalog object.
      ;; 7.7.2 Document catalog dictionary
      (epdf--insert-referenced-object-at
       catalog-id
       (epdf--dict
        `((Type . Catalog)
          (Metadata . ,(epdf--objref 1))
          (Pages . ,(epdf--objref pages-id)))))

      ;; Write font objects whose contents depend on the glyphs actually used
      ;; by page content.  This must happen before xref, because the xref loop
      ;; requires every reserved object to have been written.
      (epdf--write-late-font-objects)

      ;; 7.5.4 Cross-reference table.
      ;; PDF spec 7.5.4: the xref table is indexed by object number.
      ;; epdf-objects stores (OBJECT-ID . BYTE-OFFSET)
      (setq epdf-start-of-xref (1- (point)))
      (insert "xref\n")
      (insert (format "0 %d\n" epdf-running-id))
      (insert (format "%010d %05d f\r\n" 0 65535))

      (cl-loop for id from 1 below epdf-running-id
               for offset = (cdr (assq id epdf-objects))
               do
               (unless offset
                 (error "epdf: reserved object %d was never written" id))
               do
               (insert (format "%010d %05d n\r\n" offset 0)))

      ;; 7.5.5 File trailer
      (insert "trailer\n")
      (epdf--insert-dictionary
       `((Size . ,epdf-running-id)
         (ID . ,(epdf--array
                 (list
                  (epdf--byte-string
                   (unibyte-string
                    1 2 3 4 5 6 7 8
                    9 10 11 12 13 14 15 16))
                  (epdf--byte-string
                   (unibyte-string
                    1 2 3 4 5 6 7 8
                    9 10 11 12 13 14 15 16)))))
         (Root . ,(epdf--objref catalog-id))))

      (insert "startxref\n")
      (insert (format "%d\n" epdf-start-of-xref))
      (insert "%%EOF\n"))))

;; 7.7.3.3 Page objects
(defun epdf-begin-page (&optional paper-size)
  ;; The per-font lists are kept in embedding order, so fonts can be
  ;; embedded at any point, also between pages.
  (when epdf-current-page-info
    (epdf--close-current-page))
  
  

  (if paper-size
      (setq paper-size (epdf--get-paper-size paper-size))
    (setq paper-size epdf-paper-size))

  (setq epdf-current-page-info
        `((Type . Page)
          ;; This will be filled later:
          ;; (Contents . XXX) ;; context stream is the next object
          ;; (Parent . XXXX) ;; Pages object
          (Resources . ,(epdf--resources-dictionary))
          (MediaBox . ,(epdf--array `(0 0
                                        ,(cl-first paper-size)
                                        ,(cl-second paper-size) )))))
  (setq epdf-current-page-content ""))

(defun epdf-add-page-content (content)
  "This function adds raw page content (PDF graphics commands)"
  (setq epdf-current-page-content
        (concat epdf-current-page-content content "\n")))

;; 9.4 Text objects (BT)
(defun epdf-text-begin ()
  (setq epdf-text-font nil)
  (setq epdf-text-font-size nil)
  (setq epdf-text-x 0)
  (setq epdf-text-y 0)
  (setq epdf-text-pen-x 0)
  (setq epdf-text-spacing-line 0)
  (setq epdf-text-spacing-char 0) 
  (setq epdf-text-spacing-space 0)
  (setq epdf-text-content " BT "))

;; 9.3 Text state parameters and operators (Tf)
(defun epdf-text-font (font size)
  "Font is an index. 0 is first embedded font."
  (setq epdf-text-font font)
  (setq epdf-text-font-size size)
  (setq epdf-text-content
        (concat epdf-text-content
                (format " /F%d %d Tf " (+ 20 font) size))))

;; 9.4.2 Text-positioning operators (Td)
(defun epdf-text-xy (xy)
  (cl-incf epdf-text-x (car xy))
  (cl-incf epdf-text-y (car (cdr xy)))
  (setq epdf-text-pen-x epdf-text-x)
  (setq epdf-text-content
        (concat epdf-text-content
                (format " %d %d Td " (car xy) (car (cdr xy))))))

(defun epdf-shape-string (font string &optional direction)
  "Shape STRING using Emacs FONT object.

Return an `epdf-glyph-run'.

FONT must be an Emacs font object, not an epdf font index.

DIRECTION may be nil, `L2R', or `R2L'."
  (let* ((string (string-to-multibyte string))
         ;; Use explicit end index; it is less ambiguous than passing nil.
         (gstring (composition-get-gstring 0 (length string) font string))
         (shaped-gstring
          (or (font-shape-gstring gstring direction)
              ;; The shaper returns nil when the result does not fit in
              ;; the glyph string, which has one slot per character.
              ;; That happens when shaping expands the text, as in some
              ;; scripts.  Try again with more room before giving up:
              ;; falling back to the unshaped glyph string would silently
              ;; emit the raw cmap glyphs.
              (font-shape-gstring (epdf--grow-gstring gstring 4) direction)
              (error "epdf: cannot shape %S with %s"
                     string (font-get font :family)))))
    (make-epdf-glyph-run
     :glyphs (epdf--gstring->epdf-glyphs
              shaped-gstring string
              ;; Pixels of FONT to thousandths of an em.  Positions come
              ;; in whole pixels, so shape with a font object of
              ;; `epdf--shaping-size' pixels to lose nothing.
              (/ 1000.0 (font-get font :size))))))

(defun epdf-text-position ()
  "Return the current text position as (X . Y).

X is where the last shown text ended, Y the baseline of the current line."
  (cons epdf-text-pen-x epdf-text-y))

;; 9.4.3 Text-showing operators (TJ)
(defun epdf-text-glyph-run (run)
  "Append shaped glyph RUN to the current PDF text object.

Glyphs are shown with TJ, whose numbers move the position between two
glyphs.  That places a glyph HarfBuzz moved sideways (its x-offset) and
gives it HarfBuzz's advance instead of the font's width.  A glyph moved
up or down (its y-offset) goes on its own, with the text rise (Ts, 9.3.7)
set just for it."
  (let ((widths (nth epdf-text-font epdf-font-width-vectors))
        (items nil)                     ; elements of the current TJ, reversed
        (ops nil))                      ; operators so far, reversed
    (cl-flet* ((flush ()
                 (when items
                   (push (concat "[" (mapconcat #'identity (nreverse items) " ")
                                 "] TJ")
                         ops)
                   (setq items nil)))
               (move-left (n)
                 ;; A TJ number N moves the position left by N
                 ;; thousandths of an em.
                 (let ((n (round n)))
                   (unless (zerop n)
                     (push (number-to-string n) items))))
               (show (hex)
                 ;; Glyphs with nothing in between go in one string.
                 (if (and items (string-prefix-p "<" (car items)))
                     (setcar items (concat (substring (car items) 0 -1) hex ">"))
                   (push (concat "<" hex ">") items))))
      (dolist (glyph (epdf-glyph-run-glyphs run))
        (let ((hex (epdf--cid-hex
                    (epdf--register-glyph-cid epdf-text-font glyph)))
              (x (epdf-glyph-x-offset glyph))
              (y (epdf-glyph-y-offset glyph)))
          (unless (zerop y)
            (flush)
            ;; The text rise is in points, not thousandths of an em.
            (push (format "%.3f Ts" (/ (* y epdf-text-font-size) 1000.0)) ops))
          ;; Draw the glyph X to the right, then go on to where the next
          ;; one starts: showing it advanced X plus the width in /W, and
          ;; it should have advanced only its advance.
          (move-left (- x))
          (show hex)
          (move-left (- (+ x (epdf--font-width-for-gid
                              widths (epdf-glyph-gid glyph)))
                        (epdf--glyph-advance glyph widths)))
          (unless (zerop y)
            (flush)
            (push "0 Ts" ops))))
      (flush))
    (setq epdf-text-content
          (concat epdf-text-content
                  (mapconcat #'identity (nreverse ops) " ")
                  " ")))
  (cl-incf epdf-text-pen-x (epdf--run-advance run)))

(defun epdf-text-string (string)
  (let* ((font-object (epdf--font-object-for-index
                      epdf-text-font
                      epdf--shaping-size))
         (run (epdf-shape-string font-object string)))
    (epdf-text-glyph-run run)))

;; 9.4.2 Text-positioning operators (T*)
(defun epdf-text-next-line ()
  "Move to the start of the next line.

The PDF operator T* is equivalent to `0 -TL Td', so it moves down by
the current line spacing and returns to the x where the current line
started, which is the one tracked in `epdf-text-x'."
  (cl-decf epdf-text-y epdf-text-spacing-line)
  (setq epdf-text-pen-x epdf-text-x)
  (setq epdf-text-content (concat epdf-text-content " T* ")))

;; 9.3.5 Leading (TL)
;; TODO: Implement char and space
(defun epdf-text-spacing (line &optional _char _space)
  (when line
    (setq epdf-text-spacing-line line)
    (setq epdf-text-content (concat epdf-text-content
                                    (format " %d TL " line)))))

;; 9.4 Text objects (ET)
(defun epdf-text-end ()
  (setq epdf-text-content (concat epdf-text-content " ET "))
  (epdf-add-page-content epdf-text-content))

(provide 'epdf)

;;; epdf.el ends here
