;;
;;;
;;;    MKSHAPE.LSP - Written by Randy Kintzley
;;;    Copyright © 1999 by Autodesk, Inc.
;;;
;;;    Your use of this software is governed by the terms and conditions of the
;;;    License Agreement you accepted prior to installation of this software.
;;;    Please note that pursuant to the License Agreement for this software,
;;;    "[c]opying of this computer program or its documentation except as
;;;    permitted by this License is copyright infringement under the laws of
;;;    your country.  If you copy this computer program without permission of
;;;    Autodesk, you are violating the law."
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;  ----------------------------------------------------------------
 
;Globals used for retaining defaults
; acet:mkshape-fna - default filename
; acet:mkshape-res - default resolution for shape
;
(defun c:mkshape ( / SWAP_3 FNA LST MX A SHPTYPE NAME
                     KEY ID FLAG RES BSPNT SS FNA2 newflag xflag xfna flt
                 )
 
 
 (acet-error-init
  (list (list "cmdecho" 0
               "expert" nil
        );list
        T
  );list
 );acet-error-init
 (sssetfirst nil nil)
 
 ;local function
 (defun swap_3 ( lst / n a)
  (setq n 0)
  (repeat (length lst)
   (setq   a (nth n lst)
           a (append (list (caddr a) (cadr a) (car a))
                           (cdr (cdr (cdr a)))
             );append re-arange the order of the first 3 elements for
                 ;       searching using assoc
         lst (subst a (nth n lst) lst)
   );setq
   (setq n (+ n 1));setq
  );repeat
  lst
 );defun swap_3
                          ;get the shp filename to write to.
(if (not acet:mkshape-fna)
    (setq acet:mkshape-fna (acet-filename-ext-remove (getvar "dwgname")));setq then
);if
(setvar "expert" 2)
(setq fna (ACET-FILE-WRITEDIALOG "MKSHAPE - Select Shape File"
                            acet:mkshape-fna
                            "shp"
                            "Acet:Mkshape"
                            1665
          );ACET-FILE-WRITEDIALOG
);setq
(if (assoc "EXPERT" (car acet:sysvar-list))
    (setvar "expert" (cadr (assoc "EXPERT" (car acet:sysvar-list))));then
);if
(if fna
    (progn
     (setq acet:mkshape-fna fna);setq then set the default for next time.
     (if (and (not (findfile fna))                                ;new shp
              (setq a (findfile (strcat (acet-filename-ext-remove fna) ".shx")))    ;shx already exists
         );and
         (progn
          (acet-autoload '("yes_no.lsp" "(bns_get_yes_no a b)"))
          (if (equal (bns_get_yes_no
                      (list "OVERWRITE WARNING"
                       (acet-str-format "\n\n\tMKSHAPE will compile \"%1\"\n\t                      to create \"%2.SHX\"\n\n\t\t\"%3.SHX\" already exists.\n\n\t\t\t\tOverwrite?"
                        (xstrcase fna) 
                        (xstrcase (acet-filename-ext-remove fna)) 
                        (xstrcase (acet-filename-ext-remove fna))
                       )
                      );list
                      '(60 15)
                     );bns_get_yes_no
                     0
              );equal
              (exit)
          );if
         );progn
     );if
     (if (and (findfile fna)
              (setq lst (bns_read_shp_file fna))
         );and
         (progn
          (setq  mx (itoa (last lst))
                lst (car lst)
                lst (acet-list-isort lst 0)
          );setq
          (if (and (setq a (assoc "*0" lst))
                   (equal (cadr a) "4")
              );and
              (progn
               (princ (acet-str-format "\n*Invalid* %1 is a font file."  fna ))
               (exit)
              );progn
              (progn
               (setq shptype "Shape");setq
               (setq lst (swap_3 lst));setq
              );progn else it's shape file
          );if
         );progn then read the shape file and determine if it is a font file or a shape file.
         (progn
          (if (not shptype)
              (setq shptype "Shape");setq
          );if
          (setq mx "1")
         );progn else ask what type of file the user wishes to write, a Shape or Font?
     );if
     (if (equal shptype "Shape")
         (progn
          (setq name "")
          (while (not (snvalid name));not
           (setq name (xstrcase (getstring "\nEnter the name of the shape: ")
                      )
                 xflag nil
                  xfna nil
           );setq
           (cond
            ((not (snvalid name))
             (princ "\n*Invalid shape name*")
            );cond #1
            ((> (strlen name) 18)
             (setq name "")
             (princ "\n*Invalid* Shape name too long.")
            );cond #2
            ((and 
                  (setq xflag (bns_shape_exists name)) ;shape exist already
                  (setq xfna (acet-file-find-font (bns_get_shapefile name))) ;what file?
                  (not (acet-str-equal 
                              xfna                    ;if same file, it's cool but
                              (acet-file-find-font    ;not if another file defines
                                         (strcat (acet-filename-ext-remove fna) ;the same shape.
                                                 ".shx"
                                         );strcat
                              )
                       );acet-str-equal
                  );not
             );and
             (princ
              (acet-str-format "\n*Invalid* Shape \"%1\" already exists in loaded shape file \"%2\"." name xfna)
             );princ
             (if (findfile (strcat (acet-filename-ext-remove xfna) ".SHP"))
                 (princ
                  (acet-str-format "\nYou can redefine it only if you choose \"%1.SHP\" for the filename." (acet-filename-ext-remove xfna))
                 )
             );if
             (setq name "")
            );cond #3
           );cond
          );while
          (if lst
              (setq key mx
                    ;key (caddr (last lst))
                    ;key (itoa (+ 1 (atoi (substr key 2))))
              );setq
              (setq key "1");else
          );if
          (setq id name);setq
         );progn then SHAPE
         (progn
          (while (equal (strlen (setq key (getstring "\nEnter the desired character: ")))
                        0
                 );equal
           (princ "\n*Invalid*")
          );while
          (setq key (substr key 1 1)
                key (ascii key)
                key (itoa key)
                 id (strcat "*" key)
          );setq
          (setq name "")
         );progn else FONT
     );if
     (setq flag (assoc id lst))
     (if (and (not flag)
              (not xflag)
         );and
         (setq newflag T);set then
     );if
     (if (or newflag
             (initget "Yes No _Yes No")
             (equal "Yes"
                    (getkword "\nThis shape already exists. Overwrite it? [Yes/No] <No>: ")
             )
         );or
         (progn
          (if flag
              (setq lst (acet-list-remove-nth (vl-position flag lst) lst));setq then
          );if
          (if (equal shptype "Shape")
              (progn
               (setq lst (swap_3 lst));setq then
               (if flag
                   (setq key (caddr flag)
                         key (substr key 2)
                   );setq then
               );if
              );progn then
          );if
 
          (if (not acet:mkshape-res) (setq acet:mkshape-res 128.0));if
          (initget 6)
          (setq res (getint 
                     (acet-str-format "\nEnter resolution <%1>: "  
                      (itoa (fix acet:mkshape-res))
                     )
                    );getint
          );setq
          (if (not res)
              (setq res acet:mkshape-res);setq then
              (setq res (acet-calc-round (abs (float res))
                                   8.0
                        )
              );setq else
          );if
          (if (< res 8.0)
              (setq res 8.0)
              (progn
               (if (> res 32767)
                   (setq res 32767.0);setq
               );if
              );progn else
          );if
          (setq acet:mkshape-res res)
 
          (if (and (setq bspnt (initget 1)
                         bspnt (getpoint "\nSpecify insertion base point: ")
                   );setq
                   (setq flt (acet-ss-flt-cspace)
                         flt (list
                                    '(-4 . "<AND")
                                      (car flt)  	;; the tab
                                      (cadr flt) 	;; the 67 group code
                                      '(-4 . "<OR")
                                         '(0 . "LINE") '(0 . "ARC") '(0 . "CIRCLE")
                                         '(0 . "LWPOLYLINE") '(0 . "ELLIPSE")
                                         '(0 . "SPLINE") '(0 . "3DFACE") '(0 . "SOLID") '(0 . "TRACE")
                                         '(-4 . "<AND")     ;dis-allow 3dmesh and polyface mesh
                                            '(0 . "POLYLINE")
                                            '(-4 . "<NOT")
                                            '(-4 . "&")
                                            '(70 . 112)
                                            '(-4 . "NOT>")
                                         '(-4 . "AND>")
                                      '(-4 . "OR>")
                                    '(-4 . "AND>")
                             );list
                          ss (ssget flt)
 
                   );setq
              );and
              (progn
 
               (setq fna2 (strcat (acet-filename-ext-remove fna) ".shx"))
               (acet-file-backup fna)  ;Create temp backups of old files (shp and shx)
               (acet-file-backup fna2) ;and add a routine to *error* to restore
                                      ;the backups in the event of an error.
 
               (mkshape shptype bspnt ss fna key name res lst mx)
 
               (command "_.compile" fna)
 
               (if (and fna2
                        (bns_shx_loaded fna2)
                   );and
                   (progn
                    (bns_shx_reload fna2)
                   );progn then
                   (progn
                    (command "_.load" fna2)
                    ;(setq newflag T)
                   );progn else
               );if
               (if (and name
                        (bns_shape_exists name)
                   );and
                   (progn
                    (if newflag
                        (princ (acet-str-format "\nShape \"%1\" created." name))
                        (princ (acet-str-format "\nShape \"%1\" redefined." name))
                    );if
                    (princ "\nUse the SHAPE command to place shapes in your drawing.")
                   );progn
                   (princ "\nShape definition failed.")
               );if
               ;(command "_.shape" name pause 1.0 0)
 
               (acet-file-backup-delete)
               ;all is OK so delete the backup files
 
              );progn then
          );if
         );progn then
     );if
    );progn then got fna
 );if
 
 (acet-error-restore)
);defun c:mkshape
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun mkshape ( shptype bspnt ss fna key name res lst2 mx /
                 FH NXTKEY N A B J LST d fact
               )
 
(if (setq fh (open fna "w"));setq
    (progn
     (if lst2
         (progn
          (setq nxtkey (+ 1 (atoi mx)));setq
          (princ "\nRe-writing existing portion of shp file out...")
         );progn then
         (setq nxtkey 2);setq else
     );if
     (if (and (equal shptype "Font")
              (not lst2)
         );and
         (setq lst2 (list
                     (list
                      "*0" "4" "AutoCAD Express Tools (C) Copyright 1999 by Autodesk, Inc."
                      "21,7,2,0"
                     );list
                    );list
         );setq then
     );if
     (setq n 0)            ;write the existing portion of the font out.
     (repeat (length lst2)
      (setq a (nth n lst2)
            b ""
            j 0
      );setq
 
      (write-line (strcat (car a) "," (cadr a) "," (caddr a)) fh)
      (setq a (cdr (cdr (cdr a))));setq
      (setq j 0)
      (repeat (length a)
       (write-line (nth j a) fh)
      (setq j (+ j 1));setq
      );repeat
 
     (setq n (+ n 1));setq
     );repeat
     (if lst2 (princ "Done."));if
     (close fh)
 
     (setq   lst (mkshape_get_ent_points bspnt ss res);get a list of points on the geometry
               d (cadr lst) ;the dx
             lst (car lst)
            fact (find_best_scale_fact d res)
             lst (shape_def2 lst res d fact) ;convert the coords to shape format.
     );setq
     (princ "Done.")
     (princ "\nWriting new shape...")
     (mkshape_write_new_shape key name lst fna nxtkey)
     (princ "Done.")
 
    );progn then
);if
 
);defun mkshape
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;get_ent_points -  takes a selection set and a resolution value (integer i.e. 127).
;Returns a list of sub-lists.
;Each sub-list is a list of points along an entity from the selection set.
;
;
(defun mkshape_get_ent_points ( bspnt ss res / N NA LST P1 P2 A LST2 D LST3 J LST4)
 
 (princ "\nDetermining geometry extents...")
 (setq n 0)
 (repeat (sslength ss)       ;get the max and min points
  (setq   na (ssname ss n)
         lst (acet-geom-object-point-list na nil)
  );setq
  (if (not p1)
      (setq p1 (acet-geom-list-extents lst));setq then
      (setq p1 (acet-geom-list-extents (append p1 lst)));setq else
  );if
  (setq n (+ n 1));setq
 );repeat
 (princ "Done.")
 
 (setq p2 (cadr p1)
       p1 (car p1)
        a (min (- (car p2) (car p1))
               (- (cadr p2) (cadr p1))
          );min
 );setq
 (if (equal a 0.0 0.00000001)
     (setq a (max (- (car p2) (car p1))
                  (- (cadr p2) (cadr p1))
             );max
     );setq then
 );if
 (if (equal a 0.0 0.00000001)
     (setq a 0.00000001)
 );if
 (setq  a (/ a res)       ;calculate the resolution to use with ep_list
        a (/ a 5.0)       ;five points on the ent for every shape res grid point
       p1 nil             ;will help to ensure a good translation.
 );setq
 
 (princ "\nBuilding coord lists...")
 (setq n 0)
 (repeat (sslength ss)            ;get the list of point lists and a new max and min
  (setq   na (ssname ss n)
         lst (acet-geom-object-point-list na a)
        lst2 (append lst2 (list lst))
  );setq
  (if (not p1)
      (setq p1 (acet-geom-list-extents lst));setq then
      (setq p1 (acet-geom-list-extents (append p1 lst)));setq else
  );if
 (setq n (+ n 1));setq
 );repeat
 (princ "Done.")
 
 (princ "\nFormating coords....")
 (setq  p2 (cadr p1)             ;get ready to shift all of the points such that
        p1 (car p1)              ;the lower left most point is at the origin.
         d (acet-geom-delta-vector p1 p2)
         d ;(/ 1.0         ;the scale factor to bring the biggest dimension down to 1.0
              (max (abs (car d)) (abs (cadr d)))
           ;)
        p2 (acet-geom-delta-vector p1 p2)         ;p2 is now expressed as an offset from p1
        p2 (acet-geom-vector-scale p2 d)       ;adjust for scale to 1
        p1 (acet-geom-vector-scale bspnt -1.0) ;The offset to move from base point to 0,0
 );setq
 (setq n 0)
 (repeat (length lst2)
 (setq  lst (nth n lst2)   ;list of coords for a single ent.
       lst3 nil
 );setq
  (setq j 0)
  (repeat (length lst)
   (setq    a (nth j lst)
            a (acet-geom-vector-add a p1)  ;move it
            ;a (acet-geom-vector-scale a d)    ;scale it
   );setq
   (setq lst3 (append lst3 (list a)));setq
   (setq j (+ j 1));setq
  );repeat
  (setq lst4 (append lst4 (list lst3)));setq
 (setq n (+ n 1));setq
 );repeat
 
(list lst4 d)
);defun mkshape_get_ent_points
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun shape_def2 ( lst res dx fact / A K N LST2 LST3 J B V D LST4)
 
 
;just to put things back the way they were, add a fake entity
;sublist to make the last point a return to 0,0.
(if (not (equal (last lst) '((0.0 0.0))));not equal
    (setq lst (append lst '(((0.0 0.0))) );append
    );setq then
);if
 
(setq a '(0.0 0.0 0.0));setq the pen location
(setq k 0);setq
 
(setq n 0)
(repeat (length lst)     ;repeat through the list of point sublists (one for each object)
(setq lst2 (nth n lst)
);setq
 
;(pline (list '(62 . 1) lst2))
 
(setq      lst2 (car (snap_to_shp_res lst2 res dx))   ;the snapped points
      lst3 nil
);setq
 
;(pline (list '(62 . 2) lst2))
;(getstring "hey")
;(entdel (entlast))
;(entdel (entlast))
 
 
(acet-spinner)
 (setq j 0)
 (repeat (length lst2)                        ;repeat through coord list and convert
  (setq    b (nth j lst2)
           v (acet-geom-delta-vector a b)                  ;the offset from current pen pos to point b
           v (vtoshp v res dx)                   ;the vector converted to a SHP string vector
           d (acet-geom-vector-add a (shptov v res dx)) ;d is a candidate for new pos. of the pen
  );setq
  (if (or (not (equal "(0,0)" (substr v 1 5)))    ;not a 0 length vector
          (not lst3)
      );or
      (progn
       (setq    a d);setq             ;set the new pen location
       (if (not lst3)
           (setq lst3 (append (vect_dist_check2 (list v)) ;move to start of object
                              (list "001")                ;drop the pen down
                      );append
           );setq
           (setq lst3 (append lst3
                              (vect_dist_check2 (list v)) ;move along the object
                      );append
           );setq
       );if
      );progn then the offset was not a 0 length vector so add it to the list
  );if
 (setq j (+ j 1));setq
 );repeat
 (if lst3
     (setq lst4 (append lst4 (list lst3)));setq add the converted object geometry to lst4
 );if
(setq n (+ n 1));setq
);repeat
 
;Now add needed pen up and pen down sequences.
;Also create subshapes as needed.
(setq  lst nil
      lst2 nil
         k 0
);setq
(setq n 0)
(repeat (length lst4)
(setq lst3 (nth n lst4));setq the geometry for an object
(setq lst2 (append lst2             ;a list of one or more converted objects
                   (list "002" "9") ;pen up to get ready for new object
           );append                 ; lst2 holds one shape def at max
         k (+ k 2)
);setq
(while (not (equal (car lst3) "001")) ;loop until reaching the pen down/start point
 (setq lst2 (append lst2 (list (car lst3)))
          k (+ k 2)
       lst3 (cdr lst3)
 );setq
 (if (> k 1900)
     (setq lst2 (start_new_subshape lst2 k dx res fact)
            lst (append lst (list lst2)) ;lst is a list of shape(s)
           lst2 (list "002" "9") ;continue pen up for the next shape
              k 2
     );setq then
 );if
);while
(if (and (wcmatch (last lst2) "(*)")       ;if last item is a coord and its not (0,0)
         (not (equal (last lst2) "(0,0)")) ;to end the 9 specification
    );and
    (setq lst2 (append lst2 (list "(0,0)"))
             k (+ k 2)
    );setq
);if
(setq lst2 (append lst2 (list "001" "9")) ;now at ent start so drop the pen down
         k (+ k 2)
      lst3 (cdr lst3) ;remove the "001" from lst3
);setq
 (setq j 0)
 (repeat (length lst3)   ;with pen down, race through the coords of the ent
  (setq a (nth j lst3))
  (setq lst2 (append lst2 (list a))
           k (+ k 2)
  );setq
  (if (> k 1900)
      (setq lst2 (start_new_subshape lst2 k dx res fact)
             lst (append lst (list lst2))
            lst2 (list "9") ;continue pen down for the next shape
               k 1
      );setq then
  );if
 (setq j (+ j 1));setq
 );repeat
 
 (if (equal (last lst2) "9")
     (setq lst2 (reverse (cdr (reverse lst2)))
              k (- k 1)
     );setq then
     (progn
      (if (and (not (equal (last lst2) "(0,0)"))
               (wcmatch (last lst2) "(*)")
          );and
          (setq lst2 (append lst2 (list "(0,0)"))
                   k (+ k 2)
          );setq
      );if
     );progn else
 );if
(acet-spinner)
 
(setq n (+ n 1));setq
);repeat
 
(if lst2
    (setq lst2 (start_new_subshape lst2 k dx res fact)
           lst (append lst (list lst2))
    );setq
);if
 
 
 lst
);defun shape_def2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun start_new_subshape ( lst2 k dx res fact / )
(if (and (not (equal (last lst2) "(0,0)"))
         (wcmatch (last lst2) "(*)")
    );and
    (setq lst2 (append lst2 (list "(0,0)"))
             k (+ k 2)
    );setq
);if
(setq lst2 (mkshape_add_scale_fact lst2 k dx res fact));setq
 
);defun start_new_subshape
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun vect_dist_check2 ( lst / A B S1 S2 C D X Y FLAG)
 
(while (not flag)
 (setq a (last lst)
       a (substr a 2)
       a (substr a 1 (- (strlen a) 1))
       a (acet-str-to-list "," a)
       b (read (cadr a))
       a (read (car a))
 );setq
 (if (< a 0)
     (setq s1 -1)
     (setq s1 1)
 );if
 (if (< b 0)
     (setq s2 -1)
     (setq s2 1)
 );if
 (setq a (abs a)
       b (abs b)
 );setq
 (setq c (max a b));setq
 (if (> c 127)
     (progn
      (setq   d (/ 127.0 (float c))
              x (list (fix (acet-calc-round (* d a) 1.0))
                      (fix (acet-calc-round (* d b) 1.0))
                )
              y (list (* (- a (car  x)) s1)
                      (* (- b (cadr x)) s2)
                );list
              x (list (* (car  x) s1)
                      (* (cadr x) s2)
                );list
              x (strcat "(" (itoa (car x)) "," (itoa (cadr x)) ")")
              y (strcat "(" (itoa (car y)) "," (itoa (cadr y)) ")")
            lst (reverse (cdr (reverse lst)))
            lst (append lst (list x y))
      );setq
     );progn then
     (setq flag T);setq else no need to to split last vector
 );if
);while
 
lst
);defun vect_dist_check2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun mkshape_write_new_shape ( key name lst2 fna nxtkey / FH SUB J LST B FLAG A N)
 
(if (not (setq fh (open fna "a")))
    (setq lst2 nil);setq then abort
);if
(setq nxtkey (- nxtkey 1)
         sub name
);setq
(setq j 0)
(repeat (length lst2)
 (setq  lst (nth j lst2)
          b (cadr lst)                   ;the k or size number
 );setq
 (if (not (equal (+ j 1) (length lst2)))
     (setq    b (+ b 2)                  ;and to size for name and key?
           flag T
     );setq then this is the last time through the repeat loop
     (setq flag nil);setq else
 );if
 (setq  lst (car lst)                         ;the shape vector data
                                              ;build the header for the shape
          a (strcat "*" key ","               ;- the key number
                    (itoa (+ 1 b))            ;- size (add one for the end marker "0")
                    ","
                    sub                       ;- the name of the shape or description
                    ;(xstrcase name)           ;  for a character
            );strcat
 );setq
 (write-line a fh) ;write the header for the new shape
 
 (setq a "")
 (setq n 0);setq
 (repeat (length lst)   ;write out the vector data for the new shape
  (if (>= (strlen a) 118)
      (progn
       (write-line (substr a 2) fh)
       (setq a "")
      );progn then
  );if
  (setq a (strcat a "," (nth n lst)));setq
  (setq n (+ n 1));setq
 );repeat
 
 (if (> (strlen a) 1)
     (setq a (substr a 2))
     (setq a "")
 );if
 
 (setq    sub (strcat name "_SUBSHAPE_" key)
       nxtkey (+ nxtkey 1)
          key (itoa nxtkey)
 );setq
 (if flag
     (setq a (strcat a ",7," key));then reference the next sub shape
 );if
 (setq a (strcat a ",0"));setq
 (write-line a fh)
 
(setq j (+ j 1));setq
);repeat
(close fh)
 
);defun mkshape_write_new_shape
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun snap_to_shp_res ( lst res dx / N A C LST2 LST3 B)
 
(setq n 0)
(repeat (length lst) ;repeat throught the coords.
(setq a (nth n lst)
      c a
      a (vtoshp a res dx) ;change to shape format
      a (shptov a res dx) ;change back to vector
);setq
(if (not (equal a (last lst2))) ;if not a duplicate point
    (progn
     (if (< (length lst2) 2)               ;simply append for the first two times
         (setq lst2 (append lst2 (list a)) ;the snapped points and the
               lst3 (append lst3 (list c)) ;original points
         );setq then
         (progn
          (if (equal (angle (nth (- (length lst2) 2) lst2) ;if no change in angle
                            (last lst2)
                     )
                     (angle (last lst2) a)
                     0.00001
              );equal
              (progn
               ;(print "same angle")
               (setq lst2 (append (reverse (cdr (reverse lst2)))
                                  (list a)
                          );append
                     lst3 (append (reverse (cdr (reverse lst3)))
                                  (list c)
                          );append
               );setq then last two points are at same angle so remove last and add new one
              );progn
              (setq lst2 (append lst2 (list a))
                    lst3 (append lst3 (list c))
              );setq else add new point
          );if
          ;do some resolution enhancment if needed
          (if (and (> (length lst2) 2)
                   (setq b (shp_kill_coord lst2 lst3 b));setq resolution enhancing function.
              );and
              (setq lst2 (car b)
                    lst3 (cadr b)
                       b 99
              );setq
              (setq b nil)
          );if
         );progn else check for duplicate points/same angle points/and points that can
                 ;be removed.
     );if
    );progn
);if
(setq n (+ n 1));setq
);repeat
 
 
(list lst2 lst3) ;return the snapped points and remaining origininals that coorispond.
);defun snap_to_shp_res
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun shp_kill_coord ( lst lst2 flag / A B C P1 P2 P3 X )
 
(setq  lst (reverse lst)
         a (car lst)    ;the snapped points
         b (cadr lst)
         c (caddr lst)
      lst2 (reverse lst2)
        p1 (car lst2)   ;the original coords.
        p2 (cadr lst2)
        p3 (caddr lst2)
         x (inters a c
                   p2
                   (polar p2 (+ (angle a c) (/ pi 2.0)) 1.0)
                   nil
           );inters
)
(if (and x
         (not (equal 0.0 (acet-geom-vector-side x a c)))
         (or (and (not (equal flag 99))
                  (or
                      (<= (distance a p1) (distance b p2))
                      (<= (distance c p3) (distance b p2))
                  );or
             );and
             (and (<= (distance a p1) (distance b p2))
                  (<= (distance c p3) (distance b p2))
             );and
         );or
         (< (distance p2 x)
            (distance p2 b)
         )
    );and
    (progn
     (acet-spinner)
     ;(princ "\nResolution enhancing")
     (setq  lst (append (list a c) (cdr (cdr (cdr lst))))
            lst (reverse lst)
           lst2 (append (list p1 p3) (cdr (cdr (cdr lst2))))
           lst2 (reverse lst2)
            lst (list lst lst2)
     );setq
    );progn then enhance by removing the middle coord.
    (setq lst nil)
);if
 
lst
);defun shp_kill_coord
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find_best_scale_fact ( dx res / x mx lst lst2)
 
;Need to multiply by dx/res
;but first we need to express dx/res
;accurately with integers.
(setq mx 10000.0   ;- num of decmal places to shift the number in order
                    ;  to convert to an integer and retain at least some precision
);setq
(if (< dx mx)
    (setq   x (acet-calc-round (/ mx dx) 1.0)
           dx (* dx x)
          res (* res x)
    );setq
);if
(if (< res mx)
    (setq   x (acet-calc-round (/ mx res) 1.0)
          res (* res x)
           dx (* dx x)
    );setq
);if
(setq  dx (acet-calc-round dx 1.0)
      res (acet-calc-round res 1.0)
);setq
 
(while (or (not lst)
           (not lst2)
       );or
 (setq  lst (find_best_multiples dx)
       lst2 (find_best_multiples res)
 );setq
 (if (or (< dx 10000)
         (< res 10000)
     );or
     (progn
      (if (not lst)
          (setq dx (+ dx 1))
      );if
      (if (not lst2)
          (setq res (+ res 1))
      );if
     );progn then
     (setq   dx (acet-calc-round (/ dx 10.0) 1.0)
            res (acet-calc-round (/ res 10.0) 1.0)
     );setq
 );if
);while
(list lst
      lst2
);list
);defun find_best_scale_fact
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find_best_multiples ( a / x b c d n plst lst lst2 )
 
(setq plst
      (list 1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
            103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199
            211 223 227 229 233 239 241 251
      );list of prime numbers
);setq
 
(setq x a);setq save original val in case we have to use recursion later
(setq b 1.0)
(while (> a 254)
 
(setq d a)
 (setq n 1)
 (while (and (< n (length plst))
             (> a 254)
             (< n a)
        );and
  (setq c (float (nth n plst)))
  (if (equal (/ a c)
             (float (/ (fix a) (fix c)))
      );equal
      (progn
       (if (<= (* b c) 254)
           (setq b (* b c));setq then
           (progn
            (setq lst (append lst (list (list b)))
                    b c
            );setq then
           );progn else
       );if
       (if (not (equal 1 a))
           (setq a (acet-calc-round (/ a c) 1.0));setq
       );if
       (setq n (+ (length plst) 1));setq
      );progn then 'a is evenly divisible by c
  );if
 (setq n (+ n 1));setq
 );while
 
(if (not (equal a d))
    (setq lst (append lst (list (list b)))
            b 1.0
    );setq
    (progn
     (if (and (equal a d)
              (equal n (length plst))
         );and
         (setq   a 1
               lst nil
         );setq then jump out cuz we must have hit a prime
     );if
    );progn
);if
);while
(if (and (not (equal a 1))
         (<= a 254)
    );and
    (setq lst (append lst (list (list a))));setq
);if
 
(if lst
    (progn
     (setq lst (acet-list-isort lst 0));setq
     (setq n 0)
     (setq b 1.0)
     (repeat (length lst)
     (setq a (car (nth n lst)))
     (if (< (* b a) 254)
         (setq b (* b a))
         (setq lst2 (append lst2 (list b))
                  b a
         );setq
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal b 1.0))
         (setq lst2 (append lst2 (list b)));setq
     );if
     (setq lst lst2)
    );progn then
);if
 
 
lst
);defun find_best_multiples
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mkshape_add_scale_fact ( lst3 k dx res fact / lst lst2 a n  )
 
                       ;fact is from (find_best_scale_fact dx res)
(setq  lst (car fact)  ;the numerator factors
      lst2 (cadr fact) ;the denominator
);setq
 
(setq n 0)
(while (< n (length lst2)) ;do the division first
 (setq    a (fix (nth n lst2))
       lst3 (append (list "3" (itoa a))
                    lst3
                    (list "4" (itoa a))
            );append
          k (+ k 4)
 );setq
 (setq n (+ n 1));setq
);while
(setq n 0)
(while (< n (length lst)) ;do the multiplying
 (setq    a (fix (nth n lst))
       lst3 (append (list "4" (itoa a))
                    lst3
                    (list "3" (itoa a))
            );append
          k (+ k 4)
 );setq
 (setq n (+ n 1));setq
);while
 
(list lst3 k)
);defun mkshape_add_scale_fact
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shptov ( a res dx / b)
 
(setq res (/ (float res) dx))
(if (or (equal "8" (substr a 1 1))
        (equal "9" (substr a 1 1))
    );or
    (setq a (substr a 3));setq
);if
(setq a (acet-str-to-list "," a)
      b (cadr a)
      a (car a)
      a (list (/ (atof (substr a 2))
                 res
              )
              (/ (atof (substr b
                               1
                               (- (strlen b) 1)
                       )
                 )
                 res
              )
        );list
);setq
a
);defun shptov
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vtoshp ( a res dx / b)
 
(setq res (/ (float res) dx))
 (setq b (cadr a)
       a (car a)
       a (itoa (fix (acet-calc-round (* res a) 1.0)))
       b (itoa (fix (acet-calc-round (* res b) 1.0)))
       a (strcat "(" a "," b ")")
 );setq
a
);defun vtoshp
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun bns_read_shp_file ( fna / FH MX N A LST LST2 C D B LST4 LST3 FLAG)
 
(if (setq fh (open fna "r"))
    (progn
     (setq mx 0)
     (princ (acet-str-format "\nReading shape file: %1..."  fna ))
     (setq n 0)
     (while (setq a (read-line fh))
      (setq a (xstrcase a))
      (if (equal n (* 10 (/ n 10)))
          (acet-spinner)
      );if
      (if (equal "*"  (substr a 1 1))
          (progn
           (if lst2
               (setq  lst (append lst (list lst2))
                     lst2 nil
               );setq
           );if
           (setq c a
                 a (acet-str-to-list "," a)
                 d (acet-str-space-trim (car a))
                 d (atoi (substr d 2))
           );setq
           (if (> d mx)
               (setq mx d)
           );if
           (if (not (> (length a) 2))
               (progn
                (princ (acet-str-format "\nError in shp file at line: %1" (itoa (+ n 1))))
                (exit)
               );progn then bail out
           );if
 
           (setq b (acet-str-space-trim (caddr a)));setq
           (if (wcmatch b "*_SUBSHAPE_*")
               (progn
                (setq b (acet-str-to-list "_SUBSHAPE_" b)
                      b (strcat "*" (cadr b))
                );setq
                (if lst3
                    (setq lst4 (append lst4 (list lst3))
                          lst3 nil
                    );setq then
                );if
                (setq lst3 (list b) ;the owner/parent key of thi sub-shape
                         a (list c)
                      flag T
                );setq
               );progn then
               (progn
                (setq flag nil);setq
                (if lst3
                    (setq lst4 (append lst4 (list lst3))
                          lst3 nil
                    );setq then
                );if
                (setq lst3 nil);setq
               );progn else not a subshape
           );if
          );progn then found the begining of a character or shape.
          (setq a (list a));setq else
      );if
      (while a
       (if flag
           (progn
            (if (not (equal (car a) ""))
                (setq lst3 (append lst3 (list (car a))))
            );if
           );progn
           (progn
            (if (not (equal (car a) ""))
                (setq lst2 (append lst2 (list (car a))))
            );if
           );progn else
       );if
       (setq a (cdr a))
      );while
     (setq n (+ n 1));setq
     );while read-line succeeds
     (close fh)
     (if flag
         (progn
          (if (not (equal lst3 (last lst4)))
              (setq lst4 (append lst4 (list lst3)));setq
          );if
         );progn then
         (progn
          (if (not (equal lst2 (last lst)))
              (setq lst (append lst (list lst2)));setq
          );if
         );progn else
     );if
 
     (setq n 0)
     (repeat (length lst4)
      (setq   a (nth n lst4)
              b (assoc (car a) lst)
              a (append b (cdr a))
            lst (subst a b lst)
      );setq
     (setq n (+ n 1));setq
     );repeat
     (princ "Done.")
 
    );progn then opened the file.
);if
(if lst
    (setq lst (list lst (+ mx 1)));setq then
);if
lst
);defun bns_read_shp_file
 
; ======== BEGIN MKSHAPE FUNCTIONS ========
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Return a list of style record objects that are holders for
;loaded shape files.
;
(defun bns_get_shape_styles ( / app doc sty styob name shx lst)
 (vl-load-com)
(setq app (vlax-get-acad-object)
      doc (vla-get-activedocument app)
      sty (vla-get-textstyles doc)
);setq
 
 (vlax-for styob sty
  (setq name (vla-get-name styob)
         shx (vla-get-fontfile styob)
  );setq
  (if (equal name "")
      (progn
       (setq lst (append lst
                         (list styob
                               ;(handent
                               ;   (vla-get-handle styob)
                               ;)
                         );list
                 );append
       );setq
      );progn then
  );if
 );vlax-for
 
 lst
);defun bns_get_shape_styles
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bns_shape_exists
;Takes a shape name (string)
;and returns T if the shape exists in a loaded shx file (shape is available)
;returns nil if the shape is not available
 
(defun bns_shape_exists ( shpname / e1 a)
 
 (setq e1 (list '(0 . "SHAPE") '(100 . "AcDbEntity") '(67 . 0)
                '(8 . "0") '(100 . "AcDbShape") '(10 4.07343 3.43308 0.0)
                '(40 . 1.0)
                (cons 2 shpname)
                '(50 . 0.0) '(41 . 1.0) '(51 . 0.0) '(60 . 1)
                '(210 0.0 0.0 1.0)
          )
       e1 (entmake e1)
 );setq
 (if e1
     (progn
      (if (setq a (acet-layer-locked "0"))
          (command "_.layer" "_unlock" "0" "")
      );if
      (entdel (entlast))
      (if a
          (command "_.layer" "_lock" "0" "")
      );if
     );progn then
 );if
 
 e1
);defun bns_shape_exists
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes an entity name of a shape
;returns the associated shx filename
(defun bns_get_shapefile ( shapename / lst lst2 flag n a fna)
 (vl-load-com)
(setq lst (bns_get_shape_styles));setq
 
(cond
 ((equal 1 (length lst))
  (setq fna (vla-get-fontfile (car lst)));setq
 );cond #2
 (T
  (setq lst2 (bns_disable_shapes lst)
        flag T
  );setq
 );cond #3
);cond close
 
(setq n 0);setq    ;put the shapes back to the way they were.
(while (< n (length lst2))
 (setq a (nth n lst2))
 (bns_re-enable_shapes (list a))
 (if (and flag
          (bns_shape_exists shapename)
     );and
     (progn
      (setq  fna (vla-get-fontfile (car a))
            flag nil
      );setq
     );progn then
 );if
 (setq n (+ n 1));setq
);while
 
fna
);defun bns_get_shapefile
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_disable_shapes ( lst / n ob shx lst2 na e1)
 (vl-load-com)
(setq n 0)
(repeat (length lst)
(setq   ob (nth n lst)
        na (vlax-vla-object->ename ob)
       shx (vla-get-fontfile ob)
      lst2 (append lst2 (list (list ob shx)))
);setq
 (if (and na
          (setq e1 (entget na))
     );and
     (entmod (subst (cons 3 "") (assoc 3 e1) e1))
 );if
(setq n (+ n 1));setq
);repeat
 
lst2
);defun bns_disable_shapes
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_re-enable_shapes ( lst / n ob shx na e1)
 (vl-load-com)
(setq n 0)
(repeat (length lst)
(setq   ob (nth n lst)
       shx (cadr ob)
        ob (car ob)
        na (vlax-vla-object->ename ob)
        e1 (entget na)
);setq
(if e1
    (entmod (subst (cons 3 shx) (assoc 3 e1) e1))
);if
;(vla-put-fontfile ob shx) ;this works good!
 
(setq n (+ n 1));setq
);repeat
 
);defun bns_re-enable_shapes
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a file name and checks the style table to see if the shx file is referenced.
;returns T if referenced by shape style record
(defun bns_shx_loaded ( shx / lst n ob a flag)
 (vl-load-com)
(if (setq shx (acet-file-find-font shx))
    (setq shx (xstrcase shx)
          lst (bns_get_shape_styles)
    );setq then
);if
(setq n 0)
(while (< n (length lst))
(setq  ob (nth n lst)
        a (vla-get-fontfile ob)
        a (acet-file-find-font a)
);setq
(if (and a
         (equal (xstrcase a) shx)
    );and
    (setq flag T
             n (length lst)
    );setq
);if
(setq n (+ n 1));setq
);while
 
flag
);defun bns_shx_loaded
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes an shx file name and re-loads it
;
(defun bns_shx_reload ( shx / lst shx_sav app doc sty cnt styob name shx2 a)
 (vl-load-com)
(setq     lst #acet-shx-files
      shx_sav (xstrcase shx)
);setq
(if (and (setq shx (acet-file-find-font shx))
         (setq shx (xstrcase shx))
         (not (assoc shx lst))
    );and
    (progn
     (setq lst (append lst
                       (list (list
                               shx
                               shx_sav
                             );list
                       );list
               );append
     );setq
     (setq #acet-shx-files lst)
    );progn then
);if
(if (not #acet-shx-mod-count)
    (progn
     (setq #acet-shx-mod-count 0)
     (bns_shx_react)
    );progn
);if
(setq app (vlax-get-acad-object)
      doc (vla-get-activedocument app)
      sty (vla-get-textstyles doc)
);setq
(if shx
    (progn
     (setq            shx (xstrcase shx)
           #acet-shx-mod-count (+ #acet-shx-mod-count 1)
                      cnt #acet-shx-mod-count
     );setq
     (vlax-for styob sty
      (setq name (vla-get-name styob)
            shx2 (vla-get-fontfile styob)
            shx2 (acet-file-find-font shx2)
      );setq
      (if (and (equal name "")
               shx2
               (setq shx2 (xstrcase shx2))
               (equal shx shx2)
          );and
          (progn
           (princ (acet-str-format "\nReloading: %1" shx2))
           (setq a (bns_path_mod_it shx2 cnt))
           (vla-put-fontfile styob a)
 
          );progn then
      );if
     );vlax-for
    );progn then shx was found
);if
 
);defun bns_shx_reload
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_path_mod_it ( shx2 cnt / a)
 
 (setq shx2 (xstrcase shx2)
       shx2 (acet-filename-supportpath-remove shx2)
       ;shx2 (acet-filename-ext-remove shx2)
          a (acet-filename-directory shx2)     ;the dir
       shx2 (acet-filename-path-remove shx2)           ;the base name
 );setq
 (repeat cnt
  (setq a (strcat a ".\\"))
 );repeat
 (setq a (strcat a shx2))
 
);defun bns_path_mod_it
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_shx_react ( / )
  (vl-load-com)
  (acet-editor-reactor-add '(:vlr-beginsave . bns_shx_beginsave))
  (acet-editor-reactor-add '(:vlr-savecomplete . bns_shx_savecomplete))
 
  (acet-editor-reactor-add '(:vlr-commandwillstart . bns_shx_begin_cmd))
 
  (acet-editor-reactor-add '(:vlr-commandcancelled . bns_shx_savecomplete))
  (acet-editor-reactor-add '(:vlr-commandended . bns_shx_savecomplete))
 
  (acet-editor-reactor-add '(:vlr-beginclose . bns_shx_react_off))
 
  (setq #acet-shx-react-off nil)
);defun bns_shx_react
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_shx_react_off ( a b / )
  (vl-load-com)
  (setq   #acet-shx-react-off T
              #acet-shx-files nil
          #acet-shx-mod-count nil
        #acet-shx-changed-lst nil
  );setq
  (acet-reactor-remove '(:vlr-beginsave . bns_shx_beginsave))
  (acet-reactor-remove '(:vlr-savecomplete . bns_shx_savecomplete))
 
  (acet-reactor-remove '(:vlr-commandwillstart . bns_shx_begin_cmd))
 
  (acet-reactor-remove '(:vlr-commandcancelled . bns_shx_savecomplete))
  (acet-reactor-remove '(:vlr-commandended . bns_shx_savecomplete))
 
  (acet-reactor-remove '(:vlr-beginclose . bns_shx_react_off))
 
);defun bns_shx_react_off
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_shx_begin_cmd ( a b /  r )
 (vl-load-com)
 (if #acet-shx-react-off
     (acet-reactor-remove '(:vlr-commandwillstart . bns_shx_begin_cmd))
     (progn
      (if (or (equal (car b) "WBLOCK")
              (equal (car b) "LOAD")
              (equal (car b) "DXFOUT")
          );or
          (bns_shx_beginsave a b)
      );if
     );progn else
 );if
 
);defun bns_shx_begin_cmd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_shx_beginsave ( a b / lst2 n ob shx)
 (vl-load-com)
 (if #acet-shx-react-off
     (acet-reactor-remove '(:vlr-beginsave . bns_shx_beginsave))
     (progn
      (if #acet-shx-files
          (progn
           (setq lst2 (bns_get_shape_styles))
           (setq n 0)
           (repeat (length lst2)
            (setq  ob (nth n lst2)
                  shx (vla-get-fontfile ob)
            );setq
            (if (and shx
                     (setq a (acet-file-find-font shx))
                     (setq a (assoc (xstrcase a) #acet-shx-files))
                );and
                (progn
                 ;(print "begin")
                 ;(print "")
                 (vla-put-fontfile ob (cadr a))
                 (setq #acet-shx-changed-lst (append #acet-shx-changed-lst
                                               (list (list ob shx))
                                        );append
                 );setq
               );progn then
            );if
            (setq n (+ n 1));setq
           );repeat
          );progn then
      );if
     );progn else
);if
 
);defun bns_shx_beginsave
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_shx_savecomplete ( a b / ob shx )
 
;(print "bns_shx_savecomplete")
;(print "")
 (vl-load-com)
 (if #acet-shx-react-off
     (progn
      (acet-reactor-remove '(:vlr-savecomplete . bns_shx_savecomplete))
      (acet-reactor-remove '(:vlr-commandcancelled . bns_shx_savecomplete))
      (acet-reactor-remove '(:vlr-commandended . bns_shx_savecomplete))
     );progn remove
     (progn
      (while #acet-shx-changed-lst
       (setq                a (car #acet-shx-changed-lst)
                           ob (car a)
                          shx (cadr a)
             #acet-shx-changed-lst (cdr #acet-shx-changed-lst)
       );setq
       (if (and shx
                (acet-file-find-font shx)
           );and
           (progn
            ;(print "complete")
            (vla-put-fontfile ob shx)
           );progn then
       );if
      );while
     );progn else
);if
);defun bns_shx_savecomplete


(acet-autoload2	'("Yes_no.lsp"	(bns_get_yes_no lst size)))
(princ)
;;;-----BEGIN-SIGNATURE-----
;;; UAoAADCCCkwGCSqGSIb3DQEHAqCCCj0wggo5AgEBMQ8wDQYJKoZIhvcNAQELBQAw
;;; CwYJKoZIhvcNAQcBoIIHaDCCB2QwggVMoAMCAQICEA2+4xGUyzuWyBkNEt7WBCEw
;;; DQYJKoZIhvcNAQELBQAwaTELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDkRpZ2lDZXJ0
;;; LCBJbmMuMUEwPwYDVQQDEzhEaWdpQ2VydCBUcnVzdGVkIEc0IENvZGUgU2lnbmlu
;;; ZyBSU0E0MDk2IFNIQTM4NCAyMDIxIENBMTAeFw0yMTA4MDIwMDAwMDBaFw0yMjA4
;;; MDIyMzU5NTlaMGkxCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpDYWxpZm9ybmlhMRMw
;;; EQYDVQQHEwpTYW4gUmFmYWVsMRcwFQYDVQQKEw5BdXRvZGVzaywgSW5jLjEXMBUG
;;; A1UEAxMOQXV0b2Rlc2ssIEluYy4wggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIK
;;; AoICAQDY65QEUknk2Yy87p4LaXMXOT7AasB4BhNRJKfabtvF7wt0+TnhDOnKHKB1
;;; NvRywfn6n9qqIXN2pGjRuzWhJmjzb0C3+jA+c2Zlcp3VvisPdlizGFNzrL37XYoE
;;; yv7vg9fTKpDqiQS513cmJ8Kj38XWO55bEhAsiH6xgE9HiiD/XEUW8FUGAamdUIDD
;;; cq+NhdYsI5wgFyQM/CioZ8wttF0qJqSE4hbTaw8j2UFkDEkiFex4mCB99g7Dbzw+
;;; erXCEQCJuFYCQQN8OB8pxvTT/m8yYLYPwg9DzqVjn5SlhjLGdiPyOocuteb4QiM/
;;; JHZpRk8MQUs+wopTGDpYDhR8jfnbldfwvtwHfSPtKvq5QzErTVv9okB34Z0SaM86
;;; 518EZwUkrNfymt45CNmfa80uqC2xS+N7g4sg87EDbRCxvAnhJ6btFYRHhKfW6oAT
;;; YZFSU/4W3NFmX27Pnx8ZjATVPzoZ47rNm0JT2W4omIgfdq07Iu3SQp/e5a1QJpBY
;;; yaoT1ueqtxTdhBRHwjC5rTjVxuIQ24r9KU4ysH8R7d8skbBWGhyw2/9MB8rc6S7b
;;; g224JNJjgn0bM3cXOZRyBj8MkwWRU0XUV2wf6L1DDD9E8kBaagDv5J04VfScvtYK
;;; I+blbu8sT8is2fCnHptcPv0G4DFWOmkwFgOM6+OQoS5KsORz3wIDAQABo4ICBjCC
;;; AgIwHwYDVR0jBBgwFoAUaDfg67Y7+F8Rhvv+YXsIiGX0TkIwHQYDVR0OBBYEFDo+
;;; 4eCS86cJ84X74B6P0/A+TOO9MA4GA1UdDwEB/wQEAwIHgDATBgNVHSUEDDAKBggr
;;; BgEFBQcDAzCBtQYDVR0fBIGtMIGqMFOgUaBPhk1odHRwOi8vY3JsMy5kaWdpY2Vy
;;; dC5jb20vRGlnaUNlcnRUcnVzdGVkRzRDb2RlU2lnbmluZ1JTQTQwOTZTSEEzODQy
;;; MDIxQ0ExLmNybDBToFGgT4ZNaHR0cDovL2NybDQuZGlnaWNlcnQuY29tL0RpZ2lD
;;; ZXJ0VHJ1c3RlZEc0Q29kZVNpZ25pbmdSU0E0MDk2U0hBMzg0MjAyMUNBMS5jcmww
;;; PgYDVR0gBDcwNTAzBgZngQwBBAEwKTAnBggrBgEFBQcCARYbaHR0cDovL3d3dy5k
;;; aWdpY2VydC5jb20vQ1BTMIGUBggrBgEFBQcBAQSBhzCBhDAkBggrBgEFBQcwAYYY
;;; aHR0cDovL29jc3AuZGlnaWNlcnQuY29tMFwGCCsGAQUFBzAChlBodHRwOi8vY2Fj
;;; ZXJ0cy5kaWdpY2VydC5jb20vRGlnaUNlcnRUcnVzdGVkRzRDb2RlU2lnbmluZ1JT
;;; QTQwOTZTSEEzODQyMDIxQ0ExLmNydDAMBgNVHRMBAf8EAjAAMA0GCSqGSIb3DQEB
;;; CwUAA4ICAQBgAlhYjdQROM/ERZDYg0G6w3OPW7R5964dUS85Ijbkghpqi1KZB88n
;;; 0ItnNMz8nm0juZI7Jk1Jz+3fs4bcpfJrt4NQFaD9K1SAszXwe1zfBs0KTMqNkr7u
;;; Ji2ySyK6eFkO+ZRbKLIufwXPmY8uopzwjqn2BSoX/Q4ZOhw1H7tBxcudzOivMoL1
;;; ouUFPwAq3rN9mUl4G6nXrDvd31z24Q+dWtAA16cJbu2OgX2Tv7m7NPZIQ002iQCa
;;; ke59VqhiiUveM5UJ7Rr+Kdp732ZnGuKcGcbNl3B4KUjE1z6+wWaVJlygJX4EHZDn
;;; W+vtPcGRR3IHDWconSphlRZC7P1HhnAnfJqu7v5zyDv9+KyNL0hNNdWf0epK22HS
;;; BDC68W1DhC0ocWCFRHttRDqvvRyUhaAQBhIu7MoUzpi6hgg1S3sqM3u1D4f/Zn2C
;;; ocvEH9FOV0bq3ZOnCZjpH2HURTINElaDgM+hSfGN2zpbJSf1UKZXjkujYul75tk8
;;; 6ogI3b44wb4QdZskaIKxhw4/VZPbt31BHY2HbYCjFmvtpObX9qRwhG57EwK+o5mh
;;; KwkWifU7a8/5P1zyIwJfKdutGdB20wX9HRYPF+Bb87nKGJV/bM1tqzyRAIMGBWp/
;;; LLIee8R+FRMe7RT+v8/hRYsjPU2EVqSqweN2Fbz/rDeKSCr6Nl7XPjGCAqgwggKk
;;; AgEBMH0waTELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDkRpZ2lDZXJ0LCBJbmMuMUEw
;;; PwYDVQQDEzhEaWdpQ2VydCBUcnVzdGVkIEc0IENvZGUgU2lnbmluZyBSU0E0MDk2
;;; IFNIQTM4NCAyMDIxIENBMQIQDb7jEZTLO5bIGQ0S3tYEITANBgkqhkiG9w0BAQsF
;;; ADANBgkqhkiG9w0BAQEFAASCAgA78sJ1roTnPJAd/vWHM5eSfa716Rhl3qvJSXaz
;;; CYCHeEryYeZgkd7sisF8gnRbbna72lqm2Sx6BmOjyDul8uspW56MwqlNKTvhCDrt
;;; N872/HHCqe2y1lrppocCDN1SLAtkNjrHzV4lWdJKgq/oSSYoQzEhH1RBsR5UP0X5
;;; F65BkF47AuiIUTYhCMhjQ+kcF4r0K6yXZMZiem17WLUqWRNuJGVXzeB87cRVrJ1/
;;; SjN6Za0/wLBCnfkOMnP67cILuQjQ5O7kias26DR5MGqaCQHGny0yOP6YPiQ099Nb
;;; huDvQH2Ygr4WDnQevfm8HuV261+WC1O90h3f39KcaQrgZWOfvHfkOva/B8KF2atN
;;; lNYbTMnuwcbrCHCtCZGd1pevOe+ck9z1FWGgHBbBAQnClqtEZAgvme6LyCVNHh0T
;;; u3odU2CWidc5KWZSdos7HyOHFSk8qWeIAx8CQDEih/cmhizm5jmEAC6SjrFzHKcM
;;; HTbuu1Z1iyFSfVastImQr4/Y9XL60FtUb3tgG2YWIBo4FPnuaUgZB4kRRMJhZz9o
;;; 050YZSqQrS/VTAMcGeweYabK8nKTCEjGZrt4+cZoBdi0x42p4TFbH412IPy748RZ
;;; z6MBXOxpkdacAH40PBdY/fMFJp/Ad1uILtu+8ggOXk7guzE6PlXVbir+kaSVUIPz
;;; LVngsA==
;;; -----END-SIGNATURE-----