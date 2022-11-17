;;;
;;;    MKLTYPE.LSP
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
;
;
;acet:mkltype-fna - global used for retaining default filename.
;
 
;Autoload some stuff at load time
(acet-autoload '("mkshape.lsp" "(bns_get_shapefile shapename)"))
 
 
(defun c:mkltype ( / FNA LST2 NAME FLAG XV FLT P1 SS P2 ANG LST FLAG2 A B TMP desc fuz prec newflag)
 
(acet-error-init
 (list (list   "cmdecho" 0
                "expert" nil
               "ucsicon" 0
             "ucsfollow" 0
       );list
       T
 );list
);acet-error-init
(sssetfirst nil nil)
 
(setq prec 6);this is the decimal precision that will be used for all floating point values
             ;when written to .lin files.
 
(if (not acet:mkltype-fna)
    (setq acet:mkltype-fna (acet-filename-ext-remove (getvar "dwgname")));setq then
);if
(setvar "expert" 2)
    (setq fna (ACET-FILE-WRITEDIALOG "MKLTYPE - Select Linetype File"
                            acet:mkltype-fna
                            "lin"
                            "Acet:Mkltype"
                            1665 ;1665
          );ACET-FILE-WRITEDIALOG
);setq
(if (assoc "EXPERT" (car acet:sysvar-list))
    (setvar "expert" (cadr (assoc "EXPERT" (car acet:sysvar-list))));then
);if
(if fna
    (progn
     (setq acet:mkltype-fna fna);setq then set the default for next time.
 
     (if (findfile fna)
         (setq lst2 (bns_read_lin_file fna));setq then read the lin file.
     );if
                            ;get a valid name for the linetype.
     (while (not (snvalid
                    (setq name (xstrcase (getstring "\nEnter linetype name: ")
                               )
                    );setq
                 )
            );not
       (princ "\n*Invalid linetype name*")
     );while
 
     (setq flag (assoc (strcat "*" name) lst2));setq
     (if (and (not flag)
              (not (tblobjname "ltype" name))
         );and
         (setq newflag T)
     );if
     (if (or newflag
             (initget "Yes No _Yes No")
             (equal "Yes"
                    (getkword "\nLine definition already exists. Overwrite it? [Yes/No] <No>: ")
             )
         );or
         (progn
          (setq desc (getstring T "\nEnter linetype description: "))
          (if flag
              (setq lst2 (acet-list-remove-nth (vl-position flag lst2) lst2));setq then remove old ltype
          );if
          (setq  xv (acet-geom-cross-product (getvar "ucsxdir") (getvar "ucsydir"))
                 xv (acet-geom-unit-vector '(0.0 0.0 0.0) xv)
                flt (acet-ss-flt-cspace)
                flt (list '(-4 . "<AND")
                           (car flt)
                           (cadr flt)
                           '(-4 . "<OR")
                             '(0 . "LINE")
                             '(0 . "LWPOLYLINE")
                             '(-4 . "<AND")     ;dis-allow 3dmesh and polyface mesh
                              '(0 . "POLYLINE")
                              '(-4 . "<NOT") '(-4 . "&") '(70 . 112) '(-4 . "NOT>")
                             '(-4 . "AND>")
                             '(0 . "POINT")
                             '(-4 . "<AND") ;disallow mirrored shapes
                              '(0 . "SHAPE") '(-4 . ">") '(41 . 0) (cons 210 xv)
                             '(-4 . "AND>")
                             '(-4 . "<AND") ;disallow mirrored text
                              '(0 . "TEXT") '(-4 . ">") '(41 . 0) (cons 210 xv)
                             '(-4 . "AND>")
                           '(-4 . "OR>")
                          '(-4 . "AND>")
                    );list
          );setq
          (while (and (not flag2) ;get a start and end point and a selection set that yields a
                                  ;number of segments less than or equal to 12.
                      (setq p1 (mkltype_get_start_and_stop));setq
                      (setq ss (ssget flt));setq
                 );and
               (setq  p2 (cadr p1)
                      p1 (car p1)
                     ang (angle p1 p2)
                      p2 (trans p2 1 0)
                      p1 (trans p1 1 0)
               );setq
               (acet-ucs-cmd (list "_z" (* ang (/ 180.0 pi))));acet-ucs-cmd
               (setq  p1 (trans p1 0 1)
                      p2 (trans p2 0 1)
                      p1 (acet-geom-list-extents (list p1 p2))
                      p2 (cadr p1)
                      p1 (car p1)
                     fuz (/ (- (car p2) (car p1))
                            100000.0
                         )
                     fuz (max (/ 1.0 (expt 10 prec)) fuz)
                     lst (mkltype_ss_parse p1 p2 ang ss fuz) ;returns (list lst y cnt)
               );setq
               (setq
                     lst (mkltype_format lst fuz) ;returns (list lst cnt)
               );setq
               (acet-ucs-cmd (list "_p"))
               (cond
                ((= (cadr lst) 1)
                 (princ "\n*Invalid* That's the continuous linetype!")
                )
                ((> (cadr lst) 12)
                 (princ "\n*Invalid* Too many segments in linetype definition.")
                )
                (T
                 (setq flag2 T
                         lst (car lst)
                 );setq
                )
               );cond close
          );while
          (if flag2
              (progn
               (setq   a (mkltype_format2 lst prec));setq
               (if (equal (length a) 2)
                   (setq b (append (list (strcat "*" name)
                                         desc                ;description
                                   );list
                                   (cadr a)
                           );append
                   );setq
                   (setq b nil);setq
               );if
               (setq a (car a)
                     a (append (list (strcat "*" name)
                                     desc              ;description
                               );list
                               a
                       );append
               );setq
 
               (acet-file-backup fna) ;backup the .lin file before writing to it.
               (mkltype_write_lin_file fna (append lst2 (list a)))
               (if b
                   (progn
                    (setq tmp (getvar "tempprefix")
                          tmp (xstrcase tmp)
                          tmp (acet-str-replace "/" "\\" tmp)
                    );setq
                    (if (and (not (equal tmp ""))
                             (not (equal "\\" (substr tmp (strlen tmp) 1)))
                        );and
                        (setq tmp (strcat tmp "\\"));setq
                    );if
                    (setq fna (xstrcase (strcat tmp "bns_temp.lin")));setq
                    (mkltype_write_lin_file fna (list b))
                   );progn then
               );if
               (command "_.-linetype" "_l" name fna)
               (while (wcmatch (getvar "cmdnames") "*LINETYPE*")
                 (command "")
               );while
               (acet-file-backup-delete)
               (if (equal (acet-filename-path-remove fna) "BNS_TEMP.LIN")
                   (vl-file-delete fna);then delete the temp file.
               );if
               (if (tblobjname "ltype" name)
                   (progn
                    (if newflag
                        (princ (acet-str-format "\nLinetype \"%1\" created and loaded." name))
                        (princ (acet-str-format "\nLinetype \"%1\" redefined and loaded." name))
                    );if
                   );progn then
                   (princ "\nLinetype creation failed.")
               );if
              );progn then
          );if
         );progn then write the linetype definition to the specified file.
     );if
    );progn then got a good file name
);if
 
(acet-error-restore)
);defun c:mkltype
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mkltype_get_start_and_stop ( / p1 p2 flag)
 
(initget 1)
(if (setq p1 (getpoint "\nSpecify starting point for line definition: "))
    (progn
     (while (not flag)
      (initget 1)
      (setq p2 (getpoint p1 "\nSpecify ending point for line definition: "))
      (if p2
          (progn
           (if (not (equal 0.0 (distance p1 p2)))
               (setq   p1 (list p1 p2)
                     flag T
               );setq then
               (princ "\n*Invalid* Distance between points must be non-zero")
           );if
          );progn
          (setq p1 nil)
      );if
     );while
    );progn then
);if
p1
);defun mkltype_get_start_and_stop
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_read_lin_file ( fna  / fh a lst lst2)
 
(if (setq fh (open fna "r"));setq
    (progn
     (while (setq a (read-line fh))
      (if (equal "*" (substr a 1 1))
          (progn
           (if lst
               (setq lst2 (append lst2 (list lst)));setq
           );if
           (setq lst (acet-str-to-list "," a)
                 lst (list (car lst)
                           (eval (append '(strcat)
                                          (cdr lst)
                                 );append
                           );eval
                     );list
           );setq
          );progn then it's the begining of a lin definition
          (setq lst (append lst (list a)));setq else
      );if
     );while
     (close fh)
     (if (and lst
              (not (equal (last lst2) lst))
         );and
         (setq lst2 (append lst2 (list lst)));setq
     );if
    );progn then
);if
 
lst2
);defun bns_read_lin_file
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mkltype_write_lin_file ( fna lst / fh a n )
 
(if (setq fh (open fna "w"));setq
    (progn
     (setq n 0)
     (repeat (length lst)
     (setq a (nth n lst));setq
      (if (> (length a) 1)
          (progn
           (write-line (strcat (car a) "," (cadr a)) fh)
           (setq a (cdr (cdr a)));setq
          );progn then
      );if
      (while a
       (write-line (car a) fh)
       (setq a (cdr a));setq
      );while
     (setq n (+ n 1));setq
     );repeat
     (close fh)
    );progn then
);if
 
);defun mkltype_write_lin_file
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lst is linetype data
;d is decimal places of precision to use
(defun mkltype_format2 ( lst prec / fuz str str2 n a x lst2 lst3)
 
(setq fuz (/ 1.0 (expt 10 prec)))
 
(setq  str ""
      str2 ""
);setq
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
 
(if (not (equal str ""))
    (setq  str (strcat str ",")
          str2 (strcat str2 ",")
    );setq then
);if
 
(if (> (length a) 1)
    (progn
     (if (equal (car a) "SHAPE")
         (progn
          (setq  str (strcat str
                             "[" (nth 1 a)  ;the shape name
                             ","  (nth 2 a) ;the shx filename for target lin file
                     );strcat
                str2 (strcat str2
                             "[" (nth 1 a)  ;the shape name
                             ","  (nth 7 a)  ;the shx filename for the bns_temp.lin file
                     );strcat
          );setq
         );progn then shape
         (setq  str (strcat str
                            "[" (nth 1 a)  ;the text string
                            ","  (nth 2 a) ;the style name
                    );strcat
               str2 (strcat str2
                            "[" (nth 1 a)  ;the text string
                            ","  (nth 2 a) ;the style name
                    );strcat
         );setq else text
     );if
     (setq x "")
 
     (if (not (equal 0.0 (nth 3 a) fuz))
         (setq x (strcat x ",x=" (bns_zero_strip (nth 3 a) prec)));setq then add x offset
     );if
     (if (not (equal 0.0 (nth 4 a) fuz))
         (setq x (strcat x ",y=" (bns_zero_strip (nth 4 a) prec)));setq then add y offset
     );if
     (setq x (strcat x ",s=" (bns_zero_strip (nth 5 a) prec)));setq add the size
 
     (if (equal (car a) "TEXT")
         (progn
             (setq x (strcat x ",u=" (bns_zero_strip (nth 6 a) prec)));setq the rotation
         );progn then text
         (if (not (equal 0.0 (nth 6 a) fuz))
             (setq x (strcat x ",r=" (bns_zero_strip (nth 6 a) prec)));setq then the rotation
         );if else shape
     );if
 
     (setq a (cdr a));setq strip the "type" string off
     (setq x (strcat x "]"));setq
 
     (setq  str (strcat str x)
           str2 (strcat str2 x)
     );setq
    );progn then a shape or text
    (progn
 
     (setq  str (strcat str (bns_zero_strip (car a) prec))
           str2 (strcat str2 (bns_zero_strip (car a) prec))
     );setq
    );progn else add pen up or pen down sequences
);if
(if (or (> (strlen str) 200)
        (> (strlen str2) 200)
    );or
    (setq  str (strcat str ",")
          str2 (strcat str2 ",")
          lst2 (append lst2 (list str))
          lst3 (append lst3 (list str2))
           str ""
          str2 ""
    );setq
);if
(setq n (+ n 1));setq
);repeat
(if (not (equal str ""))
    (setq lst2 (append lst2 (list str))
          lst3 (append lst3 (list str2))
    );setq then
);if
 
(setq lst2 (append (list (strcat "A," (car lst2)))
                   (cdr lst2)
           );append
      lst3 (append (list (strcat "A," (car lst3)))
                   (cdr lst3)
           );append
);setq
(if (and (setq a (last lst2))
         (equal "," (substr a (strlen a)))
    );and
    (setq lst2 (cdr (reverse lst2))
          lst2 (cons (substr a 1 (max 0 (- (strlen a) 1)))
                     lst2
               );cons
          lst2 (reverse lst2)
    );setq then get rid of that last comma because nothing else follows this line.
);if
(if (and (setq a (last lst3))
         (equal "," (substr a (strlen a)))
    );and
    (setq lst3 (cdr (reverse lst3))
          lst3 (cons (substr a 1 (max 0 (- (strlen a) 1)))
                     lst3
               );cons
          lst3 (reverse lst3)
    );setq then get rid of that last comma because nothing else follows this line.
);if
 
(if (assoc "SHAPE" lst)
    (setq str (list lst2 lst3)   ; list with target lin file and bns_temp.lin file
    );setq then
    (setq str (list lst2));setq else return single string in a list to write to file
);if
 
str
);defun mkltype_format2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_zero_strip ( a prec / fuz b)
 
(setq fuz (/ 1.0 (expt 10 prec)))
(if (equal 0.0 a fuz)
    (setq a "0")
    (progn
     (if (< a 0)
         (setq b "-")
         (setq b "")
     );if
     (setq a (abs a)
           a (rtos a 2 prec)
     );setq
     (while (and (> (strlen a) 0)
                 (equal "0" (substr a 1 1))
            );and
      (setq a (substr a 2))
     );while
     (while (and (> (strlen a) 0)
                 (equal "0" (substr a (strlen a) 1))
            );and
      (setq a (substr a 1 (- (strlen a) 1)
              );substr
      );setq
     );while
     (if (equal "." (substr a
                            (max (strlen a) 1)
                            1
                    )
         )
         (setq a (substr a 1 (- (strlen a) 1)
                 );substr
         );setq
     );if
     (if (equal a "")
         (setq a "0")
         (setq a (strcat b a))
     );if
    );progn then
);if
 
a
);defun bns_zero_strip
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mkltype_format ( lst fuz / Y CNT LT N A DX B c LST2)
 
(setq   y (cadr lst)
      cnt (caddr lst)
      lst (car lst)
       lt (getvar "ltscale")
);setq
 
(setq n 0)
(repeat (length lst)
 (setq  a (nth n lst)
       dx (abs (- (cadr a) (car a)))
 );setq
 (cond
  ((equal "SHAPE" (cadddr a))
   (setq b (list "SHAPE"
                 (nth 4 a)                        ;the shape name or text string
                 (nth 5 a)                        ;the shx file or the stylename
                 0                                ;the x offset
                 (/ (- (caddr a) y) lt)           ;the y offset
                 (/ (nth 6 a) lt) ;the size
                 (nth 7 a)                        ;the rotation
                 (nth 8 a)                        ;the output shxname for the bns_temp.lin file.
           );list
   );setq then
  );cond #1
  ((equal "TEXT" (cadddr a))
   (setq b (list "TEXT"
                 (nth 4 a)              ;the shape name or text string
                 (nth 5 a)              ;the shx file or the stylename
                 0                      ;the x offset
                 (/ (- (caddr a) y) lt) ;the y offset
                 (/ (nth 6 a) lt)       ;the size
                 (nth 7 a)              ;the rotation
           );list
   );setq then
  );cond #2
  (T
   (setq b (list (/ (* dx (last a)) ;pen up/pen down is indicated by -1.0 or 1.0
                    lt              ;respectiveley as the last item in a
                 )
           );list
   );setq
  );cond #3
 );cond close
 (setq lst2 (append lst2 (list b)));setq
 
(setq n (+ n 1));setq
);repeat
 
(if (and (>= (car (car lst2)) 0)
         (= (car (last lst2)) 0)
    );and
    (setq lst2 (reverse (cdr (reverse lst2)))
           cnt (- cnt 1)
    );setq then remove the 0 at the end cuz it's not needed.
);if
 
(if (and (> cnt 12)
         (or (assoc "SHAPE" lst2)
             (assoc "TEXT" lst2)
         );or
    );and
    (progn          ;attempt to combine some segments that are on each side of a shape/text
                    ;object. Then adjust the x offset for the shape/text
     (princ "\nOptimizing segments...")
     (setq a (cadr lst2))
     (setq n 2)
     (while (< (+ n 1) (length lst2))
     (setq b (nth n lst2)
           c (nth (+ n 1) lst2)
     );setq
     (if (and (or (equal (car b) "SHAPE")
                  (equal (car b) "TEXT")
              );or
              (equal (length a) 1)      ;the previous element is a dash
              (equal (length c) 1)      ;dash the next element is a dash
              (>= (- n 2) 0)            ;can't remove the first pen down
              (not (equal "SHAPE" (car (nth (- n 2) lst2))));element before 'a' is not shape/text
              (not (equal "TEXT" (car (nth (- n 2) lst2))))
              (> (* (car a) (car c)) 0) ;segments have the same sign
         );and
         (progn
          (setq    b (acet-list-put-nth (abs (car a)) b 3)
                lst2 (acet-list-put-nth b lst2 n)
                   c (list (+ (car a) (car c)))
                lst2 (acet-list-put-nth c lst2 (+ n 1))
                lst2 (acet-list-remove-nth (- n 1) lst2)
                   n (- n 1)
                 cnt (- cnt 1)
          );setq
         );progn then combine a and b and adjust x offset for b
     );if
     (setq a b)
     (setq n (+ n 1));setq
     );while
     (princ "Done.")
    );progn then
);if
 
 
 
(list lst2 cnt)
);defun mkltype_format
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun mkltype_ss_parse ( p3 p4 ang ss fuz / cnt n na e1 shpname shx
                                             shx2 b p1 a lst p2 lst2 lst3
                        )
 
(setq n 0)
(repeat (sslength ss)
(setq na (ssname ss n)
      e1 (entget na)
);setq
(cond
 (
  (equal "SHAPE" (cdr (assoc 0 e1)))
 
  (if (setq shpname (cdr (assoc 2 e1)))
      (progn
       (setq  shx (bns_get_shapefile shpname)
             shx2 shx
       );setq
       (if (and shx
                (setq b (acet-file-find-font (acet-filename-path-remove shx)))
                (acet-str-equal b (acet-file-find-font shx))
           );and
           (setq shx (acet-filename-path-remove b));setq
       );if
       (setq shx (acet-filename-ext-remove shx));setq
 
 
       (setq   p1 (trans (cdr (assoc 10 e1)) na 1)
                a (list (car p1) (car p1) (cadr p1)
                        "SHAPE"
                        shpname
                        shx                       ;possibly stripped path version of the shx filename
                                                  ;shx will be used for the target linetype file
                        (cdr (assoc 40 e1))       ;the size
                        (* (acet-geom-angle-trans    ;the rotation expressed in degrees
                                (cdr (assoc 50 e1))
                                (cdr (assoc 210 e1))
                                1
                           )
                           (/ 180.0 pi)
                        )
                        shx2                ;the filename.shx as it exists in this drawing.
                                            ;this will be used for the bns_temp.lin file
                                            ;that is created and loaded in addition to
                                            ;[filename].lin that the user specifies.
                  );list
       );setq
      );progn then
  );if
 );cond #1
 ((equal "TEXT" (cdr (assoc 0 e1)))
 
  (setq p1 (trans (cdr (assoc 10 e1)) na 1)
         a (list (car p1) (car p1) (cadr p1)
                 "TEXT"
                 (strcat "\"" (cdr (assoc 1 e1)) "\"") ;the string
                 (cdr (assoc 7 e1))                    ;the style
                 (cdr (assoc 40 e1))                   ;the size
                 (*
                    (acet-geom-angle-trans    ;the rotation expressed in degrees
                      (cdr (assoc 50 e1))
                      (cdr (assoc 210 e1))
                      1
                    )
                    (/ 180.0 pi)
                 )
          );list
  );setq
 );cond #2
 (T
  (setq lst (acet-geom-object-point-list na (* 5 (acet-geom-pixel-unit)))
         p1 (acet-geom-list-extents lst)
         p2 (cadr p1)
         p1 (car p1)
          a (list (car p1)  ;min x and
                  (car p2)  ;max x
                  (cadr p1) ;and the y coord
                  (cdr (assoc 0 e1)) ;type
                  ;na
            );list
  );setq
 );cond #3
);cond close
 
(if (and (>= (cadr a) (car p3))
         (<= (car a) (car p4))
    );and
    (progn
     ;make sure the ent is fully within the start/stop definition points
     (if (> (car p3) (car a))
         (setq a (append (list (car p3)) (cdr a)));setq
     );if
     (if (> (cadr a) (car p4))
         (setq a (append (list (car a) (car p4)) (cdr (cdr a))));setq
     );if
     (if (or (equal "TEXT" (nth 3 a))
             (equal "SHAPE" (nth 3 a))
         );or
         (setq lst2 (append lst2 (list a)));setq then add it to the shape/text list
         (setq lst3 (append lst3 (list a)));setq else add to pen down line segment list
     );if
    );progn then there is overlap with the start/stop definition points
    (progn
     (if a
         (progn
          (princ (acet-str-format "\nIgnoring %1 object outside of line definition start/stop points."  (nth 3 a)))
          (if (equal "SHAPE" (nth 3 a))
              (princ "\nSHAPE insertion points must fall within start/stop points.")
          );if
          (if (equal "TEXT" (nth 3 a))
              (princ "\nTEXT start point must fall within lin definition start/stop points.")
          );if
         );progn
     );if
    );progn else let the user know whats going on.
);if
 
(setq n (+ n 1));setq
);repeat
 
(if lst2
    (setq lst2 (acet-list-isort lst2 0));setq then sort the shapes and text
);if
(setq lst3 (bns_combine_overlap p3 p4 lst3) ;pen down segments.
      lst3 (bns_add_pen_up_segments p3 p4 lst3)
);setq
 
(setq lst2 (bns_combine_symbols_and_segments lst3 lst2 fuz));setq
 
(setq cnt 0) ;count how many line/point segments
(setq n 0)
(repeat (length lst2)
(if (and (not (equal "SHAPE" (nth 3 (nth n lst2))))
         (not (equal "TEXT" (nth 3 (nth n lst2))))
    );and
    (setq cnt (+ cnt 1))
);if
(setq n (+ n 1));setq
);repeat
 
(list lst2 (cadr p3) cnt)
);defun mkltype_ss_parse
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_combine_overlap (P1 P2 LST / A N J LST2 X1 X2 X3 X4 LST3 B)
 
(setq n 0)
(while (< n (length lst))
(setq  a (nth n lst)
      x1 (max (car a) (car p1))
      x2 (min (cadr a) (car p2))
);setq
(if (not (member n lst2))
    (setq j (+ n 1))
    (setq j (length lst))
);if
(while (< j (length lst))
(setq  b (nth j lst)
      x3 (max (car b) (car p1))
      x4 (min (cadr b) (car p2))
);setq
(if (and (not (member j lst2))
         (or (and (<= x3 x2)
                  (>= x3 x1)
             );and
             (and (<= x4 x2)
                  (>= x4 x1)
             );and
         );or
    );and
    (setq   x1 (min x1 x2 x3 x4)
            x2 (max x1 x2 x3 x4)
          lst2 (append lst2 (list j))
    );setq then
);if
(setq j (+ j 1));setq
);while
(if (not (member n lst2))
    (setq    a (append (list x1 x2) (cdr (cdr a)))
          lst3 (append lst3 (list a))
    );setq
);if
(setq n (+ n 1));setq
);while
 
(if lst3
    (setq lst3 (acet-list-isort lst3 0))
);if
 
lst3
);defun bns_combine_overlap
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_add_pen_up_segments ( p1 p2 lst / a b n lst2)
 
(setq a (car lst))
(if (or (not lst)
        (< (car p1) (car a))
    );or
    (setq   a (list (car p1) (car p1) (cadr p1) "POINT")
          lst (append (list a) lst)
    );setq then add a point up front.
);if
 
(setq a (last lst));setq
(if (or (not lst)
        (< (cadr a) (car p2))
    );or
    (setq   a (list (car p2) (car p2) (cadr p1) "POINT")
          lst (append lst (list a))
    );setq then add a point to the end.
);if
 
(setq    a (car lst)
         a (append a (list 1.0))
      lst2 (list a)
);setq
(setq n 0)
(repeat (max 0 (- (length lst) 1))
(setq a (nth n lst)
      b (nth (+ n 1) lst)
);setq
 
(if (> (car b) (cadr a))
    (setq lst2 (append lst2
                       (list (list (cadr a) (car b) (cadr p1) "LINE" -1.0))
               );append
    );setq then add a pen up segment
);if
(setq    b (append b (list 1.0))
      lst2 (append lst2 (list b))
);setq add the pen down segment
 
(setq n (+ n 1));setq
);repeat
 
lst2
);defun bns_add_pen_up_segments
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bns_combine_symbols_and_segments
; takes two lists and returns a single list
;Args:
;lst is a list sublists containing line segments:
;((minx maxx y entitytype ...)
; ...
;)
;lst2 is a list of sublists containing TEXT/SHAPE information.
;((minx maxx y entitytype string/shapename style/shapefile size rotation)
;  ...
;)
;
(defun bns_combine_symbols_and_segments ( lst lst2 fuz / n a b x1 x2 x3 lst3)
 
(setq n 0)
(repeat (length lst)
(setq  a (nth n lst)
      x1 (car a)
      x2 (cadr a)
);setq
 
 (while (and lst2                ;while shape/text objects are on current segment
             (setq  b (car lst2)
                   x3 (car b)
             )
             (>= x3 x1)
             (<= x3 x2)
        );and
  (if (and (not (equal x3 x1))
           (not (equal x3 x2))
      );and
      (setq lst3 (append lst3
                         (list (append (list x1 x3) (cdr (cdr a)))
                               b
                         );list
                 );append
      );setq then split the line segment because text/shape falls between it's end points
      (progn
       (if (equal x3 x2)
           (progn
            (if (and ;lst3
                     (equal (- x2 x1) 0.0 fuz)
                     (or (not lst3)
                         (equal "SHAPE" (nth 3 (last lst3)))
                         (equal "TEXT" (nth 3 (last lst3)))
                     );or
                );and
                (setq a (append (list x1 (+ x1 fuz)) (cdr (cdr a))));setq then
            );if
            (setq lst3 (append lst3
                               (list a
                                     b
                               );list
                       );append
            );setq then
           );progn then the shape/text object is at the end of the current segment
           (progn
            (if (not lst3)
                (setq lst3 (list (list x1 x1 (caddr a)
                                       "LINE"
                                       1.0
                                 );list
                           );list
                );setq then put a dummy point at the front
            );if
            (if (and lst3
                     (or (equal "SHAPE" (nth 3 (last lst3)))
                         (equal "TEXT" (nth 3 (last lst3)))
                     );or
                );and
                (setq lst3 (append lst3
                                   (list
                                    (list (car (last lst3))
                                          (+ (car (last lst3)) fuz)
                                          (caddr (last lst3))
                                          "LINE"
                                          (last (nth (- (length lst3) 2)
                                                     lst3
                                                );nth
                                          );last use sign of prev segment
                                    );list
                                   );list
                           );append
                );setq then put a dummy point between the shape/text objects
            );if       ;so that they will not be on the same segment
            (setq lst3 (append lst3
                               (list b);list
                       );append
            );setq
           );progn else shape/text object is at begining of current segment
       );if
      );progn else shape/text is at one of the endpoints of the segment
  );if
  (setq lst2 (cdr lst2)
          x1 (max x3 x1)
           a (append (list x1 x2) (cdr (cdr a)))
  );setq
 );while shape/text objects fall between the current line segment.
 (if (not (and lst3
               (or (equal "SHAPE" (nth 3 (last lst3)))
                   (equal "TEXT" (nth 3 (last lst3)))
               );or
               (equal 0.0 (- (cadr a) (car a)))
          );and
     );not
     (setq lst3 (append lst3 (list a)))
 );if
(setq n (+ n 1));setq
);repeat
 
lst3
);defun bns_combine_symbols_and_segments


(acet-autoload2	'("Mkshape.lsp"	(bns_get_shapefile shapename)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBbcZNdmTKgAj0FoqBpkgW9SdtE/FLBHL08phpG
;;; t9dD8qv8WBq9npNf7d0lNMTddMMhSvr5iczgyUnWNRAfC8AP3mje2hiC7zP/qB1R
;;; ignrmGj9nqbhY0v0eHGel/tNVVULAjcC1krewPPk8VJ/x9ZtICHtmzHMdMFjfJk8
;;; 6SI1ip17vAOnrQ3MVT8ht5Qc6n6VqOhTfQ/NiYl6oaiNzNUpBmyNFuhwzyDW5sbH
;;; LCKAv4frIX+WAXPDV+DgW/gVgmi+oaAPdbvs4RAdr5U7t5XvbkzcQoxsHbhW6rAG
;;; FSb0nOs+RqT9w1kCYwWy3Mp+BMz5zx9g4V7caHbSuocUANS6n73RN9gBJHFw/AB7
;;; GFPOi1hCrTBGkAKvT04MNgze4xFXIJdCT9VTuucSbQGHfkzRyjuUXCYl1XOz2Q3u
;;; VvBZyv4UEcrT7/2EuJgYhmKsEnKfdpEFWcQwjRuj9fpwzGjvVA5/SVWy4Cn1ELo3
;;; E66rUIk/RYBMU+wnQIs5Cn9k0V/sVrneRJOOOKPIobcpRpfDEbsHIUcJO+LojFpM
;;; /zULl8a3pruv5tGZJIVZNffcRIDvw23GxUGuR31fvGsQ6MeVLez4qkjTRpZCKSiT
;;; FkX8EKKAGOT/RjjmWRvWhRTpWptR6LjRzlI6AAC279qP2ov0HZdgyiP5IytOq6+B
;;; fZZcLQ==
;;; -----END-SIGNATURE-----