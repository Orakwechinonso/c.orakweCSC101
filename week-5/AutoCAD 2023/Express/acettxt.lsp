;;
;;;
;;;    ACETTXT.LSP
;;;    Created 1/10/98 by Randy Kintzley and Tom Stoeckel
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TORIENT  Text-ORIENT rotates text mtext or attdef objects.
;
(defun c:torient ( / ss flt j mode)
(acet-error-init
 (list (list  "cmdecho" 0
             "highlight" (getvar "highlight")
              "limcheck" 0
             "pickstyle" 0
       );list
       T
 );list
);acet-error-init
 
(setq flt '((-4 . "<OR")
             (0 . "TEXT")
             (0 . "ATTDEF")
             (0 . "MTEXT")
             (-4 . "<AND")
              (0 . "INSERT")
              (66 . 1)
             (-4 . "AND>")
            (-4 . "OR>")
           )
);setq
(princ "\nSelect TEXT, MTEXT, ATTDEF, or BLOCK inserts w/attributes...")
(if (and (setq ss (ssget flt))
         (setq ss (car (acet-ss-filter (list ss
                                             '(("LAYERUNLOCKED") ;Dis-allow locked layers, non-current-space
                                               ("CURRENTUCS")    ;and dis-allow objects not in current ucs
                                              )
                                             T
                                       );list
                       );acet-ss-filter
                  );car
         );setq
    );and
    (progn
     (setq ss (bns_annotation_ss ss))
     (setq mode (getangle "\nNew absolute rotation <Most Readable>: "));setq
     (setvar "highlight" 0)
     (setq j (bns_trot ss mode))
     (if ss
         (command "_.select" ss "")
     );if
     (princ (strcat "\n" (itoa j) " objects modified."))
 
    );progn then
    (princ "\nNo valid objects selected.")
);if
(acet-error-restore)
);defun c:torient
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TCIRCLE - Text-CIRCle puts a circle around selected text entities
;
(defun c:tcircle ( / ss flt j rad ent offset slot)
 
(acet-error-init
 (list (list "cmdecho" 0
             "highlight" (getvar "highlight")
             "limcheck" 0
       );list
       T
 );list
);acet-error-init
 
(setq flt '((-4 . "<OR") (0 . "TEXT") (0 . "ATTDEF") (0 . "MTEXT") (-4 . "OR>")));setq
(princ "\nSelect TEXT, MTEXT or ATTDEF objects...")
(if (and (setq ss (ssget flt))
         (setq ss (car (acet-ss-filter (list ss
                                             '(("LAYERUNLOCKED") ;Dis-allow locked layers, non-current-space
                                               ("CURRENTUCS")    ;and dis-allow objects not in current ucs
                                              )
                                             T
                                       );list
                       );acet-ss-filter
                  );car
         );setq
    );and
    (progn
 
     (if (not #bns_tcircle_offset)
         (setq #bns_tcircle_offset 0.35)
     );if
     (initget 6)
     (setq offset (getreal
                   (strcat "\nEnter distance offset factor <"
                           (rtos #bns_tcircle_offset 2 (getvar "luprec"))
                           ">: "
                   )
                 )
     );setq
     (if (or (not offset)
             (equal offset "")
         );or
         (setq offset #bns_tcircle_offset)
         (setq #bns_tcircle_offset offset)
     );if
 
 
     (if (not #bns_tcircle_ent)
         (setq #bns_tcircle_ent "Circles")
     );if
     (initget "Circles Slots Rectangles")
     (setq ent (getkword
                (acet-str-format "\nEnclose text with [Circles/Slots/Rectangles] <%1>: "
                                 #bns_tcircle_ent
                );acet-str-format
               )
     );setq
     (if (or (not ent)
             (equal ent "")
         );or
         (setq ent #bns_tcircle_ent)
         (setq #bns_tcircle_ent ent)
     );if
 
     (if (not #bns_tcircle_rad)
         (setq #bns_tcircle_rad "Variable")
     );if
     (initget "Constant Variable")
     (setq rad (getkword
                (acet-str-format 
                   "\nCreate %1 of constant or variable size [Constant/Variable] <%2>: "
                   (strcase ent T)
                   #bns_tcircle_rad
                );acet-str-format
               )
     );setq
     (if (or (not rad)
             (equal rad "")
         );or
         (setq rad #bns_tcircle_rad)
         (setq #bns_tcircle_rad rad)
     );if
     (if (and (equal rad "Constant")
              (or (equal ent "Slots")
                  (equal ent "Rectangles")
              );or
         );and
         (progn
          (if (not #bns_tcircle_slot)
              (setq #bns_tcircle_slot "Both")
          );if
          (initget "Width Height Both")
          (setq slot (getkword
                      (acet-str-format
                              "\nMaintain constant %1 [Width/Height/Both] <%2>: "
                              (strcase (substr ent 1 (- (strlen ent) 1)) T)
                              #bns_tcircle_slot
                      )
                     )
          );setq
          (if (or (not slot)
                  (equal slot "")
              );or
              (setq slot #bns_tcircle_slot)
              (setq #bns_tcircle_slot slot)
          );if
         );progn else get rectang or slot size info
     );if
 
     (setq j (bns_tcircle ss rad ent slot offset))
     (if ss
         (command "_.select" ss "")
     );if
     (princ (strcat "\n" (itoa j) " " ent " created."))
    );progn then
    (princ "\nNo valid objects selected.")
);if
(acet-error-restore)
);defun c:tcircle
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TCOUNT  TEXT COUNT
(defun c:tcount ( / ss_sort ss start inc startinc flag flt n)
 
(acet-error-init
 (list (list "cmdecho" 0
             "highlight" (getvar "highlight")
             "limcheck" 0
       );list
       T
 );list
);acet-error-init
 
(setq flt '((-4 . "<OR")
             (0 . "TEXT")
             (0 . "MTEXT")
             (0 . "ATTDEF")
            (-4 . "OR>")
           )
);setq
 
(if (and (setq ss (ssget flt));setq
         (setq ss (car (acet-ss-filter (list ss
                                             '(("LAYERUNLOCKED") ;Dis-allow locked layers, non-current-space
                                               ("CURRENTUCS")    ;and dis-allow objects not in current ucs
                                              )
                                             T
                                       );list
                       );acet-ss-filter
                  );car
         );setq
    );and
    (progn
     (if (not #bns_tcount_sort)
         (setq #bns_tcount_sort "Select-order")
     );if
 
     (initget "X Y Select-order")
     (setq ss_sort (getkword (acet-str-format
                               "\nSort selected objects by [X/Y/Select-order] <%1>: "
                               #bns_tcount_sort
                             );acet-str-format
                   );getkword
     );setq
     (if (or (not ss_sort)
             (equal ss_sort "")
         );or
         (setq ss_sort #bns_tcount_sort);setq then
         (setq #bns_tcount_sort ss_sort);setq else
     );if
     (if (not #bns_tcount_start)
         (setq #bns_tcount_start 1)
     );if
     (if (not #bns_tcount_inc)
         (setq #bns_tcount_inc 1)
     );if
     (while (not flag)
      (setq flag T)
      (setq startinc (strcat (itoa (fix #bns_tcount_start)) ","
                             (itoa (fix #bns_tcount_inc))
                     );strcat
            startinc (getstring (acet-str-format
                                 "\nSpecify starting number and increment (Start,increment) <%1>: "
                                 startinc
                                );acet-str-format
                     );getstring
      );setq
      (if (equal startinc "")
          (setq startinc (strcat (itoa (fix #bns_tcount_start)) ","
                                 (itoa (fix #bns_tcount_inc))
                         );strcat
          );setq
      );if
      (setq startinc (acet-str-to-list "," startinc))
      (if (or (not (car startinc))
              (not (numberp (read (car startinc))))
              (and (cadr startinc)
                   (not (numberp (read (cadr startinc))))
              );and
          );or
          (progn
           (princ "\nInvalid input.")
           (setq flag nil)
          );progn then
      );if
      (setq start (atoi (car startinc)));setq
      (if (cadr startinc)
          (setq inc (atoi (cadr startinc)));setq then
          (setq inc 1);setq else
      );if
      (if start
          (setq #bns_tcount_start start)
      );if
      (if (or (not inc)
              (equal inc 0)
          );or
          (setq inc #bns_tcount_inc);setq then
      );if
      (if inc
          (setq #bns_tcount_inc inc)
      );if
     );while
     (bns_get_tcountmode_cmd)
     (setq n (bns_tcount
                 ss                  ;the selection set
                 ss_sort             ;the sort type
                 start               ;the start number
                 inc                 ;the increment
                 #bns_tcount_mode    ;placement mode =overwrite/prefix/suffix/find&replace
                 #bns_tcount_replace ;search string for Find&replace option.
             )
     );setq
     (princ (acet-str-format "\n%1 objects modified." (itoa n)))
     (if ss
         (command "_.select" ss "")
     );if
    );progn then
);if
 
(acet-error-restore)
);defun c:tcount
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TJUST   TEXT JUSTIFICATION change routine
(defun c:tjust ( / flag ss flt)
 
(acet-error-init
 (list (list "cmdecho" 0);list
       T
 );list
);acet-error-init
 
(setq flt '((-4 . "<OR")
             (0 . "TEXT")
             (0 . "ATTDEF")
             (0 . "MTEXT")
             (-4 . "<AND")
              (0 . "INSERT")
              (66 . 1)
             (-4 . "AND>")
            (-4 . "OR>")
           )
);setq
 
(if (and (setq ss (ssget flt));setq
         (setq ss (car (acet-ss-filter (list ss
                                             '(("LAYERUNLOCKED") ;Dis-allow locked layers, non-current-space
                                               ("CURRENTUCS")    ;and dis-allow objects not in current ucs
                                              )
                                             T
                                       );list
                       );acet-ss-filter
                  );car
         );setq
    );and
    (progn
     (setq ss (acet-ss-annotation-filter ss))
     (if (not #bns_tjust)
         (setq #bns_tjust "Start")
     );if
     (princ "\nEnter new justification...")
     (initget "Start Center Middle Right TL TC TR ML MC MR BL BC BR")
     (setq flag (getkword
                 (acet-str-format "\n[Start/Center/Middle/Right/TL/TC/TR/ML/MC/MR/BL/BC/BR] <%1>: "
                                  #bns_tjust
                 );acet-str-format
                );getkword
     );setq
     (if (or (not flag)
             (equal flag "")
         );or
         (setq flag #bns_tjust)
     );if
 
     (acet-tjust ss flag)
     (if ss
         (command "_.select" ss "")
     );if
     (princ (strcat "\n" (itoa (sslength ss)) " objects modified."))
     (setq #bns_tjust flag)
    );progn then got a selection set
);if
(acet-error-restore)
);defun c:tjust
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bns_get_tcountmode_cmd - command line prompter for bns_get_tcountmode
;
(defun bns_get_tcountmode_cmd ( / a r )
 
(if (not #bns_tcount_mode)
    (setq #bns_tcount_mode "Prefix")
);if
(if (not #bns_tcount_replace)
    (setq #bns_tcount_replace "")
);if
 
 
(initget "Overwrite Prefix Suffix Find&replace")
(setq a (getkword
         (acet-str-format "\nPlacement of numbers in text [Overwrite/Prefix/Suffix/Find&replace..] < %1>: "
                          #bns_tcount_mode
         );acet-str-format
        );getkword
);setq
(if (or (not a)
        (equal a "")
    );or
    (setq a #bns_tcount_mode);setq then
);if
(setq #bns_tcount_mode a)
(if (equal a "Find&replace")
    (progn
     (setq r "")
     (while (equal r "")
      (setq r (getstring T
                         (strcat "\nEnter search string <"
                                 #bns_tcount_replace
                                 ">: "
                         )
              )
      );setq
      (if (not (equal r ""))
          (setq #bns_tcount_replace r);setq then
          (setq r #bns_tcount_replace);setq else
      );if
      (if (equal r "")
          (princ "*Invalid* Empty search string not allowed.")
      );if
     );while
    );progn then
);if
 
#bns_tcount_mode
);defun bns_get_tcountmode_cmd
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun bns_tcount ( ss ss_sort start inc mode str / a na e1 e2 n lst b j )
 
(if (not (equal "Select-order" ss_sort))
    (progn
     (cond
      ((equal "X" ss_sort)
       (setq ss (bns_esort ss
                       '((e1) (car (cdr (assoc 10 e1)))) ;quoted function for x
                )
       );setq
      );cond #1
      ((equal "Y" ss_sort)
       (setq ss (bns_esort ss
                       '((e1) (* -1.0 (cadr (cdr (assoc 10 e1))))) ;quoted function for y
                )
       );setq
      );cond #2
      ((equal "Z" ss_sort)
       (setq ss (bns_esort ss
                       '((e1) (caddr (cdr (assoc 10 e1))))
                )
       );setq
      );cond #3
     );cond close
    );progn then
);if
 
(setq j 0)
(setq n 0);setq
(repeat (sslength ss)
 
(setq na (ssname ss n)
      e1 (entget na)
      e2 e1
);setq
(cond
 ((equal "TEXT" (cdr (assoc 0 e1)))
     (setq a (itoa start))
     (setq a (place_tcount (cdr (assoc 1 e1)) ;the original
                           a                  ;the number
                           mode
                           str               ;Search string to use if
             )                               ;mode=Find&replace
     );setq
     (entmod (subst (cons 1 a)
                    (assoc 1 e1)
                    e1
             );subst
     );entmod
 );cond #1
 ((equal "MTEXT" (cdr (assoc 0 e1)))
   ;first build one big string from the 3 and 1 group codes
   (setq a "")
   (foreach x e1 
    (if (= (car x) 3) 
        (setq a (strcat a (cdr x)))
    )
   );foreach
   ;then split it apart using \\p  (paragraph) as a delimiter
   (setq   a (strcat a (cdr (assoc 1 e1)))
         lst (acet-str-to-list "\\P" a)
           b ""
   );setq
   ;then make the number additions and build one big string again.
   (foreach x lst
     (setq a (itoa start))
     (setq a (place_tcount x                 ;the original
                           a                 ;the number
                           mode
                           str               ;Search string to use if
             )                               ;mode=Find&replace
           b (strcat b "\\P" a)
     );setq
     (setq start (+ start inc));setq
   );foreach
   (setq start (- start inc));setq
   (setq   b (substr b 3)
         lst nil
   );setq 
   ;the rebuild the e-list without the 3 and 1 group codes.
   (foreach x e1
    (if (and (/= (car x) 3)
             (/= (car x) 1)
        );and
        (setq lst (append lst (list x)))
    )
   );foreach
   ;split the new big string apart and place in the elist as 3 and 1 group codes.
   (setq e1 lst)
   (while (> (strlen b) 250)
    (setq e1 (append e1 
                     (list (cons 3
                                 (substr b 1 250)
                           )
                     )
             );append
           b (substr b 251)
    );setq
   );while
   (setq e1 (append e1 (list (cons 1 b))));setq
   (entmod e1)
 );cond #2
 ((equal "ATTDEF" (cdr (assoc 0 e1)))
     (setq a (itoa start))
     (setq a (place_tcount (cdr (assoc 2 e1)) ;the original
                           a                  ;the number
                           mode
                           str               ;Search string to use if
             )                               ;mode=Find&replace
     );setq
     (setq a (acet-str-replace " " "" a)) ;spaces are not allowed in attdefs so remove them
     (entmod (subst (cons 2 a) 
                    (assoc 2 e1)
                    e1
             );subst
     );entmod
 );cond #3
);cond close
 
(if (not (equal e2 (entget (cdr (assoc -1 e2)))))
    (setq j (+ j 1))
)
 
(setq n (+ n 1));setq
(setq start (+ start inc));setq
);repeat
 
j
);defun bns_tcount
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun place_tcount (org num mode str / )
 
(cond
 ((or (not mode)
      (equal mode "Overwrite")
  );or
  (setq str num);setq
 );cond #1
 ((equal mode "Prefix")
  (setq str (strcat num " " org));setq
 );cond #2
 ((equal mode "Suffix")
  (setq str (strcat org " " num));setq
 );cond #3
 ((equal mode "Find&replace")
  (setq str (acet-str-replace str num org));setq
 );cond #4
);cond close
 
str
);defun place_tcount
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
 
(defun bns_tcircle ( ss rad ent slot offset /
                      a b d na e1 n j lst ll lr ul ur addw addh
                      p0 p1 p2 p3 w h slot down left right up
                   )
  (setq j 0)
  (if (equal "Variable" rad)
    (progn
      (princ (strcat "\nCreating " ent "..."))
      (setq n 0);setq
      (repeat (sslength ss)
        (setq na (ssname ss n)
              e1 (entget na)
               a (acet-geom-textbox e1 offset)
               b (acet-geom-midpoint (car a) (caddr a))
        );setq
        (if (equal "Circles" ent)
            (entmake  (list '(0 . "CIRCLE")
                       (cons 10 (trans b 1 (cdr (assoc 210 e1))))
                       (cons 40 (distance (car a) b))
                       (assoc 210 e1)
                      );list
            );entmake
            (entmake
             (list  '(0 . "LWPOLYLINE")
                    '(100 . "AcDbEntity")
                    '(100 . "AcDbPolyline")
                    '(90 . 4)
                    (cons 38 (last (cdr (assoc 10 e1))))
                    '(70 . 1)
                    (cons 10 (trans (nth 0 a) 1 (cdr (assoc 210 e1))))
                    (if (equal ent "Rectangles")
                       '(42 . 0.0)
                       '(42 . -1.0)
                    );if
                    (cons 10 (trans (nth 3 a) 1 (cdr (assoc 210 e1))))
                    '(42 . 0.0)
                    (cons 10 (trans (nth 2 a) 1 (cdr (assoc 210 e1))))
                    (if (equal ent "Rectangles")
                        '(42 . 0.0)
                        '(42 . -1.0)
                    );if
                    (cons 10 (trans (nth 1 a) 1 (cdr (assoc 210 e1))))
                    '(42 . 0.0)
                    (assoc 210 e1)
             )
            );entmake
        );if
        (setq j (+ j 1));setq
        (setq n (+ n 1));setq
      );repeat
      (princ "Done.")
    );progn then VARIABLE
    (progn
      (if (equal "Circles" ent)
        (progn
          (princ "\nDetermining best radius...")
          (setq n 0
                d -99999.0
          );setq
          (repeat (sslength ss)
            (setq na (ssname ss n)
                  e1 (entget na)
                   a (acet-geom-textbox e1 offset)
                   b (acet-geom-midpoint (car a) (caddr a))
            );setq
            (setq lst (append lst
                              (list (list (trans b 1 (cdr (assoc 210 e1)))
                                          (assoc 210 e1)
                              )     )
                      );append
            );setq
            (if (> (distance (car a) b) d)
                (setq d (distance (car a) b));setq then
            );if
            (setq n (+ n 1));setq
          );repeat find the max radius and get a list of center points
          (princ "Done.")
 
          (princ (strcat "\nCreating " ent "..."))
          (setq n 0);setq
          (repeat (length lst)
            (setq a (nth n lst));setq
            (entmake (list '(0 . "CIRCLE")
                           (cons 10 (car a))
                           (cons 40 d)
                           (cadr a)
                     );list
            );entmake
            (setq j (+ j 1));setq
            (setq n (+ n 1));setq
          );repeat
        ) ;progn
        (progn
          (princ "\nDetermining best size...")
          (setq n 0
                w -99999.0
                h -99999.0
          );setq
          (repeat (sslength ss)
            (setq na (ssname ss n)
                  e1 (entget na)
                   a (acet-geom-textbox e1 offset)
            );setq
            (if (> (distance (car a) (cadr a)) w)
              (setq w (distance (car a) (cadr a)));setq then
            );if
            (if (> (distance (nth 0 a) (nth 3 a)) h)
              (setq h (distance (nth 0 a) (nth 3 a)));setq then
            );if
            (setq n (+ n 1));setq
          );repeat find the max radius and get a list of center points
          (princ "Done.")
 
          (princ (strcat "\nCreating " ent "..."))
          (setq n 0);setq
          (repeat (sslength ss)
            (setq na (ssname ss n)
                  e1 (entget na)
                   a (acet-geom-textbox e1 offset)
            );setq
            (setq ll (nth 0 a)
                  lr (nth 1 a)
                  ur (nth 2 a)
                  ul (nth 3 a)
            )
            (setq left  (angle lr ll)
                  right (angle ll lr)
                  up    (angle ll ul)
                  down  (angle ul ll)
            )
            (if (or (equal "Width" slot)
                    (equal "Both" slot))
              (setq addw (/ (- w (distance ll lr)) 2))
              (setq addw 0)
            ) ;if
            (if (or (equal "Height" slot)
                    (equal "Both" slot))
              (setq addh (/ (- h (distance ll ul)) 2))
              (setq addh 0)
            ) ;if
 
            (setq p0 (polar (polar ll left addw) down addh)
                  p1 (polar (polar lr right addw) down addh)
                  p2 (polar (polar ur right addw) up addh)
                  p3 (polar (polar ul left addw) up addh)
            )
            (entmake
              (list  '(0 . "LWPOLYLINE")
                     '(100 . "AcDbEntity")
                     '(100 . "AcDbPolyline")
                     '(90 . 4)
                     (cons 38 (last (cdr (assoc 10 e1))))
                     '(70 . 1)
                     (cons 10 (trans p0 1 (cdr (assoc 210 e1))))
                     (if (equal ent "Rectangles")
                         '(42 . 0.0)
                         '(42 . -1.0)
                     );if
                     (cons 10 (trans p3 1 (cdr (assoc 210 e1))))
                     '(42 . 0.0)
                     (cons 10 (trans p2 1 (cdr (assoc 210 e1))))
                     (if (equal ent "Rectangles")
                         '(42 . 0.0)
                         '(42 . -1.0)
                     );if
                     (cons 10 (trans p1 1 (cdr (assoc 210 e1))))
                     '(42 . 0.0)
                     (assoc 210 e1)
              )
            )
            (setq j (+ j 1));setq
            (setq n (+ n 1));setq
          );repeat
        ) ;progn
      ) ;if
      (princ "Done.")
    );progn else CONSTANT
  );if
 
  j
);defun bns_tcircle
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_trot ( ss mode / na e1 n a b j )
  (setq j 0)
  (setq n 0)
  (repeat (sslength ss)
    (setq na (ssname ss n)
          e1 (entget na)
    );setq
    (if (= "MTEXT" (cdr (assoc 0 e1)))
        (setq a (cdr (assoc 50 e1)))
        (setq a (acet-geom-angle-trans (cdr (assoc 50 e1)) na 1))
    );if
    (setq a (- a (getvar "angbase")))
 
    (if (not mode)
        (progn
         (setq a (* (/ 180.0 pi) a))
         (if (< a 0)
             (setq a (+ a 360));setq
         );if
         (if (and (> a 100.0) (< a 280.0))
             (progn
              (bns_rotate_text e1 pi)
              (setq j (+ j 1));setq
             );progn then
         );if
        );progn then most-readable
        (progn
         (setq a (- mode a));setq
         (bns_rotate_text e1 a)
         (setq j (+ j 1));setq
        );progn else
    );if
 
    (setq n (+ n 1));setq
  );repeat
 
  j ;the number of objects modified.
);defun bns_trot
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rotate_text ( e1 ang / p1 p2 a)
 
(setq p1 (acet-geom-textbox e1 0.2)
      p1 (acet-geom-midpoint (car p1) (caddr p1))
       a (cdr (assoc 50 e1))
       a (+ ang a)
      e1 (subst (cons 50 a) (assoc 50 e1) e1)
);setq
(entmod e1)
(entupd (cdr (assoc -1 e1)))
 
(setq e1 (entget (cdr (assoc -1 e1))))
 
(setq p2 (acet-geom-textbox e1 0.2)
      p2 (acet-geom-midpoint (car p2) (caddr p2))
       a (acet-geom-delta-vector p2 p1)
       a (trans a 1 na T)
      p1 (cdr (assoc 10 e1))
      p1 (acet-geom-vector-add p1 a)
 
      p2 (cdr (assoc 11 e1))
      p2 (acet-geom-vector-add p2 a)
 
      e1 (subst (cons 10 p1) (assoc 10 e1) e1)
      e1 (subst (cons 11 p2) (assoc 11 e1) e1)
);setq
(entmod e1)
(entupd (cdr (assoc -1 e1)))
 
);defun bns_rotate_text
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bns_esort - Sorts a selection set of entities by a specified property.
;Takes - a selection set of entities and a quoted
;function that determines the value by which the entities should be sorted.
;Returns - a selection set that is sorted.
;
;
;NOTE: The 'func' argument must meet the following criteria:
;        - a quoted name of a lisp function
;        - Function must take a single argument that is an entity list
;        - Function must return one of the following dtat types:
;             str, list, int, or real.
;
;    For example:
;   ;To sort a selection set based on x coordinates of entities:
;   ;- Define the function to extract the x coordinate.
;    (defun get_x_coord ( ent )
;     (car (cdr (assoc 10 ent)))
;    )
;   ;Then call bns_esort like this:
;   (setq newss (bns_esort ss 'get_x_coord)); ness will be the sorted selection set.
;
;   Thats it! You get selection set back that is sorted left-to right.
;
(defun bns_esort ( ss func / a lst ss2 na e1 n )
 
(setq ss2 (ssadd));setq
(setq n 0)
(repeat (sslength ss)
(setq na (ssname ss n)
      e1 (entget na)
);setq
;(setq lst (append lst
;                  (list (list (eval '(func e1))
;                              e1
;                        );list
;                  );list
;          );append
;);setq
(setq lst (cons (list (eval '(func e1)) e1)
                lst
          );cons
);setq
 
(setq n (+ n 1));setq
);repeat
 
(setq lst (acet-list-isort lst 0));setq
 
(setq n 0)
(repeat (length lst)
 (setq   a (nth n lst)
         a (cadr a)
         a (cdr (assoc -1 a))
       ss2 (ssadd a ss2)
 );setq
(setq n (+ n 1));setq
);repeat
 
ss2
);defun bns_esort


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgB6qaW8XT7l18A8sahq9AsjyFQZlkDW2uV+JT7W
;;; H5R2BwFPIF7NIOaapHvmow/0VcSJIw+oTfzAQsmLG7ih0lfmL55LQiK3Nel63WOt
;;; PlO7uJCXX2rBunUw40RoqgHs+PX8wLthT0ZRcm6uoyCX3aOPZKNhHB/KWxcfv6vi
;;; YV2GkGhJBeajNYH6uKeYiHBo3jHUBotdD7MnLmfsdS/CkJXhPRXWRNz6qTesAmfC
;;; 5TnNg1frGnPHLFjqO0OONv2H54b3F3k3O1aNo+KdvadMsihEefFS1W7yUCEgaER5
;;; JksDOa0W40QqqXrv9FVL+K4F7dEn5WrPUvAfCQeWux7xpx39JNVGWoLtz0Vs4+gS
;;; sWJDYgJ+651/pHBqT8JDrnC4dVBxW7+6n7Dk+wBKBLcxA/AitnrzTJQonzumJDs+
;;; jex0zEc5P7o7hYVHTMegwRjT60xDzEJ3InEuxx8tCrWef4rxDBT43BFp0C4GaEb8
;;; 23AwVKrKFgjVsYmgC0K1io8f0cpYSaCVMXR/OY++aqOhEZlWY8t2XTU4yvdcC6lU
;;; sOxf0g9ehXaLmiuoOO3+3ChGXkCVXAkcl9KevyTuHG9GKIZd6nzKTxnubBxq+jUN
;;; yTFqlM+d7A/pn9jNRDIoS/XKLsA8+P5yFFbLBno6uFf1+ULK9S+S/sAo9kf3Rbj2
;;; xTh+yg==
;;; -----END-SIGNATURE-----