;;;
;;;    RTUCS.LSP - Written by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;[tab] toggle axis
;[r]   restore
;[s]   save current
;[d]   delete
;[c]   cycle
;[a]   snap angle
;[o]   origin
;[w]   world
;[v]   view
;[u]   undo
;
;
(defun c:rtucs ( / snap p1 p2 vh lst lst2 lst3 lst4
                   flag flag2 da a b c pn pndn pr
               )
 
(acet-error-init (list (list     "cmdecho" 0
                                "ucsicon" 3
                              "modemacro" ""
                              "ucsfollow" 0
                                 "osmode" 0
                               "gridmode" 0
                        );list
                        T
                  );list
);acet-error-init
(setq *error* (append (list (car *error*) (cadr *error*))
                      (list '(bns_rtucs_finish lst lst3 lst4)
                            '(acet-error-restore)
                      );list
              );append
);setq
 
(setvar "ucsicon" 0)
(command "_.undo" "_m")
(setvar "ucsicon" 3)
 
(setq snap (acet-getvar (list "BNS_RTUCS_ASNAP" 2))) ;Look in the reg
(if (not snap)
    (progn
     (setq snap 15.0)
     (acet-setvar (list "BNS_RTUCS_ASNAP" 15.0 2))
    );progn then set up defaults
);if
 
(setq  p1 (acet-geom-view-points)
       p2 (trans (cadr p1) 1 2)
       p1 (trans (car p1) 1 2)
       p1 (list (car p1) (cadr p1) 0.0)            ;lower left corner in display coords.
       p2 (list (car p2) (cadr p2) 0.0)            ;upper right corner in display coords.
       vh (/ (- (cadr p2) (cadr p1))               ;view height divided by 2.0
             2.0
          )
      lst (list "X" "Y" "Z")                       ;axis list, first element is active axis
     lst2 '(                                       ;standard ucs list
             ((1.0 0.0 0.0)  (0.0 1.0 0.0))        ; - top (parallel to World)
             ((1.0 0.0 0.0)  (0.0 0.0 1.0))        ; - front
             ((0.0 1.0 0.0)  (0.0 0.0 1.0))        ; - right
             ((-1.0 0.0 0.0) (0.0 0.0 1.0))        ; - back
             ((0.0 -1.0 0.0) (0.0 0.0 1.0))        ; - left
             ((-1.0 0.0 0.0) (0.0 1.0 0.0))        ; - bottom
          )
     lst3 (bns_rtucs_saved)        ;saved ucs'
     flag 0                        ;pick flag 0=inactive or 1=active rotation
       da 0.0                      ;delta angle
);setq
 
(setvar "ucsicon" 3)
(bns_rtucs_display_status lst da flag)
 
 
(setq pr (strcat "\nPress TAB key to change axis or "
                 "\n[Save/Restore/Delete/Cycle/Angle/Origin/View/World/Undo] <Drag to rotate>"
         );strcat
);setq
(princ pr)
 
(while (not (equal (setq a (bns_rtucs_getinput))
                   " "
            );equal
       );not
 
(cond
 ;;;;-------streamed coord. ------------------
 ((and (equal (type a) 'LIST)      ;its a
       (or (equal (car a) 5)       ;Streamed coord or
           (equal (car a) 3)       ;a picked coord
       );or
       (setq pn (cadr a))       ;Set the last known coord.
  );and
  (setq flag2 (acet-sys-lmouse-down));setq
  (if (and flag2
           (equal flag 0)
           (equal 3 (car a))
      );and
      (progn
       (setq pndn pn                      ;the pen down point
                c (acet-ucs-get nil)
             lst4 (cons (list "PICK-Rotate" c) lst4)
             flag 1
       );setq
      );progn then set pen down at location
      (progn
       (if (not flag2) ;mouse is up
           (progn
            (setq da 0.0)
            (if (not (equal flag 0)) ;if this is pick button up event
                (bns_rtucs_display_status lst da flag)  ;then display current the status.
            );if
            (setq flag 0);setq set pen up.
           );progn else pen is up
       );if
      );progn else
  );if
  (if (equal flag 1)                            ;pen down
      (progn
       (setq   da (bns_rtucs_calc_da vh         ;view height
                                     snap       ;angle snap
                                     (car lst)  ;current axis (string)
                                     pndn       ;pen down
                                     pn         ;last pen location
                                     da         ;current delta angle
                  );bns_rtucs_calc_da
       );setq
       (bns_rtucs_display_status lst da flag)  ;display current the status.
      );progn then pen is down
  );if
 );cond #1
 
 ;;;;-------axis toggle ------------------
 ((equal a "\t")              ;Toggle active axis
  (setq  lst (list (cadr lst) (caddr lst) (car lst))
          da 0.0 ;reset delta angle
        pndn pn  ;set pen down point to last known point
  );setq
  (bns_rtucs_display_status lst da flag)  ;display current the status.
  ;(princ pr)
 );cond #2
 
 ;;;;-------Restore ------------------
 ((equal a "R")                ;Restore a ucs
  (setq flag 0)     ;pen up
  (if lst3
      (progn
       (setq c (acet-ucs-get nil))
       (acet-ucs-set (cadr (car lst3)))
       (princ (strcat "\nRestored UCS: " (car (car lst3)) "."))
       (setq lst3 (append (cdr lst3) (list (car lst3))));setq
       (setq lst4 (cons (list "RESTORE" c) lst4));setq ;log the event to undo cue
       (bns_rtucs_display_status lst da flag)  ;display current the status.
      );progn then
      (princ "\nNo saved UCS'.")
  );if
  (princ pr)
 );cond #3
 
 ;;;;-------Delete ------------------
 ((equal a "D")                ;Delete a saved ucs
  (setq flag 0)     ;pen up
  (if lst3
      (progn
       (setq c (acet-ucs-get nil))
 
       (setq a nil)
       (while (and (setq a (xstrcase (getstring "\nDelete UCS: ")))
                   (not (equal a ""))
                   (not (assoc a lst3))
              );and
         (princ (strcat "\nUCS " a " not found."))
       );while
       (if (not (equal a ""))
           (progn
            (setq lst4 (cons (list "DELETE" c a lst3) lst4));setq log event to undo cue
            (setq lst3 (append (reverse (cdr (member (assoc a lst3) (reverse lst3))))
                               (cdr (member (assoc a lst3) lst3))
                       );append remove the item from saved list
            );setq
           );progn then delete it.
       );if
      );progn then
      (princ "\nNo saved UCS'.")
  );if
  (princ pr)
 );cond #4
 
 ;;;;-------save  ------------------
 ((equal a "S")                ;Save current
  (setq    a nil
        flag 0      ;pen up
  );setq
  (while (and (not (equal a ""))
              (or (not a)
                  (not (snvalid a))
              );or
         );and
   (setq a (getstring "\nSave current UCS as: "))
   (if (and (not (equal a ""))
            (not (snvalid a))
       );and
       (princ "\n*Invalid name*")
   );if
  );while
  (if (and a
           (not (equal a ""))
      );and
      (progn
       (setq a (xstrcase a))
       (if (or (not (assoc a lst3))
               (equal "Yes"
                      (progn
                       (initget "Yes No")
                       (getkword (strcat "\nUCS " a " already exists.  Replace it? <N> "))
                      );progn
               );equal
           );or
           (progn
            (setq c (acet-ucs-get nil)
                  a (xstrcase a)
            );setq
            (setq lst4 (cons (list "SAVE" ;the operation
                                    c     ;current ucs orientation
                                    a     ;the name to save it as
                                    lst3  ;the saved list as it exists before saving
                             );list
                             lst4  ;the undo que
                       );cons
            );setq log the event to undo que
 
            (if (not (assoc a lst3))
                (setq lst3 (append lst3
                                   (list (list a c))
                           );append
                );setq then add the new ucs to the saved list
                            ;or replace the old one with the new one.
                (setq lst3 (subst (list a c) (assoc a lst3) lst3));setq
            );if
            (princ (strcat "\nUCS " a " saved."))
           );progn then save it
       );if
      );progn then save it?
  );if
  (princ pr)
 );cond #5
 
 ;;;;-------Cycle through top, front, right, back, left, bottom  ------------------
 ((equal a "C")                ;Cycle through standard orientations top, front, right, back, left, bottom
  (setq flag 0    ;pen up
           c (acet-ucs-get nil)
           b (append (list (getvar "ucsorg"))
                     (car lst2)
             )
  );setq
  (while (equal (cdr b) (cdr c)) ;make sure we are not already lined up to the next cycled ucs
   (setq lst2 (append (cdr lst2) (list (car lst2)))
            b (append (list (getvar "ucsorg"))
                      (car lst2)
              )
   );setq
  );while
  (acet-ucs-set b)
  (setq lst2 (append (cdr lst2) (list (car lst2)))
        lst4 (cons (list "CYCLE" c) lst4) ;log the event to undo cue
  );setq
  (bns_rtucs_display_status lst da flag)  ;display current the status.
 );cond #6
 
 ;;;;------- angle snap------------------
 ((equal a "A")                ;Angle snap
 
  (setq c (acet-ucs-get nil))
 
  ;;;Get the new angle snap value or accept a default.
  (initget 4);dis-allow negative values.
  (setq snap (getangle (strcat "\nEnter angle snap increment or 0 for off <"
                              (angtos (* snap (/ pi 180.0)))
                               ">: "
                      )
             );getangle
  );setq
  (if snap
      (progn
       (setq snap (* snap (/ 180.0 pi)));setq
       (setq lst4 (cons (list "Angle-Snap" c
                              (acet-getvar (list "BNS_RTUCS_ASNAP" 2))
                        );list
                        lst4
                  );cons
       );setq log the event to undo cue
 
       (acet-setvar (list "BNS_RTUCS_ASNAP" snap 2)) ;set the new one
       (if (equal flag 1) ;if the pen is down set cur ucs to proper location
           (progn
            ;;;Snap the ucs to new value
            (setq da (bns_rtucs_calc_da vh         ;view height
                                        snap       ;angle snap
                                        (car lst)  ;current ax is (string)
                                        pndn       ;pen down
                                        pn         ;last pen location
                                        da         ;current delta angle
                     )
            );setq
           );progn then
       );if
      );progn then set new snap
      (setq snap (acet-getvar (list "BNS_RTUCS_ASNAP" 2)))      ;else get the original
  );if
  (bns_rtucs_display_status lst da flag)  ;display current the status.
  (princ pr)
 );cond #7
 
 ;;;;------- set Origin ------------------
 ((equal a "O")                ;set Origin
 
  (if (setq a (assoc "OSMODE" (car acet-#-sysvar-list))) ;restore user's osnap
      (setvar "osmode" (cadr a))
  );if
  (if (setq p1 (getpoint "\nPick new origin point: "))
      (progn
       (setq    c (acet-ucs-get nil)
             flag 0                  ;pen up
       );setq
       (command "_.ucs" "_or" p1)
       (setq lst4 (cons (list "ORIGIN" c) lst4));setq
      );progn then
  );if
  (princ pr)
  (setvar "osmode" 0) ;turn it back off.
  (bns_rtucs_display_status lst da flag)  ;display current the status.
 );cond #8
 
 ;;;;------- World ------------------
 ((equal a "W")                ;set World
  (setq flag 0                ;pen up
           c (acet-ucs-get nil)
  );setq
  (command "_.ucs" "_w")
  (setq lst4 (cons (list "WORLD" c) lst4));setq
  (bns_rtucs_display_status lst da flag)  ;display current the status.
 );cond #9
 
 ;;;;------- View ------------------
 ((equal a "V")                ;set View
  (setq flag 0                 ;pen up
           c (acet-ucs-get nil)
  );setq
  (command "_.ucs" "_view")
  (setq lst4 (cons (list "VIEW" c) lst4));setq
  (bns_rtucs_display_status lst da flag)  ;display current the status.
 );cond #10
 
 ;;;;------- Undo ------------------
 ((equal a "U")
  (if lst4
      (progn
       (setq a (car lst4))
       (cond
        ((equal (car a) "Angle-Snap")
         (setq snap (last a))
         (acet-setvar (list "BNS_RTUCS_ASNAP" snap 2))
         (acet-ucs-set (cadr a))
        );cond #1
        ((or (equal (car a) "SAVE")         ;Need to special case SAVE and DELETE
             (equal (car a) "DELETE")
         );or
         (setq lst3 (last a)) ;retsore the previous saved list
        );cond #2
        (T
         (acet-ucs-set (cadr a))
        );cond #3
       );cond close
 
       (princ (strcat "\nUndo " (car a)))
       (setq lst4 (cdr lst4)) ;pull the operation out of the cue
      );progn then
      (princ "\nNothing to undo.")
  );if
  (setq flag 0)          ;bring the pen up.
  (bns_rtucs_display_status lst da flag)  ;display current the status.
  (princ pr)
 );cond #11
);cond close
 
(if (equal flag 0) ;if pen is up then set da to 0
    (setq da 0.0)
);if
 
 
);while
 
(bns_rtucs_finish lst lst3 lst4)
 
(acet-error-restore)
);defun c:rtucs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rtucs_finish ( lst lst3 lst4 / a n e1 na )
 
(princ "\n")
 
(bns_rtucs_tmp_update 0 lst)
 
(setvar "ucsicon" 0)
(setq a (acet-ucs-get nil))
(command "_.undo" "_back")
(if (not (equal (getvar "cmdnames") "")) ;just in case code.
    (command nil)
);if
(acet-ucs-set a)
 
;get ready to do the qued save and delete operations....
(setq lst4 (reverse lst4));setq
(setq n 0)
(repeat (length lst4)
(setq a (nth n lst4));setq
(cond
 ((equal (car a) "SAVE")
  (if (assoc (caddr a) lst3)
      (progn
       (setq a (assoc (caddr a) lst3));setq
       (if (setq na (tblobjname "ucs" (car a)))
           (progn
            (setq e1 (entget na)
                  e1 (subst (cons 10 (car (cadr a))) (assoc 10 e1) e1)
                  e1 (subst (cons 11 (cadr (cadr a))) (assoc 11 e1) e1)
                  e1 (subst (cons 12 (caddr (cadr a))) (assoc 12 e1) e1)
            );setq
            (entmod e1)
           );progn then re-define it with entmod
           (progn
            (setq e1 (list '(0 . "UCS") '(100 . "AcDbSymbolTableRecord")
                           '(100 . "AcDbUCSTableRecord")
                            (cons 2 (car a))
                           '(70 . 0)
                           (cons 10 (car (cadr a)))
                           (cons 11 (cadr (cadr a)))
                           (cons 12 (caddr (cadr a)))
                     );list
            );setq
            (entmake e1)
           );progn else create it with entmake
       );if
      );progn then save it
  );if
 );cond #1
 ((equal (car a) "DELETE")
  (if (and (caddr a)
           (tblobjname "ucs" (caddr a))
      );and
      (command "_.ucs" "_d" (caddr a))
  );if
 );cond #2
);cond close
(setq n (+ n 1));setq
);repeat
 
);defun bns_rtucs_finish
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rtucs_calc_da ( vh snap axis pndn pn da / a p1 p2 p3 p4 p5 da2)
 
(setq p1 (trans '(0.0 0.0 0.0) 1 2)) ;setq trans 0,0 from current to display
(cond
 ((equal axis "X") (setq p2 (trans '(1.0 0.0 0.0) 1 2)))
 ((equal axis "Y") (setq p2 (trans '(0.0 1.0 0.0) 1 2)))
 ((equal axis "Z") (setq p2 (trans '(0.0 0.0 1.0) 1 2)))
);cond
(setq p1 (list (car p1) (cadr p1) 0.0)
      p2 (list (car p2) (cadr p2) 0.0)
);setq
(if (equal p1 p2 0.000001)
    (setq p2 (polar pndn 0 1.0)) ;then vector is directly into screen.
    (setq p2 (polar pndn (angle p1 p2) 1.0));setq From last pick along current axis a distance of 1
);if
(setq p2 (list (car p2) (cadr p2) 0.0)    ;Make sure 0 elevation
      p3 (bns_rtucs_findpint pndn p2 pn)  ;Get perp from pn to vector pndn-p2 (along cur axis)
      p4 (angle pndn p2)                  ;Angle of cur axis with respect to display coords.
      p5 (angle pndn pn)                  ;Angle from last pick to streamed coord.
);setq
(if (< p4 0)         ;make sure p4 is pos
    (setq p4 (+ p4 (* 2.0 pi)))
);if
(if (< p5 0)         ;make sure p5 is pos
    (setq p5 (+ p5 (* 2.0 pi)))
);if
(if (< p5 p4)        ;make sure p5 is greater than p4
    (setq p5 (+ p5 (* 2.0 pi)))
);if
(if (and (>= (- p5 p4) 0)            ;if angle change is less than 180 then set dist a
         (<= (- p5 p4) pi)           ;to a neg
    );and
    (setq a (* -1.0 (distance pn p3)))
    (setq a (distance pn p3))        ;else use pos value
);if
(setq   a (/ (* 180.0 a) vh));setq ;where it should to be
(if (not (equal 0.0 snap))
    (progn
     (setq a (acet-calc-round a snap));setq
    );progn then
);if
(setq da2 (- a da));setq where it should be minus where it is
 
(if (not (equal 0.0 da2))
    (command "_.ucs" (strcat "_" axis) da2)
);if
 
a
);defun bns_rtucs_calc_da
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of saved ucs'
(defun bns_rtucs_saved ( / lst n a b)
 
(setq lst (acet-table-name-list "ucs"))
(setq n 0)
(repeat (length lst)
(setq a (nth n lst)
      b (tblobjname "ucs" a)
      b (entget b)
      b (list (xstrcase a)
              (list (cdr (assoc 10 b))
                    (cdr (assoc 11 b))
                    (cdr (assoc 12 b))
              );list
        );list
);setq
(setq lst (subst b a lst));setq
(setq n (+ n 1));setq
);repeat
 
lst
);defun bns_rtucs_saved
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rtucs_findpint ( p1 p2 p3 / m1 m2 p4 b1 b2 dx dy)
 
 
(cond
 ((equal 0.0
	 (- (car p2)
	    (car p1)
	 );minus
         0.000001
  );equal
  (setq p4 (list (car p1) (cadr p3));list
  );setq
 );cond #1 then line is verticle
 ((equal 0.0
	 (- (cadr p2)
	    (cadr p1)
	 );minus
         0.000001
  );equal
  (setq p4 (list (car p3) (cadr p1));list
  );setq
 );cond #2 then horizontal
 (T
  (progn
   (setq dx (- (car p2)
	       (car p1)
	    );minus
	 dy (- (cadr p2)
	       (cadr p1)
	    );minus
	 m1 (/ dy dx)
	 m2 (* -1 (/ dx dy))
	 b1 (- (cadr p1)
	       (* (car p1)
		  m1
	       );multiply
	    );minus
	 b2 (- (cadr p3)
	       (* (car p3)
		  m2
	       );multiply
	    );minus
	 p4 (/ (- b1 b2)
	       (- m2 m1)
	    );div. the x coord.
	 p4 (list p4
		  (+ (* m2 p4) b2) ;the y coord.
	    );list
   );setq
  );progn
 );cond #3
);cond close
 
(list (car p4) (cadr p4) 0.0)
);defun bns_rtucs_findpint
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rtucs_getinput ( / a b)
 
(setq a (grread 1  ;stream input
                14  ;4=use pointer draw flag, 2 don't move the key cursor, 8 no console erro message
                0  ;draw flag 0=normal, 1=none, 2=select
        )
      b (cadr a)
      a (car a)
);setq
(cond
 ((equal a 2)
  (setq b (xstrcase (chr b)))
  (if (equal b "\r")
      (setq b " ")
  );if
  (princ (chr 8))
 )
 ((equal a 11)
  (setq b " ")
 )
 ((or (equal a 3)
      (equal a 5)
  )
  (setq b (trans b 1 2)
        b (list (car b) (cadr b) 0.0)
        b (list a b)
  );setq
 );or
);cond close
 
b
);defun bns_rtucs_getinput
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rtucs_display_status ( lst da flag / a x y z fuz)
 
 (bns_rtucs_tmp_update 1 lst)
 
 (setq   a (strcat "Current axis = " (car lst))
       fuz 0.000001
 );setq
 (if (equal flag 1)
     (setq a (strcat a " < "
                     (rtos da 2 (getvar "auprec"))
             )
     );setq
 );if
 (setq x (getvar "ucsxdir")
       y (getvar "ucsydir")
       z (acet-geom-cross-product x y)
 );setq
 (cond
  ((equal z '(0.0 0.0 1.0) fuz)
   (if (and (equal x '(1.0 0.0 0.0) fuz)
            (equal y '(0.0 1.0 0.0) fuz)
       );and
       (progn
        (if (equal (getvar "ucsorg") '(0.0 0.0 0.0) fuz)
            (setq a (strcat a " WORLD (Top)"));setq
            (setq a (strcat a " Top"))
        );if
       );progn then either world or top
       (setq a (strcat a " (Top)"));else parallel to top (indicated by parens)
   );if
  );cond 1
  ((equal z '(0.0 -1.0 0.0) fuz)
   (if (and (equal x '(1.0 0.0 0.0) fuz)
            (equal y '(0.0 0.0 1.0) fuz)
       );and
       (setq a (strcat a " Front"))   ;then front
       (setq a (strcat a " (Front)")) ;else parallel to front
   );if
  );cond 2
  ((equal z '(1.0 0.0 0.0) fuz)
   (if (and (equal x '(0.0 1.0 0.0) fuz)
            (equal y '(0.0 0.0 1.0) fuz)
       );and
       (setq a (strcat a " Right"))
       (setq a (strcat a " (Right)")) ;parallel
   );if
  );cond 3
  ((equal z '(0.0 1.0 0.0) fuz)
   (if (and (equal x '(-1.0 0.0 0.0) fuz)
            (equal y '(0.0 0.0 1.0) fuz)
       );and
       (setq a (strcat a " Back"))
       (setq a (strcat a " (Back)")) ;parallel
   );if
  );cond 4
  ((equal z '(-1.0 0.0 0.0) fuz)
   (if (and (equal x '(0.0 -1.0 0.0) fuz)
            (equal y '(0.0 0.0 1.0) fuz)
       );and
       (setq a (strcat a " Left"))
       (setq a (strcat a " (Left)"))
   );if
  );cond 5
  ((equal z '(0.0 0.0 -1.0) fuz)
   (if (and (equal x '(-1.0 0.0 0.0) fuz)
            (equal y '(0.0 1.0 0.0) fuz)
       );and
       (setq a (strcat a " Bottom"))
       (setq a (strcat a " (Bottom)"))
   );if
  );cond 6
 );cond close
 
 (setvar "modemacro" a)
 
);defun bns_rtucs_display_status
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rtucs_tmp_update ( flag xlst / d n lst lst2 lst3 )
 
(if #bns_rtucs_tmp
    (progn
     (setq lst #bns_rtucs_tmp)
     (setq n 0)
     (repeat (length lst)
      (bns_rtucs_tmp_draw (nth n lst) ;list of points
                          0           ;color
                          0
      )
      (bns_rtucs_tmp_draw (nth n lst) ;list of points
                          0           ;color
                          0
      )
      (setq n (+ n 1));setq
     );repeat
     (setq #bns_rtucs_tmp nil)
    );progn then un-draw the previous stuff
);if
(if (equal flag 1)
    (progn
     (setq d (* 0.15 (getvar "viewsize")));setq
     (setq  lst (list (trans '(0.0 0.0 0.0) 1 0)
                      (trans (list d 0.0 0.0) 1 0)
                );list
           lst2 (list (trans '(0.0 0.0 0.0) 1 0)
                      (trans (list 0.0 d 0.0) 1 0)
                );list
           lst3 (list (trans '(0.0 0.0 0.0) 1 0)
                      (trans (list 0.0 0.0 d) 1 0)
                );list
     );setq
     (if (equal (car xlst) "X")
         (bns_rtucs_tmp_draw lst 1 1)
         (bns_rtucs_tmp_draw lst 1 0)
     )
     (if (equal (car xlst) "Y")
         (bns_rtucs_tmp_draw lst2 2 1)
         (bns_rtucs_tmp_draw lst2 2 0)
     )
     (if (equal (car xlst) "Z")
         (bns_rtucs_tmp_draw lst3 3 1)
         (bns_rtucs_tmp_draw lst3 3 0)
     )
 
     (setq #bns_rtucs_tmp (list lst lst2 lst3
                          );list
     );setq
    );progn then
);if
 
 
);defun bns_rtucs_tmp_update
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rtucs_tmp_draw ( lst c h / n a b)
 
(setq a (trans (car lst) 0 1))
(setq n 1)
(repeat (max 0 (- (length lst) 1))
(setq b (trans (nth n lst) 0 1));setq
(grdraw a b c h)
(setq a b)
(setq n (+ n 1));setq
);repeat
 
);defun bns_rtucs_tmp_draw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:acetUcs-top ()
 (acet-error-init '(("cmdecho" 0 "osmode" 0)))
 (command "_.ucs" "_3p" "0,0,0" 
          (trans '(1.0 0.0 0.0) 0 1 T) (trans '(0.0 1.0 0.0) 0 1 T)
 )
 (acet-error-restore)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:acetUcs-front ()
 (acet-error-init '(("cmdecho" 0 "osmode" 0)))
 (command "_.ucs" "_3p" "0,0,0" 
          (trans '(1.0 0.0 0.0) 0 1 T) (trans '(0.0 0.0 1.0) 0 1 T)
 )
 (acet-error-restore)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:acetUcs-right ()
 (acet-error-init '(("cmdecho" 0 "osmode" 0)))
 (command "_.ucs" "_3p" "0,0,0" 
          (trans '(0.0 1.0 0.0) 0 1 T) (trans '(0.0 0.0 1.0) 0 1 T)
 )
 (acet-error-restore)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:acetUcs-back ()
 (acet-error-init '(("cmdecho" 0 "osmode" 0)))
 (command "_.ucs" "_3p" "0,0,0" 
          (trans '(-1.0 0.0 0.0) 0 1 T) (trans '(0.0 0.0 1.0) 0 1 T)
 )
 (acet-error-restore)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:acetUcs-left ()
 (acet-error-init '(("cmdecho" 0 "osmode" 0)))
 (command "_.ucs" "_3p" "0,0,0" 
          (trans '(0.0 -1.0 0.0) 0 1 T) (trans '(0.0 0.0 1.0) 0 1 T)
 )
 (acet-error-restore)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:acetUcs-bottom ()
 (acet-error-init '(("cmdecho" 0 "osmode" 0)))
 (command "_.ucs" "_3p" "0,0,0" 
          (trans '(-1.0 0.0 0.0) 0 1 T) (trans '(0.0 1.0 0.0) 0 1 T)
 )
 (acet-error-restore)
)


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAeXsuoUYwiVNAhRyetEz3iE72tc6a/JppY3P3C
;;; fp+Lbx5/N4RQQftp5rWKoNzPVweAIypsT+cFrmYIWwScLvdyRsyiyRa/YCCOS/uj
;;; oUqis9P3ZpxQb7NSimrD8ekoDvl+KfFBWI/qcx3v8AntHLVOH+r9ExW5gKhIiDFS
;;; O5YnznhO+aVqPwv/Zweb8QhhTqbsZr7R80MaIqCLRhNDhtwL2404me9rrs+5waFh
;;; dHpjPEWvINBPxK0W7MTbbjhSjmcUxYhNum+ADqo7wcxCz+XHBFyVrjx6yk1/9UZi
;;; e5XeBdiO5p1ZQiT9+dq+GZXdPZG7DTHaAfVF8jK56KJet27v8Iq4GmxnSvjJVI/i
;;; urHv2uIcKPo9pBitIzo/gMOR0RbAci1Gv1wXXkH4BxxDb/0XJPohQqGyRTqqtGZ2
;;; 2lDfH/p+71F9BIg1xQL1jMlb/gQW8aMC1GwcRUaM9uIPA2qu7OManBn9Uk8N5YZE
;;; ksOatAR0KPpels33t/WE7tGIPNHKFt+Y2apIqERIFyjsrWUpftMhQ8AA/VZUWtBQ
;;; /VXYDkdOx/0X3xUdRPYzmntfeBqknTVqz3WDlVU81RkQJ1WF+t4yK2IMmmWg6aBD
;;; oRSHs5FpPkaR0qtUTPkoO8KeXgQJx+pzqhHCQm+flJ/iv1suxP4CTmqjtC1XZSik
;;; c8UF9g==
;;; -----END-SIGNATURE-----