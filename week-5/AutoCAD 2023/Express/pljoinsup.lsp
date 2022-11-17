;;;
;;;    PLJOINSUP.LSP
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
(if (not #acet-pljoin-prec)
    (setq #acet-pljoin-prec 0.0000001)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin2 ( ss st fuzz / flt )
 
(setq flt '((-4 . "<OR")
             (0 . "LINE")
             (0 . "ARC")
             (-4 . "<AND")
              (0 . "*POLYLINE")
              (-4 . "<NOT") (-4 . "&") (70 . 89)  (-4 . "NOT>") ;1 8 16 64
             (-4 . "AND>")
            (-4 . "OR>")
           )
);setq
(if (and (setq ss (acet-pljoin-do-ss-pre-work2 ss flt)) ;convert lines/arcs/heavy plines ..etc.
                                                       ;to lighweight plines
         (setq ss (acet-pljoin-1st-pass2 ss flt))       ;initial pass with pedit command
    );and
    (acet-pljoin-2nd-pass2 ss fuzz st flt) ;where the work is..
);if
 
);defun acet-pljoin
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Try to join as many as possible before performing the
;hashing.
;
(defun acet-pljoin-1st-pass2 ( ss flt / na )
 
 (acet-spinner)
 
 (setq na (entlast))
 (command "_.pedit" (ssname ss 0) "_j" ss "" "_x")
 
 (command "_.select" ss)
 (if (not (equal na (entlast)))
     (command (entlast) "")
     (command "")
 );if
 (setq ss (acet-ss-ssget-filter ss flt));setq
 (if (and ss
          (<= (sslength ss) 1)
     );and
     (setq ss nil)
 );if
 (setq ss (acet-pljoin-ss-flt2 ss flt))
 
 ss
);defun acet-pljoin-1st-pass
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-2nd-pass2 ( ss fuzz st flt / g lst lst3 len x lst2 lst4 n a
                                              tmpe1 tmpe2 tmpna tmpna2 flst
                           )
 
 ;;(print "acet-pljoin-2nd-pass")
 ;;(print "")
 
 ;;create a couple of temporary entities for intersection checking
 (setq tmpe1 (list
                '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
               '(60 . 1)
               '(62 . 1)
               '(100 . "AcDbPolyline")
               '(90 . 2) '(70 . 0) '(43 . 0.0) '(38 . 0.0) '(39 . 0.0)
               '(10 0.0 0.0) '(40 . 0.0) '(41 . 0.0) '(42 . 0.0) '(10 1.0 1.0)
               '(40 . 0.0) '(41 . 0.0) '(42 . 0.0)
               (cons 210 (acet-geom-cross-product (getvar "ucsxdir") (getvar "ucsydir"))) ;;(210 0.0 0.0 1.0)
              )
       tmpe2 tmpe1
 );setq
 (entmake tmpe1)
 (setq tmpna (entlast)
       tmpe1 (entget tmpna)
 );setq
 (entmake tmpe2)
 (setq tmpna2 (entlast)
        tmpe2 (entget tmpna2)
 );setq
 
 (if (equal fuzz 0.0)
     (setq fuzz #acet-pljoin-prec)
 );if
 
 ;Pljoin checks distances between neighboring points of differing objects
 ;to find the closest candidates for joining. The performance problem is
 ;largely one of minimizing the number of distance calculations that occur.
 ;Here's the approach... Points are placed into a grid where each point
 ;is checked against other points that fall within neighboring grid points.
 ;This operation is similar to drawing in AutoCAD with snap turned on.
 ;Picked points snap to the nearest grid point.
 ;
 ;
 (setq   g (* 2.01 fuzz)              ;grid size
       lst (acet-pljoin-round2 ss g)  ;round points to the grid
                                     ;lst - sub-lists (roundedpoint originalpoint 0/1 ename)
       len (length lst)
         x (/ len 8)
 );setq
 (if (< len 2000) ;for performance reasons if the list is greater than 2000
                  ;point the split the operation into 8 separate chunks
                  ;so they can be processed independantly.
     (setq  len 0
           lst4 lst
            lst nil
     );setq
 );if
 
 (setq n 0)
 (repeat len
 (setq    a (nth n lst)
       lst2 (cons a lst2)
 );setq
 (if (equal n (* x (/ n x)))
     (progn
       (setq lst2 (acet-pljoin-get-matched-pairs2 lst2 ;list of point data lists
                                                 lst3 ;entname map
                                                 fuzz ;fuzz distance
                                                    g ;grid size
                                                   st ;mode
                                                tmpe1 ;temp ent
                                                tmpe2 ;temp ent2
                                                 flst ;pairs that failed a join attempt
                  )
             lst3 (cadr lst2)
             flst (caddr lst2)
             lst2 (car lst2)
       );setq
       (if lst2
           (setq lst4 (append lst4 lst2)
                 lst2 nil
           );setq
       );if
     );progn then
 );if
 (setq n (+ n 1))
 );repeat
 (if lst2
     (setq lst2 (acet-pljoin-get-matched-pairs2 lst2 ;list of point data lists
                                               lst3 ;entname map
                                                fuzz ;fuzz distance
                                                  g ;grid size
                                                 st ;mode
                                              tmpe1 ;temp ent
                                              tmpe2 ;temp ent2
                                               flst ;pairs that failed a join attempt
                )
           lst3 (cadr lst2)
           flst (caddr lst2)
           lst2 (car lst2)
     );setq
 );if
 (if lst2
     (setq lst4 (append lst4 lst2));setq
 );if
 (setq  lst nil
       lst2 nil
 );setq
 
 (while lst4
  (setq  lst4 (acet-pljoin-get-matched-pairs2 lst4 ;list of point data lists
                                             lst3 ;entname map
                                              fuzz ;fuzz distance
                                                g ;grid size
                                               st ;mode
                                            tmpe1 ;temp ent
                                            tmpe2 ;temp ent2
                                             flst ;pairs that failed a join attempt
              )
         lst3 (cadr lst4)
         flst (caddr lst4)
         lst4 (car lst4)
  );setq
 );while
 
 (entdel tmpna)
 (entdel tmpna2)
 
 
);defun acet-pljoin-2nd-pass
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-get-matched-pairs2 ( lst lst5 fuzz g st tmpe1 tmpe2 flst /
                                       na na2 p1 p2 n j a b c d lst2 lst3
                                       id id2 ulst x flst flag flag2 ulst2 flst2
                                       nskip
                                     )
 
;(print "acet-pljoin-get-matched-pairs")
;(print "")
 
(setq n 0)               ;;;create a list of sublist pairs in lst2 i.e. ((0 4) (2 5)...)
(repeat (length lst)     ;;;also create list of non-candidate indexs in lst3
 (cond
  ((setq j (acet-pljoin-get-closest2 (nth n lst) lst fuzz g flst))
   (setq    j (list (nth n lst) j)        ;the point and it's closest candidate
         lst2 (cons j lst2)
   );setq then add this closest match pair
  );cond #1
  (T
   (setq lst3 (cons n lst3)) ;non-candidates
  );cond #2
 );cond close
 (if (equal n (* 20 (/ n 20)))
     (acet-spinner)
 );if
(setq n (+ n 1))
);repeat
 
;Loop through lst2 and look for pairs that point back at each other. i.e. (p1 p2 ...) (p2 p1 ...)
;attempt the join. Track the success of the joins in ulst and the failures in flst.
 
 
(setq nskip 0)
(setq n 0)
(repeat (length lst2)
 (setq     x (nth n lst2)   ;a sublist with the a point and it's 5 closest point buddies.
          id (car x)
         id2 (cadr x)
 );setq
 
 (if (and (not (member id ulst))                   ;both are points not used yet
          (not (member id2 ulst))
          (not (member (list id id2) flst))        ;have not already tried this pair and failed
          (setq b (assoc id2 lst2))
          (equal id (cadr b))                      ;closest pairs point at each other
          (setq  na (last id)                      ;get some of the data out of id and id2
                na2 (last id2)
                 p1 (cadr id)                      ;the real points
                 p2 (cadr id2)
          );setq
          (progn                                   ;get the proper entity names from the ename map lst5
           (while (setq c (assoc na lst5))   (setq na (cadr c)));while
           (while (setq c (assoc na2 lst5))  (setq na2 (cadr c)));while
           T
          );progn
          na                                       ;both entities still exist?
          na2
 
;          (/= 1 (logand 1 (cdr (assoc 70 (entget na)))))
;          (/= 1 (logand 1 (cdr (assoc 70 (entget na2)))))
     );and
     (progn
      ;then attempt a join
      (setq flag nil
            lst5 (acet-pljoin-do-join2 fuzz st na p1 na2 p2 lst5 tmpe1 tmpe2)
            flag (cadr lst5) ;join success?
            lst5 (car lst5)
      );setq return updated entname map and success flag
      (if flag
          (setq ulst (cons id ulst)     ;Then the join succeeded.
                ulst (cons id2 ulst)    ;mark the two as used by adding the them to ulst
          );setq the success
          (setq flst (cons (list id id2) flst)
                flst (cons (list id2 id) flst)
          );setq else join failed so mark as such in flst
      );if
     );progn then
     (progn
      (setq nskip (+ nskip 1));setq
 
      ;(print '(not (member id ulst)))
      ;(print (not (member id ulst)))
      ;(print '(not (member id2 ulst)))
      ;(print (not (member id2 ulst)))
      ;(print '(not (member (list id id2) flst)))
      ;(print (not (member (list id id2) flst)))
      ;(print '(setq b (assoc id2 lst2)))
      ;(print (setq b (assoc id2 lst2)))
      ;(print '(equal id (cadr b)))
      ;(print (equal id (cadr b)))
      ;(print 'na)
      ;(print na)
      ;(print 'na2)
      ;(print na2)
      ;
      ;(d-point (cadr id) "1")
      ;(d-point (cadr id2) "2")
      ;(princ "\ndecided not to try it.")
      ;(getstring "")
      ;(entdel (entlast))
      ;(entdel (entlast))
 
     );progn else
 );if
(setq n (+ n 1))
);repeat
 
(if (equal nskip n)
    (setq lst nil);then all were skipped so the job is finished.
);if
 
(setq lst2 nil);setq ;;;remove the used and non-candidate point data from lst
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
 (if (and (not (member n lst3))    ;not a non-candidate
          (not (member a ulst))    ;not used
     );and
     (setq lst2 (cons a lst2))
 );if
(setq n (+ n 1))
);repeat
 
(list lst2 lst5 flst)
);defun acet-pljoin-get-matched-pairs
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-get-closest2 ( p1 lst fuzz g flst / a b c d x1 x2 x3 y1 y2 y3 n j
                                                     lst2 lst3 len2 len3 clst
                               )
 
;(print "acet-pljoin-get-closest")
;(print "")
 
(setq   b (cadr p1) ;the real point
        a (car p1)  ;the grid point
);setq
 
;determine the grid points to examine.
(cond
 ((equal fuzz 0.0 #acet-pljoin-prec)
  (setq lst2 (list (list (car a) (cadr a))
             );list
  );setq else
 );cond #2
 (T
  (if (<= (car a) (car b))
      (setq x1 (car a)
            x2 (acet-calc-round (+ (car a) g) g)
      );setq
      (setq x1 (acet-calc-round (- (car a) g) g)
            x2 (car a)
     );setq
  );if
  (if (<= (cadr a) (cadr b))
      (setq y1 (cadr a)
            y2 (acet-calc-round (+ (cadr a) g) g)
      );setq
      (setq y1 (acet-calc-round (- (cadr a) g) g)
            y2 (cadr a)
      );setq
  );if
  (setq lst2 (list (list x1 y1)
                   (list x2 y1)
                   (list x2 y2)
                   (list x1 y2)
             );list
  );setq
 );cond #3
);cond close
 
(setq    d (* fuzz 2.0)
      len2 (length lst2)
);setq
;;loop through the grid points and check each of the points that fall on each grid point
(setq n 0)
(while (< n len2)
(setq lst3 (acet-list-m-assoc (nth n lst2) lst) ;get a list of assoc point based on grid point
      len3 (length lst3)
);setq
 
 (setq j 0)
 (while (< j len3)                       ;loop through the current list of grid points
                                         ;and find the closest point
  (setq a (nth j lst3))
  (if (and
           ;@rk 4:13 PM 9/7/98
           ;removed
           ;;;(not (equal (last a) (last p1)))       ;not same entity name
           ;and changed to ...
           (not (equal a p1))
 
           (setq c (distance (cadr p1) (cadr a))) ;distance between real original points
           (<= c fuzz)                            ;less than or equal to fuzz
           (< c d)
           (not (member (list p1 a) flst))
      );and
      (progn
       (setq    d c
             clst a
       );setq
       (if (equal c 0.0 #acet-pljoin-prec)
           (setq n len2
                 j len3
           );setq then jump out of the loop
       );if
      );progn then
  );if
 (setq j (+ j 1))
 );while
 
(setq n (+ n 1))
);while
 
clst
);defun acet-pljoin-get-closest
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-do-join2 ( fuzz st na p1 na2 p2 lst3 tmpe1 tmpe2 / x b e1 e2 flag closed )
 
(if (or (equal st "Add")
        (equal 0.0 (distance p1 p2) #acet-pljoin-prec)
        (and (setq p1 (acet-pljoin-fillet-with-fuzz2 fuzz na p1 tmpe1 na2 p2 tmpe2)
                   p2 (cadr p1)
                   p1 (car p1)
             );setq
             (equal st "Both")
        );and
        (and (equal p1 p2)
             (equal st "Fillet")
        );and
    );or
    (progn
 
     (setq flag T) ;then set the success flag
 
     (if (not (equal p1 p2)) ;;avoid the distance calc. (not (equal 0.0 (distance p1 p2)))
         (progn
          (command "_.pline" p1 p2 "")
          (command "_.pedit" na "_j" na (entlast) "" "_x")
          (if (equal 1 (logand 1 (cdr (assoc 70 (entget na)))))
              (progn
               (if (setq b (assoc na lst3))
                   (setq lst3 (subst (list na nil) b lst3));setq then subst
                   (setq lst3 (cons (list na nil) lst3));setq else add
               );if
               (setq na nil)
              );progn then
          );if
         );progn then
     );if
 
     (cond
      ((not na)
       na
      );cond #1
      ((and (equal na na2)
            (<= (length (acet-geom-vertex-list na)) 2);then it's a single segment polyline so don't change it
       );and
       ;then make the ename inactive by pointing it to nil in the ename map list
       (if (setq b (assoc na2 lst3))
           (setq lst3 (subst (list na2 nil) b lst3));then subst
           (setq lst3 (cons (list na2 nil) lst3));setq else add
       );if
      );cond #2
      (T
       (acet-spinner)
       (command "_.pedit" na "_j" na na2 "" "_x")
       ;The na2 is gone now so update the ename map list so that na2 points at na
       (if (setq b (assoc na2 lst3))
           (setq lst3 (subst (list na2 na) b lst3));then subst
           (setq lst3 (cons (list na2 na) lst3));setq else add
       );if
       (if (or (equal na na2)
               (equal 1 (logand 1 (cdr (assoc 70 (entget na)))))
           );or
           (progn
            ;then na is closed now so update ename map so that it points to nil.
            (if (setq b (assoc na lst3))
                (setq lst3 (subst (list na nil) b lst3));then subst
                (setq lst3 (cons (list na nil) lst3));setq else add
            );if
            (setq na nil)
           );progn then
       );if
      );cond #3
     );cond close
    );progn then add
    (progn
     ;(print '(equal 0.0 (distance p1 p2)))
     ;(print (equal 0.0 (distance p1 p2)))
     ;(print "skipping")
    );progn else
);if
 
(list lst3 flag) ;return the entity name map and a flag of success or failure for join.
);defun acet-pljoin-do-join
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of sub-list of the form (roundedpoint originalpoint 0/1 ename)
;where 0 mean start point and 1 means end point of the object.
;
(defun acet-pljoin-round2 ( ss g / lst na a b c d n )
 
;;(princ "\nCreating data grid of points...")
;;(print "acet-pljoin-round")
(setq n 0)
(repeat (sslength ss)
(setq na (ssname ss n)
       a (acet-pljoin-get-epoints2 na)
       b (cadr a)
       a (car a)
);setq
(if (and a b)
    (setq   c (list (acet-calc-round (car a) g)
                    (acet-calc-round (cadr a) g)
              );list
            d (list (acet-calc-round (car b) g)
                    (acet-calc-round (cadr b) g)
              );list
          lst (cons (list c a 0 na) lst)
          lst (cons (list d b 1 na) lst)
    );setq then
);if
 
(if (equal n (* (/ n 10) 10)) ;update the spinner once every ten objects
    (acet-spinner)
);if
(setq n (+ n 1));setq
);repeat
;(princ "Done.")
 
lst
);defun acet-pljoin-round
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-get-epoints2 ( na / e1 a b z v )
 
;(print "acet-pljoin-get-epoints")
;(print "")
 
 (if (and (setq e1 (entget na))
          (setq e1 (acet-lwpline-remove-duplicate-pnts2 e1))
     );and
     (progn
      (setq z (cdr (assoc 38 e1)));setq
      (if (not z) (setq z 0.0))
      (setq v (cdr (assoc 210 e1))
            a (cdr (assoc 10 e1))
            a (list (car a) (cadr a) z)
            a (trans a v 1)
           e1 (reverse e1)
            b (cdr (assoc 10 e1))
            b (list (car b) (cadr b) z)
            b (trans b v 1)
      );setq
      (setq a (list a b))
     );progn then
 );if;
 
;(print "done epoints")
 
a
);defun acet-pljoin-get-epoints
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes an entity list of lwpolylines and modifies the object
;removing neighboring duplicate points. If no duplicated points
;are found then the object will not be passed to (entmod ).
;Returns the new elist when done.
(defun acet-lwpline-remove-duplicate-pnts2 ( e1 / a n lst e2)
 
(setq n 0)
(repeat (length e1)
(setq a (nth n e1));setq
(cond
 ((not (equal 10 (car a)))
  (setq e2 (cons a e2))
 );cond #1
 ((not (equal (car lst) a))
  (setq lst (cons a lst)
         e2 (cons a e2)
  );setq
 );cond #2
);cond close
(setq n (+ n 1));setq
);repeat
(setq e2 (reverse e2))
(if (and e2
         (not (equal e1 e2))
         lst
    );and
    (progn
     (if (equal 1 (length lst))
         (progn
          (entdel (cdr (assoc -1 e1)))
          (setq e2 nil)
         );progn then single vertex polyline so delete it.
         (progn
          (setq e2 (subst (cons 90 (length lst)) (assoc 90 e2) e2)
          );setq
          (entmod e2)
         );progn else
     );if
    );progn then
);if
 
e2
);defun acet-lwpline-make-remove-duplicate-pnts
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-fillet-with-fuzz2 ( fuzz na p1 tmpe1 na2 p2 tmpe2 /
                                     e1 e2 p1a p2a lst flag flag2 n a
                                     tmpna tmpna2 x y v
                             )
 
;(print "acet-pljoin-fillet-with-fuzz")
;(print "")
 
 
(setq  tmpna (cdr (assoc -1 tmpe1)) ;get the temp entitiy names out of the ent lists
      tmpna2 (cdr (assoc -1 tmpe2))
         lst (acet-pljoin-mod-tmp2 na p1 tmpe1) ;make the temp ent look like the begining or ending segment
          e1 (car lst)                         ;the modified temp ent list
        flag (cadr lst)                        ;0 or 1 start or end
         p1a (caddr lst)                       ;segment info sub-list (p1 p2 bulge) where p2 is always the endpoint
         lst (acet-pljoin-mod-tmp2 na2 p2 tmpe2)
          e2 (car lst)
       flag2 (cadr lst)                          ;0 or 1 start or end
         p2a (caddr lst)                         ;segment info sub-list (p1 p2 bulge) ;in entity ucs
         lst (acet-geom-intersectwith tmpna tmpna2 3) ;get the intersection list
           v (cdr (assoc 210 e1))
         lst (acet-geom-m-trans lst 0 v) ;trans to entity coord system
);setq
 
(if lst
    (progn
     (setq x (acet-pljoin-get-best-int2 p1a lst))            ;get the best intersection
     (setq y (acet-pljoin-get-best-int2 p2a lst))            ;get the best intersection
     ;put the best intersections in the list x
     (cond
      ((and x y)
       (setq x (list x y))
      );cond #1
      ;;(x (setq x (list x))) ;commented because both objects must pass the best intersect test
      ;;(y (setq x (list y)))
      (T (setq x nil))
     );cond
     (if (and x
              (setq x (acet-geom-m-trans x v 1))
              (setq x (acet-pljoin-get-closest-int2 p1 p2 x))
              (<= (distance p1 x) fuzz)
              (<= (distance p2 x) fuzz)
         );and
         (progn
          (acet-pljoin-fillet-mod-epoint2 e1 flag x)
          (if (equal na na2)
              (setq e2 (entget na))
          );if
          (acet-pljoin-fillet-mod-epoint2 e2 flag2 x)
          (setq lst (list x x))
         );progn then
         (setq lst (list p1 p2))
     );if
 
    );progn then
    (setq lst (list p1 p2))
);if
 
lst
);defun acet-pljoin-fillet-with-fuzz
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes:
; a  - segment info sub-list (p1 p2 bulge) where p2 is always the endpoint
; lst - list of intersections
;returns the best candidate
;
(defun acet-pljoin-get-best-int2 ( a lst / p1 p2 a1 a2 n j b c d nb )
 
;(print "acet-pljoin-get-best-int")
;(print "")
 
(setq p1 (car a)   ;the iner segement
      p2 (cadr a)  ;the end point (first or last)
       b (caddr a) ;the bulge
);setq
(if (equal b 0.0)
    (setq a1 (angle p1 p2))                      ;line segment so get the angle
    (setq a1 (caddr (acet-geom-pline-arc-info p1 p2 b))) ;arc segment, so get delta angle from arc_info
);if
(setq n 0)
(repeat (length lst)
(setq a (nth n lst))
(if (equal b 0.0)
    (progn
     ;the it's a line segment
     (if (and (or (equal (angle p1 a) a1 #acet-pljoin-prec)                  (equal (abs (- (angle p1 a) a1))
                         (* 2.0 pi)
                         #acet-pljoin-prec
                  )
              );or
              (or (not d)
                  (< (setq c (distance p2 a)) d)
              );or
         );and
         (progn
          (setq d c
                j n
          );setq
         );progn then
     );if
    );progn then line segment
    (progn
     (if (equal p1 a #acet-pljoin-prec)
         (progn
          (setq a2 (* pi 2.0
                      (/ (abs a1) a1)
                   );mult
          );setq then make it 360 degrees and preserve the sign.
         );progn then
         (progn
          (setq nb (acet-pljoin-calc-new-bulge2 p1 b p2 a)
                a2 (acet-geom-pline-arc-info p1 a nb)
                a2 (caddr a2) ;delta angle
          );setq
         );progn else
     );if
     (setq c (abs (- (abs a2)
                     (abs a1)
                  )
             )
     );setq
     (if (and (>= (* a2 a1) 0.0) ;same sign delta angle
              (or (not d)
                  (< c d)
              );or
         );and
         (progn
          (setq d c
                j n
          );setq
         );progn then
     );if
    );progn else
);if
(setq n (+ n 1));setq
);repeat
(if j
    (setq d (nth j lst))
    (setq d nil)
);if
 
;;;for debuging only
;(d-point p1 "1")
;(d-point p2 "2")
;(if d (d-point d "3"));if
;(print 'p1)
;(print p1)
;(print 'lst)
;(print lst)
;(print d)
;(if d
;    (progn
;     (getstring "\n\nit thinks this is   COOL")
;    );progn then
;    (getstring "\n\nit thinks this   SUCKs")
;);if
;(entdel (entlast))
;(entdel (entlast))
;(if d (entdel (entlast)));if
 
d
);defun acet-pljoin-get-best-int
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-get-closest-int2 ( p1 p2 lst / n a j d )
 
(setq n 0)
(repeat (length lst)
(setq a (nth n lst)
      a (+ (distance a p1) (distance a p2))
);setq
(if (or (not d)
        (< a d)
    );or
    (setq d a
          j n
    );setq
);if
(setq n (+ n 1));setq
);repeat
(if j
    (setq a (nth j lst))
    (setq a nil)
);if
 
a
);defun acet-pljoin-get-closest-int
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-fillet-mod-epoint2 ( e1 flag x / p1 p2 a b e2 blg n v)
 
;(print "acet-pljoin-fillet-mod-epoint")
;(print "")
 
(setq v (cdr (assoc 210 e1))
      x (trans x 1 v)
      ;x (trans x 1 (cdr (assoc -1 e1)))
      x (list (car x) (cadr x))
);setq
 
(if (equal flag 1)
    (setq e1 (reverse e1))
);if
(setq n 0)
(while (and e1
            (not p2)
       );and
 (setq  a (car e1)
       e1 (cdr e1)
       e2 (cons a e2)
 );setq
 (cond
  ((equal 10 (car a))
   (if (not p1)
       (setq p1 n)
       (setq p2 n)
   );if
  );cond #1
  ((and p1
        (equal 42 (car a))
   );and
   (setq b n)
  );cond #2
 );cond close
(setq n (+ n 1))
);while
(setq e2 (reverse e2))
(if (equal 0.0 (cdr (nth b e2)))
    (setq e2 (acet-list-put-nth (cons 10 x) e2 p1));setq then line segment
    (progn
     (if (equal flag 0)
         (setq blg (acet-pljoin-calc-new-bulge2 (cdr (nth p2 e2))
                                              (* -1.0 (cdr (nth b e2)))
                                              (cdr (nth p1 e2))
                                              x
                   )
                e2 (acet-list-put-nth (cons 42 (* -1.0 blg)) e2 b)
                e2 (acet-list-put-nth (cons 10 x) e2 p1)
         );setq then
         (setq blg (acet-pljoin-calc-new-bulge2 (cdr (nth p2 e2))
                                              (cdr (nth b e2))
                                              (cdr (nth p1 e2))
                                              x
                   )
                e2 (acet-list-put-nth (cons 42 blg) e2 b)
                e2 (acet-list-put-nth (cons 10 x) e2 p1)
         );setq then
     );if
    );progn else arc segment
);if
(setq e1 (append e2 e1))
(if (equal flag 1)
    (setq e1 (reverse e1))
);if
(entmod e1)
 
);defun acet-pljoin-fillet-mod-epoint
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Make the temporary ent match the segment of interest to get ready to
;use the intersectwith method.
;Takes an entity name and a point that is on one end of the entity
;and a entity list of a single segment lwpolyline
;modifies the single segment polyline such that it matches the
;first or last segment (depending on the p1 provided) of the
;polyline 'na'
;
(defun acet-pljoin-mod-tmp2 ( na p1 tmpe1 / e1 e2 a b z p2 flag v )
 
(setq    e1 (entget na)
          v (cdr (assoc 210 e1))
         p1 (trans p1 1 v)
         p1 (list (car p1) (cadr p1))
      tmpe1 (subst (assoc 38 e1)  (assoc 38 tmpe1)  tmpe1)
      tmpe1 (subst (assoc 39 e1)  (assoc 39 tmpe1)  tmpe1)
      tmpe1 (subst (assoc 210 e1) (assoc 210 tmpe1) tmpe1)
          z (cdr (assoc 38 e1))
          a (assoc 10 e1)
);setq
 
(if (equal (cdr a) p1 #acet-pljoin-prec)
    (progn
     (setq  flag 0
           tmpe1 (reverse tmpe1)
           tmpe1 (subst a (assoc 10 tmpe1) tmpe1)
           tmpe1 (reverse tmpe1)
              e2 (cdr (member (assoc 10 e1) e1))
              p2 (list (car p1) (cadr p1) z)
              p1 (cdr (assoc 10 e2))
              p1 (list (car p1) (cadr p1) z)
           tmpe1 (subst (assoc 10 e2) (assoc 10 tmpe1) tmpe1)
               b (* -1.0 (cdr (assoc 42 e2)))
           tmpe1 (subst (cons 42 b)
                        (assoc 42 tmpe1)
                        tmpe1
                 )
     );setq
    );progn then
    (progn
     (setq  flag 1
              e2 (reverse e1)
           tmpe1 (reverse tmpe1)
               a (assoc 10 e2)
              p2 (cdr a)
              p2 (list (car p2) (cadr p2) z)
           tmpe1 (subst a (assoc 10 tmpe1) tmpe1)
              e2 (cdr (member a e2))
              p1 (cdr (assoc 10 e2))
              p1 (list (car p1) (cadr p1) z)
               b (cdr (assoc 42 e2))
           tmpe1 (reverse tmpe1)
           tmpe1 (subst (cons 42 b) (assoc 42 tmpe1) tmpe1)
               a (assoc 10 e2)
           tmpe1 (subst (assoc 10 e2) (assoc 10 tmpe1) tmpe1)
     );setq
    );progn else
);if
 
(entmod tmpe1)
 
(list e1 flag (list p1 p2 b))
);defun acet-pljoin-mod-tmp
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculates the new bulge formed by moving
;point p2 to p3 and still retaining the same radius and center point.
;
(defun acet-pljoin-calc-new-bulge2 ( p1 b p2 p3 / p4 x r a c b2 info )
 
(setq c (distance p1 p3))
(if (not (equal c 0.0))
    (progn
     (setq   p4 (acet-geom-midpoint p1 p3)
           info (acet-geom-pline-arc-info p1 p2 b)
              r (cadr info);radius
              x (car info) ;center point
              a (- r
                   (distance x p4)
                )
     );setq
     (setq b2 (/ (* 2.0 a) c)
           b2 (* b2 (/ (abs b) b))
     );setq
     (setq info (acet-geom-pline-arc-info p1 p3 b2))
     (if (not (equal x (car info) #acet-pljoin-prec))
         (progn
          (setq a (- (* r 2.0) a));setq
          (setq b2 (/ (* 2.0 a) c)
                b2 (* b2 (/ (abs b) b))
          );setq
         );progn then
     );if
    );progn then
    (setq b2 0.0)
);if
 
b2
);defun acet-pljoin-calc-new-bulge
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;- explode all curve fitted and/or splined plines and re-join
;- convert all to light weight plines.
;- turn all arcs and lines into lightweight plines.
;- finally return a selection set of all plines.
(defun acet-pljoin-do-ss-pre-work2 ( ss flt / na ss2 ss3 n w)
 
 
(command "_.select" ss "")
(setq ss2 (ssget "_p" '((-4 . "&") (70 . 6)))) ;fit or splined
(command "_.select" ss "")
(setq ss3 (ssget "_p" '((-4 . "<OR") (0 . "LINE") (0 . "ARC") (-4 . "OR>")))) ;lines and arcs
 
(if ss2
    (progn
     (setq n 0)
     (repeat (sslength ss2)
     (setq na (ssname ss2 n)
            w (acet-pljoin-get-width2 na)
     );setq
     (command "_.explode" na)
     (while (wcmatch (getvar "cmdnames") "*EXPLODE*") (command ""))
     (command "_.pedit" (entlast) "_y" "_j" "_p" "")
     (if (not (equal w 0.0))
         (command "_w" w)
     );if
     (command "_x")
     (setq ss (ssdel na ss)
           ss (ssadd (entlast) ss)
     );setq
     (setq n (+ n 1));setq
     );repeat
    );progn then
);if
(command "_.convertpoly" "_light" ss "")
(if ss3
    (progn
     (setq n 0)
     (repeat (sslength ss3)
      (setq na (ssname ss3 n));setq
      (command "_.pedit" na "_y" "_x")
      (setq ss (ssdel na ss)
            ss (ssadd (entlast) ss)
      );setq
     (setq n (+ n 1));setq
    );repeat
   );progn then
);if
(if (equal 0 (sslength ss))
    (setq ss nil)
);if
(setq ss (acet-pljoin-ss-flt2 ss flt))
 
 
ss
);defun acet-pljoin-do-ss-pre-work
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;return the with of the heavy polyline provided in 'na'
(defun acet-pljoin-get-width2 ( na / e1 a b)
 
(if (and (setq e1 (entget na))
         (equal (cdr (assoc 0 e1)) "POLYLINE")
    );and
    (progn
     (setq a (cdr (assoc 40 e1))
           b (cdr (assoc 41 e1))
     );setq
     (while (and (equal a b)
                 (setq na (entnext na))
                 (setq e1 (entget na))
                 (not (equal (cdr (assoc 0 e1)) "SEQEND"))
            );and
      (setq a (cdr (assoc 40 e1))
            b (cdr (assoc 41 e1))
      );setq
     );while
    );progn then
    (setq a 0.0)
);if
a
);defun acet-pljoin-get-width
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-ss-flt2 ( ss flt / n na e1 p1 p2 )
(if (and ss
         (> (sslength ss) 0)
    );and
    (progn
     (command "_.select" ss "")
     (setq ss (ssget "_p" flt))
    );progn then
    (setq ss nil)
);if
 
ss
);defun acet-pljoin-ss-flt
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;prompt for a joinmode setting of "Fillet" or "Add"
(defun acet-pljoinmode2 ( / st )
  (acet-pljoin-init-mode2)
  (initget "Fillet Add Both _Fillet Add Both")
  (setq st (getkword
            (acet-str-format "\nEnter join type [Fillet/Add/Both] <%1>: " #acet-pljoinmode)
           );getkword
  );setq
  (if st
      (progn
       (setq #acet-pljoinmode st)
       (acet-setvar (list "ACET-PLJOINMODE" #acet-pljoinmode 2))
      );progn
  );if
);defun acet-pljoinmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pljoin-init-mode2 ()
 (if (not #acet-pljoinmode)
     (setq #acet-pljoinmode (acet-getvar '("ACET-PLJOINMODE" 2)))
 );if
 (if (not #acet-pljoinmode)
     (progn
      (setq #acet-pljoinmode "Both")
      (acet-setvar (list "ACET-PLJOINMODE" #acet-pljoinmode 2))
     );progn then
 );if
 #acet-pljoinmode
);defun acet-pljoin-init-mode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;prompt for fuzz distance and/or pljoinmode setting.
;return list... (fuzz pljoinmode)
;
(defun acet-pljoin-get-fuzz-and-mode2 ( / st fuzz )
  (setq st (acet-pljoin-init-mode2))
  (princ (acet-str-format "\n Join Type = %1" st))
  (if (equal "Both" st)
      (princ " (Fillet and Add) ")
  );if
  (if (not #acet-pljoin-fuzz)
      (setq #acet-pljoin-fuzz 0.0)
  );if
  (if (assoc "OSMODE" (car acet:sysvar-list))
      (setvar "OSMODE" (cadr (assoc "OSMODE" (car acet:sysvar-list))))
  );if
  (setq fuzz "")
  (while (equal (type fuzz) 'STR)
   (initget "Jointype _Jointype" 4)
   (setq fuzz (getdist
                (acet-str-format "\nEnter fuzz distance or [Jointype] <%1>: " (rtos #acet-pljoin-fuzz))
              );getdist
   );setq
   (cond
    ((not fuzz)
     (setq fuzz #acet-pljoin-fuzz)
    );cond #1
    ((equal "Jointype" fuzz)
     (acet-pljoinmode2)
    );cond #2
    ((equal (type fuzz) 'REAL)
     (setq #acet-pljoin-fuzz fuzz)
    );cond #3
   );cond close
  );while
  (setvar "osmode" 0)
 
  (list #acet-pljoin-fuzz #acet-pljoinmode)
);defun acet-pljoin-get-fuzz-and-mode


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCfWuo4GZOrumg9Q3oMkiLs6imHRa3PzmIxBnio
;;; pJMSwhxvOLN4qDdBqsQA9E+CNiDL6YY520vJmj1XlT+lVLP+4dv+XyQJVW7zy32t
;;; CT3TArUkM4ucFYpc9a/mTg64GXMY4NmeiTKjafq6lsb72ZnKYINFhbVTWW8MwjPG
;;; yq5GQLwZbKGS2BnDvCOuWqxYPYOZZbdWBD9ABiDmNEV9LWRtK7dPv71GqF1OXbJ5
;;; OjRKoGD96MQvaWKl4U1Q5zUs8PU0mR2D2QyoFpJjyVMaOoFQHiPeDnLpporxjn2L
;;; 8lKMdqrZJQgEpnY7W+rxlExKnLYDBfxhRhiYrpbXVsOyWXZOqNune66qgvdaXQD/
;;; 0BeEiNBI9jyjJfsK9pvX+hg5ID39esVfkz/9W8+ANWk9h8fZLZ/jH9GiuzpQQh1v
;;; 7Hc8K9N0PTuTuBa6mVv0sCAmaNH/V3c7oLhpCJcF+l8YKpHs5dDJ/Vk7DfkhQPef
;;; kcSZa/GPaXrsmS0zbIBUkCeyoqOrqL9TwbLPlAv4jjVOmBGmmnGzW2ko2ddGZsHy
;;; ORik6fOg6w2k80V1wMdphPM18B4BLX1fqfDrXeJPAxOJZ9JNSSrzAI4DR/Lky+Ca
;;; wanPF/I9KZuzN2XiBF+8VzcackZ+m9XBcMGV+XWD3rAMxBmA92PtLy9uYt2U4aaX
;;; clHcwg==
;;; -----END-SIGNATURE-----