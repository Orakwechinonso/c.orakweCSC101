;;;
;;;    CLIPIT.LSP
;;;  Copyright 2022 Autodesk, Inc.  All rights reserved.
;;;
;;;  Use of this software is subject to the terms of the Autodesk license 
;;;  agreement provided at the time of installation or download, or which 
;;;  otherwise accompanies this software in either electronic or hard copy form.   
;;;
;;;  ----------------------------------------------------------------
 
(defun c:clipit ( / na na2 e1 e2 a lst zflag redraw_it glst vplocked vpna )
 
(acet-error-init (list
                   (list   "cmdecho" 0
                         "highlight" 0
                         "regenmode" 1
                            "osmode" 0
                         "orthomode" 0
                         "ucsfollow" 0
                          "limcheck" 0
                          "plinewid" 0
                   )
                   T     ;flag. True means use undo for error clean up.
                  '(while redraw_it
                     (redraw (car redraw_it) 4)
                     (setq redraw_it (cdr redraw_it))
                   )
                  );list
);acet-error-init
 
(setq glst (bns_groups_unsel));setq
 
(princ "\nPick a POLYLINE, CIRCLE, ARC, ELLIPSE, or TEXT object for clipping edge...")
(setq na (acet-ui-single-select '((-4 . "<OR")
                           (0 . "*POLYLINE")
                           (0 . "ARC")
                           (0 . "CIRCLE")
                           (0 . "ATTDEF")
                           (0 . "TEXT")
                           (0 . "MTEXT")
                           (0 . "ELLIPSE")
                           (0 . "SPLINE")
                           (0 . "3DFACE")
                           (0 . "SOLID")
                           (0 . "TRACE")
                          (-4 . "OR>")
                         )
                         T ;enable locked layer selection for boundary
         );acet-ui-single-select
);setq
(if na
    (progn
 
     (setq e1 (entget na))
     (setq redraw_it (list na))
     (redraw na 3)
     (princ "\nPick an IMAGE, a WIPEOUT, or an XREF/BLOCK to clip... ")
     (setq na2 (acet-ui-single-select '((-4 . "<OR")
                                (0 . "IMAGE")
                                (0 . "INSERT")
                                (0 . "WIPEOUT")
                                (-4 . "OR>")
                               )
                               nil ;dis-allow locked layer selection for object to be clipped
               );acet-ui-single-select
     );setq
     (setq redraw_it nil)
     (redraw na 4)
  (if na2
      (progn
       (setq e2 (entget na2))
       (setvar "cmdecho" 0)
       (setvar "highlight" 1)
       (command "_.select" na na2)
 
       (setq a (get_clipitres))
       (command "")
 
       (entupd na)
       (setq lst (acet-geom-object-point-list na a))
       (if (or (not (equal "INSERT" (acet-dxf 0 e2)))
               (not (acet-geom-self-intersect lst nil))
           );or
           (progn
            (if (equal (car lst) (last lst) 0.001)
                (setq lst (cdr lst))
            );if
            (if (or (equal "INSERT" (acet-dxf 0 e2))
                    (equal "IMAGE"  (acet-dxf 0 e2))
                );or
                (progn
                 (if (setq vpna (acet-currentviewport-ename))
                     (setq vplocked (acet-viewport-lock-set vpna nil))
                 );if
                 (if (setq zflag (acet-geom-zoom-for-select lst))
                     (command "_.zoom" "_w" (car zflag) (cadr zflag))
                 );if
                 (c_clipit na2 lst)
                 (if zflag (command "_.zoom" "_p"))
                 (if vplocked
                     (acet-viewport-lock-set vpna T) ;relock the viewport
                 );if
                );progn then
                (wipeout_clipit na2 lst)
            );if
           );progn then valid boundary selected
           (princ "\nInvalid. Bounding entity cannot self intersect for xclip.")
       );if
      );progn then got na2
  );if
   );progn then got na
);if
 
(bns_groups_sel glst)
(acet-error-restore)
(princ)
);defun c:clipit
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_clipitres ( / a n)
 
(if (not #clipitres)
    (progn
     (setq #clipitres 0.0);setq
     (setq n 0)
     (while (equal #clipitres 0.0)
      (setq #clipitres (acet-geom-pixel-unit)
            #clipitres (rtos #clipitres 2
                             (+ (fix (/ (getvar "luprec") 2.0)) n)
                       )
            #clipitres (atof #clipitres)
      );setq
      (setq n (+ n 1));setq
     );while
    );progn then
);if
(initget 2)
(setq  a (getdist
           (acet-str-format "\nEnter maximum allowable error distance for resolution of arc segments <%1>: "
                   (rtos #clipitres))
         );getdist
);setq
(if (not a)
    (setq a #clipitres)
    (setq #clipitres a)
);if
 
#clipitres
);defun get_clipitres
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c_clipit ( na lst / e1 n lst2 lst3 res)
 
 
(setq lst (plist_optimize lst));setq
 
 
(if (equal 'PICKSET (type na))
    (setq e1 (entget (ssname na 0)));setq then it's a selection set
    (setq e1 (entget na));setq else
);if
(if (equal "IMAGE" (cdr (assoc 0 e1)))
    (progn
 
     (setq lst2 (acet-geom-image-bounds na)
           lst3 (pre_proccess_boundary lst lst2)
     );setq
 
 
     (setq n 0);setq
     (repeat (length lst3)
     (setq lst (nth n lst3))
 
 
     (setq res (c_clipit_sub na lst))
     (command "_.copy" na "" "0,0,0" "0,0,0")
     (setq na (entlast))
     (setq n (+ n 1));setq
     );repeat
     (entdel na)
    );progn
    (setq res (c_clipit_sub na lst))
);if
 
res
);defun c_clipit
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c_clipit_sub ( na lst / e1 lst2 a n )
 
;(setvar "cecolor" "1")
;(pline (list lst))
;(if (equal (type na) (type (entlast)))
;    (ss_visible (ssadd na (ssadd)) 0)
;    (ss_visible na 0)
;);if
;(if (equal "" (getstring "hey"))
;    (entdel (entlast))
;);if
 
 (if (equal 'PICKSET (type na))
     (setq e1 (entget (ssname na 0)));setq then it's a selection set
     (setq e1 (entget na));setq else
 );if
 (if (equal "INSERT" (cdr (assoc 0 e1)))
     (progn
      (command "_.xclip" na "" "_n")
      (if (and (setq e1 (member '(102 . "{ACAD_XDICTIONARY") e1))
               (setq e1 (cdr e1))
               (setq na (cdr (assoc 360 e1)))
               (setq e1 (entget na))
               (assoc 360 (member '(3 . "ACAD_FILTER") e1))
          );and
          (command "_y") ;delete previous clipping boundary
      );if
     );progn then
     (progn
      (setq lst2 (acet-geom-image-bounds na)
            ;lst3 (pre_proccess_boundary lst lst2) ;12:34 PM 8/18/97
      );setq
      (setq  ;lst lst3 ;12:34 PM 8/18/97
            lst2 nil
            ;lst3 nil
      );setq
      (command "_.imageclip" na "_n")
      (if (equal 1 (cdr (assoc 280 e1)))
          (command "_y")
      );if
     );progn else
 );if
 (command "_p")
 (setq n 0);setq
 (repeat (length lst)
  (setq a (nth n lst))
  (if (and ;(not (member a lst2))
           (or (not lst2)
               (not (equal (nth n lst) (last lst2) 0.000001))
           );or
      );and
      (progn
       (command a)
       (setq lst2 (append lst2 (list a)))
      );progn
  );if
  (setq n (+ n 1));setq
 );repeat
 
 
 (if (< (length lst2) 3)
     (command nil) ;just cancel out
     (command "")
 );if
 
(>= (length lst2) 3);return success or failure
);defun c_clipit_sub
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pre_proccess_boundary ( lst lst2 / sp_inters a lst3 dst p1 p2 p3 p4
                                          n b side d c lst4 cnt lst5 lst6 j
                                          dx dy
                             )
 
 (defun sp_inters ( a b c d / x )
;(setvar "cecolor" "1")
;(command "_.line" a b "")
;(command "_.line" c d "")
 
  (if (setq x (inters a b c d))
      (setq x (list x))
  );if
 
;(print x)
;(getstring "whatup")
;(entdel (entlast))
;(entdel (entlast))
 
  x
 );defun sp_inters
 
(if (not (equal (car lst) (last lst) 0.0001))
    (setq lst (append lst (list (car lst))));setq then
);if
(setq    a (car lst)   ;The first point of the first segment to check
      lst3 (list a)    ;
       dst (list a)    ;dst will hold the max and min points of lst later
        p1 (nth 0 lst2);the rectang bounds
        p2 (nth 1 lst2)
        p3 (nth 2 lst2)
        p4 (nth 3 lst2)
);setq
(setq n 1);setq
(repeat (max 0 (- (length lst) 1))
(setq    b (nth n lst)
       dst (acet-clipit-list-extents (append dst (list b))) ;max and min points of lst
      side 0
);setq
 (setq d nil)
 (if (setq c (sp_inters a b p1 p2)) ;check the a-b segment against the boundary rectang
     (setq    d (append d c)
           side (+ side 1)
     );setq
 );if
 (if (setq c (sp_inters a b p2 p3))
     (setq    d (append d c)
           side (+ side 2)
     );setq
 );if
 (if (setq c (sp_inters a b p3 p4))
     (setq    d (append d c)
           side (+ side 4)
     );setq
 );if
 (if (setq c (sp_inters a b p4 p1))
     (setq    d (append d c)
           side (+ side 8)
     );setq
 );if
;(print c)
;(getstring "hey")
(if d
    (progn
     (if (> (length d) 1)
         (progn
     
          (if ;(equal (length d) 4) ;rk commented out and added line below 7:41 PM 8/27/97
              (> (length d) 2)
              (progn
               (setq d (list (car d) (caddr d)));setq
              );progn
          );if
          ;find the closest intersection and append that one first
          (if (< (distance (car d) a) (distance (cadr d) a))
              (setq lst3 (append lst3 d))
              (setq lst3 (append lst3 (reverse d)))
          );if
     
          ;lst3 (a list of the resulting boundary points)
          ;lst4 is a list of sublists. Each sub-list has the form: (index, flag, side)
          ;where index is an index to lst3
          ;and flag is 0 or 1 meaning enter and exit respectively.
          ;side is a bit coded value. The sum of the bits where:
          ; crossing segment 1 is bit 1
          ; crossing segment 2 is bit 2
          ; crossing segment 3 is bit 4
          ; crossing segment 4 is bit 8
          (if (and
                (or
                    (not (in_bounds a lst2))
                    (equal a (car d) 0.0001)
                    (equal a (cadr d) 0.0001)
                );or
                (not (equal 0 (cadr (last lst4))))
              );and
              (setq lst4 (append lst4
                                 (list (list (- (length lst3) 2)
                                             0                    ;enter
                                             side                 ;
                                       );list
                                 );list
                         );append
              );setq
          );if
          (if (and (not (in_bounds b lst2))
                   (not (equal 1 (cadr (last lst4))))
              );and
              (setq lst4 (append lst4
                                 (list (list (- (length lst3) 1)
                                             1                  ;exit
                                             side
                                       );list
                                 );list
                         );append
              );setq
          );if
         );progn then passing through the rectang bounds with segment a-b
         (progn
          (setq d (car d));setq
        
          (setq lst3 (append lst3 (list d)));setq
          (if (and (in_bounds b lst2)
                   (or
                       (not (in_bounds a lst2))
                       (equal a d 0.0001)
                   );or
                   (not (equal 0 (cadr (last lst4))))
              );and
              (progn
               ;(print "enter")
               (setq lst4 (append lst4
                                  (list (list (- (length lst3) 1)
                                              0           ;entering
                                              side
                                        );list
                                  );list
                          );append
               );setq
              );progn then entering
              (progn
               (if (and (in_bounds a lst2)
                        (not (in_bounds b lst2))
                        (not (equal 1 (cadr (last lst4))))
                   );and
                   (progn
                    ;(print "exit")
                    (setq lst4 (append lst4
                                       (list (list (- (length lst3) 1)
                                                   1                 ;exiting
                                                   side
                                             );list
                                       );list
                               );append
                    );setq
                   );progn then exiting
               );if
              );progn else possibly exiting
          );if
         );progn else
     );if
    );progn then we are entering or exiting the rectang boundary
);if
;(print b)
;(getstring "yo")
(if (not (equal b (last lst3) 0.0001))
    (setq lst3 (append lst3 (list b)));setq
);if
 
(setq a b)
(setq n (+ n 1));setq
);repeat
 
;(print lst3)
;(print lst4)
;(getstring "heydude")
 
(setq dst (* 2.0 (distance (car dst) (cadr dst))));setq
(if lst4
    (progn ;then possibly need to add corners of image to clip boundary loop
 
     (if (not (equal 1 (cadr (car lst4))))
         (setq lst4 (append (cdr lst4) (list (car lst4))));setq
     );if
     (if (not (equal (length lst4) (* 2 (/ (length lst4) 2)))) ;odd number of exits/entries
         (progn
          (setq lst4 (reverse (cdr (reverse lst4))));setq
         );progn then remove the last one.
     );if
 
     (setq n 0)
     (repeat (/ (length lst4) 2)
     (setq   b (nth n lst4)
             c (nth (+ n 1) lst4)
           cnt (length lst5)
     );setq
     (setq lst5 (append lst5
                        (add_corners (car b) (car c) lst3 lst2)
                );append
     );setq
 
     (while (< (+ cnt 1) (length lst5))
      (setq lst6 (append lst6
                         (list cnt);list
                 );append
      );setq
     (setq cnt (+ cnt 1))
     );while save a list of index's to lst5 to use for boundary overlap checking later.
 
     (setq j (car c));setq
     (if (equal (+ n 2) (length lst4))
         (setq c (car lst4))
         (setq c (nth (+ n 2) lst4));setq
     );if
     (while (/= j (car c))
      (if (not (equal (last lst5) (nth j lst3) 0.0001))
          (setq lst5 (append lst5 (list (nth j lst3))));setq
      );if
      (setq j (+ j 1));setq
      (if (equal j (length lst3)) (setq j 0))
     );while
 
     (setq n (+ n 2));setq
     );repeat
    );progn then lst4
);if
 
;(print lst5)
(if lst5
    (progn
;(getstring "1")
     (setq dx 0.0
           dy 0.0
     );setq
     (setq n 0)
     (while (and (< (+ n 1) (length lst5))  ;make sure lst5 has non-zero extents
                 (or (equal dx 0.0 0.00001)
                     (equal dy 0.0 0.00001)
                 );or
            );and
     (setq dx (+ (abs (- (car (nth (+ n 1) lst5))
                         (car (nth n lst5))
                      );minus
                 );abs
                 dx
              );plus
           dy (+ (abs (- (cadr (nth (+ n 1) lst5))
                         (cadr (nth n lst5))
                      );minus
                 );abs
                 dy
              );plus
     );setq
     (setq n (+ n 1))
     );while
     (if (or (equal dx 0.0 0.00001)
             (equal dy 0.0 0.00001)
         );or
         (setq lst3 nil
               lst5 nil
         );setq
         (progn
          (setq lst3 lst5
                lst5 nil
          );setq
          (if (and lst3
                   lst6
              );and
              (progn
               (setq lst3 (split_at_pinch2 lst3 lst6))
              );progn
              (progn
               (if lst3
                   (setq lst3 (list lst3));setq
               );if
              );progn then
          );if
         );progn else lst5 has non-zero extents
     );if
 
    );progn
    (progn
;(getstring "2")
     (if (not (in_bounds (car lst3) lst2))
         (progn
          (if (or (acet-geom-point-inside (nth 0 lst2) lst3 dst)
                  (acet-geom-point-inside (nth 1 lst2) lst3 dst)
                  (acet-geom-point-inside (nth 2 lst2) lst3 dst)
                  (acet-geom-point-inside (nth 3 lst2) lst3 dst)
              );or
              (setq lst3 lst2);then clip boundary totally encloses the image bounds
                              ;so use image_bounds as clip boundary
              (setq lst3 nil);else clip boundary has no overlap with image so return nil
          );if
         );progn then clip boundary is not within the image_bounds
     );if
     (if lst3
         (setq lst3 (list lst3));setq
     );if
    );progn else either totally inside or totally outside with no intersections
);if
 
 
lst3
);defun pre_proccess_boundary
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split_at_pinch2 ( lst lst2 / n k a b x1 flag j m c d x2 x3 lst3 lst4 lst5
                       )
 
(setq n 0)
(repeat (max (- (length lst2) 1) 0)
(setq    k (nth n lst2)
         a (nth k lst)
         b (nth (+ k 1) lst)
        x1 (acet-clipit-list-extents (list a b))
);setq
 (setq flag nil)
 (setq j (+ n 1))
 (while (and (not flag)
             (< j (length lst2))
        );and
 (setq m (nth j lst2)
       c (nth m lst)
       d (nth (+ m 1) lst)
      x2 (acet-clipit-list-extents (list c d))
      x3 (acet-clipit-list-extents (append x1 x2))
 );setq
 (if (and (or (and (equal (car x3) (car x1) 0.0001)
                   (equal (cadr x3) (cadr x1) 0.0001)
              );and
              (and (equal (car x3) (car x2) 0.0001)
                   (equal (cadr x3) (cadr x2) 0.0001)
              );and
          );or
          (equal 0.0 (acet-geom-vector-side c a b))
          (equal 0.0 (acet-geom-vector-side d a b))
     );and
     (setq lst3 (append lst3
                        (list (list k m))
                );append
     );setq then
 );if
 (setq j (+ j 1));setq
 );while
 
(setq n (+ n 1));setq
);repeat
 
(setq n 0);setq
(repeat (max 0 (- (length lst3) 1))
(setq a (nth n lst3)
      b (cadr a)
      a (car a)
      c (nth (+ n 1) lst3)
      d (cadr c)
      c (car c)
);setq
(setq lst4 (append lst4
                   (list (list (+ a 1)
                               (if (not (equal (min b c) a))
                                   (min b c)
                                   b
                               );if
                         );list
                   );list
           );append
);setq
(setq n (+ n 1));setq
);repeat
(if lst3
    (progn
     (setq a (nth n lst3)
           b (cadr a)
           a (car a)
           c (car lst3)
           d (cadr c)
           c (car c)
     );setq
     (setq lst4 (append lst4
                        (list (list (max (+ a 1)
                                         (if lst4
                                             (+ (cadr (last lst4)) 1)
                                             0
                                         );if
                                    );max
                                    b
                              );list
                        );list
                        (list (list (+ b 1)
                                    c
                              );list
                        );list
                );append
     );setq
    );progn then
);if
 
(if (and lst3 lst4)
    (progn
     (setq n 0)
     (repeat (length lst4)
     (setq a (nth n lst4)
           j (car a)
     );setq
     (setq lst3 nil)
     (while (not (equal j (cadr a)))
      (setq lst3 (append lst3 (list (nth j lst))));setq
     (setq j (+ j 1))
     (if (= j (length lst))
         (setq j 0)
     );if
     );while
     (setq lst3 (append lst3 (list (nth j lst))));setq
     (if (> (length lst3) 2)
         (progn
          (if (> (length lst3) 3)
              (progn
               (if (not (equal (car lst3) (last lst3)))
                   (setq lst3 (append lst3 (list (car lst3))));setq
               );if
               (setq lst3 (split_at_pinch2 lst3    ;a little recursion excursion.
                                           (list 0 (- (length lst3) 2))
                          );split_at_pinch2
               );setq
              );progn then
              (setq lst3 (list lst3))
          );if
          (setq lst5 (append lst5 lst3));setq
          ;(setq lst5 (append lst5 (list lst3)));setq
 
         );progn
     );if
     (setq n (+ n 1));setq
     );repeat
     (if lst5
         (setq lst lst5)
         (setq lst (list lst))
     );if
    );progn then
    (setq lst (list lst));setq else
);if
 
lst
);defun split_at_pinch2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add_corners ( nn jj lst3 lst2 / mxmn len n a b lst lst4
                                       ;func flag flag2 mxmn len lst lst4
                                       ;n a b d1 d2 d3 d4
                   )
 
 
(setq mxmn (list (nth nn lst3))
       len (length lst3)
);setq
 
(setq n nn)
(while (not (equal n jj));build a list that's a loop of points from exit to entry point
(setq    a (nth n lst3)
      mxmn (acet-clipit-list-extents (append (list a) mxmn))
);setq
(if (not (equal a (last lst) 0.0001))
    (setq lst (append lst (list a)));setq
);if
(setq n (+ n 1))
(if (= n len) (setq n 0));if
);while
(setq    a (nth n lst3)
      mxmn (acet-clipit-list-extents (append (list a) mxmn))
       lst (append lst (list a))
);setq
 
(setq mxmn (* 4.0 (distance (car mxmn) (cadr mxmn)))
         b mxmn
);setq
(if (not (equal (car lst) (last lst) 0.0001)) ;make sure the loop is fully closed
    (setq lst (append lst (list (car lst))));setq then
);if
(setq lst2 (cdr lst2))
(setq n 0)
(repeat (length lst2)
(setq a (nth n lst2))
(if (not (acet-geom-point-inside a lst mxmn))
    (setq a nil)
);if
(setq lst4 (append lst4 (list a)))
(setq n (+ n 1));setq
);repeat
(setq b (list b (car lst4)))
(if (member nil lst4)
    (progn
     (while (car lst4)
      (setq lst4 (append (cdr lst4) (list (car lst4))))
     );while
     (while (and lst4
                 (not (car lst4))
            );and
      (setq lst4 (cdr lst4))
     );while
     (if (not (last lst4))
         (progn
          (setq lst4 (reverse lst4))
          (while (and lst4
                      (not (car lst4))
                 );and
           (setq lst4 (cdr lst4))
          );while
          (setq lst4 (reverse lst4))
         );progn
     );if
    );progn
    (progn
     (if (equal (length lst4) 4)
         (progn
          (while (not (equal 0
                             (acet-geom-vector-side (nth nn lst3) (car lst4) (last lst4))
                 )    )
           (setq lst4 (append (cdr lst4) (list (car lst4))));setq
          );while
         );progn
     );if
    );progn
);if
 
(if lst4
    (progn
     (if (< (distance (nth nn lst3) (last lst4))
            (distance (nth nn lst3) (car lst4))
         )
         (setq lst4 (reverse lst4))
     );if
     (if (and (< (distance (nth jj lst3) (car lst4))
                 (distance (nth nn lst3) (car lst4))
              )
              (< (distance (nth jj lst3) (car lst4))
                 (distance (nth jj lst3) (last lst4))
              )
         );and
         (setq lst4 (reverse lst4))
     );if
    );progn then
);if
(if (not (equal (nth nn lst3) (car lst4) 0.0001))
    (setq lst4 (append (list (nth nn lst3))
                       lst4
               );append
    );setq
);if
(if (not (equal (nth jj lst3) (last lst4) 0.0001))
    (setq lst4 (append lst4
                       (list (nth jj lst3))
               );append
    );setq
);if
 
lst4
);defun add_corners
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun in_bounds ( p1 lst / a b c)
 (setq b (car lst)
       b (list (car b) (cadr b))
       c (caddr lst)
       c (list (car c) (cadr c))
       a (angle b (cadr lst))
 );setq
 (if (< a 0) (setq a (+ a (* 2.0 pi))))
 (setq  a (* -1.0 a)
        c (acet-geom-point-rotate c b a)
       p1 (acet-geom-point-rotate p1 b a)
      lst (acet-clipit-list-extents (list b c p1))
 );setq
 (and (equal (car lst) b 0.000001)
      (equal (cadr lst) c 0.000001)
 );and
);defun in_bounds
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wipeout_clipit ( na lst / na2 la n a)
(if na
    (entdel na)
);if
(if (setq la (acet-layer-locked (getvar "clayer")))
    (command "_.layer" "_un" (getvar "clayer") "")
);if
 
(command "_.pline")
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
(command a)
(setq n (+ n 1));setq
);repeat
(command "")
 
(setq na2 (entlast))
(command "_.pedit" na2 "_cl" "_x")
(command "_.wipeout" "_p" na2 "_y")
(if la
    (command "_.layer" "_lock" (getvar "clayer") "")
);if
);defun wipeout_clipit
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;take a list of point ans removes duplicated points and unneeded points
;as a result of no angle change.
(defun plist_optimize ( lst / n a b c d lst2)
 
(if (not (equal (car lst) (last lst) 0.00001))
    (setq lst (append lst (list (car lst))));setq then
);if
(setq n 0)
(repeat (max 0
             (- (length lst) 1)
        )
(setq a (nth n lst)
      b (nth (+ n 1) lst)
);setq
(if (equal n 0)
    (setq lst2 (list a b))
);if
(setq c (nth (max (- (length lst2) 2) 0) lst2)
      d (last lst2)
);setq
 (if (equal (angle a b)
            (angle c d)
            0.000001
     );equal
     (setq lst2 (reverse (cdr (reverse lst2))));setq
 );if
 (if (not (equal b (last lst2)))
     (setq lst2 (append lst2 (list b)));setq
 );if
(setq n (+ n 1));setq
);repeat
(if (and (>= (length lst2) 4)
         (equal (angle (car lst2) (cadr lst2))
                (angle (nth (- (length lst2) 2) lst2) (last lst2))
                0.000001
         );equal
    );and
    (setq lst2 (cdr lst2)
          lst2 (reverse (cdr (reverse lst2)))
    );setq
);if
 
lst2
);defun plist_optimize
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-clipit-list-extents ( lst / a b )
  (setq lst (acet-geom-list-extents lst)
          a (car  lst)
          b (cadr lst)
  )
  (list (list (car a) (cadr a))
        (list (car b) (cadr b))
  );list
);defun acet-clipit-list-extents


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCUQHKTVjCku3q/xTOd+JAbxB0yabYVCW81FV1m
;;; 0pzQrmaFSNxrDfMiqXufLrjdaW404ao8alaVDIn4LskgVlE+cWcR01Q3jvzkWhPH
;;; 6dLWzUBEa0U77ksy5zzkS8laXctCEXFEJuEvPRC5TJJBIWwVHEMG+usx9hj/emlg
;;; rUFD/uiSXDMmr9JMnA7QZ3iKZiiyJXf5b+UpqEeichx0h31Xx7JRiDB81MIh3GA3
;;; AMLQGr0GorvVver0RjHcBfEE68HYCpaduzfcui0e/HXpm02lv7hEYvo6thzjcFeO
;;; FaZLmV8BLn6URkg+88VTMJEMAlU9WfbO7XjFiP+TF34LELfdAs1VPZjAUCFkbPFX
;;; QZX7iP7OVkz4u36A3pPTLW9MI0By2WZecYudEMry6ZtMX3NCLnh7ZA6PDCEtOJ3A
;;; G1B5bToUXAaIbTtmtuIdHDnt327WbtMk+JkNRy/OdNiYSMjG6Dv7ukFYr+5KTBUb
;;; tnAkr/k3limrKG/Lwu5XCk4KBz/uRccW04yfjI/g0RjsqrDs485Rg/fESEGQR6UZ
;;; 4tOBz5KI1gIOTPHHbAbLtOBCh7rKuXbgs8s8mJDqwMmdqfhXZOHWrQuooDALPU+Z
;;; yjGo8nLz2UhCjnm90izkB1rRhDOY54K68Z1GdROQB+ayWbLvETB/m38YkU8ur3La
;;; k6g1CA==
;;; -----END-SIGNATURE-----