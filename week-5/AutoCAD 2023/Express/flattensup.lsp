;;
;;  FlattenSup.lsp
;;
;;  Copyright © 1999-2011 by Autodesk, Inc.
;;
;;  Your use of this software is governed by the terms and conditions
;;  of the License Agreement you accepted prior to installation of this
;;  software.  Please note that pursuant to the License Agreement for this
;;  software, "[c]opying of this computer program or its documentation
;;  except as permitted by this License is copyright infringement under
;;  the laws of your country.  If you copy this computer program without
;;  permission of Autodesk, you are violating the law."
;;
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;  DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;  UNINTERRUPTED OR ERROR FREE.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a selection set of objects and create flat versions.
;Returns a seletion set of new objects
;
(defun acet-flatn ( ss hide / na n na2 lok ss ss2 wmf:ss )
 (acet-sysvar-set
   (list      "cmdecho" 0 
            "highlight" 0
              "cecolor" "bylayer"
              "celtype" "bylayer"
            "celweight" -1
            "thickness" 0.0
               "osmode" 0
            "pickstyle" 0
            "ucsfollow" 0
              "ucsicon" 0
               "attreq" 0
             "thumbsave" nil
             "gridmode" 0
             "peditaccept" 0
   );list
 )
 (setq lok (acet-layer-unlock-all))
 (setq na2 (entlast))
 
 (acet-ucs-cmd (list "_view"))
 (if (or hide 
         (= (getvar "viewmode") 1) ;perspective
     );or
     (progn
      (if hide
          (acet-wmf-convert (list ss 2 nil nil)) ;; specify hide
          (acet-wmf-convert (list ss 1 nil T)) ;; just update with a move
      );if
 
     );progn then wmf
     (progn
      (setq wmf:ss (acet-flatn-objects (list ss nil nil))
            wmf:ss (car wmf:ss)
      );setq
 
      (if wmf:ss
          (acet-wmf-convert (list wmf:ss 3 nil T)) ;; wmf with scale to viewport update option
      );if
     );progn else do standard flatten
 );if
 
 (acet-ucs-cmd (list "_p"))
 
 (command "_.erase" ss "")
 (setq ss (acet-ss-new na2))
 (command "_.select" ss "")
 
 (if lok
     (command "_.-layer" "_lock" lok "")
 );if
 (if hide
     (command "_.regen")
 );if
 (acet-sysvar-restore)
 
 ss
);defun acet-flatn
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Flattens a selection set of objects
;Takes a single argument list:
; ss    -  selection set
; wmfOK - A flag that specifies whether wmfout should be be used when cannot flatten directly.
;         If nil objects that required wmf-ing will be returned in a selection set
; newSS - a flag specifies if a new selection set of objects created should be returned.
; 
;Returns a list of two elements:
; (wmf-ss new-ss)
; - wmf:ss is selection set of objects that can be passed to acet-wmf-convert.
;   If wmfOK is nil then objects that could not be flattened are returned in this selection set.
;   If wmfOK is true then this will always be nil because the wmf will be performed before returning.
; - new:ss is a selection set of new objects. This element is always nil unless the newSS 
;   parameter is true.
;
;
(defun acet-flatn-objects ( alst  / lst n ss wmfOK newSS na e1 a na2 wmf:ss new:ss )
 
 ;; extract the arguments from the argument list
 (setq lst '(ss wmfOK newSS))
 (setq n 0)
 (repeat (min (length alst) (length lst))
  (set (nth n lst) (nth n alst))
  (setq n (+ n 1));setq
 );repeat
 
 (setq ss (acet-flatn-remove-hatch-edges ss));setq 
 
 ;; Next, call acet-flatn-object for each object and build the wmf and new selection sets as needed.
 (setq wmf:ss (ssadd)
       new:ss (ssadd)
 );setq
 (setq n 0)
 (repeat (sslength ss)
  (setq na (ssname ss n)
        e1 (entget na)
         a (acet-flatn-object (list e1 wmfok newss))
  );setq
 
  (if (car a)
      (setq wmf:ss (acet-ss-union (list wmf:ss (car a))))
  );if
  (if (cadr a)
      (setq new:ss (acet-ss-union (list new:ss (cadr a))))
  );if
 
  (setq n (+ n 1));setq
 );repeat
 
 (if (= (sslength wmf:ss) 0)
     (setq wmf:ss nil)
 );if
 (if (= (sslength new:ss) 0)
     (setq new:ss nil)
 );if
 
 (list wmf:ss new:ss) 
);defun acet-flatn-objects
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Flattens a specified object.
;Takes a single argument list:
; e1    -  an ename or elist
; wmfOK - A flag that specifies whether wmfout should be be used when cannot flatten directly.
;         If nil objects that required wmf-ing will be returned in a selection set
; newSS - a flag specifies if a new selection set of objects created should be returned.
; 
;Returns a list of two elements:
; (wmf-ss new-ss)
; - wmf:ss is selection set of objects that can be passed to acet-wmf-convert.
;   If wmfOK is nil then objects that could not be flattened are returned in this selection set.
;   If wmfOK is true then this will always be nil because the wmf will be performed before returning.
; - new:ss is a selection set of new objects. This element is always nil unless the newSS 
;   parameter is true.
;
;
(defun acet-flatn-object ( alst  / lst n e1 wmfOK newSS na tp na2 new:ss wmf:ss ss cxv xv parallel )
 
 ;; extract the arguments from the argument list
 (setq lst '(e1 wmfOK newSS))
 (setq n 0)
 (repeat (min (length alst) (length lst))
  (set (nth n lst) (nth n alst))
  (setq n (+ n 1));setq
 );repeat
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq  tp (cdr (assoc 0 e1))
       na2 (entlast)
 );setq
 
 
;(print (cdr (assoc 0 e1)))
;(print (cdr (assoc 5 e1)))
;(print "")
 
 (cond
  ((acet-flatn-object-has-thickness e1)
   (if wmfOK
       (acet-wmf-convert (list na 3 nil nil)) ;then go ahead and wmf
       (progn
        (if (not wmf:ss)
            (setq wmf:ss (ssadd));then create the wmf:ss
        );if
        (setq wmf:ss (ssadd na wmf:ss))
       );progn else add the entity to the wmf:ss
   );if
 
  );
  ((= tp "LINE")
   (setq na (acet-flatn-line e1))
  )
  ((or (= tp "ARC")
       (= tp "CIRCLE")
   );or
   (setq na (acet-flatn-arc e1))
  )
  ((or (= tp "POLYLINE")
       (= tp "LWPOLYLINE")
   );or
   (setq na (acet-flatn-pline e1))
  )
  ((or (= tp "TEXT")
       (= tp "ATTRIB")
       (= tp "ATTDEF")
   );or
   (setq na (acet-flatn-text e1))
  )
  ((or (= tp "MTEXT")
       (= tp "RTEXT")
       (= tp "ARCALIGNEDTEXT")
   );or
   (setq na (acet-flatn-mtext e1))
  )
  ((= tp "INSERT")
   (setq na (acet-flatn-insert e1))
  )
  ((= tp "ELLIPSE")
   (setq na (acet-flatn-ellipse e1))
  )
  ((= tp "HATCH")
   (setq na (acet-flatn-hatch e1))
  )
  ((= tp "IMAGE")
   (setq na (acet-flatn-image e1))
  )
  ((= tp "WIPEOUT")
   (setq na (acet-flatn-wipeout e1))
  )
  ((= tp "SPLINE")
   (setq na (acet-flatn-spline e1))
  )
  ((= tp "POINT")
   (setq na (acet-flatn-point e1))
  )
  ((= tp "3DFACE")
   (setq na (acet-flatn-3dface e1))
  )
  ((or (= tp "TRACE")
       (= tp "SOLID")
   );or
   (setq na (acet-flatn-trace e1))
  )
  ((and (setq cxv (acet-geom-z-axis)
               xv (acet-geom-object-z-axis e1)
        )
        (setq parallel (acet-geom-vector-parallel xv cxv))
        (or (= tp "DIMENSION")
            (= tp "LEADER")
        );or
   );and
   (setq na (acet-flatn-dim e1))
  )
  (;(and parallel
         (= tp "TOLERANCE")
   ;);and
   (setq na (acet-flatn-tolerance e1))
  )
  ((or (= tp "XLINE")
       (= tp "RAY")
   );or
   (setq na (acet-flatn-xline e1))
  )
  ((= tp "MLINE")
   (setq na (acet-flatn-mline e1))
  )
  ((= tp "SHAPE")
   (setq na (acet-flatn-shape e1))
  )
  ((or (= tp "3DSOLID")
       (= tp "BODY")
   );or
   (setq ss (acet-flatn-3dsolid e1))
  )
  (T
   (setq ss (acet-flatn-unknown e1 wmfOK))
   (if (and (not wmfOK)
            ss
       );and
       (setq wmf:ss (acet-ss-union (list wmf:ss ss)))
   );if
  )
 );cond close
 (if (and newss
          (not (equal na2 (entlast)))
     );and
     (setq new:ss (acet-ss-new na2))
     (setq new:ss nil)
 );if
 
;(print "exiting")
;(print "")
 
 (list wmf:ss new:ss)
);defun acet-flatn-object
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes and entity and attempts to explode it and then flattens each of the new objets if 
;successful.
;If explode cannot be done and wmfOK is true then the object will be wmf'd
;If wmfOK is nil then a wmf:ss will be returned if the object cannot be exploded.
;
(defun acet-flatn-unknown ( e1 wmfOK / na na2 ss n a wmf:ss ss2 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (command "_.copy" na "" "0,0" "0,0")
 (setq na2 (entlast))
 (if (setq ss (acet-inherit-xplode na2 nil))
     (progn
      (setq ss (acet-flatn-remove-hatch-edges ss))
      (setq n 0)
      (repeat (sslength ss)
       (setq na2 (ssname ss n))
       (setq   a (acet-flatn-object 
                  (list na2 wmfOK nil) ;; pass wmfOK through and nil for new selection set
                 )
             ss2 (car a) ;wfm ss
       );setq
       (if ss2
           (progn
            (if (not wmf:ss)
                (setq wmf:ss (ssadd))
            );if
            (setq wmf:ss (acet-ss-union (list wmf:ss ss2)))
           );progn then add object(s) to the wmf selection set
           (entdel na2)
       );if
       (setq n (+ n 1));setq
      );repeat
     );progn then explode succeeded
     (progn
      (if wmfOK
          (acet-wmf-convert (list na2 3 nil nil))
          (progn
           (if (not wmf:ss)
               (setq wmf:ss (ssadd))
           );if
           (setq wmf:ss (ssadd na wmf:ss))
           (entdel na2)
          );progn else
      );if
      
     );progn else
 );if
 wmf:ss
);defun acet-flatn-unknown
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-flatn-line ( e1 / na p1 p2 xv xv2 cxv th p3 p4 ss lst plst thv )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq  p1 (cdr (assoc 10 e1))
        p2 (cdr (assoc 11 e1))
       cxv (acet-geom-z-axis)
        xv (cdr (assoc 210 e1))
       xv2 (acet-geom-vector-scale xv -1.0) 
        th (cdr (assoc 39 e1))
 );setq
 (if (and th
          (not (equal cxv xv 0.0000001))
          (not (equal cxv xv2 0.0000001))
     );and
     (progn
      (setq  thv (acet-geom-unit-vector '(0.0 0.0 0.0) xv)
             thv (acet-geom-vector-scale thv th)
              p3 (acet-geom-vector-add p2 thv)
              p4 (acet-geom-vector-add p1 thv)
            plst (acet-general-props-get e1)
             lst (list p1 p2 p3 p4 p1)
             lst (mapcar '(lambda (x) (acet-point-flat x 0 cxv)) lst)
      );setq
      (acet-lwpline-make
       (list 
         (list (cons 210 cxv))
         lst
       );list
      );acet-lwpline-make 
      (setq ss (ssadd (entlast) (ssadd))) 
      (acet-general-props-set ss plst)
     );progn then it has thickness and it's not parallel to the current ucs so create pline
     (progn
      (setq p1 (acet-point-flat p1 0 0)
            p2 (acet-point-flat p2 0 0)
            e1 (subst (cons 10 p1) (assoc 10 e1) e1)
            e1 (subst (cons 11 p2) (assoc 11 e1) e1)
            e1 (subst (cons 210 cxv) (assoc 210 e1) e1)
            e1 (subst (cons 39 0.0) (assoc 39 e1) e1)
      );setq
      (if (entmake e1)
          (setq na (entlast))
          (setq na nil)
      );if
     );progn else
 );if
 
 na
);defun acet-flatn-line
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-flatn-3dsolid ( e1 / na ss ss2 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq ss (acet-wmf-convert (list (ssadd na) 3 nil nil))) 
 ss
);defun acet-flatn-3dsolid
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes an arc or circle (ename or elist) and creates an arc or ellipse that 
;lies entirely within the plane of the current ucs.
;Returns the new ename if successful.
;
(defun acet-flatn-arc ( e1 / na xv cxv c a1 a2 xvda xva ax1 ax2 c2 x f p1 p2 gp r p3 p4 d fuz ) 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)))
 );if
 
 ;; get the extrusion vector for the arc and the current z axis
 (setq   r (cdr (assoc 40 e1))
        xv (cdr (assoc 210 e1))
       cxv (acet-geom-z-axis)
         c (cdr (assoc 10 e1))
        e1 (subst '(39 . 0.0) (assoc 39 e1) e1) ;; force thickness to 0.0
        a1 (cdr (assoc 50 e1))
        a2 (cdr (assoc 51 e1))
       fuz (acet-geom-object-fuz na)
 );setq
 (if (not a1)
     (setq a1 0.0
           a2 (+ pi pi)
     );setq then it's a circle so set the begining and ending angles
 );if
 
 (cond
  ((equal xv cxv fuz)
   (setq  c (acet-point-flat c xv cxv)
         e1 (subst (cons 10 c) (assoc 10 e1) e1)
   );setq
  );cond #1 then it's parallel
 
  ((equal (acet-geom-vector-scale xv -1.0)
          cxv 
          fuz
   );equal
   (setq  c (acet-point-flat c xv cxv)
         e1 (subst (cons 10 c) (assoc 10 e1) e1)
         e1 (subst (cons 210 cxv) (assoc 210 e1) e1)
   );setq
   (if (setq c (cdr (assoc 50 e1)))
       (setq  c (cdr (assoc 10 e1))
             p1 (polar c a1 r)
             p2 (polar c a2 r)
             p1 (trans p1 xv cxv)
             p2 (trans p2 xv cxv)
              c (trans c xv cxv)
             a1 (angle c p2)
             a2 (angle c p1)
             e1 (subst (cons 50 a1) (assoc 50 e1) e1)
             e1 (subst (cons 51 a2) (assoc 51 e1) e1) 
       );setq then it's an arc so adjust the begining and ending angles
   );if
  );cond #2 then it's parallel but the extrusion is reversed so swap begining and ending angles
 
  ((equal r 0.0 fuz)
   ;; radius is zero
   (setq  c (acet-point-flat c xv xv)
         e1 (subst (cons 10 c) (assoc 10 e1) e1)
   );setq
  );cond #3
 
  ((and (setq xvda (acet-geom-vector-d-angle xv cxv))
        (equal (/ pi 2.0) xvda 0.00000001)
   );and
   (setq e1 (acet-flatn-orthogonal-object-elist na))
  );cond #4
 
  (xvda			;; turn it into an ellipse
 
   (setq c (acet-point-flat c xv xv)) 
 
   ;; The angle of the object's z axis in the current ucs points towards the minor axis
   ;; of the ellipse that will be formed. Conversly, the angle of the current ucs's z axis,
   ;; expressed in the objects coordinate system, points towards the minor axis.
   ;; The major axis length will always be equal to the radius of the arc.
   ;; The minor axis length will equal r*sin(90-da) where da is the angle between the extrusion 
   ;; vector of the object and the z axis of the current ucs.
   ;; We will use a combination of the angle of the object extrusion vector in the the current ucs 
   ;; and the known major axis length to get a vector from the cent to the major axis quadrant.
   ;; From there; everything falls into place.
 
   (setq ;xvda (acet-geom-vector-d-angle xv cxv)         ;; angle between extrusion vectors of object and cur. ucs
          xva (angle '(0.0 0.0 0.0) (trans xv 0 cxv T)) ;; extrusion vector angle in current ucs
          ax1 r 				;; major axis length
          ax2 (* r (sin (- (/ pi 2.0) xvda)))	;; minor axis length
           c2 (trans c xv cxv)			;; the center in current coords
           c2 (list (car c2) (cadr c2) 0.0)	;; force z to 0.0
            x (polar c2 (- xva (/ pi 2.0)) ax1) ;; major axis endpoint in current coords
            x (acet-geom-delta-vector c2 x) 	;; convert x to a relative vector from c to x (major axis vector)
            x (trans x cxv 0 T)                 ;; convert major axis to world coords.
            f (abs (/ ax2 ax1))			;; ratio of minor to major axis (use abs so ucs does not flip)
            c (trans c xv 0)			;; convert center to world coords.
   );setq
 
   (setq gp (acet-general-props-get-pairs e1)   ;; get general properties
         e1 (list '(0 . "ELLIPSE") '(100 . "AcDbEntity")
                  (assoc 67 e1) (assoc 410 e1) (assoc 8 e1)
                 '(100 . "AcDbEllipse")
                 (cons 10 c)
                 (cons 11 x) ;; was a y
                 (cons 210 cxv)
                 (cons 40 f)
                 '(41 . 0.0) '(42 . 6.28319)
           );list
        e1 (acet-general-props-set-pairs e1 gp)
   );setq
 
   ;; If it is not a full circle then we need to translate the start 
   ;; and end angles. 
   (if (not (and (= a1 0.0)
                 (= a2 (+ pi pi))
            );and
       );not
       (progn
        ;; We'll get the endpoints and center and translate them to the current 
        ;; ucs and get the angles. Then we'll use acet-geom-angle-to-ellipseAngle
        ;; to get the start and end parameters for the ellipse.
        ;;
        (setq  c (trans c 0 xv) ;new
              p1 (polar c a1 r)
              p2 (polar c a2 r)
               c (trans c xv cxv) ;new
              p1 (trans p1 xv cxv)
              p2 (trans p2 xv cxv)
              a1 (angle c p1)
              a2 (angle c p2)
              a1 (acet-geom-angle-to-ellipseAngle a1 e1)
              a2 (acet-geom-angle-to-ellipseAngle a2 e1)
        );setq
        (if (equal a1 a2 0.00000001)
            (setq a2 (+ a2 pi pi));setq then
        );if 
        (if (>= ax2 0.0)
            (setq e1 (subst (cons 41 a1) (assoc 41 e1) e1)
                  e1 (subst (cons 42 a2) (assoc 42 e1) e1)
            );setq
            (setq e1 (subst (cons 41 a2) (assoc 41 e1) e1)
                  e1 (subst (cons 42 a1) (assoc 42 e1) e1)
            );setq else swap the start and end angles so that the extrusion vector comes out right
        );if
       );progn then not a full ellipse
   );if
  );cond #5
 );cond close
 (if (and e1
          (entmake e1)
     );and
     (setq na (entlast))
     (setq na nil)
 );if
 (if (and ax1
          na
     );and
     (acet-ellipse-major-axis-flip na)
 );if
 na
);defun acet-flatn-arc
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-flatn-orthogonal-object-elist ( na / e1 xv cxv ss p1 p2 p3 p4 c d xva gp )
   ;; then the arc is orthogonal to the current ucs so just create a line
   (setq  e1 (entget na)
          xv (cdr (assoc 210 e1))
         cxv (acet-geom-z-axis)
          ss (ssadd na (ssadd))
          p1 (acet-geom-ss-extents ss T) ;; shrinkwrap=T
          p2 (cadr p1)
          p1 (car p1)
          p1 (acet-point-flat p1 cxv cxv)	;; flatten the extents points and convert to current ucs
          p2 (acet-point-flat p2 cxv cxv)
           c (acet-geom-midpoint p1 p2)
           d (distance c p1)
   );setq
   (if (not xv)
       (setq xv cxv)
   );if
 
   (setq xva (trans xv 0 cxv T)			;; extrusion vector
         xva (- (angle '(0.0 0.0 0.0) xva)	;; angle of arc z vector - 90
                (/ pi 2.0)
             )
          p3 (polar c xva d)
          p4 (polar c (+ xva pi) d)
 
          gp (acet-general-props-get-pairs e1)   ;; get general properties
          e1 (list '(0 . "LINE") '(100 . "AcDbEntity")
                   (assoc 67 e1) (assoc 410 e1) (assoc 8 e1)
                   '(100 . "AcDbLine")
                   (cons 10 (trans p3 cxv 0))
                   (cons 11 (trans p4 cxv 0))
                   (cons 210 cxv)
            );list
         e1 (acet-general-props-set-pairs e1 gp)
   );setq
 e1
);defun acet-flatn-orthogonal-object-elist
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This routine ensures that consistant ellipses are produced with flatten.
;This allows overkill to work properly.
;
(defun acet-ellipse-major-axis-flip ( na / e1 xv ax a b )
 (setq e1 (entget na)
       ax (cdr (assoc 11 e1))
       xv (cdr (assoc 210 e1))
       ax (trans ax 0 xv)
 );setq
 (if (and (< (car ax) 0)
          (< (cadr ax) 0)
     );and
     (progn
      (setq ax (acet-geom-vector-scale ax -1.0)
            ax (trans ax xv 0)
            e1 (subst (cons 11 ax) (assoc 11 e1) e1)
             a (cdr (assoc 41 e1))
             b (cdr (assoc 42 e1))
      );setq
      (if (not (acet-angle-equal a b 0.00000001))
          (setq
             a (acet-angle-format (+ (cdr (assoc 41 e1)) pi))
             b (acet-angle-format (+ (cdr (assoc 42 e1)) pi))
            e1 (subst (cons 41 a) (assoc 41 e1) e1)
            e1 (subst (cons 42 b) (assoc 42 e1) e1)
          );setq
      );if
      (entmod e1)
     );progn then flip it (flip it good)
 );if
);defun acet-ellipse-major-axis-flip
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a text entity (ename or elist) and creates a flat version of the text object.
;Returns an new ename on success or nil on failure.
;
(defun acet-flatn-text ( e1 / na xv xv2 cxv p1 p2 hf wf w obang ang lst lst2 h just dang d1 d2 pfuz p3 p4 )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
  
 ;; get the extrusion vectors
 (setq  xv (cdr (assoc 210 e1))
       xv2 (acet-geom-vector-scale xv -1.0)
       cxv (acet-geom-z-axis)
        e1 (subst '(39 . 0.0) (assoc 39 e1) e1) ;; force thickness to 0.0
 );setq
 (cond
  ((or (equal xv cxv 0.0000001)
       (equal xv2 cxv 0.0000001)
   );or
   ;; parallel so just entmod it
   (setq p1 (cdr (assoc 10 e1))
         p1 (acet-point-flat p1 xv cxv)
         p2 (cdr (assoc 11 e1))
         p2 (acet-point-flat p2 xv cxv)
         e1 (subst (cons 10 p1) (assoc 10 e1) e1)
         e1 (subst (cons 11 p2) (assoc 11 e1) e1)
   );setq
  );cond #1
 
  ((and (setq  lst (acet-geom-textbox e1 0.0))
        (equal (car lst) (caddr lst) 0.00000001)
   );and
   (setq e1 nil) ;; empty text so skip it.
  );cond #2
 
  (T
   (setq just (acet-tjust-keyword e1))		;; save the original justification
   (acet-tjust (ssadd na (ssadd)) "Start")	;; then set it to start
   (setq   e1 (entget na)
          lst (acet-geom-m-trans lst 1 cxv)
         lst2 (mapcar '(lambda (x) (list (car x) (cadr x) 0.0)) 
                      lst
              );mapcar
          ;ang (angle (car lst) (cadr lst))			;; rotation angle
          ang (acet-geom-angle-trans (cdr (assoc 50 e1)) xv cxv)
 
           p1 (cdr (assoc 10 e1))
        obang (cdr (assoc 51 e1)) 
            h (cdr (assoc 40 e1))				;; height
        obang (- (/ pi 2.0) obang)
        obang (+ obang (cdr (assoc 50 e1)))
           p2 (polar p1 obang
                     (/ h 
                        (cos (cdr (assoc 51 e1)))
                     )
              );polar
           p1 (trans p1 xv cxv)
           p2 (trans p2 xv cxv)
           p1 (list (car p1) (cadr p1) 0.0)
           p2 (list (car p2) (cadr p2) 0.0)
 
           p3 '(1.0 0.0 0.0)				;; x axis direction vector
           p3 (acet-geom-point-rotate p3 '(0.0 0.0 0.0) (cdr (assoc 50 e1)))
           p3 (trans p3 xv cxv T)
           p3 (list (car p3) (cadr p3) 0.0)
           p4 '(0.0 1.0 0.0)				;; y axis direction vector
           p4 (acet-geom-point-rotate p4 '(0.0 0.0 0.0) (cdr (assoc 50 e1)))
           p4 (trans p4 xv cxv T)
           p4 (list (car p4) (cadr p4) 0.0)
 
        obang (acet-geom-vector-d-angle				;; oblique angle
                ;(acet-geom-delta-vector (nth 0 lst2) (nth 1 lst2))
                p3
                (acet-geom-delta-vector p1 p2)
              )
        obang (- (/ pi 2.0) obang)
 
 
         dang (acet-geom-vector-d-angle				;; oblique angle delta
                ;(acet-geom-delta-vector (nth 0 lst2) (nth 1 lst2))
                ;(acet-geom-delta-vector (nth 0 lst2) (nth 3 lst2))
                p3
                p4
              )
         dang (- (/ pi 2.0) dang)
           d1 (distance (nth 0 lst2) (nth 3 lst2))
           d2 (distance (nth 0 lst) (nth 3 lst))
         pfuz (acet-geom-point-fuz (nth 0 lst))
    );setq
    (if (and (= d2 0.0)
             (equal d1 d2 pfuz)
        );and
        (setq hf 1.0)
        (setq hf (/ d1 d2))					;; height multiplyer
    );if 
;(print (rtos d1 2 16))
;(print (rtos d2 2 16))
;(print hf)
;(print "")
    (setq   h (cdr (assoc 40 e1))				;; height
            h (* h hf)
            h (* h (cos dang))
           d1 (distance (car lst2) (cadr lst2))
           d2 (distance (car lst) (cadr lst))
         pfuz (acet-geom-point-fuz (nth 0 lst))
    );setq
    (if (and (= d2 0.0)
             (equal d1 d2 pfuz)
        );and
        (setq wf 1.0)
        (setq wf (/ d1 d2))						;; width multiplyer
    );if 
;(print (rtos d1 2 16))
;(print (rtos d2 2 16))
;(print wf)
;(print "")
   ;; get the width factor
   (if (not (setq w (cdr (assoc 41 e1))))
       (setq w 1.0)
   );if
   (setq w (* w wf)
         w (* w (/ 1.0 hf) (/ 1.0 (cos dang)))
   );setq
 
   ;; translate the points and force z=0.0
   (setq p1 (acet-point-flat (cdr (assoc 10 e1)) xv cxv)
         p2 (acet-point-flat (cdr (assoc 11 e1)) xv cxv)
   );setq
 
   (setq e1 (acet-list-assoc-put (cons 41 w) e1)
         e1 (acet-list-assoc-put (cons 50 ang) e1)
         e1 (acet-list-assoc-put (cons 51 obang) e1)
         e1 (acet-list-assoc-put (cons 40 h) e1)
         e1 (subst (cons 10 p1) (assoc 10 e1) e1)
         e1 (subst (cons 11 p2) (assoc 11 e1) e1)
         e1 (subst (cons 210 cxv) (assoc 210 e1) e1)
   );setq
  );cond #3
 );cond close
 
 (if (entmake e1)
     (progn
      (setq na (entlast))
      (if just
          (acet-tjust (ssadd na (ssadd)) just);then put the original justification back
      );if
     );progn
     (setq na nil)
 );if
 
 na
);defun acet-flatn-text
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a shape entity (ename or elist) and creates a flat version of the object.
;Returns an new ename on success or nil on failure.
;
(defun acet-flatn-shape ( e1 / na xv xv2 cxv p1 p2 p3 p4 hf wf w obang ang lst lst2 h ss e2 na2 dang x )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
  
 ;; get the extrusion vectors
 (setq  xv (cdr (assoc 210 e1))
       cxv (acet-geom-z-axis)
        e1 (subst '(39 . 0.0) (assoc 39 e1) e1) ;; force thickness to 0.0
 );setq
 (cond
  ((acet-geom-vector-parallel xv cxv)
   (setq p1 (cdr (assoc 10 e1))
         p1 (acet-point-flat p1 xv cxv)
         e1 (subst (cons 10 p1) (assoc 10 e1) e1)
   );setq
  );cond #1
 
  (T
   (setq e2 e1
         e2 (subst '(210 0.0 0.0 1.0) (assoc 210 e2) e2)
         e2 (subst '(50 . 0.0) (assoc 50 e2) e2)
         e2 (subst '(10 0.0 0.0 0.0) (assoc 10 e2) e2)
   );setq
   (if (assoc 60 e2)
       (setq e2 (subst '(60 . 1) (assoc 60 e2) e2))
       (setq e2 (append e2 '((60 . 1))));setq
   );if
   (entmake e2)
   (setq  na2 (entlast)
          lst (acet-ent-geomextents na2)
   );setq
   (entdel na2)
 
   (setq   p1 (car lst)
           p3 (cadr lst)
           p2 (list (car p3) (cadr p1) (caddr p1))
           p4 (list (car p1) (cadr p3) (caddr p1))
          ang (cdr (assoc 50 e1))
           p1 (acet-geom-point-rotate p1 '(0.0 0.0 0.0) ang)	;; rotate
           p2 (acet-geom-point-rotate p2 '(0.0 0.0 0.0) ang)
           p3 (acet-geom-point-rotate p3 '(0.0 0.0 0.0) ang)
           p4 (acet-geom-point-rotate p4 '(0.0 0.0 0.0) ang)
            x (cdr (assoc 10 e1))		;; apply the vector offsets from the e1 basepoint
           p1 (acet-geom-vector-add x p1)
           p2 (acet-geom-vector-add x p2)
           p3 (acet-geom-vector-add x p3)
           p4 (acet-geom-vector-add x p4)
          lst (list p1 p2 p3 p4 p1)
          lst (acet-geom-m-trans lst xv cxv)
         lst2 (mapcar '(lambda (x) (list (car x) (cadr x) 0.0)) 
                      lst
              );mapcar
          ang (angle (car lst) (cadr lst))			;; rotation angle
 
           p1 (car lst)
           p1 (trans p1 cxv xv)
        obang (cdr (assoc 51 e1)) 
            h (distance (car lst) (cadr lst))			;; height
        obang (- (/ pi 2.0) obang)
        obang (+ obang (cdr (assoc 50 e1)))
           p2 (polar p1 obang
                     (/ h 
                        (cos (cdr (assoc 51 e1)))
                     )
              );polar
           p1 (trans p1 xv cxv)
           p2 (trans p2 xv cxv)
           p1 (list (car p1) (cadr p1) 0.0)
           p2 (list (car p2) (cadr p2) 0.0)
        obang (acet-geom-vector-d-angle				;; oblique angle
                (acet-geom-delta-vector (nth 0 lst2) (nth 1 lst2))
                (acet-geom-delta-vector p1 p2)
              )
        obang (- (/ pi 2.0) obang)
 
 
         dang (acet-geom-vector-d-angle				;; oblique angle
                (acet-geom-delta-vector (nth 0 lst2) (nth 1 lst2))
                (acet-geom-delta-vector (nth 0 lst2) (nth 3 lst2))
              )
         dang (- (/ pi 2.0) dang)
           hf (/ (distance (nth 0 lst2) (nth 3 lst2))		;; height multiplyer
                 (distance (nth 0 lst) (nth 3 lst))
              )
            h (cdr (assoc 40 e1))				;; height
            h (* h hf)
            h (* h (cos dang))
           wf (/ (distance (car lst2) (cadr lst2))		;; width multiplyer
                 (distance (car lst) (cadr lst))
              )
   );setq
   ;; get the width factor
   (if (not (setq w (cdr (assoc 41 e1))))
       (setq w 1.0)
   );if
   (setq w (* w wf)
         w (* w (/ 1.0 hf) (/ 1.0 (cos dang)))
   );setq
 
   ;; translate the points and force z=0.0
   (setq p1 (acet-point-flat (cdr (assoc 10 e1)) xv cxv));setq
 
   (setq e1 (acet-list-assoc-put (cons 41 w) e1)
         e1 (acet-list-assoc-put (cons 50 ang) e1)
         e1 (acet-list-assoc-put (cons 51 obang) e1)
         e1 (acet-list-assoc-put (cons 40 h) e1)
         e1 (subst (cons 10 p1) (assoc 10 e1) e1)
         e1 (subst (cons 210 cxv) (assoc 210 e1) e1)
   );setq
  );cond #2
 );cond close
 
 (if (entmake e1)
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defun acet-flatn-shape
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes an mtext, arctext or rtext entity (ename or elist) and creates flat 
;text objects if not parallel or if parallel creates a copy at the proper elevation.
;
;Returns a selection set of new objects
;
(defun acet-flatn-mtext ( e1 / na na2 xv xv2 cxv ss n p1 lay na3 )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 ;; get the extrusion vectors
 (setq  xv (cdr (assoc 210 e1))
       xv2 (acet-geom-vector-scale xv -1.0)
       cxv (acet-geom-z-axis)
        e1 (subst '(39 . 0.0) (assoc 39 e1) e1) ;; force thickness to 0.0
       lay (cdr (assoc 8 e1))
       na2 (entlast)
 );setq
 (cond
  ((or (equal xv cxv 0.0000001)
       (equal xv2 cxv 0.0000001)
   );or
   ;; parallel so just force z=0.0 
   (setq p1 (cdr (assoc 10 e1))
         p1 (acet-point-flat p1 0 0)
         e1 (subst (cons 10 p1) (assoc 10 e1) e1)
   );setq
   (entmake e1)
  );cond #1
  (T
   (entmake e1)
   (setq na (entlast))
   (if (and (= "RTEXT" (cdr (assoc 0 e1)))
            (acet-explode na)
       );and
       (setq na (entlast))
   );if
 
   (setq ss (acet-explode na))
   (if ss
       (progn
        (setq n 0)
        (repeat (sslength ss)
         (setq  na (ssname ss n)
               na3 (cdr (acet-flatn-object (list na T T))) ;; this is usually text but it can be a line. ;@rk
         );setq
         (if (and na3
                  (equal 'ENAME (type na3))
                  (setq e1 (entget na3))
             );and
             (progn
              (setq e1 (subst (cons 8 lay) (assoc 8 e1) e1))
              (entmod e1)
             );progn then
         );if
         (entdel na)
         (setq n (+ n 1));setq
        );repeat
       );progn then exploded
   );if
  );cond #2
 );cond close
 
 (acet-ss-new na2)  
);defun acet-flatn-mtext
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This thing will flatten a 3dface and returns a selection set of lines.
;;
(defun acet-flatn-3dface ( e1 / na xv cxv bits gp e2 na2 p1 p2 ss )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq    xv '(0.0 0.0 1.0)
         cxv (acet-geom-z-axis)
        bits (cdr (assoc 70 e1))
          gp (acet-general-props-get-pairs e1)   ;; get general properties
          e2 (list '(0 . "LINE") '(100 . "AcDbEntity")
                   (assoc 67 e1) (assoc 410 e1) (assoc 8 e1)
                   '(100 . "AcDbLine")
                   '(10 0.0 0.0 0.0)
                   '(11 1.0 0.0 0.0)
                   (cons 210 cxv)
            );list
          e2 (acet-general-props-set-pairs e2 gp)
         na2 (entlast)
 );setq
 (if (/= 1 (logand 1 bits))
     (progn
      (setq p1 (acet-point-flat (cdr (assoc 10 e1)) xv xv)
            p2 (acet-point-flat (cdr (assoc 11 e1)) xv xv)
            e2 (subst (cons 10 p1) (assoc 10 e2) e2)
            e2 (subst (cons 11 p2) (assoc 11 e2) e2)
      );setq
      (entmake e2)
     );progn then
 );if
 (if (/= 2 (logand 2 bits))
     (progn
      (setq p1 (acet-point-flat (cdr (assoc 11 e1)) xv xv)
            p2 (acet-point-flat (cdr (assoc 12 e1)) xv xv)
            e2 (subst (cons 10 p1) (assoc 10 e2) e2)
            e2 (subst (cons 11 p2) (assoc 11 e2) e2)
      );setq
      (entmake e2)
     );progn then
 );if
 (if (/= 4 (logand 4 bits))
     (progn
      (setq p1 (acet-point-flat (cdr (assoc 12 e1)) xv xv)
            p2 (acet-point-flat (cdr (assoc 13 e1)) xv xv)
            e2 (subst (cons 10 p1) (assoc 10 e2) e2)
            e2 (subst (cons 11 p2) (assoc 11 e2) e2)
      );setq
      (entmake e2)
     );progn then
 );if
 (if (/= 8 (logand 8 bits))
     (progn
      (setq p1 (acet-point-flat (cdr (assoc 13 e1)) xv xv)
            p2 (acet-point-flat (cdr (assoc 10 e1)) xv xv)
            e2 (subst (cons 10 p1) (assoc 10 e2) e2)
            e2 (subst (cons 11 p2) (assoc 11 e2) e2)
      );setq
      (entmake e2)
     );progn then
 );if
 (if (not (equal na2 (entlast)))
     (setq ss (acet-ss-new na2))
 );if
 ss
);defun acet-flatn-3dface
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This thing will flatten a trace or solid (2d) object.
;;
(defun acet-flatn-trace ( e1 / na p1 xv cxv )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq xv (cdr (assoc 210 e1)))
 (if (not xv)
     (setq  xv '(0.0 0.0 1.0)
           cxv xv
     );setq then world
     (setq cxv (acet-geom-z-axis)
            e1 (subst (cons 210 cxv) (assoc 210 e1) e1)
     );setq else
 );if
 
 (setq  p1 (acet-point-flat (cdr (assoc 10 e1)) xv cxv)
        e1 (subst (cons 10 p1) (assoc 10 e1) e1)
 
        p1 (acet-point-flat (cdr (assoc 11 e1)) xv cxv)
        e1 (subst (cons 11 p1) (assoc 11 e1) e1)
 
        p1 (acet-point-flat (cdr (assoc 12 e1)) xv cxv)
        e1 (subst (cons 12 p1) (assoc 12 e1) e1)
 
        p1 (acet-point-flat (cdr (assoc 13 e1)) xv cxv)
        e1 (subst (cons 13 p1) (assoc 13 e1) e1)
 );setq
 (if (entmake e1)
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defun acet-flatn-trace
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a pline entity (ename or elist) and creates a flat version of it.
;Returns a selection set of new object(s). (usually only one object)
;
(defun acet-flatn-pline ( e1 / na )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (cond
  ((acet-pline-is-2d e1)
   (setq na (acet-flatn-2d-pline e1))
  );cond #1
  (T
   (setq na (acet-flatn-3d-pline e1))
  );cond #2
 );cond close
 
 na
);defun acet-flatn-pline
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;see acet-flatn-pline
;
(defun acet-flatn-2d-pline ( e1 / na xv xv2 cxv na2 na3 plst p1 p2 n ss lulst a ltgen elst wlst z )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 ;; get the extrusion vectors
 (setq  xv (cdr (assoc 210 e1))
       xv2 (acet-geom-vector-scale xv -1.0)
       cxv (acet-geom-z-axis)
        e1 (subst '(39 . 0.0) (assoc 39 e1) e1) ;; force thickness to 0.0
       na3 (entlast)
 );setq
 (cond
  ((or (equal xv cxv 0.0000001)
       (equal xv2 cxv 0.0000001)
   );or
   ;; parallel so just force z=0.0 
   (setq p1 (car (acet-geom-object-end-points na))
         p2 (list (car p1) (cadr p1) 0.0)
   );setq
   (command "_.copy" na "" p1 p2)
   ;; skip the flip
   ;(if (equal xv2 cxv 0.0000001)
   ;    (progn
   ;     (setq plst (acet-plines-explode (ssadd (entlast) (ssadd)))
   ;           plst (cadr plst)
   ;     );setq
   ;     (acet-plines-rebuild plst)
   ;    );progn then flip the extrusion by exploding and re-building
   ;);if        
  );cond #1
  (T
   (command "_.copy" na "" "0,0" "0,0")
   (setq plst (acet-plines-explode (ssadd (entlast) (ssadd)))
           ss (car plst)
         plst (cadr plst)
   );setq
 
   (setq n 0)
   (repeat (sslength ss)
    (setq na (ssname ss n)
          e1 (entget na)
    );setq
    (if (= "ARC" (cdr (assoc 0 e1)))
        (progn
         (if (setq na2 (acet-flatn-arc e1))
             (setq lulst (cons (list na na2) lulst))	;; add the new entity to the lookup list
         );if
         (entdel na)
        );progn then create a flat version of it and add it to the lookup list for re-assembly later
        (progn
         (setq p1 (acet-point-flat (cdr (assoc 10 e1)) 0 0)
               p2 (acet-point-flat (cdr (assoc 11 e1)) 0 0) 
               e1 (subst (cons 10 p1) (assoc 10 e1) e1)
               e1 (subst (cons 11 p2) (assoc 11 e1) e1)
         );setq
         (entmod e1)
        );progn else it's a line
    );if
    (setq n (+ n 1));setq
   );repeat
 
   ;; use the look up list to update the plst so that the pline can be re-assembled.
   (setq  plst (nth 0 plst)
         ltgen (nth 0 plst)
          elst (nth 1 plst)
          wlst (nth 2 plst)
             z (nth 4 plst)
             z (acet-point-flat z xv cxv)           ;; flatten the z down
          plst (cdr (cdr (cdr (cdr (cdr plst)))))   ;; strip ltgen, enames, widths, extrusion vector, z
          plst (append (list wlst (acet-geom-z-axis) z) plst) ;; add new xv and z
   );setq
   ;; use the look-up list of ename to map in the new enames for flat objects
   (foreach na elst
    (if (setq a (assoc na lulst))
        (setq elst (subst (cadr a) na elst))
    );if
   );foreach
   (if lulst
       (setq ltgen "_on") ;; force ltgen on if we are converting arcs (ellipses to plines)
   );if
   (setq plst (cons elst plst)	;; add the new elst and the ltgen
         plst (cons ltgen plst)
         plst (list plst)
   );setq
 
   (acet-plines-rebuild plst)
 
 
   (acet-flatn-pline-adjust-widths (entlast) xv)   
 
   ;(acet-lwpline-remove-duplicate-pnts (entget (entlast))) ;; remove any duplicated points
  );;cond #2 not parallel
 );cond close
 
 (if (acet-ss-new na3)
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defun acet-flatn-2d-pline
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;See acet-flatn-pline
;
(defun acet-flatn-3d-pline ( e1 / na na2 p1 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (command "_.copy" na "" "0,0" "0,0")
 (setq  na (entlast)
       na2 na
 );setq
 (while (and (setq na (entnext na))
             (setq e1 (entget na))
             (/= "SEQEND" (cdr (assoc 0 e1)))
        );and
  (setq ;p1 (cdr (assoc 10 e1))
        ;p1 (trans p1 0 1)
        ;p1 (list (car p1) (cadr p1) 0.0)
        ;p1 (trans p1 1 0)
        p1 (acet-point-flat (cdr (assoc 10 e1)) 0 0)
        e1 (subst (cons 10 p1) (assoc 10 e1) e1)
  );setq
  (entmod e1)
 );while
 (entupd na2)
 
 na2
);defun acet-flatn-3d-pline
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-flatn-wipeout ( e1 / na lst xt p1 p2 dx dy pxl xv yv cxv )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq cxv (acet-geom-z-axis)
       lst (mapcar 'cdr (acet-list-m-assoc 14 e1)) 	;; get a list of bounding points
       lst (acet-geom-trans-image-to-ucs lst na cxv) 	;; translate to current extrusion vector
        xt (acet-geom-list-extents lst)
        p1 (car xt)
        p2 (cadr xt)
        dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1))
       pxl (max dx dy) 			;; the pixel size
        p1 (list (car p1)		;; the new insert point will be the max minus the pixel size
                 (cadr p1)
                 (caddr (car lst))
           );list
 );setq
 (setq  ;p1 (trans p1 cxv 1)		;; force the z to 0
        ;p1 (list (car p1) (cadr p1) 0.0)
        ;p1 (trans p1 1 0)
        p1 (acet-point-flat p1 cxv 0)
       lst (acet-geom-m-trans lst cxv 1)
       lst (mapcar '(lambda (x) (list (car x) (cadr x) 0.0)) lst) ;; force z to 0.0
       ;; lst (mapcar '(lambda (x) (acet-point-flat x cxv 1)) lst)
 );setq
 
 (setq
        xv (acet-geom-arbitrary-x cxv)			;; get the x and y direction vectors
        xv (acet-geom-unit-vector '(0.0 0.0 0.0) xv)
        yv (acet-geom-cross-product cxv xv) 		;; 	cross prod of z axis and x axis gives y axis
        yv (acet-geom-unit-vector '(0.0 0.0 0.0) yv)
 
        xv (acet-geom-vector-scale xv pxl)		;; 	length of one pixel	
        yv (acet-geom-vector-scale yv pxl)
 
        e1 (subst (cons 10 p1) (assoc 10 e1) e1)
        e1 (subst (cons 11 xv) (assoc 11 e1) e1)
        e1 (subst (cons 12 yv) (assoc 12 e1) e1)
       lst (acet-geom-trans-ucs-to-image lst e1 1) ;; translate the coords from current ucs to the new wipeout
 );setq
 
 
 ;; strip the old point data so the new point data can be added
 (setq e1 (reverse e1))
 (while (= (car (car e1)) 14)
  (setq e1 (cdr e1))
 );while
 
 (setq  e1 (reverse e1)
       lst (mapcar '(lambda (x) (cons 14 x)) lst)
        e1 (append e1 lst)
 );setq
 
;(mapcar 'print e1)
 
 (if (entmake e1)
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defun acet-flatn-wipeout
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-flatn-image ( e1 / na lst p1 x y )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq lst (mapcar 'cdr (acet-list-m-assoc 14 e1)) 		;; get a list of bounding points
       lst (acet-geom-trans-image-to-ucs lst na 1) 		;; translate to current extrusion vector
       lst (mapcar '(lambda (x) (list (car x) (cadr x) 0.0))	;; force z=0.0
                   lst
           );mapcar
        ;p1 (trans (cdr (assoc 10 e1)) 0 1)
        ;p1 (list (car p1) (cadr p1) 0.0)
        ;p1 (trans p1 1 0)
        p1 (acet-point-flat (cdr (assoc 10 e1)) 0 0)
         x (trans (cdr (assoc 11 e1)) 0 1 T)
         x (list (car x) (cadr x) 0.0)
         x (trans x 1 0 T)
         y (trans (cdr (assoc 12 e1)) 0 1 T)
         y (list (car y) (cadr y) 0.0)
         y (trans y 1 0 T)
        e1 (subst (cons 10 p1) (assoc 10 e1) e1)
        e1 (subst (cons 11 x) (assoc 11 e1) e1)
        e1 (subst (cons 12 y) (assoc 12 e1) e1)
        e1 (reverse e1)
 );setq
 (while (= 14 (car (car e1)))
  (setq e1 (cdr e1))
 );while
 (setq  e1 (reverse e1)
       lst (acet-geom-trans-ucs-to-image lst e1 1)
       lst (mapcar '(lambda (x) (cons 14 x)) lst)
        e1 (append e1 lst)
 );setq
 (if (entmake e1)
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defun acet-flatn-image
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a spline and creates a lwpolyline
;
(defun acet-flatn-spline ( e1 / na cxv lst x proplst ss na2 gp xt fuz lst2 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq     cxv (acet-geom-z-axis)
           lst (acet-geom-object-point-list na -1)
           lst (mapcar '(lambda (x) 
                         (acet-point-flat x 1 cxv)
                        )
                        lst
               );mapcar
           ;lst (acet-geom-list-remove-straight-segs lst nil)
            xt (acet-geom-list-extents lst)
            xt (distance (car xt) (cadr xt))
           fuz (acet-geom-calc-arc-error xt) 
       proplst (acet-general-props-get e1)
           na2 (entlast)
 );setq
 
 (cond
  ((= (length lst) 2)
   (setq  gp (acet-general-props-get-pairs e1)   ;; get general properties
          e1 (list '(0 . "LINE") '(100 . "AcDbEntity")
                   (assoc 67 e1) (assoc 410 e1) (assoc 8 e1)
                   '(100 . "AcDbLine")
                   (cons 10 (trans (car lst) 1 cxv))
                   (cons 11 (trans (cadr lst) 1 cxv))
                   (cons 210 cxv)
             );list
          e1 (acet-general-props-set-pairs e1 gp)
   );setq
   (entmake e1)
  );cond #1 draw a line
  ((setq lst2 (acet-geom-point-list-to-arc-list lst fuz))
      (acet-geom-arc-list-to-pline 
        (list (assoc 67 e1) (assoc 410 e1) (assoc 8 e1) (cons 210 cxv))
        lst2
      )
  );cond #2 create a pline with arcs
  ((assoc 210 e1)
   (setq e1 (acet-flatn-orthogonal-object-elist na))
   (entmake e1)
  );cond #3
 );cond close
 
 (if (not (equal na2 (entlast)))
     (progn
      (setq ss (ssadd (entlast) (ssadd)))
      (acet-general-props-set ss proplst)
      (setq na (ssname ss 0))
     );progn
     (setq na nil)
 );if
 na
);defun acet-flatn-spline
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Takes an ellipse and creates a flat version of it.
;;Returns a new ename if successful.
;
(defun acet-flatn-ellipse ( e1 / na cxv xv xv2 par a1 a2 ax1 na2 a xt lst xvda p1 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq cxv (acet-geom-z-axis)
        xv (cdr (assoc 210 e1))
       xv2 (acet-geom-vector-scale xv -1.0)
 );setq
 
 (cond 
  ((equal xv cxv 0.0000001)
   (setq p1 (cdr (assoc 10 e1))
         p1 (acet-point-flat p1 0 0) 
         e1 (subst (cons 10 p1) (assoc 10 e1) e1)
   );setq
   (if (entmake e1)
       (setq na (entlast))
       (setq na nil)
   );if
  );cond #1
  ((equal xv2 cxv 0.0000001)
   (setq   par T
            p1 (cdr (assoc 10 e1))
            p1 (acet-point-flat p1 0 0)
            e1 (subst (cons 10 p1) (assoc 10 e1) e1)
            e1 (subst (cons 210 cxv) (assoc 210 e1) e1) 
            a1 (acet-angle-format (cdr (assoc 41 e1)))
            a2 (acet-angle-format (cdr (assoc 42 e1)))
            a1 (- pi a1)
            a2 (- pi a2)
  );setq
  (if (equal a1 a2 0.00000001)
      (setq a1 (+ a1 pi pi));setq then
  );if
  (setq  e1 (subst (cons 41 a2) (assoc 41 e1) e1)
         e1 (subst (cons 42 a1) (assoc 42 e1) e1)
        ax1 (cdr (assoc 11 e1))
        ax1 (acet-geom-vector-scale ax1 -1.0)		;; reverse the direction of the major axis vector
         e1 (subst (cons 11 ax1) (assoc 11 e1) e1) 
   );setq
 
   (if (entmake e1)
       (setq na (entlast))
       (setq na nil)
   );if
  );cond #2 then ecs is parallel but has negative extrusion so make adjustments
 
  ((and (setq xvda (acet-geom-vector-d-angle xv cxv))
        (equal (/ pi 2.0) xvda 0.00000001)
   );and
	;;orthogonal
   (setq e1 (acet-flatn-orthogonal-object-elist na))
   (if (entmake e1)
       (setq na (entlast))
       (setq na nil)
   );if
  );cond #3
 
  (T
   (setq lst (acet-geom-object-point-list na -1)
         lst (mapcar '(lambda (x) (acet-point-flat x 1 cxv)) lst)
          xt (acet-geom-list-extents lst)
          xt (distance (car xt) (cadr xt))
          xt (acet-geom-calc-arc-error xt)
         lst (acet-geom-point-list-to-arc-list lst xt)
          e1 (subst (cons 210 cxv) (assoc 210 e1) e1)
   );setq
   (setq na (acet-geom-arc-list-to-pline e1 lst))
  );cond #4
 );cond close
 na
);defun acet-flatn-ellipse
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Takes an ellipse and creates a flat version of it.
;;Returns a new ename if successful.
;
(defun acet-flatn-point ( e1 / na p1 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq p1 (cdr (assoc 10 e1))
       p1 (acet-point-flat p1 0 0)
       e1 (subst (cons 10 p1) (assoc 10 e1) e1)
 );setq
 (if (entmake e1)
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defun acet-flatn-point
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-flatn-hatch ( e1 / na na5 e2 lst na2 e2 loop loop2 ss na3 xv cxv xvda )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (if (not acet-hatch-remake)
     (load "hatchutil.lsp" "")
 );if
 (setq  xv (cdr (assoc 210 e1))
       cxv (acet-geom-z-axis)
       na5 (entlast)
 );setq
 
 (cond
  ((and (setq xvda (acet-geom-vector-d-angle xv cxv))
        (equal (/ pi 2.0) xvda 0.00000001)
   );and
	;; it's orthogonal so just create a line
   (setq e1 (acet-flatn-orthogonal-object-elist na))
   (entmake e1)
  );cond #1
 
  (T
   (if (= 1 (cdr (assoc 71 e1)))
       (progn
        ;; make the hatch invisible while working with it
        ;(if (assoc 60 e1)
        ;    (setq e1 (subst '(60 . 1) (assoc 60 e1) e1))
        ;    (setq e1 (append e1 '((60 . 1))))
        ;);if
   
        ;; first create a flat hatch with boundaries created from raw hatch data.
        ;; This will get the hatch to update and display in the proper location.
        ;; Then swap in the flat versions of the original boundary objects and 
        ;; don't touch it.
   
        (setq lst (acet-hatch-boundary-enames e1) ;; the original boundaries
              na2 (acet-flatn-hatch-make e1)	;; new hatch with boundaries created via acet-hatch-loop-make
               e2 (entget na2)
        );setq
   
        ;; create flat boundary objects and update the boundary loop list.
        (foreach loop lst
         (setq loop2 nil)
         (foreach edge loop
          (if (entget edge)
              (setq    ss (acet-flatn-object (list edge T T)) ;; wmf is ok and newss also
                       ss (cadr ss) ;; newss
                      na3 (ssname ss 0)
                    loop2 (cons na3 loop2)
              );setq then
              (acet-alert "hatch edge was deleted")
          );if
         );foreach
         (setq loop2 (reverse loop2) 
                 lst (subst loop2 loop lst)
         );setq
        );foreach
   
        (setq ss (acet-hatch-boundary-ss e2)
              e2 (acet-hatch-edge-id-remove-all e2)   ;; remove the 330 edge references
              e2 (subst (cons 60 0) (assoc 60 e2) e2) ;; make it visible again
        );setq
   
  ;(setq rke2 e2)
  ;(setq rklst lst)
  ;(getstring "hey")
   
        (acet-hatch-boundary-assoc-db e2 lst T)
        (acet-ss-entdel ss)
   
       );progn then its an associative hatch
       (progn
  	;; try to create a non-associative hatch
  	;; and if that fails create an anonymous block from a copy
        (if (setq na3 (acet-flatn-hatch-make na))
            (progn
             (setq e1 (entget na3)
                   ss (acet-hatch-boundary-ss e1)
             );setq
             (acet-ss-entdel ss) ;; delete the temp boundary objects
            );progn then
            (progn
             ;; create an anonymous block from an exploded copy
             (acet-flatn-hatch-make-anon na)
            );progn then could not create a non-assoc hatch
        );if
       );progn else not associative
   );if
 
  );cond #2
 );cond close
 
 (if (not (equal na5 (entlast)))
     (setq ss (acet-ss-new na5))
     (setq ss nil)
 );if
 
 ss
);defun acet-flatn-hatch
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch ename or elist and creates a flat version of the hatch along with 
;new flat boundary objects.
;Returns the ename of the new hatch if successful
;
(defun acet-flatn-hatch-make ( e1 / na lst loop loop2 edge ss )
 
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq lst (acet-hatch-loop-make e1)		;; make the boundary edges
        e1 (acet-elist-add-defaults e1)
 );setq
 (entmake e1)					;; make a copy of the hatch
 (setq  na (entlast)
        e1 (entget na)
 );setq
 
;(getstring "hey")
 ;; create flat versions of the boundaries and delete the others
 (foreach loop lst
  (setq loop2 nil)
  (foreach edge loop
 
;(redraw edge 3)
;(print (entget edge))
;(getstring "hey dudue")
 
   (setq ss (acet-flatn-object (list edge T T)));setq
 
;(print ss)(print "")
 
   (setq ss (cadr ss));setq ;; newss
 
;(print ss)(print "")
;(getstring "again")
 
   (if ss
       (setq loop2 (append loop2 (acet-ss-to-list ss)))
   );if
   (entdel edge)
  );foreach edge
  (setq lst (subst loop2 loop lst)) ;put the new flat objects in the loop lists
 );foreach loop
;(getstring "hey2")
 
 
 (setq e1 (acet-flatn-hatch-mod-pat e1)			;; modify the pattern data and seed points
       e1 (acet-hatch-edge-id-remove-all e1) 		;; remove the boundary object references
 );setq
 
 
 (acet-hatch-boundary-assoc-db e1 lst nil)		;; add the new ones
 
 (if (not (acet-hatch-update na))
     (progn
      ;; then the update failed so abort and delete the boundary objects and the hatch copy
      (setq e1 (entget na)
            ss (acet-hatch-boundary-ss e1)
      );setq
      (if ss
          (acet-ss-entdel ss)
      );if
      (entdel na)
      (setq na nil)
     );progn
 );if 
 
 na
);defun acet-flatn-hatch-make
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch pattern and modifies the pattern portion of the data
;and returns the modified elist.
;
(defun acet-flatn-hatch-mod-pat ( e1 / na cxv xv e2 pata sca alst bxlst bylst oxlst oylst dlst 
                                       lst a dlst2 e3 z p1 p2 p3 of n j da d df name lst2
                                )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq   cxv (acet-geom-z-axis)
          xv (cdr (assoc 210 e1))
          p1 (cdr (assoc 10 e1))			;; elevation point
           z (caddr p1)
          p1 (acet-point-flat p1 xv cxv)
          p1 (list 0.0 0.0 (caddr p1))
          e1 (subst (cons 210 cxv) (assoc 210 e1) e1)	;; new extrusion vector
          e1 (acet-list-put-nth 			;; new elevation
               (cons 10 p1)
               e1
               (vl-position (assoc 10 e1) e1)
             )
          e1 (reverse e1)
 );setq
 (while (= (car (car e1)) 10)
  (setq p1 (cdr (car e1))
        p1 (list (car p1) (cadr p1) z) 		;; put the proper z value in
        p1 (acet-point-flat p1 xv cxv)
        p1 (list (car p1) (cadr p1) 0.0)	;; force z=0
        e2 (cons (cons 10 p1) e2)
        e1 (cdr e1)
  );setq
 );while
 (setq e1 (append (reverse e1) (reverse e2))
       e2 nil
 );setq
 
 (if (/= 1 (cdr (assoc 70 e1)))
     (setq  name (cdr (assoc 2 e1))
            name (strcat "ACET-2D-" name)  
              e1 (subst (cons 2 name) (assoc 2 e1) e1)		;; put new name in so the translated pattern data is used
              e2 (member (assoc 75 e1) e1)			;; get to the pattern data
            pata (cdr (assoc 52 e2)) 				;; pattern angle
            pata (acet-geom-angle-trans pata xv cxv)
             sca (cdr (assoc 41 e2))				;; pattern scale/spacing
            alst (mapcar 'cdr (acet-list-m-assoc 53 e2))	;; line angles
           bxlst (mapcar 'cdr (acet-list-m-assoc 43 e2))	;; line x basepoint
           bylst (mapcar 'cdr (acet-list-m-assoc 44 e2))	;; line y basepoint
           oxlst (mapcar 'cdr (acet-list-m-assoc 45 e2))	;; line x offsets
           oylst (mapcar 'cdr (acet-list-m-assoc 46 e2))	;; line y offsets
            dlst (mapcar 'cdr (acet-list-m-assoc 79 e2))	;; number of dash lengths
     );setq then not a solid hatch
 );if
 
 (foreach d dlst
  (setq  e2 (member (cons 79 d) e2)
        lst nil
  );setq
  (repeat d
   (setq   a (assoc 49 e2)
         lst (cons (cdr a) lst)
          e2 (cdr (member a e2))
   );setq
  );repeat
  (setq   lst (reverse lst)
        dlst2 (cons lst dlst2)
  );setq
 );foreach 
;(print e1)
;(getstring "hey2")
 
 (if (/= 1 (cdr (assoc 70 e1)))					;; not solid fill pattern
     (progn
      (setq dlst (reverse dlst2)				;; list of dash-sublists
               a (assoc 78 e1)					;; marks start of pattern data 
              e2 (cdr (member a e1))				;; from pattern line data to the end
      );setq
      (if (assoc 47 e2)
          (setq e3 (member (assoc 47 e2) e2))			;; the end portion - pixel size and seeds
          (setq e3 (member (assoc 98 e2) e2))			;; the end portion - just seeds
      );if    
      (setq e1 (reverse (member a (reverse e1)))		;; the first portion - before pattern line data
            e1 (subst (cons 52 pata) (assoc 52 e1) e1)
      );setq
     );progn then
 );if
;(print e1)
;(getstring "hey3")
 
 (setq e1 (reverse e1))
 (setq n 0)
 (repeat (length alst)
;; (getstring "hey4")
 
  (setq    a (nth n alst)
          p1 (list (nth n bxlst) (nth n bylst) z)
          p3 (polar p1 a 1.0)					;; temp point used to get ratio for new dash lengths
          of (list (nth n oxlst) (nth n oylst) 0.0)		;; x and y offset to next parallel line
          p2 (acet-geom-vector-add p1 of)			;; a point on that line
          p1 (acet-point-flat p1 xv cxv)
          p2 (acet-point-flat p2 xv cxv)
          p3 (acet-point-flat p3 xv cxv)
          df (distance p1 p3)					;; dash factor
           a (acet-geom-angle-trans a xv cxv)			;; translate the angle
          p3 (polar p1 a 1.0)					;; a point on this line
          da (acet-geom-vector-d-angle 				;; angle between this line and the offset vector
               (acet-geom-delta-vector p1 p3)                      
               (acet-geom-delta-vector p1 p2)
             )
          of (* (distance p1 p2) (sin da))			;; the new offset distance
          p2 (polar p1 (+ a (/ pi 2.0)) of)			;; generate the new offset point
          p2 (acet-geom-delta-vector p1 p2)			;; change it to a vector
         lst (nth n dlst)
        lst2 nil
  );setq
;; (getstring "hey5")
  ;; add the hatch data for this line
  (setq e1 (cons (cons 53 a)            e1)
        e1 (cons (cons 43 (car p1))     e1)
        e1 (cons (cons 44 (cadr p1))    e1)
        e1 (cons (cons 45 (car p2))     e1)
        e1 (cons (cons 46 (cadr p2))    e1)
        e1 (cons (cons 79 (length lst)) e1)
  );setq
  ;; adjust the dash lengths
  (setq j 0)
  (repeat (length lst)
   (setq  d (nth j lst)
          d (* df d)
          d (cons 49 d)
         e1 (cons d e1)
   );setq
  (setq j (+ j 1));setq
  );repeat
 
 (setq n (+ n 1));setq
 );repeat
 (setq e1 (reverse e1)
       e1 (append e1 e3)
 );setq
 
 e1
);defun acet-flatn-hatch-mod-pat
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Create and anonymous block from a copy of the provided hatch
;Returns the ename of the new block insert if successful
;
(defun acet-flatn-hatch-make-anon ( e1 / na ss bna ss2 e2 flag )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq e1 (acet-elist-add-defaults e1))
 (if (entmake e1)					;; make a copy of the hatch
     (setq na (entlast)
           e1 (entget na)
     );setq then
 );if
 (if (and (/= 1 (cdr (assoc 70 e1))) 			;; not a solid fill hatch
          (setq ss (acet-inherit-xplode na nil))
     );and
     (progn
      (setq ss2 (acet-flatn-objects (list ss T T))
            ss2 (cadr ss2)
      );setq
      (acet-ss-entdel ss) ;delete the old versions.
      (setq bna (acet-block-make-anon ss2 nil)) ;make an anonymous block fomr the flat elements
      (acet-ss-entdel ss2)
      (setq e2 (list '(0 . "INSERT")
                     '(100 . "AcDbEntity")
                     (assoc 67 e1)
                     (assoc 410 e1)
                     (assoc 8 e1)
                     '(100 . "AcDbBlockReference")
                     (cons 2 bna)
                     '(10 0.0 0.0 0.0)
                     '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
                     '(50 . 0.0)
                     '(70 . 0)
                     '(71 . 0)
                     '(44 . 0.0)
                     '(45 . 0.0)
                     '(210 0.0 0.0 1.0)
               );list
      );setq
      (if (entmake e2)
          (setq flag (entlast))
      );if
     );progn then
     (entdel na)
 );if
 flag
);defun acet-flatn-hatch-make-anon
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-flatn-insert ( e1 / na bna na2 e2 a na2 ss n p1 xv bna2 attlst )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq bna (cdr (assoc 2 e1))
       na2 (tblobjname "block" bna)
        e2 (entget na2)
         a (cdr (assoc 70 e2))
 );setq
 (cond
  ((member '(100 . "AcDbMInsertBlock") e1)	;; minsert
   (setq na2 (entlast))
   (setq ss (acet-minsert-to-inserts na))
   (if ss
       (progn
        (setq n 0)
        (repeat (sslength ss)
         (setq na (ssname ss n))
         (acet-flatn-insert na)
         (setq n (+ n 1));setq
        );repeat
       );progn then
   );if
   (acet-ss-entdel ss)
   (if (setq ss (acet-ss-new na2))
       (progn
           (setq bna (strcat bna "-flat-"))
           (setq n 1)
           (while (tblobjname "block" (strcat bna (itoa n)))
            (setq n (+ n 1))
           );while
           (setq bna (strcat bna (itoa n))
                  xv (cdr (assoc 210 e1))
                  p1 (cdr (assoc 10 e1))
                  p1 (acet-point-flat p1 xv 1)
           );setq
           (command "_.-block" bna p1 ss "")
           (command "_.-insert" bna p1 "1" "1" "0")
           (setq na (entlast))
       );progn
       (setq na nil)
   );if
  );cond #1
 
  ((= 4 (logand 4 a))
   (setq na (acet-flatn-xref e1))	;; xref
  );cond #2
 
  (T
      ;; Make a copy and explode it and then flatten each of the new objects and delete 
      ;; the non-flat ones from the explode.
      (command "_.copy" na "" "0,0" "0,0")
      (setq    na2 (entlast)
            attlst (acet-insert-attrib-get na2)
                ss (acet-inherit-xplode na2 nil) ;;the nil used to be T to convert attdefs to text 5:37 PM 4/10/00
      );setq
 
      (acet-flatn-objects (list ss T nil)) ;; wmf when needed
      (acet-ss-entdel ss)
 
      ;; Define a new block from the flat objects and create an insert of of it
      (setq ss (acet-ss-new na2))
      (if (equal "*" (substr bna 1 1))
          (progn
           (setq bna2 (acet-block-make-anon ss nil))
           (acet-ss-entdel ss)
           (setq e2 e1
                 e2 (subst (cons 2 bna2) (assoc 2 e2) e2)
                 e2 (subst '(10 0.0 0.0 0.0) (assoc 10 e2) e2)
                 e2 (subst '(50 . 0.0) (assoc 50 e2) e2)
                 e2 (subst '(41 . 1.0) (assoc 41 e2) e2)
                 e2 (subst '(42 . 1.0) (assoc 42 e2) e2)
                 e2 (subst '(43 . 1.0) (assoc 43 e2) e2)
                 e2 (subst '(210 0.0 0.0 1.0) (assoc 210 e2) e2)
           );setq
           (entmake e2)
          );progn then anonymous
          (progn
           (acet-flatn-insert-preprocess-text ss)
           ;; get a unique name    
           (setq bna (strcat bna "-flat-"))
           (setq n 1)
           (while (tblobjname "block" (strcat bna (itoa n)))
            (setq n (+ n 1))
           );while
           (setq bna (strcat bna (itoa n))
                  xv (cdr (assoc 210 e1))
                  p1 (cdr (assoc 10 e1))
                  p1 (acet-point-flat p1 xv 1)
           );setq
           (command "_.-block" bna p1 ss "")
           (command "_.-insert" bna p1 "1" "1" "0")
          );progn then
      );if
      (setq na (entlast))
      (if attlst
          (acet-insert-attrib-set na attlst T)
      );if
  );cond #3
 );cond close
 
 na
);defun acet-flatn-insert
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is a work-around for an AutoCAD bug.
;attdefs with Middle justification go wacky when blocks are inserted in non-world 
;coords. so we change these to "start" justification to avoid problems.
;
(defun acet-flatn-insert-preprocess-text ( ss / n na e1 kword )
 (setq n 0)
 (repeat (sslength ss)
  (setq na (ssname ss n)
        e1 (entget na)
  );setq
  (if (and (or (= "ATTDEF" (cdr (assoc 0 e1)))
               (= "TEXT" (cdr (assoc 0 e1)))
           );or
           (setq kword (acet-tjust-keyword e1))
           (= "MID" (xstrcase (substr kword 1 3)))
      );and
      (acet-tjust (ssadd na (ssadd)) "Start")
  );if
  (setq n (+ n 1));setq
 );repeat
 ss
);defun acet-flatn-insert-preprocess-text
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes an an xref insert and creates a flat block insert. (new block def)
;Returns ename of new insert entity.
;
(defun acet-flatn-xref ( e1 / na bna tmp na2 ss n xv p1 e2 bna2 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq bna (cdr (assoc 2 e1))
       tmp (vl-filename-mktemp "ACET" (getvar "tempprefix") ".dwg")
        xv (cdr (assoc 210 e1))
        p1 (cdr (assoc 10 e1))
        p1 (acet-point-flat p1 xv 1)
 );setq
 ;; get a unique name    
 (setq bna2 (strcat bna "-xref-"))
 (setq n 1)
 (while (tblobjname "block" (strcat bna2 (itoa n)))
  (setq n (+ n 1))
 );while
 (setq bna2 (strcat bna2 (itoa n)));setq


 ;; bind the xref and then wblock the bound version to a temp file 
 ;; and undo to bring back original state
 (command "_.-xref" "_bind" bna)

 (setq e2 (entget (tblobjname "block" bna)))
 (if (/= 4 (logand 4 (cdr (assoc 70 e2))))
     (progn
      (acet-sysvar-set (list "thumbsave" 0)) ;turn thumbnail creation off before wblock.
      (command "_.-wblock" tmp bna)
      (acet-cmd-exit)
      (acet-sysvar-restore)
      (command "_.undo" 2)
 
      ;; temporarily insert the block to import the definition and then delete it.
      (command "_.-insert" (strcat bna2 "=" tmp) "0,0" 1 1 0)
      (entdel (entlast))
      (vl-file-delete tmp)
 
      ;; now entmake an insert of the new block in the proper location
      (setq e1 (subst (cons 2 bna2) (assoc 2 e1) e1)) ;; swap in the new name and entmake
      (entmake e1)
 
      (setq na2 (entlast)) 
      (setq na (acet-flatn-insert na2))  ;;now call flatn on the new insert
 
      (entdel na2) ;; delete the temp block
      (acet-table-purge "block" bna2 T)
     );progn then bind succeeded
     (setq na nil);setq else
 );if
 na
);defun acet-flatn-xref
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a dimension or a leader
;
(defun acet-flatn-dim ( e1 / na xv xv2 cxv p1 p2 na2 )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq  xv (cdr (assoc 210 e1))
       cxv (acet-geom-z-axis)
       na2 (entlast)
 );setq
 (if (acet-geom-vector-parallel cxv xv)
     (progn
      (setq p1 (cdr (assoc 10 e1))
            p1 (trans p1 0 1)
            p2 (acet-point-flat p1 1 1)
      );setq
      (command "_.copy" na "" p1 p2)
     );progn then
     (progn
      (acet-flatn-unknown e1 T) ;; wmf is ok
     );progn else
 );if
 (acet-ss-new na2)
);defun acet-flatn-dim
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes an mline entity 
;
(defun acet-flatn-mline ( e1 / na xv xv2 cxv p1 p2 na2 ss xvda )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq  xv (cdr (assoc 210 e1))
       cxv (acet-geom-z-axis)
       na2 (entlast)
 );setq
 (cond
  ((acet-geom-vector-parallel cxv xv)
      (setq p1 (cdr (assoc 10 e1))
            p1 (trans p1 0 1)
            p2 (acet-point-flat p1 1 1)
      );setq
      (command "_.copy" na "" p1 p2)
  );cond #1
  ((and (setq xvda (acet-geom-vector-d-angle xv cxv))
        (equal (/ pi 2.0) xvda 0.00000001)
   );and
   (setq e1 (acet-flatn-orthogonal-object-elist na))
   (entmake e1)
  );cond #2
  (T
    (acet-flatn-unknown e1 T) ;; wmf is ok
  );cond #3
 );cond close
 
 (setq ss (acet-ss-new na2))
 (if (and ss
         (= (sslength ss) 0)
     )
     (setq ss nil)
 );if
 ss
);defun acet-flatn-mline
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-flatn-tolerance ( e1 / na xv xv2 cxv p1 p2 na2 xvda ss )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq  xv (cdr (assoc 210 e1))
       cxv (acet-geom-z-axis)
       na2 (entlast)
 );setq
 (cond
  ((acet-geom-vector-parallel cxv xv)
      (setq p1 (cdr (assoc 10 e1))
            p1 (trans p1 0 1)
            p2 (acet-point-flat p1 1 1)
      );setq
      (command "_.copy" na "" p1 p2)
  );cond #1
  ((and (setq xvda (acet-geom-vector-d-angle xv cxv))
        (equal (/ pi 2.0) xvda 0.00000001)
   );and
   (setq e1 (acet-flatn-orthogonal-object-elist na))
   (entmake e1)
  );cond #2
  (T
     (acet-wmf-convert (list na 3 nil nil));else
  );cond #3
 );cond close
 (setq ss (acet-ss-new na2))
 (if (and ss
         (= (sslength ss) 0)
     )
     (setq ss nil)
 );if
 ss
);defun acet-flatn-tolerance
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes an xline or ray entity 
(defun acet-flatn-xline ( e1 / na p1 p2 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq p1 (cdr (assoc 10 e1))
       p1 (acet-point-flat p1 0 0)
       p2 (cdr (assoc 11 e1))
       p2 (acet-point-flat p2 0 0)
       e1 (subst (cons 10 p1) (assoc 10 e1) e1) 
       e1 (subst (cons 11 p2) (assoc 11 e1) e1) 
 );setq
 (if (entmake e1)
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defun acet-flatn-xline
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns true if the entity displays any thickness when view is plan to the current ucs.
;
(defun acet-flatn-object-has-thickness ( e1 / na th cxv xv tp a )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (and (setq tp (cdr (assoc 0 e1)))
      (/= tp "LINE")
      (setq th (cdr (assoc 39 e1)))
      (/= 0.0 th)							;; has a thickness gc
      (setq cxv (acet-geom-z-axis))
      (setq xv (cdr (assoc 210 e1)))
      (not (equal xv cxv 0.00000001))					;; not parallel to current view
      (not (equal (acet-geom-vector-scale xv -1.0) cxv 0.00000001))
      (or (and (/= tp "TEXT")						;; not text or the text is .shx
               (/= tp "ATTDEF")
               (/= tp "ATTRIB")
          );and
          (and (setq na (tblobjname "style" (cdr (assoc 7 e1))))
               (setq e1 (entget na))
               (setq a (cdr (assoc 3 e1)))
               (acet-str-equal ".shx" (acet-filename-extension a))
          );and
      );or 
 );and
);defun acet-flatn-object-has-thickness
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find all associative hatches within the selection set and get the edges they point to.
;; Remove those edges from the selection because they will automatically get picked up when 
;; the hatch gets processed.
(defun acet-flatn-remove-hatch-edges ( ss / n na e1 lst na2 ss2 ss3 )
 
 ;; get a selection set of hatch edges
 (setq ss2 (ssadd))
 (setq n 0)
 (repeat (sslength ss)
  (setq na (ssname ss n)
        e1 (entget na)
  );setq
  (if (and (equal "HATCH" (cdr (assoc 0 e1)))	;; assocative hatch 
           (= 1 (cdr (assoc 71 e1)))
      );and
      (progn
       (setq lst (acet-hatch-boundary-enames e1)) ;; get the boundary enames and remove them from ss
       (setq lst (apply 'append lst))
       (foreach na2 lst
        (if (ssmemb na2 ss)
            (setq ss2 (ssadd na2 ss2))
        );if
       );foreach
      );progn then
  );if
  (setq n (+ n 1));setq
 );repeat
 
 ;; build a new selection set without those edges
 (setq ss3 (ssadd))
 (if (= 0 (sslength ss2))
     (setq ss3 ss)
     (progn
      (setq n 0)
      (repeat (sslength ss)
      (setq na (ssname ss n))
      (if (not (ssmemb na ss2))
          (setq ss3 (ssadd na ss3));setq then
      );if
      (setq n (+ n 1));setq
      );repeat
     );progn else
 );if
 
 ss3
);defun acet-flatn-remove-hatch-edges
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes an entity name and a flag where T means convert attributes to text.
;
(defun acet-inherit-xplode ( na att2txt / na2 e1 e2 
                                  col lt lay lw psty ss n ss2 
                                  col2 lt2 lay2 lw2 psty2 laycol
                           )
 
  (setq   na2 na
           e2 (entget na2)
           e2 (acet-elist-add-defaults e2)
          col (cdr (assoc 62 e2))		;; color
           lt (cdr (assoc 6 e2))		;; ltype
          lay (cdr (assoc 8 e2))		;; layer
           lw (cdr (assoc 370 e2))		;; lineweight
         psty (cdr (assoc 390 e2))		;; plot style
       laycol (entget (tblobjname "layer" lay))	;; insert's layer color
  );setq
  (if (not psty)
      (setq psty (cdr (assoc 380 e2)));setq then
  );if
  (if (and att2txt
           (= "INSERT" (cdr (assoc 0 e2)))
           (= 1 (cdr (assoc 66 e2)))
      );and
      (setq ss2 (acet-insert-attribs-to-text na))
  );if
  (if (and (not (equal "TOLERANCE" (cdr (assoc 0 e2))))
           (setq ss (acet-explode na))
      );and
      (progn
       (setq n 0)
       (repeat (sslength ss)
        (setq    na (ssname ss n)
                 e1 (entget na)
                 e1 (acet-elist-add-defaults e1)
               col2 (cdr (assoc 62 e1))
                lt2 (cdr (assoc 6 e1))
               lay2 (cdr (assoc 8 e1))
                lw2 (cdr (assoc 370 e1))
              psty2 (cdr (assoc 390 e1))
        );setq
        (if (not psty2)
            (setq psty2 (cdr (assoc 380 e1)));setq then
        );if
        (if (and col
                 (= col2 0) ;; byblock
            );and
            (progn
             (if (and (= col2 0)  ;; byblock
                      (= col 256) ;; insert is bylayer
                 );and
                 (setq e1 (subst (assoc 62 laycol) ;; use the color of the layer
                                 (assoc 62 e1)
                                 e1
                          )
                 );setq
                 (setq e1 (subst (cons 62 col) (assoc 62 e1) e1)) ;; else use the color of the insert
             );if
            );progn then
        );if
        (if (and lay
                 (= lay2 "0")
                 (= col2 256) ;; bylayer
            );and
            (setq e1 (subst (cons 8 lay) (assoc 8 e1) e1))
        );if
        (if (and lt
                 lt2
                 (acet-str-equal lt2 "BYBLOCK")
            );and
            (setq e1 (subst (cons 6 lt) (assoc 6 e1) e1))
        );if
        (if (and lw
                 (= lw2 -2) ;; byblock lineweight
            );and
            (setq e1 (subst (cons 370 lw) (assoc 370 e1) e1))
        );if
        (if (and psty
                 (= psty2 1) ;; byblock (as far as I can tell)
            );and
            (progn
             (if (equal (type psty) 'ENAME)
                 (setq e1 (subst (cons 390 psty) (assoc 380 e1) e1))
                 (setq e1 (subst (cons 380 psty) (assoc 380 e1) e1))
             );if
            );progn then
        );if
        
        (if (and att2txt
                 (= "ATTDEF" (cdr (assoc 0 e1)))
            );and
            (progn
             (entdel na)
             (setq ss (ssdel na ss)
                    n (- n 1)
             );setq
            );progn then
            (entmod e1)
        );if
        (setq n (+ n 1));setq
       );repeat
       (if ss2 
           (progn
            (setq n 0)
            (repeat (sslength ss2)
             (setq ss (ssadd (ssname ss2 n) ss));setq then
             (setq n (+ n 1));setq
            );repeat
           );progn then add ss2 to ss
       );if
      );progn then
      (progn
       (if ss2
           (acet-ss-entdel ss2) ;; the explode failed so clean up any converted attribs
       );if
      );progn else explode failed or it's a tolerance
  );if
  ss
);defun acet-inherit-xplode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes an ename of an atttrib and creates a text object that looks just like it.
;Returns a new ename if successful.
;
(defun acet-attrib-to-text ( na  / lst e1 x )
 (setq lst (entget na)
       lst (acet-elist-add-defaults lst)
 );setq
 (foreach x lst
  (cond
   ((= 0 (car x))
    (setq x (cons 0 "TEXT"))
   )
   ((or (= 100 (car x))
        (= 2 (car x))
        (= 3 (car x))
        (= 70 (car x))
        (= 73 (car x))
    )
    (setq x nil)
   )
   ((= 74 (car x))
    (setq x (cons 73 (cdr x)))
   )
  );cond close 
  (if x
      (setq e1 (cons x e1))
  )
 );foreach
 (if (entmake (reverse e1))
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defdun acet-attrib-to-text
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-insert-attribs-to-text ( na / e1 ss na2 )
 (if (and na
          (setq e1 (entget na))
          (= "INSERT" (cdr (assoc 0 e1)))
          (= 1 (cdr (assoc 66 e1)))
     );and
     (progn
      (setq ss (ssadd))
      (while (and (setq na (entnext na))
                  (setq e1 (entget na))
                  (/= "SEQEND" (cdr (assoc 0 e1)))
             );and
       (if (setq na2 (acet-attrib-to-text na))
           (setq ss (ssadd na2 ss))
       );if
      );while
     );progn then
 );if
 ss
);defun acet-insert-attribs-to-text
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Convert an minsert (multiple insert) object to several individual inserts and returns a 
; selection set of new inserts. NOTE: Does not delete the original
;
(defun acet-minsert-to-inserts ( na / e1 cc rc dx dy x y z ss p1 na2 e2 elst n c r ang dxv dyv )
 
 (entmake nil)
 (setq  e1 (entget na)
        cc (cdr (assoc 70 e1)) ;column count
        rc (cdr (assoc 71 e1)) ;row count
        dx (cdr (assoc 44 e1)) ;column spacing
        dy (cdr (assoc 45 e1)) ;row spacing
       ang (cdr (assoc 50 e1))
       dxv (polar '(0.0 0.0 0.0) ang 1.0)			;; dx direction vector
       dyv (polar '(0.0 0.0 0.0) (+ ang (/ pi 2.0)) 1.0)	;; dy direction vector
       
        e1 (subst '(70 . 0) (assoc 70 e1) e1)
        e1 (subst '(71 . 0) (assoc 71 e1) e1)
        e1 (subst '(44 . 0.0) (assoc 44 e1) e1)
        e1 (subst '(45 . 0.0) (assoc 45 e1) e1)
        e1 (subst '(100 . "AcDbBlockReference") '(100 . "AcDbMInsertBlock") e1)
        ss (ssadd)
 );setq
 (if (not cc)
     (setq cc 0)
 );if
 (setq elst (list e1))
 
 (if (= 1 (cdr (assoc 66 e1)))
     (progn
      (setq na2 na)
      (while (and (setq na2 (entnext na2))
                  (setq e2 (entget na2))
                  (/= "SEQEND" (cdr (assoc 0 e2)))
             );and
       (setq elst (append elst (list e2)));setq
      );while
      (if (= "SEQEND" (cdr (assoc 0 e2)))
          (setq elst (append elst (list e2)));setq
      );if
     );progn then attribs follow
 );if
 
 (setq c 0)
 (repeat cc ;; columns
 
  (setq r 0)
  (repeat rc	;; rows
 
   (setq n 0)
   (repeat (length elst)	;; insert and any attribs
    (setq e1 (nth n elst))
    (if (setq p1 (cdr (assoc 10 e1)))
        (setq x (acet-geom-vector-scale dxv (* c dx)) 
              y (acet-geom-vector-scale dyv (* r dy)) 
              p1 (list 10
                       (+ (car p1) 
                          (car x)
                          (car y)
                       )
                       (+ (cadr p1) 
                          (cadr x)
                          (cadr y)
                       )
                       (+ (caddr p1) 
                          (caddr x)
                          (caddr y)
                       )
                 )
              e1 (subst p1 (assoc 10 e1) e1) 
        );setq then
    );if
    (if (and (entmake e1)
             (= (+ n 1) (length elst))
        );and
        (setq ss (ssadd (entlast) ss));setq then
    );if
    (setq n (+ n 1));setq
   );repeat
 
   (setq r (+ r 1));setq
  );repeat
 
  (setq c (+ c 1));setq 
 );repeat
 
 (if (= (sslength ss) 0)
     (setq ss nil)
 );if
 ss 
);defun acet-minsert-to-inserts


(acet-autoload2	'("acet-wmf.lsp"	(acet-flatn-pline-adjust-widths e1 xv)))
(acet-autoload2	'("acet-wmf.lsp"	(acet-geom-arc-list-to-pline e1 alst)))
(acet-autoload2	'("acet-wmf.lsp"	(acet-geom-point-list-to-arc-list lst fuz)))
(acet-autoload2	'("acet-wmf.lsp"	(acet-wmf-convert alst)))
(acet-autoload2	'("hatchutil.lsp"	(acet-hatch-boundary-assoc-db e1 lst updateOK)))
(acet-autoload2	'("hatchutil.lsp"	(acet-hatch-boundary-enames e1)))
(acet-autoload2	'("hatchutil.lsp"	(acet-hatch-boundary-ss e1)))
(acet-autoload2	'("hatchutil.lsp"	(acet-hatch-edge-id-remove-all e1)))
(acet-autoload2	'("hatchutil.lsp"	(acet-hatch-update e1)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBSdu4S7Y07S48KK/4SspWqkNngpsqFVt5sLblo
;;; 8d57GrpuMkF6p3NPaoPWc2j+Oz04T6oVQEDPhWJKDwxaginHd+osozeRd1NzS2mw
;;; adlO0+pDXX4l8K1voYga3g5agjKddr69rptwpmiKKpR+pey8Y519itUdRcA2hGHU
;;; Nsl+ScJSyUSjlDu7cuIWSl3S4bbyvzNKf9dcIiMQ3SjaqAEqrcw3/mD7ANlcqFsP
;;; cS81k0WLQt9ok/rrcahE5pIwGt+c5ZkUU4JPVxC7kZ6OiTZ6HTzUnEaFqmMjyBVS
;;; alK3BMF3HNpgr9fZxwUqQENvP+kZfD46NysALh2+mEnx3Jg52glxgTMjT5KD0v60
;;; EsNYJLdZpWCBWpA82X8Bgrub8FtZ4oxt8X+mJX2kIUjCaOUVUDzigIkBBWC76tg5
;;; wKo8HmUQrrCHX51MyRaGO2g7w4Gi1bmkedJeWhjdj11CqQiPpTMCXQZm4CMAlmrj
;;; t09HgpwnJB4CdS+yKgdZio9CD/kaK/RPdXcZcNjuVkVcWPMg3RWFDZuc89/KRMZz
;;; jfJrvDOuIpb4VrRR8mUL66ROcDpqUuKYMXeoUJaTheGGb976J+OvOp8f+nDfPINz
;;; WbXY35Vq0gf9i10D4yAPHHrr78hplxs6icFcUNBpr1lgEunSarK2SIv95WJH/0So
;;; 04EGvQ==
;;; -----END-SIGNATURE-----