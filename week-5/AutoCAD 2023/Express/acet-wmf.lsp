;;
;;  Acet-wmf.lsp - Utility functions used by flatten.lsp and others.
;;                 
;;
;;  Copyright © 1999 by Autodesk, Inc.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Uses wmfout and wfmin to convert to two dimensional objects.
;Takes:
;ss   - a selection set 
;disp - a display update flag, 
;         0 or nil= means do nothing, 
;         1= means use the move command to force a display update
;         2= perform a hide prior to wmfout operations.
;         3= scale each object to max size possible before wmf 
;
;zoom - T means zoom to the extents of the selected objects before hand
;
;plretain - T means attempt to retain width information for plines that are either 
;           parallel to the plane or have 0 thickness.
;
;Returns a selection set of new objects created.
;
;color info:
; 188 - looks like darkest color
; 121 - probably the lightest
;
(defun acet-wmf-convert ( alst / lst n ss disp zoom plretain 
                                 lok tmp blklst stylst p1 p2 na2 plst ss 
                                 na e1 a proplst ss2 lst2 lulst orig:ss
                        )
 
 ;; extract the arguments from the argument list
 (setq lst '(ss disp zoom plretain))
 (setq n 0)
 (repeat (min (length alst) (length lst))
  (set (nth n lst) (nth n alst))
  (setq n (+ n 1));setq
 );repeat
 (if (equal (type ss) 'ENAME)
     (setq ss (ssadd ss (ssadd))) ;; convert to a selection set
 );if
 
 ;; Create a temp copy of the objects in ss (they will be deleted when done)
 ;;
 (setq orig:ss ss)
 (setq na (entlast))
 (acet-safe-command T T (list "_.copy" ss "" "0,0" "0,0"))
 (if (not (equal na (entlast)))
     (setq ss (acet-ss-new na))
 );if
 (acet-ss-entdel orig:ss)
 
 ;; if disp=scale and front or back clipping is on then downgrade to move
 (if (and (= disp 3)					
          (or (= 2 (logand 2 (getvar "viewmode")))	;; front clipping
              (= 4 (logand 4 (getvar "viewmode")))	;; back clipping
          )
     );and
     (setq disp 1)
 );if
 (acet-ucs-cmd (list "_view"))
 (setq lok (acet-layer-unlock-all))		;; unlock all layers
 (if zoom
     (setq zoom (acet-ss-zoom-extents ss)) ;; zoom to the extents of the selection set
 );if           
 (setq    tmp (vl-filename-mktemp nil nil ".wmf") ;; get a temp filename
       blklst (acet-table-name-list "block") 	  ;; original list of block names
       stylst (acet-table-name-list "style") 	  ;; original list of text style names
           p1 (acet-geom-view-points) 		  ;; get insert point at view -upper left corner
           p2 (cadr p1)
           p1 (car p1)
          na2 (entlast)
 );setq
 
 (if plretain
     (setq plst (acet-wmf-convert-plines-explode ss)
             ss (car plst)  ;; the original selection set plus new objects from explode
           plst (cadr plst) ;; the rebuild list (used later to re-assemble the new objects)
     );setq then explode plines that are 0 thickness or parallel to current ucs
 );if
 
 ;; Get a list of all the original properties up front and prep the objects
 ;; for the wmfout/in operation.
 (setq n 0)
 (repeat (sslength ss)
  (setq      na (ssname ss n)
             e1 (entget na)
              a (acet-general-props-get e1)
        proplst (cons a proplst)
  );setq
  ;; Prepare for wmfout/in by setting linetype to continuous and lineweight to 0
  (acet-wmf-convert-prep e1)
 (setq n (+ n 1));setq
 );repeat
 (setq proplst (reverse proplst))
 
 
 (cond
  ((= disp 1)
   (command "_.move" ss "" "0,0" "0,0") ;; force an update of the geometry
  )
  ((= disp 2)
   (command "_.hide")
  )
 );cond close
 
 
 ;; perform the wmfout/in and apply the original props to the new entities
 (setq n 0)
 (repeat (sslength ss)
  (setq    na (ssname ss n)
          ss2 (acet-wmf-convert-ent (list na disp tmp p1 p2)) ;; wmfout/in
         lst2 (acet-ss-to-list ss2)
        lulst (cons (cons na lst2) lulst) ;; create a lookup list of for matching old to new objects
  );setq
 
  (if ss2
      (acet-general-props-set ss2 (nth n proplst))	;; restore the original properties
  );if
 (setq n (+ n 1));setq
 );repeat
 
 
 (setq na (entlast))
 (acet-wmf-convert-rebuild-plines plst lulst)
 (setq ss (acet-ss-union (list ss (acet-ss-new na))))
 
 ;; put the ucs back
 (acet-ucs-cmd (list "_previous"))
 
 ;; purge any temp blocks that were created
 (setq lst (acet-table-name-list "block"))
 (foreach x lst
  (if (not (member x blklst))
      (acet-table-purge "block" x T)
  );if
 );foreach
 
 ;; purge any temp text styles that were created
 (setq lst (acet-table-name-list "style"))
 (foreach x lst
  (if (not (member x stylst))
      (acet-table-purge "style" x T)
  );if
 );foreach
 
 ;; delete the temporary file
 (vl-file-delete tmp) 
 
 (command "_.erase" ss "") ;; erase the temp copy of the original objects
 
 (if lok
     (command "_.-layer" "_lock" lok "") ;then re-lock the layers that were unlocked
 );if
 
 (if zoom
     (command "_.zoom" "_prev") ;; restore the original view
 );if
 
 (acet-ss-entdel orig:ss)
 
 (acet-ss-new na2)
);defun acet-wmf-convert
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-wmf-convert-ent ( alst / lst n na disp tmp p1 p2 cxv na2 ss e1 tp xv xv2 )
 
  ;; extract the argumets from the argument list
  (setq lst '(na disp tmp p1 p2))
  (setq n 0)
  (repeat (min (length alst) (length lst))
   (set (nth n lst) (nth n alst))
   (setq n (+ n 1));setq
  );repeat
 
  (setq  e1 (entget na)
         tp (cdr (assoc 0 e1))
         xv (cdr (assoc 210 e1))
        xv2 (acet-geom-vector-scale xv -1.0)
        cxv (acet-geom-z-axis)
        na2 (entlast)
  );setq
 
  (if (setq ss (acet-wmf-round-trip (list na disp tmp p1 p2)))
      (progn
       (acet-pljoin2 ss "Both" 0.000001)
       (setq ss (acet-ss-new na2))
       (if (and (/= "POLYLINE" tp)
                (/= "LWPOLYLINE" tp)
           );and
           (acet-wmf-convert-ltgen-on ss) ;; then turn ltgen on
       );if
       (cond
        ;; if it was a text object then restore the original 
        ;; style if text came back in
        ((or (= "TEXT" tp) (= "MTEXT" tp) (= "ATTDEF" tp));or
         (acet-wmf-convert-restore-style ss (cdr (assoc 7 e1)))
        );cond #1
        
        ;; if it was originally an arc, try to turn the segmented plines
        ;; into a true arc or elliptical arc segments. Only do this if
        ;; the object is either in a parallel ucs or it's thickness is 0.0
        ((and (or (= tp "ARC")
                  (= tp "CIRCLE")
              );or
              (not (acet-flatn-object-has-thickness e1)) ;; (= 0.0 (cdr (assoc 39 e1))) ;; thickness of 0
         );and
         (setq ss (acet-wmf-convert-arcs ss e1))
        );cond #2
         
        ;; if it was an ellipse then attempt to rebuild it or create plines
        ;; that contain arc segments
        ((= "ELLIPSE" tp)
         (setq ss (acet-wmf-convert-ellipses ss e1 cxv xv xv2))
        );cond #3
       );cond close
      );progn then wmfin imported something
  );if
 
 ss
);defun acet-wmf-convert-ent
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a list of arguments
;ss selection set or ename
;disp - optional flag. if disp=3 then objects will be scaled to extents of screen
;       before wmf operation.
;tmp - temp files name
;p1 view corner point
;p2 view corner point
;
;Returns a selection set of new objects if successful.
;
(defun acet-wmf-round-trip ( alst / lst n ss disp tmp p1 p2 scale x plst na )
 
 ;; extract the argumets from the argument list
 (setq lst '(ss disp tmp p1 p2))
 (setq n 0)
 (repeat (min (length alst) (length lst))
 (set (nth n lst) (nth n alst))
 (setq n (+ n 1));setq
 );repeat
 
 
 (if (equal (type ss) 'ENAME)
     (setq ss (ssadd ss (ssadd)));;then convert to a selection set
 );if
 
 (setq x (list (car p1) (cadr p2) 0.0));setq insert point
 (if (= disp 3)
     (progn
      ;; make a copy of the originals and change their color so thay cannot be easily seen
      ;; then scale them to fit the screen.
      (setq na (entlast))
      (command "_.copy" ss "" "0,0" "0,0")
      (setq ss (acet-ss-new na))
      (if (and ss
               (= 0 (sslength ss))
          );and
          (setq ss nil)
      );if
      (if ss
          (progn
           (setq plst (mapcar 'acet-general-props-get (acet-ss-to-list ss)))
           (acet-safe-command T T (list "_.chprop" ss "" "_c" (acet-subtle-color) ""))
           (setq scale (acet-ss-scale-to-fit ss p1 p2 1.0))
          );progn then
      );if
     );progn then  
 );if
 
 (acet-safe-command T T (list "_.wmfout" tmp ss ""))
 (if scale
     (command "_.erase" ss "")
 );if
 (setq na (entlast))
 (if (findfile tmp)
     (acet-safe-command T T (list "_.wmfin" tmp x "2" "2" "0"))
 );if
 (if (and (not (equal na (entlast)))
          (setq ss (acet-explode (entlast)))
     );and
     (progn
      (if scale
          (progn
           (acet-safe-command T T (list "_.scale" ss "" (car scale) (/ 1.0 (cadr scale)))) ;scale it back
           (acet-general-props-set ss (car plst))
          );progn then restore the original scale and general properties
      );if
     );progn then wmfin created something
     (setq ss nil)
 );if
 (if (and ss
          (= (sslength ss) 0)
     );and
     (setq ss nil)
 );if
 ss
);defun acet-wmf-round-trip
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;color info:
; 188 - looks like darkest color
; 121 - probably the lightest
(defun acet-subtle-color ( / app pref disp c )
 (vl-load-com)
 (setq  app (vlax-get-acad-object)
       pref (vla-get-preferences app)
       disp (vla-get-display pref)
 );setq
 (if (= 0 (getvar "tilemode"))
     (setq c (vla-get-GraphicsWinLayoutBackgrndColor disp));setq then paper space
     (setq c (vla-get-GraphicsWinModelBackgrndColor disp));setq else model space
 );if
 (setq c (vlax-variant-change-type c 3)
       c (vlax-variant-value c)
 );setq
 (if (= c 0)
     (setq c 188)
     (setq c 121)
 );if
 c
);defun acet-subtle-color
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a selection set of straight segment pline approximations of ellipses and 
;attempts to create genuine ellipse objects or intelligent pline approximations 
;with arc segments. 
;If the provided extrusion vectors are parallel to the provided current extrusion 
;vector then ellipses are created, otherwise plines are created.
;
;ss   - selection set
;e1   - original ellipse elist
;cxv  - current extrusion vector (provided for speed to avoid re-calculation)
;xv   - extrusion vector of original ellipse
;xv2  - negative of ellipse extrusion vector
;
(defun acet-wmf-convert-ellipses ( ss e1 cxv xv xv2 / par a1 a2 ax1 na ss n da axang e2 vmode c lst )
 
 (setq vmode (getvar "viewmode"))
 (if (not cxv) 
     (setq cxv (acet-geom-z-axis)
            xv (cdr (assoc 210 e1))
           xv2 (acet-geom-vector-scale xv -1.0)
     );setq then calculate the extrusion vector information
 );if
 
 (cond 
  ((= vmode 1)
   (setq par nil)
  );cond #1
  ((equal xv cxv 0.0000001)
   (setq   par T
           ax1 (cdr (assoc 11 e1))
           ax1 (trans ax1 0 cxv T)
         axang (acet-angle-format (angle '(0.0 0.0 0.0) ax1))
   );setq
  );cond #2
  ((equal xv2 cxv 0.0000001)
   (setq   par T
            e1 (subst (cons 210 cxv) (assoc 210 e1) e1) 
            a1 (cdr (assoc 41 e1))
            a2 (cdr (assoc 42 e1))
            e1 (subst (cons 41 a2) (assoc 41 e1) e1)
            e1 (subst (cons 42 a1) (assoc 42 e1) e1)
           ax1 (cdr (assoc 11 e1))
           ax1 (acet-geom-vector-scale ax1 -1.0)		;; reverse the direction of the major axis vector
            e1 (subst (cons 11 ax1) (assoc 11 e1) e1) 
           ax1 (trans ax1 0 cxv T)
         axang (acet-angle-format (angle '(0.0 0.0 0.0) ax1))	;; angle of major axis
   );setq
  );cond #3 then ecs is parallel but has negative extrusion so make adjustments
 );cond close
 
 (if par
     (setq  ;c (trans (cdr (assoc 10 e1)) 0 cxv)
            ;c (list (car c) (cadr c) 0.0)
 
            c (acet-point-flat (cdr (assoc 10 e1)) 0 0)
           e1 (subst (cons 10 c) (assoc 10 e1) e1) ;; set elevation to 0
            c (trans c 0 cxv)
     );setq
 );if
 
 (setq n 0)
 (repeat (sslength ss)
  (setq na (ssname ss n))
  (if par
      (progn
       (setq lst (acet-geom-vertex-list (list na cxv))
              da (acet-geom-arc-3p-d-angle
                    (car lst)
                    (nth (/ (length lst) 2) lst)
                    (last lst)
                 )
       );setq
 
       (if (< da 0.0)
           (setq lst (reverse lst))
       );if
       (setq a1 (acet-angle-format (angle c (car lst)))
             a2 (acet-angle-format (angle c (last lst)))
;             a1 (acet-angle-format (- a1 axang))
;             a2 (acet-angle-format (- a2 axang))
       );setq
       (if (= a1 a2)
           (setq a1 0.0
                 a2 (+ pi pi)
           );setq then full ellipse
           (setq a1 (acet-geom-angle-to-ellipseAngle a1 e1) ;; convert the angle to the ellipse parameter
                 a2 (acet-geom-angle-to-ellipseAngle a2 e1)
           );setq
       );if
       (setq e2 e1
             e2 (subst (cons 41 a1) (assoc 41 e2) e2)
             e2 (subst (cons 42 a2) (assoc 42 e2) e2)
       );setq
       (if (entmake e2)
           (entdel na)
       );if
      );progn then parallel so create elliptical arcs
      (progn
       (if (acet-pline-to-arc-pline na -1)
           (entdel na);then delete the straight segment pline
       );if
      );progn else create a pline with arcs
  );if
  (setq n (+ n 1));setq
 );repeat
 
);defun acet-wmf-convert-ellipses
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a selection set of lwpolyines and an arc/circle entity list.
;Each pline is made up of a series of short straight segments that are an 
;approximation of an arc segment along the provided entity.
;
;This function will convert those approximations to true entities
;along the original object.
;
(defun acet-wmf-convert-arcs ( ss e2 / na2 n na e1 p1 p2 c a1 a2 tp lst na3 ss2 axang xv vmode )
 
 (setq   na3 (entlast)
       vmode (getvar "viewmode")
         ss2 (ssadd)
 );setq
 (if (/= vmode 1)
     ;; create a flat version of the circle/arc object (this may be an ellipse)
     (progn
      (setq na2 (acet-flatn-arc e2)
             e2 (entget na2) 	;; get the new version of it
             xv (cdr (assoc 210 e2)) ;; extrusion vector
             tp (cdr (assoc 0 e2))
              c (cdr (assoc 10 e2))
      );setq
      (if (= tp "ELLIPSE")
          (setq c (trans c 0 1))
          (setq c (trans c na2 1))
      );if
     );setq then not in perspective view
 );if
 
 (setq n 0)
 (repeat (sslength ss)
  (setq  na (ssname ss n)
         e1 (entget na)
  );setq
 
  (if (/= 1 (logand 1 vmode))
      (progn
 
       ;; Get a listr of point along the pline ent
       ;; Find out if the delta angle is positive or negative and reverse the list if it's negative
       (setq lst (acet-geom-vertex-list na)
              p1 (car lst)
              p2 (cadr lst)
              p1 (acet-geom-delta-vector c p1)
              p2 (acet-geom-delta-vector c p2)
              a1 (acet-geom-vector-d-angle p1 p2) ;; absolute value of delta angle from endpoint to next point
              p1 (car lst)
              p2 (cadr lst)
              e1 nil
       );setq
       (if (equal (polar c (angle c p2) (distance c p2))	;;this is same as p2 but it's at the proper z
                  (polar c (+ (angle c p1) a1) (distance c p2))	;; if this is the same then it's positive delta ang
                  (acet-geom-list-fuz (list c p1 p2))
           )
;(progn
           (setq a1 (angle c (car lst))			;; then positive angle
                 a2 (angle c (last lst))
           );setq
;(print a1)
;(print a2)
;(print "dddddood")
;);progn
;(progn
           (setq a2 (angle c (car lst))			;; then negative angle so reverse
                 a1 (angle c (last lst))
           );setq 		
;(print "negatory")
;);progn
       );if
      );progn then not a perspective view
  );if
 
 
  (cond
   ((= 1 (logand 1 vmode))
    (if (acet-pline-to-arc-pline na -1)
        (progn
         (entdel na)
         (setq ss2 (ssadd (entlast) ss2));setq
        );progn then
        (setq ss2 (ssadd na ss2));setq else just keep the old one
    );if 
   );cond #1 in perspective so just attempt to traverse the object with arcs.
   ((and (= tp "CIRCLE")
         (equal a1 a2 0.0000001)
    );and
    (setq e1 e2) ;;copy it.
   );cond #2 just copy the full circle
   ((= tp "CIRCLE")
    (setq e1 e2
          a1 (acet-geom-angle-trans a1 1 xv)
          a2 (acet-geom-angle-trans a2 1 xv)
          e1 (subst '(0 . "ARC") (assoc 0 e1) e1)
          e1 (reverse e1)
          e1 (cons (cons 50 a1) e1)
          e1 (cons (cons 51 a2) e1)
          e1 (reverse e1)
    );setq
   );cond #3 pline does not traverse entire circle
   ((= tp "ARC")
    (setq e1 e2
          a1 (acet-geom-angle-trans a1 1 xv)
          a2 (acet-geom-angle-trans a2 1 xv)
          e1 (subst (cons 50 a1) (assoc 50 e1) e1)
          e1 (subst (cons 51 a2) (assoc 51 e1) e1)
    );setq
   );cond #4
   ((= tp "ELLIPSE")
    (setq    e1 e2
          axang (angle '(0.0 0.0 0.0) 
                        (trans (cdr (assoc 11 e2)) 0 1 T)
                )
             ;a1 (- a1 axang)
             ;a2 (- a2 axang)
    );setq
    (if (/= a1 a2)
        (setq a1 (acet-geom-angle-to-ellipseAngle a1 e1)
              a2 (acet-geom-angle-to-ellipseAngle a2 e1)
        );setq then
        (setq a1 0.0
              a2 (+ pi pi)
        );setq else
    );if
    (setq e1 (subst (cons 41 a1) (assoc 41 e1) e1)
          e1 (subst (cons 42 a2) (assoc 42 e1) e1)
    );setq
   );cond #5
  );cond close
 
 
 (if (/= vmode 1)
      (progn
       (if (or (and e1
                    (entmake e1)
               );and
               (acet-pline-to-arc-pline na -1)
           );or
           (progn
            ;; then delete the approximation pline cuz the true object or the arc plines succeeded.
            (entdel na)
            (setq ss2 (ssadd (entlast) ss2));setq
           );progn then
           (setq ss2 (ssadd na ss2)) ;; else just keep the old one
       );if
      );progn then
  );if
 
 (setq n (+ n 1));setq
 );repeat
 
 (if na2
     (entdel na2) ;; delete the temporary flat object
 );if
 (if (= 0 (sslength ss2))
     (setq ss2 nil)
 );if
 
 
 ss2
);defun acet-wmf-convert-arcs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Make the specified entity continuous and 0 lineweight and 0 width for polylines.
;
(defun acet-wmf-convert-prep ( e1 / )
 
  ;; set the linetype to continuous and line and weight to 0 prior to 
  ;; the export operation
  (command "_.chprop" (cdr (assoc -1 e1)) "" 
      "_lt" (car (acet-table-name-list "ltype")) ;; continuous
      "_lw" 0.0                                  ;; lineweight 0
      ""
  );command
 
);defun acet-wmf-convert-prep
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a selection set and calls acet-plines-explode on the plines that are either 
;parallel to the current ucs or have 0 thickness.
;
(defun acet-wmf-convert-plines-explode ( ss / ss2 xv xv2 flt plst )
 
 ;; clear the previous selection set
 (acet-ss-clear-prev)
 
 ;; Filter for light weight plines and 2d polylines.
 (command "_.select" ss "")
 (setq  xv (acet-geom-cross-product (getvar "ucsxdir") (getvar "ucsydir"))
       xv2 (acet-geom-vector-scale xv -1.0) 
       flt (list 		;; filter for 2d plines that are parallel to current ucs or thickness=0
             '(-4 . "<AND")
                '(-4 . "<OR")
                  '(0 . "LWPOLYLINE")
                  '(-4 . "<AND")
                    '(0 . "POLYLINE")
                    '(-4 . "<NOT") '(-4 . "&") '(70 . 88) '(-4 . "NOT>") ;8 16 64 - 3dpoly/3dmesh/polyfacemesh
                  '(-4 .  "AND>")
                '(-4 . "OR>")
                '(-4 . "<OR")
                  (cons 210 xv)
                  (cons 210 xv2)
                  '(39 . 0.0)		;; thickness
                '(-4 . "OR>")
             '(-4 . "AND>")
           );list
        ss (ssget "_p")
       ss2 (ssget "_p" flt)
 );setq
 (if ss2
     (progn
      (setq plst (acet-plines-explode ss2)
             ss2 (car plst)
            plst (cadr plst)
      );setq
      (acet-ss-clear-prev)
      (command "_.select" ss ss2 "")
      (setq ss (ssget "_p")) 
     );progn then
 );if
 (list ss plst)
);defun acet-wmf-convert-plines-explode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Need to re-format the plst that would have been used for (acet-plines-rebuild ...)
;and place the new objects in before calling it. 
;The new objects are retrieved via the lookup list lulst.
;
(defun acet-wmf-convert-rebuild-plines ( plst lulst / na lst ltgen elst wlst nlst n elst2 wlst2 lst2 a )
 
 (foreach lst plst
  (setq ltgen (car lst)
         elst (cadr lst)
         wlst (caddr lst)
        elst2 nil
        wlst2 nil
  );setq
  (setq n 0)
  (repeat (length elst)
   (setq   na (nth n elst)
         nlst (cdr (assoc na lulst)) ;; the list of new entities
            a (nth n wlst)
   );setq
   (if (> (length nlst) 1)
       (setq     a (list (car a) (car a))
             ltgen "_on"
       );setq then the entity was split up so use same start/end width
   );if
   ;;add the new ones
   (while (setq nlst (cdr nlst))
    (setq elst2 (cons (car nlst) elst2)
          wlst2 (cons a wlst2)
    );setq
   );while
  );repeat entity name
  (if elst2
      (setq lst2 (list ltgen elst2 wlst2)
            plst (subst lst2 lst plst)
      );setq then update the plst
  );if 
 
 );foreach pline description list
 
 (acet-plines-rebuild plst)
 
);defun acet-wmf-convert-rebuild-plines
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a selection set and turns ltgen on for the plines contained in the selection set.
;
(defun acet-wmf-convert-ltgen-on ( ss / na e1 n )
 (if (and ss
          (> (sslength ss) 0)
     );and
     (progn
      (setq n 0)
      (repeat (sslength ss)
       (setq na (ssname ss n)
             e1 (entget na)
       );setq
       (if (or (= "POLYLINE" (cdr (assoc 0 e1)))
               (= "LWPOLYLINE" (cdr (assoc 0 e1)))
           );or
           (command "_.pedit" na "_lt" "_on" "")
       );if
      (setq n (+ n 1));setq
      );repeat
     );progn then
 );if
);defun acet-wmf-convert-ltgen-on
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-wmf-convert-restore-style ( ss sty / n na e1 tp )
 (if (and ss
          (> (sslength ss) 0)
     );and
     (progn
      (setq n 0)
      (repeat (sslength ss)
       (setq na (ssname ss n)
             e1 (entget na)
             tp (cdr (assoc 0 e1))
       );setq
       (if (or (= "TEXT" tp)
               (= "MTEXT" tp)
               (= "ATTDEF" tp)
           );or
           (progn
            (setq e1 (subst (cons 7 sty) (assoc 7 e1) e1));setq
            (entmod e1)
           );progn then
       );if
      (setq n (+ n 1));setq
      );repeat
     );progn then
 );if
);defun acet-wmf-convert-restore-style
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a pline made up of straight segments (that approximate curves) and 
;creates a pline made of arc segments.
;Returns new entity name if successful.
;
(defun acet-pline-to-arc-pline ( e1 fuz / na lst xt alst )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (if (or (not fuz)
         (< fuz 0.0)
     );or
     (setq  xt (acet-ent-geomextents na)
            xt (distance (car xt) (cadr xt))
           fuz (acet-geom-calc-arc-error xt)
     );setq then
 );if
 
 (setq  lst (acet-geom-object-point-list na -1) ;; use plot-ready resolution
        lst (acet-geom-m-trans lst 1 na)
       alst (acet-geom-point-list-to-arc-list lst fuz)
 );setq
 (if alst
     (setq na (acet-geom-arc-list-to-pline e1 alst))
     (progn
      (if (entmake e1)
          (setq na (entlast))
          (setq na nil)
      )
     );progn
 );if
 
 na
);defun acet-pline-to-arc-pline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes an original elist or list of dotted pair properties and a list of arc 
;descriptions i.e. (p1 p2 bulge)
;and creates a pline entity.
;Returns the new ename on success or nil on failure.
;
(defun acet-geom-arc-list-to-pline ( e1 alst / na p1 b arc space ctab clayer cxv th c lt lw ps lts a e2 x )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (if (and (= 0 (getvar "tilemode"))
          (= 1 (getvar "cvport"))
     );and
     (setq space '(67 . 1))
     (setq space '(67 . 0))
 );if
 (setq clayer (cons 8 (getvar "clayer"))
         ctab (cons 410 (getvar "ctab"))
          cxv (cons 210 (acet-geom-z-axis))
           th (cons 39 (getvar "thickness"))
           lt (cons 6 (getvar "celtype"))
          lts (cons 48 (getvar "celtscale"))
           lw (cons 370 (getvar "celweight"))
            c (getvar "cecolor")
           ps (getvar "cplotstyle")
 );setq
 (cond
  ((= c "BYBLOCK") (setq c 0))
  ((= c "BYLAYER") (setq c 256))
  (T               (setq c (atoi c)))
 );cond close
 (setq c (cons 62 c))
 
 (setq e2 (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                 space ctab clayer 			;; (assoc 67 e1) (assoc 410 e1) (assoc 8  e1)
                 c lt lts lw
          );list
 );setq
 (if (setq ps (acet-dict-ename "acad_plotstylename" ps))
     (setq e2 (append e2 (list (cons 390 ps))));setq then add the plotstyle
 );if
 (setq e2 (append 
           e2 
           (list '(100 . "AcDbPolyline")
                  (cons 90 (length alst))
                  '(70 . 0) '(43 . 0.0)
                  (cons 38 (caddr (car (car alst)))) 	;; elevation
                  th					;; thickness
                  cxv					;; (assoc 210 e1)
           );list
          );append
 );setq
 
 ;; put the the provided properties from e1 into e2
 (foreach x e1
  (setq a (car x))
  (if (or (= a 6)
          (= a 8)
          (= a 39)	;; thickness
          (= a 48)	;; lt scale
          (= a 62)
          (= a 67)
          (= a 210)
          (= a 370)	;; line weight
          (= a 390)	;; plot style
          (= a 410)
      );or
      (setq e2 (subst x (assoc a e2) e2))
  );if
 );foreach 
 
 (setq e1 (reverse e2));setq
 
 (foreach arc alst
  (setq p1 (cons 10 (car arc))
         b (cons 42 (caddr arc))
        e1 (cons p1 e1)
        e1 (cons '(40 . 0.0) e1)
        e1 (cons '(41 . 0.0) e1)
        e1 (cons b e1)
  );setq
 );foreach
 (if (equal (car (car alst)) (cadr (last alst)) 0.0000001)   ;;;;(equal (car lst) (last lst) 0.0000001)
     (setq e1 (subst '(70 . 1) (assoc 70 e1) e1));setq then close it
     (setq p1 (cadr (last alst))
           e1 (cons (cons 10 p1) e1)	;; add the last point and some dummy width and bulge values
           e1 (cons '(40 . 0.0) e1)
           e1 (cons '(41 . 0.0) e1)
           e1 (cons '(42 . 0.0) e1)
     );setq then
 );if
 (if (entmake (reverse e1))
     (setq na (entlast))
     (setq na nil)
 );if
 na
);defun acet-geom-arc-list-to-pline
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a list of points that approximate a curve or curves and returns a list of arc
;descriptions of the form (p1 p2 bulge)
;
(defun acet-geom-point-list-to-arc-list ( lst fuz / n j k p1 p2 p3 p4 c b alst da r dir x1 x2 flag bail )
 
 (if (equal (car lst) (last lst))
     (setq lst (acet-list-remove-duplicates lst fuz)
           lst (append lst (list (car lst)))
     );setq
     (setq lst (acet-list-remove-duplicates lst fuz));setq
 );if
 
 (if (= 1 (length lst))
     (setq lst (append lst lst))
 )
 (if (= 2 (length lst))
     (setq alst (list 
                 (list (car lst) (cadr lst) 0.0)
                )
           flag T	;skip the loop
     );setq
 )
 
 (setq n 0)
 (setq j 2)
 (while (and (not flag)
             (not (equal (last lst)
                         (cadr (car alst)) ;; p2
                         fuz
                  );equal
             );not
        );and
 
  (if (not alst)
      (setq p1 (car lst)) ; set it to the first on on the point list
      (setq p1 (cadr (car alst))) ; else set it to the p2 of the previous arc segment
  );if
  (setq n (vl-position p1 lst))
  (if (>= (+ n 2) (- (length lst) 1))
      (setq n (- (length lst) 3)) ;; setq p1 back a bit
  );if
 
  (if (< n 0)
      (setq    n 0
            bail T
      );setq then get out
  );if
 
  (setq k (+ n j));setq
  (if (>= k (length lst))
      (progn
       (setq k (- (length lst) 1));setq then
       (if (not alst)
           (setq flag T)
       );if
      );progn then
  );if
  (setq  p1 (nth n lst)
         p2 (nth (/ (+ n k) 2) lst)
         p3 (nth k lst)
          c (acet-geom-arc-center p1 p2 p3)
  );setq
 
  (if c
      (setq dir (acet-geom-arc-3p-d-angle p1 p2 p3)
             p4 (nth (+ k 1) lst)
      );setq
      (setq p4 nil)
  );if
 
  (cond
   ( bail
     (setq flag T)
   );cond #1
   ((and p4
         (acet-geom-is-arc p1 p2 p3 p4 fuz)
    );and
    ;; p4 is part of the arc
       (cond
        ((equal p1 p4 (/ fuz 2.0))		;; circle
         (setq    r (distance p1 c)
                 x1 (polar c 0.0 r)
                 x2 (polar c pi r)
               alst (cons (list x1 x2 1.0) alst)
               alst (cons (list x2 x1 1.0) alst)
                  j 1
         );setq
        );cond #1 then it's a circle so create two half-circle arcs
      
        ((= p4 (last lst)) 				;; last point
         (setq da (acet-geom-arc-d-angle c p1 p4))	
         (if (< dir 0.0)
             (setq da (- (+ pi pi) da)
                   da (* -1.0 da)
             );setq then negative delta angle
         );if
         (setq    b (acet-geom-arc-bulge c p1 da)	;; the bulge
               alst (cons (list p1 p4 b) alst)
                  j 1
         );setq
        );cond #2
       );cond close
    
       (setq j (+ j 1))
   );cond #2
   ( c
            ;; else p4 is NOT part of the arc
            (if (not alst)
                (setq p1 (car lst))
                (setq p1 (cadr (car alst)))
            );if
            (setq da (acet-geom-arc-d-angle c p1 p3))		;; the delta angle
            (if (< dir 0.0)
                (setq da (- (+ pi pi) da)
                      da (* -1.0 da)
                );setq then negative delta angle
            );if
            (setq    b (acet-geom-arc-bulge c p1 da)		;; the bulge
                  alst (cons (list p1 p3 b) alst)
            );setq
            (setq j 2) ;reset j
   );cond #3
   (T
    (setq alst (cons (list p1 p3 0.0) alst)
             j 2
    );setq
 
    ;(setq j (+ j 1))
   );cond #4
  );cond close
 );while
 
 (reverse alst)
);defun acet-geom-point-list-to-arc-list
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a list of points and an error tolerance/fuzz value.
;Returns a list of points with un-needed points removed.
;i.e. If several points form a straight segment then the middle points get removed.
;
(defun acet-geom-list-remove-straight-segs ( lst fuz / p1 p2 p3 p4 lst2 n j k flag point-is-needed )
 
   ;; local defun
   ;Takes 3 points and a fuz value
   ;returns T if the middle point is needed
   ;
   (defun point-is-needed ( p1 p2 p3 fuz / a v1 v2 )
     (setq v1 (acet-geom-delta-vector p1 p2)
           v2 (acet-geom-delta-vector p1 p3)
            a (acet-geom-vector-d-angle v1 v2)
            a (* (sin a) (distance p1 p2))
     );setq
     (if (> a fuz)
         (setq a T)
         (setq a nil)
     );if
     a
   );defun point-is-needed 
 
 
 (if (not fuz)
     (setq fuz (acet-geom-list-extents lst)
           fuz (distance (car fuz) (cadr fuz))
           fuz (acet-geom-calc-arc-error fuz)
     );setq
 );if
 (if (and lst
          (> (length lst) 2)
     );and
     (progn
      (setq lst2 (list (car lst)))
      (setq n 0
            j 1
      );setq
      (while (< (+ j 1) (length lst))
       (setq   p1 (nth n lst)
               p2 (nth j lst)
               p3 (nth (+ j 1) lst)
                k (+ n 1)
             flag nil
       );setq
 
       (while (<= k j)
        (setq p4 (nth k lst))
        (if (point-is-needed p1 p4 p3 fuz)
            (setq flag T)
        );if
        (setq k (+ k 1))
       );while
       (if flag
           (setq lst2 (cons p2 lst2)
                    n j
           );setq
       );if
       (setq j (+ j 1)) 
      );while
      (if (not (equal (car lst2) (last lst)))
          (setq lst2 (cons (last lst) lst2));seq then
      );if
      (setq lst2 (reverse lst2))
     );progn then
     (setq lst2 lst);setq else
 );if
 lst2
);defun acet-geom-list-remove-straight-segs 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a block name and attempts to purge the block.
;if it cannot be purged then it attempts to delete
;all instances of the block and then tries to purge again.
;
(defun acet-block-kill ( bna / ss lk )
 (if (tblobjname "block" bna)
     (progn
      (acet-table-purge "block" bna T);keep quiet
      (if (and (tblobjname "block" bna)
               (setq ss (ssget "_x" (list '(0 . "INSERT") (cons 2 bna))));setq
          );and
          (progn
           (setq lk (acet-layer-unlock-all))
           (acet-safe-command T T (list "_.erase" ss ""))
           (acet-table-purge "block" bna T);keep quiet
           (if lk
               (command "_.-layer" "_lo" lk "")
           );if
          );progn then try again after deleting
      );if
     );progn nthen
 );if
 (not (tblobjname "block" bna))
);defun acet-block-kill
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a pline that has been flattened and an original extrusion vector and adjusts the 
;widths based on the change in width that occurs from the flattening.
;
(defun acet-flatn-pline-adjust-widths ( e1 xv / na cxv lst swlst ewlst svlst evlst n 
                                                 v1 v2 v3 w1 w2 da swlst2 ewlst2
                                      )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq   cxv (cdr (assoc 210 e1)) 		;; entities current extrusion vector
         lst (acet-pline-segment-list e1)
       swlst (cadr lst)  ;; starting width list
       ewlst (caddr lst) ;; ending width list
 );setq
;(mapcar 'print lst)
;(getstring "\n\nyo")
 (setq
       svlst (acet-pline-width-vector-list lst)
       evlst (cadr svlst) ;; ending width vectors
       svlst (car svlst)  ;; starting width vectors
 );setq
;(print svlst)
;(print "")
;(print evlst)
;(getstring "\n\nyo2")
 
 (setq n 0)
 (repeat (length svlst)
 (setq v1 (nth n svlst)
       v3 (trans v1 cxv xv T)
       v3 (list (car v3) (cadr v3) 0.0)
       v3 (trans v3 xv 0 T)
       v1 (trans v1 cxv 0 T)
       da (acet-geom-vector-d-angle v1 v3) ;; angle between the vectors
       w1 (* (nth n swlst) (cos da))
 
       v2 (nth n evlst)
       v3 (trans v2 cxv xv T)
       v3 (list (car v3) (cadr v3) 0.0)
       v3 (trans v3 xv 0 T)
       v2 (trans v2 cxv 0 T)
       da (acet-geom-vector-d-angle v2 v3) ;; angle between the vectors
       w2 (* (nth n ewlst) (cos da))
 
       swlst2 (cons w1 swlst2)
       ewlst2 (cons w2 ewlst2)
 );setq
 
 (setq n (+ n 1));setq
 );repeat
 
 (if (< (length swlst2) (length swlst))
     (setq swlst2 (cons (car swlst2) swlst2)
           ewlst2 (cons (car ewlst2) ewlst2)
     );setq then
 );if
 (setq swlst2 (reverse swlst2)
       ewlst2 (reverse ewlst2)
 );setq
 
;(print ewlst)
;(print ewlst2)
;(print "")
;(print swlst)
;(print swlst2)
;(print "")
;(getstring "heyhey")
 
 (setq lst (list (nth 0 lst) swlst2 ewlst2 (nth 3 lst)))
 (acet-pline-segment-list-apply e1 lst)
 
);defun acet-flatn-pline-adjust-widths

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a pline segment list of the form returned by acet-pline-segment-list
;and returns a list of vectors that point from the endpoint of each vertex to the outer edge.
; Each vector length is 1/2 of the width of the coorisponding vertex.
;The list returned is a list of sublists:
; (start-width-vector end-width-vector)
;
(defun acet-pline-width-vector-list ( lst / plst swlst ewlst blst svlst evlst 
                                                 n p1 p2 p3 b c pv1 pv2 
                                         )
 
 (setq  plst (nth 0 lst)
       swlst (nth 1 lst)
       ewlst (nth 2 lst)
        blst (nth 3 lst)
         lst nil
 );setq
 (setq n 0)
 (repeat (- (length plst) 1)
  (setq p1 (nth n plst)
        p2 (nth (+ n 1) plst)
         b (nth n blst)
  );setq
  (if (= b 0.0)
      (setq  p3 (polar p1 (+ (angle p1 p2) (/ pi 2.0)) 1.0)
            pv1 (acet-geom-delta-vector p1 p3)
            pv1 (acet-geom-vector-scale pv1 (* 0.5 (nth n swlst)))
             p3 (polar p2 (+ (angle p1 p2) (/ pi 2.0)) 1.0)
            pv2 (acet-geom-delta-vector p2 p3)
            pv2 (acet-geom-vector-scale pv2 (* 0.5 (nth n ewlst)))
      );setq then line
      (setq   c (car (acet-geom-pline-arc-info p1 p2 b))
            pv1 (acet-geom-unit-vector c p1)
            pv1 (acet-geom-vector-scale pv1 (* 0.5 (nth n swlst)))
            pv2 (acet-geom-unit-vector c p2)
            pv2 (acet-geom-vector-scale pv2 (* 0.5 (nth n ewlst)))
      );setq else arc
  );if
  (setq svlst (cons pv1 svlst)
        evlst (cons pv2 evlst)
  );setq
  (setq n (+ n 1));setq
 );repeat
 
 (list (reverse svlst) (reverse evlst))
);defun acet-pline-width-vector-list


(acet-autoload2	'("FLATTENSUP.LSP"	(acet-flatn-arc e1)))
(acet-autoload2	'("FLATTENSUP.LSP"	(acet-flatn-object-has-thickness e1)))
(acet-autoload2	'("PLJOINSUP.LSP"	(acet-pljoin2 ss st fuzz)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgC0hCrqTJ8F1AtNVMw8N7ZWSXNX2AdRnK2KFI+0
;;; 9+OoDhZssaat8/jfuYPJFmUNfQlaGUD0xW27pJvHsJ5zxNxNcHZnjhXubTQOt7/J
;;; gTnte9yyGf+YYvVm1Vum0SNA0utfMvV3d3Z9oglLn7Ce/qSWv2m61slkBlEQisKT
;;; hZEPdB/U2JGzJknujwysIIHNv94sLsUHFW/16dxRHKMmJ4UBs54TkgmjrhgJsu89
;;; b6tgACpymJEESTreb4r1cbPEGZcZh0LmPH4NPI5hLk4Wkp+awV2Ufzra30TK8E9r
;;; F+7I/zO39LPj13UdTpSOecg4Q55wwwCkXrpB9eRTKpJHcd/fI4aN5B0CKx2QcDwa
;;; 3LmDpUxBtaqg21Y6ARfkuk2eq8JzLGhgBgD3zEWW4hAwhYs5i+Puzcpnc7vKdaN8
;;; mb0oNroVy347mdJVpp3FV6VXg4SF7EaDC/BK7tKQwHGuDvOh++bi8oFj50tjxEn+
;;; TqoV3IOFo9TxCPVBtASkibwtm9mK+ED2MwKQn4d3pGRZR166Wy6PO/HY3St/cEUd
;;; MinCq7seXX4EY1adPnVNIWNGtaxxTGkXAwrq9Xy5g96Wn9weB8NXYNOVwiriDLIM
;;; O0sHy3flw81QWR+P/Vtp3CfXIdTcn9Vp3sB2uaR0qkWKZZ86pgegSE9T1jvOeDV+
;;; 1WtMMw==
;;; -----END-SIGNATURE-----