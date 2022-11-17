;;
;;;
;;;    EXTRIM.LSP
;;;    Copyright � 1999 by Autodesk, Inc.
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
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Extended-TRIM - cookie-cutter routine
;
;Select a polyline, line, circle or arc and a side to trim on
;
(defun c:extrim ( / na e1 p1 redraw_it lst n )
 
(acet-error-init (list
                   (list   "cmdecho" 0
                         "highlight" 0
                         "regenmode" 1
                            "osmode" 0
                           "ucsicon" 0
                        "offsetdist" 0
                            "attreq" 0
                          "plinewid" 0
                         "plinetype" 1
                          "gridmode" 0
                           "celtype" "CONTINUOUS"
                         "ucsfollow" 0
                          "limcheck" 0
                   )
                   T     ;flag. True means use undo for error clean up.
                   '(if redraw_it (redraw na 4))
                  );list
);acet-error-init
 
 
(princ "\nPick a POLYLINE, LINE, CIRCLE, ARC, ELLIPSE, IMAGE or TEXT for cutting edge...")
(setq na (acet-ui-single-select '((-4 . "<OR")
                           (0 . "CIRCLE")
                           (0 . "ARC")
                           (0 . "LINE")
                           (0 . "ELLIPSE")
                           (0 . "ATTDEF")
                           (0 . "TEXT")
                           (0 . "MTEXT")
                           (0 . "IMAGE")
                           (0 . "SPLINE")
                           (0 . "INSERT")
                           (0 . "SOLID")
                           (0 . "3DFACE")
                           (0 . "TRACE")
                           (0 . "LWPOLYLINE")
                           (-4 . "<AND")
                            (0 . "POLYLINE")
                            (-4 . "<NOT")
                              (-4 . "&")
                              (70 . 112)
                            (-4 . "NOT>")
                           (-4 . "AND>")
                          (-4 . "OR>")
                         )
                         T
         );acet-ui-single-select
);setq
(if na
    (progn
     (setq e1 (entget na));;setq
     (if (or (equal "TEXT"   (cdr (assoc 0 e1)))
             (equal "MTEXT"  (cdr (assoc 0 e1)))
             (equal "ATTDEF" (cdr (assoc 0 e1)))
             (equal "IMAGE"  (cdr (assoc 0 e1)))
             (equal "INSERT" (cdr (assoc 0 e1)))
             (equal "SOLID"  (cdr (assoc 0 e1)))
             (equal "3DFACE" (cdr (assoc 0 e1)))
             (equal "TRACE"  (cdr (assoc 0 e1)))
         );or
         (progn
          (setq lst (acet-geom-object-point-list na nil))
          (setq n 0)
          (command "_.pline")
          (repeat (length lst)
          (command (nth n lst))
          (setq n (+ n 1));setq
          );repeat
          (if (not (equal (car lst) (last lst) 0.0000001))
              (command "_cl")
              (command "")
          );if
          (setq na (entlast)
                e1 na
          );setq
         );progn then draw a temp pline to be the cutting edge.
         (setq e1 nil)
     );if
     (redraw na 3)
     (setq redraw_it T)
 
     (setq p1 (getpoint "\nSpecify the side to trim on:"));setq
     (redraw na 4)
     (setq redraw_it nil)
     (if p1 (etrim na p1));if
     (if e1
         (progn
          (if (setq p1 (acet-layer-locked (getvar "clayer")))
              (command "_.layer" "_un" (getvar "clayer") "")
          );if
          (entdel e1)
          (if p1
              (command "_.layer" "_lock" (getvar "clayer") "")
          );if
         );progn then
     );if
    );progn
);if
 
(acet-error-restore)
(princ)
);defun c:extrim
 
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;Entity-TRIM function
;takes: na - entity name
;  a - a point, the side to trim on
;NOTE: This function does not allow for the possible miss of
;      non-continuous linetypes.
;
(defun etrim ( na a / la b d e1 lst lst2 n j k m ss na2 na3 na4
                      x y z flag flag2 flag3 zlst vpna vplocked
             )
 
 
(setq e1 (entget na));setq
(if (or (setq flag (equal (acet-dxf 0 e1) "POLYLINE"))
        (setq flag (equal (acet-dxf 0 e1) "LWPOLYLINE"))
        (equal (acet-dxf 0 e1) "LINE")
        (equal (acet-dxf 0 e1) "CIRCLE")
        (equal (acet-dxf 0 e1) "ARC")
        (equal (acet-dxf 0 e1) "ELLIPSE")
        (equal (acet-dxf 0 e1) "TEXT")
        (equal (acet-dxf 0 e1) "ATTDEF")
        (equal (acet-dxf 0 e1) "MTEXT")
        (equal (acet-dxf 0 e1) "SPLINE")
    );or
    (progn
     (if (and flag
              (equal 8 (logand 8 (acet-dxf 70 e1)))
         );and
         (setq flag nil)
     );if
     (setq     a (trans a 1 0)
            vpna (acet-currentviewport-ename)
     );setq
     (acet-ucs-cmd (list "_View"))
 
     (setq   lst (acet-geom-object-point-list na nil)  ;;;find extents of selected cutting edge object
             lst (acet-geom-list-extents lst)
               x (- (car (cadr lst)) (car (car lst)))
               y (- (cadr (cadr lst)) (cadr (car lst)))
               x (* 0.075 x)
               y (* 0.075 y)
               z (list x y)
               x (list (+ (car (cadr lst)) (car z))
                       (+ (cadr (cadr lst)) (cadr z))
                 );list
               y (list (- (car (car lst)) (car z))
                       (- (cadr (car lst)) (cadr z))
                 );list
            zlst (zoom_2_object (list x y))
     );setq
     (if vpna
         (setq vplocked (acet-viewport-lock-set vpna nil)) ;unlock cur viewport if needed.
     );if
     (command "_.zoom" "_w" (car zlst) (cadr zlst))
 
     (entupd na)                  ;;;update the ent. so it's curves display smoothly
 
     (setq lst (acet-geom-object-point-list na
                       (/ (acet-geom-pixel-unit) 2.0)
               )
     );setq
     (if (or (not flag)
             (not (acet-geom-self-intersect lst nil))
         );or
         (progn             ;then the object is valid and not a self intersecting polyline.
          (if (and flag
                   (equal (car lst) (last lst) 0.0001)
              );and
              (setq flag3 T);then the polyline could potentialy need a second offset
          );if
          (if (setq la (acet-layer-locked (getvar "clayer")))
              (command "_.layer" "_unl" (getvar "clayer") "")
          );if
 
          (command "_.pline")
          (setq b nil)
          (setq n 0);setq
          (repeat (length lst)
           (setq d (nth n lst))
           (if (not (equal d b 0.0001))
              (progn
               (command d)
               (setq lst2 (append lst2 (list d)));setq
               (setq b d);setq
              );progn
           );if
           (setq n (+ n 1))
          );repeat
          (command "")
          (setq  na2 (entlast)
                  ss (ssadd)
                  ss (ssadd na2 ss)
                 lst nil
          );setq
          (acet-ss-visible ss 1)
          (setq lst2 (get_fence_points na2 a lst2 flag3 flag));setq
 
          (if la
              (command "_.layer" "_lock" (getvar "clayer") "")
          );if
          (acet-ucs-cmd (list "_p"))
          ;Move the ents to force a display update of the ents to avoid viewres problems.
          (setvar "highlight" 0)
          (if (setq ss (ssget "_f" (last lst2)))
              (command "_.move" ss "" "0,0,0" "0,0,0")
          );if
          (if flag
              (progn
               (if (setq la (acet-layer-locked (acet-dxf 8 e1)))
                   (command "_.layer" "_unl" (acet-dxf 8 e1) "")
               );if
               (acet-ucs-set-z (acet-dxf 210 e1))
               (command "_.copy" na "" "0,0,0" "0,0,0")
               ;(entdel na)
               (acet-ss-visible (ssadd na (ssadd)) 1);make it invisible for a while.
                                                    ;rk 12:01 PM 3/10/98
               (setq na3 na
                      na (entlast)
               );setq
               (command "_.pedit" na "_w" "0.0" "_x")
               (acet-ucs-cmd (list "_p"))
               (if la (command "_.layer" "_lock" (acet-dxf 8 e1) ""));if
              );progn
          );if
          (command "_.trim" na "")
          (setq m (- (length lst2) 1));setq
          (setq k 0)
          (repeat (length lst2)
           (setq lst (nth k lst2))
           (setq a (trans (car lst) 0 1))
           (setq n 1)
           (repeat (- (length lst) 1) ;repeat each fence list
            (setq b (trans (nth n lst) 0 1))
            (if (equal a b 0.0001)
                (setq flag2 T)
                (setq flag2 nil)
            );if
            (setq na4 nil);setq
            (setq j 0);setq
            (while (not flag2)       ;repeat each segment of the fence until no new ents are created.
             (setq na4 (entlast));setq
             (command "_F" a b "")
             (if (and (equal na4 (entlast))
                      (or (not (equal k m))
                          (> j 0)
                      );or
                 );and
                 (setq flag2 T)
             );if
             (setq j (+ j 1));setq
            );while
            (setq a b);setq
            (setq n (+ n 1));setq
           );repeat
 
           (setq k (+ k 1))
          );repeat
          (command "")
 
          (if flag
              (progn
               (if (setq la (acet-layer-locked (acet-dxf 8 e1)))
                   (command "_.layer" "_unl" (acet-dxf 8 e1) "")
               );if
               (entdel na) ;get rid of the copy
 
               ;(entdel na3);bring back the original
               (acet-ss-visible (ssadd na3 (ssadd)) 0) ;bring back the original
                                                      ;rk 12:01 PM 3/10/98
               (if la (command "_.layer" "_lock" (acet-dxf 8 e1) ""));if
              );progn
          );if
         );progn
         (progn
          (acet-ucs-cmd (list "_p"))
          (princ "\nSelf intersecting edges are not acceptable.")
         );progn else invalid self intersecting polyline
     );if
     (command "_.zoom" "_p")
     (if vplocked
         (acet-viewport-lock-set vpna T) ;then re-lock the viewport
     );if
    );progn then it's a most likely a valid entity.
);if
);defun etrim
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun another_offset ( pl1 pl2 a1 a2 b na2 lst2 a lst3 lst4 / na ss lst da1 da2)
 
(setq da1 (abs (- a2 a1)));setq
(setq da2 (- (* b (max pl2 pl1))
             (/ (* b (abs (- pl2 pl1)))
                 2.0
             )
          )
);setq
(if (> (abs (- da2 da1))
       (* 0.01 (max a1 a2))
    )
    (progn
 
     (acet-pline-make (list lst2))
     (setq  na (entlast)
           na2 (entlast)
            ss (ssadd)
            ss (ssadd na ss)
     );setq
     (acet-ss-visible ss 1)
     (command "_.offset" b na2 a "")
     (if (and (not (equal na (entlast)))
              (setq lst3 (acet-geom-vertex-list (entlast)))
              (setq lst3 (intersect_check lst2 lst3 lst4))
         );and
         (progn
          (acet-ss-visible (ssadd (entlast) (ssadd)) 1)
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
          (setq lst (list (acet-geom-vertex-list (list (entlast) 0))));setq
          (entdel (entlast));then offset was a success so delete the ent after getting it's info
         );progn then
         (if (not (equal na (entlast))) (entdel (entlast)));if else
     );if
     (entdel na2)
    );progn then let's do that second offset
);if
 
lst
);defun another_offset
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_fence_points ( na2 a lst2 flag plflag / a1 a2 pl1 pl2 b c d n
                                                   lst lst2 lst3 lst4 na
                        )
 
(if flag
    (progn
     (setq lst2 (cdr lst2));setq
     (repeat (fix (/ (length lst2) 2))
      (setq lst2 (append (cdr lst2) (list (car lst2)));append
      );setq
     );repeat
     (setq lst2 (append lst2 (list (car lst2))));setq
     (command "_.area" "_ob" na2)
     (setq pl1 (getvar "perimeter")
            a1 (getvar "area")
     );setq
    );progn
);if
 
(setq    a (trans a 0 1)
         b (* (getvar "viewsize") 0.05);initial offset distance
         n 3.0                         ;number of offsets
         d (/ b (- n 1))               ;delta offset
         c (acet-geom-pixel-unit)
      lst4 (acet-geom-view-points)
);setq
 
(while (> b c)
(setq na (entlast))
(command "_.offset" b na2 a "")
(if (and (not (equal na (entlast)))
         (setq lst3 (acet-geom-vertex-list (entlast)))
         (or (not plflag)
             (setq lst3 (intersect_check lst2 lst3 lst4))
         );or
    );and
    (progn
     (setq lst3 (acet-geom-m-trans lst3 1 0))
     (acet-ss-visible (ssadd (entlast) (ssadd)) 1)
     (if flag
         (progn
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
         );progn
     );if
     (setq lst (append lst (list lst3)));setq
     (entdel (entlast))  ;delete the ent after getting it's vertex info
     (if flag
         (setq lst (append lst
                           (another_offset pl1 pl2 a1 a2 b na2 lst2 a lst3 lst4)
                   );append
         );setq
     );if
    );progn then offset was a success
    (if (not (equal na (entlast))) (entdel (entlast)));if else
);if
(setq b (- b d));setq
);while
(setq na (entlast))
(command "_.offset" c na2 a "")
(if (and (not (equal na (entlast)))
         (setq lst3 (acet-geom-vertex-list (entlast)))
         (or (not plflag)
             (setq lst3 (intersect_check lst2 lst3 lst4))
         );or
    );and
    (progn
     (setq lst3 (acet-geom-m-trans lst3 1 0))
     (acet-ss-visible (ssadd (entlast) (ssadd)) 1)
     (if flag
         (progn
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
         );progn
     );if
     (setq lst (append lst (list lst3)));setq
     (entdel (entlast));then offset was a success so delete the ent after getting it's info
     (if flag
         (setq lst (append lst
                           (another_offset pl1 pl2 a1 a2 c na2 lst2 a lst3 lst4)
                   );append
         );setq
     );if
    );progn then
    (if (not (equal na (entlast))) (entdel (entlast)));if else
);if
(entdel na2)
 
lst
);defun get_fence_points
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;returns a list of points on screen if the first two lists do not
;contain segments that intersect each other.
;
(defun intersect_check ( lst lst2 lst3 / x x2 y y2 lst4 flag len len2
                                         a aa b bb c d n j)
 
(setq  len (length lst)
      len2 (length lst2)
         x (car (car lst3))
        x2 (car (cadr lst3))
         y (cadr (car lst3))
        y2 (cadr (cadr lst3))
);setq
 
(setq n 0);setq
(while (and (not flag)
            (< (+ n 1) len2)
       );and
(setq   aa (nth n lst2)
        bb (nth (+ n 1) lst2)
         a (bns_truncate_2_view aa bb x y x2 y2)
         b (bns_truncate_2_view bb aa x y x2 y2)
      lst4 (append lst4 (list a))
);setq
(if (or (not (equal a aa))
        (not (equal b bb))
    );or
    (setq lst4 (append lst4 (list b)))
);if
(setq j 0);setq
 (while (and (not flag)
             (< (+ j 1) len)
        );and
 (setq    c (nth j lst)
          d (nth (+ j 1) lst)
       flag (inters a b c d)
 );setq
 
 (setq j (+ j 1));setq
 );while
 
(setq n (+ n 1));setq
);while
(if (not (equal b (last lst4)))
    (setq lst4 (append lst4 (list b)));setq
);if
(if (not flag)
    (setq flag lst4)
    (setq flag nil)
);if
flag
);defun intersect_check
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zoom_2_object ( lst / p1 p2 p3 p4 p5 p6 mp dx dy dx2 dy2
                             r1 r2 na e1 x w h dv1 dv2 x
                     )
 
 (setq  lst (acet-geom-m-trans lst 1 2)
         p1 (acet-geom-m-trans (acet-geom-view-points) 1 2)    ;p1 and p2 are the viewpnts
         p2 (cadr p1)
         p1 (car p1)
         p1 (list (car p1) (cadr p1))
         p2 (list (car p2) (cadr p2))
 );setq
 (if lst
     (progn
      (setq   p5 (acet-geom-list-extents lst)              ;p5 and p6 are the geometry points
              p6 (cadr p5)
              p5 (car p5)
              p5 (list (car p5) (cadr p5))
              p6 (list (car p6) (cadr p6))
              mp (acet-geom-midpoint p5 p6)           ;prepare to resize the geometry rectang to
              dx (- (car p2) (car p1))    ;have the same dy/dx ratio as p1 p2.
              dy (- (cadr p2) (cadr p1))
             dx2 (- (car p6) (car p5))
             dy2 (- (cadr p6) (cadr p5))
      );setq
      (if (equal dx 0.0)  (setq dx 0.000001))  ;just in case div by zero
      (if (equal dx2 0.0) (setq dx2 0.000001))
      (setq   r1 (/ dy dx)
              r2 (/ dy2 dx2)
      );setq
      (if (< r2 r1)
          (setq dy2 (* r1 dx2));then scale dy2 up
          (progn
           (if (equal r1 0.0)  (setq r1 0.000001))  ;just in case div by zero
           (setq dx2 (* dy2 (/ 1.0 r1)));else scale dx2 up
          );progn
      );if
      (setq p5 (list (- (car mp) (/ dx2 1.98))   ;1.98 is used instead of 2.0 to expand
                     (- (cadr mp) (/ dy2 1.98))  ;the rectangle slightly
               );list
            p6 (list (+ (car mp) (/ dx2 1.98))
                     (+ (cadr mp) (/ dy2 1.98))
               );list
      );setq
     );progn then lst
 );if
 (if (and lst
          (equal 0 (getvar "tilemode"))
          (not (equal 1 (getvar "cvport")))
          (setq na (acet-currentviewport-ename))
     );and
     (progn
      (setq  e1 (entget na)
              x (cdr (assoc 10 e1))
              w (cdr (assoc 40 e1))
              h (cdr (assoc 41 e1))
             p3 (list (- (car x) (/ w 2.0))
                      (- (cadr x) (/ h 2.0))
                );list
             p4 (list (+ (car x) (/ w 2.0))
                      (+ (cadr x) (/ h 2.0))
                );list
             p3 (trans p3 3 2)      ;p3 and p4 are the viewport points
             p4 (trans p4 3 2)
            dv1 (acet-geom-delta-vector p1 p3)
            dv2 (acet-geom-delta-vector p2 p4)
              x (distance p1 p2)
      );setq
      (if (equal 0 x) (setq x 0.000001));just in case
      (setq   x (/ (distance p5 p6)
                   x
                )
            dv1 (acet-geom-vector-scale dv1 x)
            dv2 (acet-geom-vector-scale dv2 x)
             p5 (acet-geom-vector-add p5 dv1)
             p6 (acet-geom-vector-add p6 dv2)
       );setq
     );progn then
 );if
 (setq p1 (list (car p1) (cadr p1) 0.0)
       p2 (list (car p2) (cadr p2) 0.0)
       p5 (list (car p5) (cadr p5) 0.0)
       p6 (list (car p6) (cadr p6) 0.0)
 );setq
 (if lst
     (setq lst (list (trans p5 2 1)
                     (trans p6 2 1)
               );list
     );setq
     (setq lst nil)
 );if
 
 lst
);defun zoom_2_object


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAq3LOz8t+8FWfbgqGfHPq0eNgdbiGMe+V53M7g
;;; aGMFibtJLiFPuZEr2JJfte4+TOqC5UGD25/VQiZQc7R1BEupyCgKcPRv8fBItzKY
;;; y35xsa9naYfjEXYC+e0fEOEWHOCLVz82Fy1SGolvYKekQxLV/aJWgX0mG2Ob7TaP
;;; 1cU3fLb3kr4c1U6HMcK9T2jr/pb16h697ToCZfwmDjgK/K30Aeo868cJJxAl5iAi
;;; tKS+dYdMoJ4F0PpgbRPFrlaMh1RlMbHHjWQYe92bHn6Q5jzGY6ugFNjMDI9A7g51
;;; LbI72DTxVB/VJY1pQJyMs3g3xAfqtfNzVTGjvbxflzbfvyjraZeFUkhnqIV9+FSH
;;; zfczxPNPOTCS88EdZg+naE17skbKMwmbNfKlinQ5TPen2ItaLRS3n2Q50up+Zhve
;;; 6IMtiF2M1buH67Mw4iQcbtJXq8axh//MvtGXhIZ4JbkFZT3LHd45LaA2r5fRcbDw
;;; myCO9Q4t/cg72WB8MMWF1D7yQ5xFpdIHEYbqYjnFuUF6/ekrYoh1C3++T4j0XLJk
;;; i3rAIL0z9s55sI9boLbVVZyY98WSqe536xdgG4112Ka3+PESBBVyfdvLPWFQ+0aF
;;; Bh6eU2b9yagj/BSM1lupCrfLIuAw5b7WcIHoP2AJKM9tQcOBr7TfovqRbQcARpvJ
;;; wL4Vyg==
;;; -----END-SIGNATURE-----