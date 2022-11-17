;;
;;  Hatchutil.lsp - This file contains several general purpose functions for 
;;                  dealing with hatch objects and their associated boundaries.
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
 
;(if (not acet-wmf-convert)
;    (load "acet-wmf.lsp" "")
;);if
;
;
;NOTE: group data will be lost on entmake need to use entmakex and entmod on dictionary.
;      Mabe create an acet-entmake function the looks at reactor object references and 
;      resolves them with a swap of old name to new ename from entmakeX
;
;
;
;Terminology and naming convention used for hatch utility function names:
; entity   - when used in explaining parameters - a general term meaning an ename OR elist
;            ename or elist will be used explicitly when only one or the other is accepted.
; boundary - the collective boundary or boundaries of a hatch
; loop     - one component of a hatch boundary. A hatch boundary may contain one or more loops.
; edge     - An ename that is one compenent of a loop. A loop may contain one or more edge enames.
;
; Functions that modify and/or create entities will contain a "-db" suffix to indicate 
; a data base change will occur when called.
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;acet-flatn-hatch 
;takes a hatch entity (ename or elist) and creates a flattened version of it.
;returns a selection set of new objects created.
;
; General approach to flattening the hatch...
; - flatten the pattern data and the seed points
; - get ss of boundary objects
; - flatn each boundary object and replace references to the old one with the new one.
; - entmod the hatch
;
;General hatch notes:
;
; ;91 ;marks start of path data
;
;  ;92 marks start of new loop
;   ;93 may be present to indicate how many edges
; 
;   ;72 marks start of new edge (if pline it indicates bulge)
;
; ;75 marks end of path data
;
;
;92 gc map
;
;0=default	1=external	2=polyline	4=derived	8=textbox	16=outermost
;
;32 is sometimes present but is undocumented
;   Seems to be present when select is used on objects that do not create end-to-end closed loops.
;
;
;Pline
; 1 2 4 	boundary pick
; 1 		selected
; 
;lines
; 1 2 4 	boundary pick
; 1 		selected
;
;lines extended
; 1 2 4 	boundary pick
; 1 32          selected
;
;
;ellipse/pline
; 1 4		boundary pick
; 1 		selected
;
;
;
;------------------------------------------------------------------------------
;Remaining problems:
;------------------------------------------------------------------------------
;
;DID IT (needs testing) - flatn non-associative hatch - need to re-associate it and create 
;                         boundary objects
;
;DID IT acet-hatch-remake - takes a hatch entity and re-creates it as associative.
; - entmake to copy then use entlast/entget
; - flip the associative flag on
; - acet-hatch-loop-make
; - acet-hatch-boundary-assoc-db
; - force an update
;
;DID IT  acet-hatch-boundary-delete-db Delete boundary objects from hatch.
; - Use acet-hatch-edge-id-remove to remove it from all loops
; if a loop no longer has any object associated with it then remove
; the loop and update the loop count. 
; Re-make the boundary object being removed to get rid of reactors.
;
;DID IT - Adding new loops. takes a hatch and a loop list and appends the loop to the hatch.
; - increment the loop count
; - get first loop data and copy it.
; - check enpoints of provided ename loop and set proper 92 flags. 
;    - if endpoints don't match use 1 and 32
;    - else use 4 for derived
; - add references to new loop enames in the appended loop
; - force an update
;
;------------------------------------------------------------------------------
;- Loss of associativity when to far out or too close up.
;  
;  Need to determine when a loop has points such that the problem will occur.
;  1. If endpoints have a gap. If the gap is greater than or equal to the 
;     current pixel-unit then there may be a problem.
;
;  2. If endpoints are too close such that arcs or linesegments become duplicated 
;     single points. If the distance between any set of adjacent points is less 
;     than or equal to the current pixel unit then there may be a problem.
;
;
; Additional info:  This problem will occur only for hatches that contain boundary pick 
;                   loops.
; - For boundary pick loops (derived- bit=4) make sure all end points for the 
; loop edges match exactly.
;
;2dconvert
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch ename or elist and creates an new associative version of 
;the hatch along with new boundary objects.
;Returns the ename of the new hatch if successful.
;
(defun acet-hatch-remake ( e1 / na lst loop loop2 edge )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq lst (acet-hatch-loop-make e1))			;; make the boundary edges		
 (entmake e1)						;; make a copy of the hatch
 (setq na (entlast)
       e1 (entget na)
       e1 (acet-hatch-edge-id-remove-all e1) 		;; remove the existing boundary object references
 );setq
 (acet-hatch-boundary-assoc-db e1 lst T)		;; add the new ones
 (setq e1 (entget na))
 (acet-hatch-update na)
 na
);defun acet-hatch-remake
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch entity and a list of loops (edge enames) and associates each loop 
;with the hatch.
;Returns a new loop list.
;
;NOTES: - The hatch gets entmoded and the boundaries get re-created and provided ones get deleted.
;       - Also note that any existing boundary objects on the hatch are removed. The new ones
;         are simply placed at the specified loop index.
;
(defun acet-hatch-boundary-assoc-db ( e1 lst updateOK / na n a update )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq e1 (subst (cons 71 1) (assoc 71 e1) e1));setq flip associativity on
 
 (setq n 0)
 (repeat (length lst)
  (setq      a (acet-hatch-loop-assoc-db e1 (nth n lst) n)
            e1 (car a)
        update (or update (cadr a))
  );setq
 (setq n (+ n 1));setq
 );repeat
 
 (entmod e1)
 
 (if (and updateOK
          update
     );and
     (acet-hatch-update na)
 );if
 
 (setq lst (acet-hatch-boundary-enames e1))
);defun acet-hatch-boundary-assoc-db
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch entity and a list of new enames for the loop to associate and the 
;index to place the loop in.
;Returns a list of two elements. (e1 update) 
; The first is the updated hatch elist and the second is a true or nil flag that 
;when true indicates that the hatch needs to be updated after it is entmoded.
;
;NOTES: - The hatch does not get entmoded.
;       - The existing boundary objects that are no longer part of the boundary set will
;          be re-created to dis-associate them them from the hatch. 
;       - The new boundary objects will also be re-created to "associate" them to the hatch.
;
(defun acet-hatch-loop-assoc-db ( e1 elst2 n / na loop ss edge e2 lptype ss loop2 edgetp lptype2
                                               update ss2 na2 n 
                                )
  (if (equal (type e1) 'ENAME)
      (setq na e1
            e1 (entget na)
      );setq then
      (setq na (cdr (assoc -1 e1)));setq else
  );if
 
  (setq loop (acet-hatch-loop-data-at-index e1 n) 	;; the original loop
          ss (acet-hatch-boundary-ss e1)		;; original edge objects
  );setq
 
  ;; re-create the new boundaries for this loop to associate them to the hatch
  (foreach edge elst2
   (setq e2 (entget edge));setq
   (if (not (member (cons 330 na) e2))
       (progn
        (setq e2 (acet-acadreactor-id-add e2 na));setq
        (entmake e2)
        (entdel edge)
        (setq elst2 (subst (entlast) edge elst2))
       );progn then not already associated to the hatch
   );if
  );foreach
 
  (setq lptype (cdr (assoc 92 loop)))				;; old loop type
 
  (if (= 4 (logand 4 lptype))
      (progn
       ;; Then loop is derived from boundary pick
       ;; Assemble the new loop data and insert it at the proper location
       (setq     ss2 (acet-list-to-ss elst2)	 			;; objects for new loop
               loop2 (acet-hatch-loop-data-generate ss2) 		;; new loop geometry data
              edgetp (mapcar 'cdr (acet-list-m-assoc 72 loop2))		;; list of 72 values (edge types)
             lptype2 (cdr (assoc 92 loop2))				;; temp loop type
       );setq
       
       ;; to get the new loop type... modify the old loop type by adding or removing the pline bit as needed 
       (cond
        ((and (= 2 (logand 2 lptype))
              (or (member 3 edgetp)	;; new loop contains ellipse or spline
                  (member 4 edgetp)
              );or
         );and
         (setq lptype (- lptype 2))
        );cond #1 then old boundary was a pline and new one does not have pline bit set so remove it from lptype
        ((and (/= 2 (logand 2 lptype))
              (= 2 (logand 2 lptype2))
         );and
         (setq lptype (+ lptype 2))
        );cond #1 then old one had no pline and new one has pline bit set so add it to lptype
       );cond close
      );progn then derived
  );if
  (if (/= lptype (cdr (assoc 92 loop)))
      (progn
       ;; then loop type is derived and it has changes so update the geometry of the loop
       (setq  loop2 (subst (cons 92 lptype) (assoc 92 loop2) loop2)	;; put the proper loop type in
             update T
       );setq
      );progn then
      (progn
       ;; strip the old edge ids out
       (setq loop (reverse loop))
       (while (= 330 (car (car loop)))
        (setq loop (cdr loop))
       );while
       ; add the new edge ids in
       (foreach edge elst2
        (setq loop (cons (cons 330 edge) loop))
       );foreach
       (setq loop2 (reverse loop))
      );progn else loop type has not changed so keep original geometry and just update references to edges
  );if
 
  (setq loop2 (subst (cons 97 (length elst2)) (assoc 97 loop2) loop2)	;; make sure edge count is correct
           e1 (acet-hatch-loop-data-put e1 loop2 n) 			;; insert the new loop in proper location 
  );setq
 
  (if ss
      (progn
       ;; Where needed; re-create the original boundary edge objects to 
       ;; dis-associate them from the hatch
       (setq ss2 (acet-hatch-boundary-ss e1))
 
       (setq n 0)
       (repeat (sslength ss)
        (setq na2 (ssname ss n))
        (if (not (ssmemb na2 ss2))
            (progn
             (setq e2 (entget na2)
                   e2 (acet-acadreactor-id-remove e2 na)
             );setq
             (entmake e2)
             (entdel na2)
            );progn then no longer associated to hatch so re-create
        );if
        (setq n (+ n 1));setq
       );repeat
      );progn then
  );if
 
 (list e1 update)
);defun acet-hatch-loop-assoc-db
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch entity and returns true if any of the loops have the derived bit set.
;loop created via a boundary pick instead of select objects.
;
(defun acet-hatch-is-derived ( e1 / na lst flag a )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq lst (mapcar 'cdr (acet-list-m-assoc 92 e1)))
 (while lst
  (setq   a (car lst)
        lst (cdr lst)
  );setq
  (if (= 4 (logand 4 a))
      (setq flag T
             lst nil
      );setq then derived
  );if
 );while
 
 flag
);defun acet-hatch-is-derived
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Delete boundary objects from hatch.
; - Use acet-hatch-edge-id-remove to remove it from all loops
; if a loop no longer has any object associated with it then remove
; the loop updating the loop count. 
; Re-make the boundary object being removed to get rid of reactors.
;
(defun acet-hatch-boundary-delete-db ( e1 ss / na n j na2 e2 loop loop2 lst2 lst )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq lst (acet-hatch-boundary-enames e1)
        e1 (acet-hatch-edge-id-remove-all e1)	;; remove all references to edges
 );setq
 ;; re-build each loop with specified edges removed
 (foreach loop lst
  (setq loop2 nil)
  (foreach edge loop
   (if (not (ssmemb edge ss))
       (setq loop2 (cons edge loop2))
       (progn
        (setq e2 (entget edge)
              e2 (acet-acadreactor-id-remove e2 na)
        );setq
        (entmake e2)
        (entdel edge)
       );progn else remove the reactors by re-entmaking without the reactor references
   );if
  );foreach
 
  (if loop2
      (setq lst2 (cons loop2 lst2))
      (setq e1 (acet-hatch-loop-remove e1 (vl-position loop lst)));setq else delete the loop
  );if
 );foreach
 (setq lst2 (reverse lst2))
 (acet-hatch-boundary-assoc-db e1 lst2 T)
 
);defun acet-hatch-boundary-delete-db
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch entity (ename or elist) and returns a selection set of associated boubndary 
;objects.
;
(defun acet-hatch-boundary-ss ( e1 / na lst ss )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq lst (member (assoc 97 e1) e1)
       lst (acet-list-m-assoc 330 lst)
       lst (mapcar 'cdr lst)
 );setq
 (if lst
     (setq ss (acet-list-to-ss lst));setq
 );if
 ss
);defun acet-hatch-boundary-ss
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch entity and returns a list of loop lists.
; Each loop list is a list of enames.
;
(defun acet-hatch-boundary-enames ( e1 / na loop lst a )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq e1 (cdr (member (assoc 330 e1) e1))
       e1 (cdr (member (assoc 91 e1) e1))
       e1 (reverse (cdr (member (assoc 75 e1) (reverse e1)))) 
 );setq
 
 (while e1
  (setq  a (car e1)
        e1 (cdr e1)
  );setq
  (if (= 92 (car a))
      (progn
       (if loop
           (setq  lst (cons (reverse loop) lst)
                 loop nil
           );setq then
       );if
      );progn then
      (progn
       (if (= 330 (car a))
           (setq loop (cons (cdr a) loop));setq then
       );if
      );progn else
  );if
 );while
 (if loop
     (setq  lst (cons (reverse loop) lst)
           loop nil
     );setq then
 );if
 
 (reverse lst)
);defun acet-hatch-boundary-enames
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Adding new loops. Takes a hatch and a loop ename list and appends the loop to the hatch.
;
(defun acet-hatch-loop-add-db ( e1 loop / na n )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq  n (cdr (assoc 91 e1))
       e1 (acet-hatch-loop-assoc-db e1 loop n)
       e1 (car e1)
 );setq
 (entmod e1)
 
 (acet-hatch-update na)
 
);defun acet-hatch-loop-add-db
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Used by acet-hatch-boundary-delete-db to remove unneeded loops
;Delete a single loop from the hatch.
;Takes a hatch entity and a loop index
;updates the loop count and deletes the loop and returns the new elist
;NOTE: No entmod is performed and boundary objects will still reference the hatch.
;
(defun acet-hatch-loop-remove ( e1 j / na n a e2 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq  n (cdr (assoc 91 e1))
       e1 (subst (cons 91 (- n 1)) (assoc 91 e1) e1)
        n -1
 );setq
 (while (setq a (car e1))
  (if (= 92 (car a))
      (progn
       (setq n (+ n 1))
       (if (= n j)
           (progn
            (setq e1 (cdr e1)) 
            (while (and (setq a (car e1))
                        (/= 92 (car a))
                        (/= 75 (car a))
                   );and
              (setq e1 (cdr e1))
            );while
           );progn then strip the loop data
       );if
      );progn then
  );if
  (if a
      (setq e2 (cons a e2))
  );if
  (setq e1 (cdr e1));setq
 );while
 (setq e2 (reverse e2))
);defun acet-hatch-loop-remove
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch entity and a loop index and returns the hatch data for that loop.
;
(defun acet-hatch-loop-data-at-index ( e1 j / na a n loop )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq n -1)
 (while (setq a (car e1))
  (setq e1 (cdr e1));setq
  (cond
   ((= 92 (car a))
    (setq n (+ n 1))
   )
   ((= 75 (car a))
    (setq  n -1
          e1 nil
    );setq
   )
  );cond close
  (if (= n j)
      (setq loop (cons a loop))     
  );if
 );while
 
 (reverse loop)
);defun acet-hatch-loop-data-at-index
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch entity and data for one complete loop and places it at the specified index.
;returns the new elist.
;NOTE: Any existing data at the specified index will be removed.
;
(defun acet-hatch-loop-data-put ( e1 loop j / na e2 e3 n k )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 
 (setq e2 (reverse e1)
       e2 (member (assoc 91 e2) e2)
       e2 (reverse e2)			;; portion before path data
       e3 (member (assoc 75 e1) e1)	;; portion after path data
        k (cdr (assoc 91 e1))
 );setq
 (if (>= j k)
     (setq e2 (subst (cons 91 (+ k 1)) (assoc 91 e2) e2)
           e1 (append e2 loop e3)
     );setq then actually adding another loop
     (progn
      (setq n 0)
      (repeat k
       (if (= n j)
           (setq e2 (append e2 loop));setq then
           (setq e2 (append e2 (acet-hatch-loop-data-at-index e1 n)));setq else
       );if
       (setq n (+ n 1));setq
      );repeat
      (setq e1 (append e2 e3));setq
     );progn then
 );if
 e1
);defun acet-hatch-loop-data-put
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a selection set for one loop and creates a temporary hatch object and then
;extracts the loop data at index 0 and returns it.
(defun acet-hatch-loop-data-generate ( ss / xt d sf na e1 loop n )
 
 ;; Find the smallest object in the selection set and base that hatch scale off of that
 (setq n 0)
 (repeat (sslength ss)
  (setq na (ssname ss n))
  (if (and (setq xt (acet-ent-geomextents na))
           (setq xt (distance (car xt) (cadr xt)))
           (> xt 0.0)
           (or (not d)
               (< xt d)
           );or
      );and
      (setq d xt)
  );if
  (setq n (+ n 1));setq
 );repeat
 (if (and (setq xt (acet-geom-ss-extents ss nil))
          (setq xt (distance (car xt) (cadr xt)))
          d
          (< d (* 0.02 xt))
     );and
     (setq d (/ (+ d xt) 2.0)
     );setq then d is way less that the extents of the full selection set so avergage the two.
 );if
 (if d
     (progn
      (setq sf (/ (* 0.75 d) 0.125)
            na (entlast)
      );setq
      (command "_.-bhatch" "_prop" "ansi37" sf 0.0 "_select" ss "" "")
     );progn then
 );if
 (if (not (equal na (entlast)))
     (progn
      (setq na (entlast)
            e1 (entget na)
      );setq
      (command "_.undo" "1")
      (setq loop (acet-hatch-loop-data-at-index e1 0))
      (if (/= (sslength ss) (cdr (assoc 97 loop)))
          (progn
           (setq loop (subst (cons 97 (sslength ss)) (assoc 97 loop) loop))
           (setq n 0)
           (repeat (sslength ss)
            (setq na (ssname ss n)
                  na (cons 330 na)
            );setq
            (if (not (member na loop))
                (setq loop (append loop (list na)));setq then
            );if
            (setq n (+ n 1));setq
           );repeat           
          );progn then
      );if
     );progn then
 );if
 
 loop
);defun acet-hatch-loop-data-generate
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch elist, an ename, and a loop index and 
;adds the ename to the hatch boundary path at the specified loop index.
;Returns the new elist.
;
(defun acet-hatch-edge-id-add ( e1 na2 j / e2 e3 e4 e5 n a b lst k tp )
 (setq  e2 (acet-list-split e1 (assoc 92 e1))
        e3 (cadr e2)				;; path data and beyond
        e2 (car e2)				;; prior to path data
        e3 (acet-list-split e3 (assoc 75 e3))
        e4 (cadr e3)				;; after path data
        e3 (car e3)				;; path data
       ;e2 (reverse (member (assoc 91 e1) (reverse e1)))		;; the begining
       ;e3 (cdr (member (assoc 91 e1) e1))			;; the path data
       ;e3 (reverse e3)
       ;e3 (cdr (member (assoc 75 e3) e3))
       ;e3 (reverse e3) 
       ;e4 (member (assoc 75 e1) e1) 				;; the end
       tp (cdr (assoc 0 (entget na2)))
 );setq
 (setq n -1)
 (while e3
  (setq a (car e3))
  (if (= 92 (car a))
      (progn
       (setq a (cdr a)) 
       (if (and (or (= tp "ELLIPSE")	;; if new object is a ellipse or spline and the loop was a pline
                    (= tp "SPLINE")
                );or
                (= 2 (logand 2 a))
           );and
           (setq a (- a 2));setq then remove the pline bit
       );if
       (setq e5 (cons (cons 92 a) e5)
             e3 (cdr e3)
       );setq
       (setq n (+ n 1))
       (if (= n j)
           (progn
            (while (/= (car (car e3)) 97)	;; traverse the data until source object count is found
             (setq e5 (cons (car e3) e5)
                   e3 (cdr e3)
             );setq
            );while
            (setq  a (car e3)
                  e3 (cdr e3)
                  e5 (cons (cons 97 (+ 1 (cdr a)))	;; update source object count
                           e5
                     )
            );setq
            (while (= (car (car e3)) 330)	;; add in existing source object references
             (setq e5 (cons (car e3) e5)
                   e3 (cdr e3)
             );setq
            );while
            (setq e5 (cons (cons 330 na2) e5));setq	;; add the one we need to add
           );progn then 
       );if
      );progn then starting a new loop
      (setq e5 (cons a e5)
            e3 (cdr e3)
      );setq else
  );if
  
 );while
 (setq e5 (reverse e5))
 (append e2 e5 e4)
);defun acet-hatch-edge-id-add
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch elist, an ename, and a loop index and 
;removes the ename from the hatch boundary path at the specified loop index.
;Returns the new elist.
;
(defun acet-hatch-edge-id-remove ( e1 na2 j / e2 e3 e4 e5 n a b lst k tp groupc )
 (setq e2 (acet-list-split e1 (assoc 92 e1))
       e3 (cadr e2)				;; 
       e2 (car e2)				;; before path data
       e3 (acet-list-split e3 (assoc 75 e3))
       e4 (cadr e3)				;; after path data
       e3 (car e3)				;; path data
 
       ;e2 (reverse (member (assoc 91 e1) (reverse e1)))		;; the begining
       ;e3 (cdr (member (assoc 91 e1) e1))			;; the path data
       ;e3 (reverse e3)
       ;e3 (cdr (member (assoc 75 e3) e3))
       ;e3 (reverse e3) 
       ;e4 (member (assoc 75 e1) e1) 				;; the end
       tp (cdr (assoc 0 (entget na2)))
       groupc (cons 330 na2)
 );setq
 (setq n -1)
 (while e3
  (setq a (car e3))
  (if (= 92 (car a))
      (progn
       (setq e5 (cons a e5)
             e3 (cdr e3)
       );setq
       (setq n (+ n 1))
       (if (= n j)
           (progn
            (while (/= (car (car e3)) 97)	;; traverse the data until source object count is found
             (setq e5 (cons (car e3) e5)
                   e3 (cdr e3)
             );setq
            );while
            (setq  a (car e3)
                  e3 (cdr e3)
                  e5 (cons (cons 97 (- (cdr a) 1))	;; update source object count
                           e5
                     )
            );setq
            (while (= (car (car e3)) 330)	;; add in existing source object references
             (if (not (equal (car e3) groupc))
                 (setq e5 (cons (car e3) e5)) 	;; then not the one we are removing so add it
             );if
             (setq e3 (cdr e3));setq
            );while
           );progn then 
       );if
      );progn then starting a new loop
      (setq e5 (cons a e5)
            e3 (cdr e3)
      );setq else
  );if
  
 );while
 (setq e5 (reverse e5))
 (append e2 e5 e4)
);defun acet-hatch-edge-id-remove
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch elist and removes references to boundary source objects from the list.
;Returns the new list.
;
(defun acet-hatch-edge-id-remove-all ( e1 / x e2 pflag )
 (foreach x e1
  (cond
   ((= (car x) 97) 
    (setq     x '(97 . 0)
          pflag T ;; into the path data now
    )
   )
   ((and pflag 
         (= (car x) 330)
    );and
    (setq x nil)
   )
  );cond close
  (if x
      (setq e2 (cons x e2))
  );if
 );foreach
 (reverse e2)
);defun acet-hatch-edge-id-remove-all
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch elist, an old ename and a new ename and replaces references
;to the old object with references to the new one.
;Returns a list of three items:
; (elist oldna newna)
;
;elist - the updated version of the hatch elist
;oldna - the newly created version of oldna (the original is deleted)
;newna - the newly created version of newna (the original is deleted)
;
;NOTE: The newna and oldna objects are re-created in order to add/remove the reactors.
;      Also note that the hatch is not modified via entmod. Only its list is modified.
;
(defun acet-hatch-edge-id-replace-db ( e1 oldna newna / na lst e2 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (if (and (setq e2 (entget newna))
          (setq e2 (acet-acadreactor-id-add e2 na)) ;; point at the hatch
          (entmake e2)
     );and
     (progn
      (entdel newna)
      (setq newna (entlast)
               e1 (subst (cons 330 newna) (cons 330 oldna) e1)
               e2 (entget oldna)
               e2 (acet-acadreactor-id-remove e2 na)
      );setq
      (entmake e2)
      (entdel oldna)
      (setq oldna (entlast)
              lst (list e1 oldna newna)
      );setq
     );progn then
 );if
 lst
);defun acet-hatch-edge-id-replace-db
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a hatch elist and an ename of a boundary object
;Returns the first loop index that the ename is referenced in. Returns -1 if not present.
;First index is 0
;
(defun acet-hatch-edge-id-loop-index ( e1 na2 / a n flag )
 (setq e1 (cdr (member (assoc 91 e1) e1))
       e1 (reverse (cdr (member (assoc 75 e1) (reverse e1)))) 
        a (cons 330 na2)
 );setq
 
 (setq n -1)
 (while e1
  (if (= 92 (car (car e1)))
      (setq n (+ n 1));setq
      (progn
       (if (equal a (car e1))
           (setq   e1 nil
                 flag T
           );setq then
       );if
      );progn
  );if
  (setq e1 (cdr e1));setq
 );while
 
 (if (not flag)
     (setq n -1)
 );if
 
 n
);defun acet-hatch-edge-id-loop-index
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;General purpose update function for associative hatches.
;Takes a hatch entity (ename or elist) and safely updates the hatch without loss of 
;associativity.
;NOTE: This may require re-creating the boundary objects for re-associating a hatch
;      that has lost it during the update.
;
;Returns T if successful and nil if not. (hatch will be associative on return no matter what)
;
(defun acet-hatch-update ( e1 / na lst a ss xt p1 p2 p3 p4 p5 p6 sf d1 d2 p7 ss2 elst n lst2 na2 e2 flag )
 
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq flag T)
 
 (setq ss (acet-hatch-boundary-ss e1));setq
 (if (not (acet-hatch-is-derived e1))
     (command "_.move" na ss "" "0,0" "0,0");then just move it to force an update
     (progn
 
      (acet-ucs-cmd '("_view"))
      (setq xt (acet-geom-ss-extents ss T)	;; get the extents of the hatch boundaries
            p1 (car xt)
            p2 (cadr xt)
            p1 (list (car p1) (cadr p1) 0.0)
            p2 (list (car p2) (cadr p2) 0.0)
            xt (acet-geom-view-points)		;; get the view corner points
            p3 (car xt)
            p4 (cadr xt)
            p3 (list (car p3) (cadr p3) 0.0)
            p4 (list (car p4) (cadr p4) 0.0)
 
            ;sf (/ (distance p3 p4)		;; scale factor to make the hatch same size as the current view
            ;      (distance p1 p2)
            ;   )
            ;sf (* sf 0.75)			;; adjust it down to allow for differences in height to width ratio
                                                ;; of screen versus boundary extents.
            sf (/ (acet-geom-pixel-unit)
                  (cdr (assoc 47 e1))
               )
            p5 (acet-geom-midpoint p1 p2)	;; want to find a base point to scale from such that the mid points
            p6 (acet-geom-midpoint p3 p4)       ;; of p1-p2 and p3-p4 will coincide.
            p7 (acet-ss-scale-to-fit-base p5 p6 sf) ;; get the base point to scale from
      );setq
 
      ;; get the original boundary objects in case we need to re-hook them up
      ;; after the scale operations.
      (setq lst (acet-hatch-boundary-enames e1));setq
 
      (acet-ss-redraw ss 4)
      (redraw na 4)
      (command "_.redraw")
            
      (setq ss2 (acet-hatch-boundary-ss e1)
            ss2 (ssadd na ss2)
      );setq
      (acet-ss-visible ss2 0) ;; go stealth
 
      (setq elst (acet-hatch-boundary-dumb-down ss)) ;; get rid of point along plines that are too close together
 
      (command "_.scale" na ss "" p7 sf)		;; scale it then reverse it
 
      (setq e1 (entget na))
      (if (/= 1 (cdr (assoc 71 e1)))
          (progn
 
           ; (print (assoc 92 e1))
           ; (getstring "lost assocativity on first scale")
 
           (acet-hatch-boundary-assoc-db e1 lst nil) ;; re-assoc the objects
           (setq   e1 (entget na)			
                   ss (acet-hatch-boundary-ss e1)
                  lst (acet-hatch-boundary-enames e1) ;; get updated version of lst ;@rk
                 lst2 (apply 'append lst)
           );setq
           ;; swap in the new enames for the boundaries
           ; (getstring "what up")
           (setq n 0)
           (repeat (length elst)
            (setq   e2 (nth n elst)
                   na2 (nth n lst2)
                    e2 (subst (cons -1 na2) (assoc -1 e2) e2)
                  elst (subst e2 (nth n elst) elst)
            );setq 
           (setq n (+ n 1));setq
           );repeat
 
           (command "_.move" na ss "" "0,0" "0,0")
           (setq e1 (entget na))
           (if (/= 1 (cdr (assoc 71 e1)))
               (setq flag nil);then we failed to coax the hatch into updating.
           );if
           ;(getstring "did we get it back?")
          );progn then lost associativity so re-assoc and re-build the selection set
      );if
 
      (command "_.scale" na ss "" p7 (/ 1.0 sf))	;; then reverse the scaling 
 
      (foreach x elst (entmod x)) 			;; put the boundary back to its original accuracy.
 
      (setq e1 (entget na))
      (if (/= 1 (cdr (assoc 71 e1)))
          (progn
              
; (print (assoc 92 e1))
; (getstring "lost assocativity dude")
 
           (acet-hatch-boundary-assoc-db e1 lst nil) ;; re-assoc the objects
; (getstring "Just did a acet-hatch-boundary-assoc-db")
 
           (setq  e1 (entget na)			
                 ss2 (acet-hatch-boundary-ss e1)
                 ss2 (ssadd na ss2)
           );setq
          );progn then lost associativity so re-assoc and re-build the selection set
      );if
      (acet-ss-visible ss2 0)	;; make the hatch and boundaries visible again
 
      (acet-ucs-cmd '("_prev"))
     );progn else it has one or more derived (boundary pick) loops.
 );if
 flag
);defun acet-hatch-update
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-hatch-boundary-dumb-down ( ss / lst na e1 n px )
 (setq px (acet-geom-pixel-unit))
 (setq n 0)
 (repeat (sslength ss)
  (setq na (ssname ss n)
        e1 (entget na)
  );setq
  (if (= "LWPOLYLINE" (cdr (assoc 0 e1)))
      (progn
       (setq lst (cons e1 lst))
       (acet-lwpline-remove-adjacent-dups e1 px)
      );progn then
  );if
  (setq n (+ n 1));setq
 );repeat
 
 lst
);defun acet-hatch-boundary-dumb-down
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-lwpline-remove-adjacent-dups ( e1 fuz / na lst plst wlst1 wlst2 blst j n p1 p2 )
 (if (equal (type e1) 'ENAME)
     (setq na e1
           e1 (entget na)
     );setq then
     (setq na (cdr (assoc -1 e1)));setq else
 );if
 (setq   lst (acet-pline-segment-list e1)
        plst (nth 0 lst)
       wlst1 (nth 1 lst)
       wlst2 (nth 2 lst)
        blst (nth 3 lst)
         lst nil
 );setq
 (setq n 0)
 (while (< (+ n 1) (length plst))
  (setq p1 (nth n plst)
         j (+ n 1)
  );setq
  (while (and (< j (length plst))
              (setq p2 (nth j plst))
              (<= (distance p1 p2) fuz)
         );and
   (setq lst (cons j lst)) ;; add to list of items to be removed
   (setq j (+ j 1));setq
  );while
  (setq n j);setq
 );while
 
 (setq  plst (acet-list-m-remove-nth lst plst)
       wlst1 (acet-list-m-remove-nth lst wlst1)
       wlst2 (acet-list-m-remove-nth lst wlst2)
        blst (acet-list-m-remove-nth lst blst)
         lst (list plst wlst1 wlst2 blst)
 );setq
 (acet-pline-segment-list-apply e1 lst)
 
);defun acet-lwpline-remove-adjacent-dups
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Remove the indexes listed in ids from lst and return new lst
;
(defun acet-list-m-remove-nth ( ids lst / n lst2 )
 (setq n 0)
 (repeat (length lst)
  (if (not (member n ids))
      (setq lst2 (cons (nth n lst) lst2));setq then
  );if
  (setq n (+ n 1));setq
 );repeat
 (reverse lst2)
);defun acet-list-m-remove-nth


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBDfZJpR6aY9hrCSlaLuKOzhbFDglkTbI+0shbF
;;; VkrT3QlZJXL0e30mZn6Uo7a89aSLTc/4QWcO0YFP+u797+SqhsKUkIjkKMVJRfw6
;;; oFOjD8g4OspXOZ0C7NrE5l7dRt7RmJrEHkhPljCUjZ8JjUEJfzUFxxY9qib/Wd9M
;;; Ipt4Thudpy+DCXK5+lrv8XLfWHkBRVNnLeNeZmBtEVmyD/I3Mh2/qnLZfsMn0ylc
;;; 8z7CwrUKEKaLrjkoH4cgFtYVZ8TEhSr3nEcx3QOBFtaLz7MbOUAIMB7MTw/DFpL6
;;; LEecvm5BT/YBBDmQ71MaXH8zlLDVcrLmAyMlR0nh1EOw20JebflSjXHLISxB9Sog
;;; F78I20n9760Y2p/blc7mTUtU/nzm5nbZiIT7VjsbILP+grCmv7ZnUX6Byq+aEV/c
;;; PnTsDLivCQmvBOkTGrETvN13NooLIfPX4rKWA1od/hC60bevmlMqmus6nDFOq/SE
;;; QKiahvRx8H6XKh4j6Z2pqOCZ3k+sNMXHjZ75g5DH2I+G58uNCfptOHXBzHVAPmCk
;;; ihVhg2qGlmLzC9xorxI3quup7vplF4+8Nxt+yR+5NBD/+U1iCDWg9qTg49QtHEvx
;;; Op1+aCmLtC0wL4OzpupbJ5yYH7eY8ppOfHawVMd0IbKrkSYKEB1UAcAXRUlJ40jL
;;; 3hWJpA==
;;; -----END-SIGNATURE-----