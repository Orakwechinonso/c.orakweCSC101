;;
;;;
;;;    TREXBLK.LSP
;;;    Copyright © 1999-2006 by Autodesk, Inc.
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
 
(defun find_pline_header ( na na2 / na3 na4)
  (setq na3 (tblobjname "block" (cdr (assoc 2 (entget na)))));setq
  (while (and na3
            (not (equal na2 na3))
         );and
  (if (equal "POLYLINE" (cdr (assoc 0 (entget na3))))
    (setq na4 na3);a polyline header that could be the one we need.
  );if
  (setq na3 (entnext na3))
);while
 
na4
);defun find_pline_header
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract_clone ( lst / flag flag2 bna bna2 bna3 lst lst3 j n na ss e1
                             bClonedMATTS blkRefName )

  (setq   na (car lst)
          e1 (entget na)
        lst3 (last lst);the ent family tree
           j (- (length lst3) 1)
        bClonedMATTS nil
  );setq

  (if (setq flag2 (car lst3))
     (progn
       (acet-ucs-set-z (cdr (assoc 210 (entget (car lst3)))))
     );progn
  );if
  (if (equal "VERTEX" (cdr (assoc 0 e1)))
     (setq na (find_pline_header (car lst3) na)
           e1 (entget na)
     );setq
  );if
  (if (> (length lst3) 0)
     (progn
       (if (not (assoc 60 e1))            ;set the invisibility flag
          (setq e1 (append e1 (list (cons 60 1))));setq then
          (setq e1 (subst (cons 60 1) (assoc 60 e1) e1));setq else
       );if
     );progn
  );if
 
  (setq e1 (rem_codes e1));setq
  (if (equal "IMAGE" (cdr (assoc 0 e1)))
     (image_make na e1)                    ; Make the nested ent the way it was first drawn
     (progn
         ; For multiline text attributes, clone the ATTDEF to the current space
         ; using _matts_util.  Otherwise, just entmake e1 to clone the entity, 
         ; as previously done for single line attributes and other entities.
         (if (_matts_util na)
             (progn
                (if (equal (type lst3) 'LIST)
                    (setq blkRefName (car lst3))
                    (setq blkRefName nil)
                )
                (if (_matts_util na 4 blkRefName)
                    (setq bClonedMATTS T)
                )
             )
             (entmake e1)
         );if
     );progn
  );if
 
  (if (equal '(66 . 1) (assoc 66 e1));just in case its some variety of polyline,
     (progn                         ;then better finish the entmake job.
      (setq na (entnext na)
            e1 (entget na)
      );setq
      (if (not (equal "ATTRIB" (cdr (assoc 0 e1))))
          (setq e1 (rem_codes e1));setq  ;rk added if code around this
      );if
      (while (and na
                  (not (wcmatch (cdr (assoc 0 e1)) "*END*"))
             );and
       (entmake e1)
       (setq na (entnext na)
             e1 (entget na)
       );setq
       (if (not (equal "ATTRIB" (cdr (assoc 0 e1))))
           (setq e1 (rem_codes e1));setq  ;rk added if code around this
       );if
      );while
      (entmake e1)
     );progn then
  );if
 
  (if bClonedMATTS
     (progn
       ; Add the multiline text attribute definition to the selection set
       (setq ss (ssadd (entlast)))
     );progn
     (progn 
       ;else process other entities by creating an anonymous block table record
       ; and reference (based on the selected reference), then explode the new
       ; reference and purge the block table record
       (setq   na (entlast)
               ss nil
               ss (ssadd)
               ss (ssadd na ss)
       );setq
       (setq  bna (acet-block-make-anon ss nil)      ;define an anonymous block
              bna2 bna                     ;save the name of the original for comparison later
              bna3 ""                      ;a string for adding names of purge-ables
       );setq
 
       (setvar "cmdecho" 0)
       (setvar "highlight" 0)
       (if (> (length lst3) 0)
           (command "_.erase" na "")          ;and then remove the ent.
       );if

       (setq n 0);setq
       (repeat (length lst3)
          (setq  e1 (entget (nth n lst3))                ;get the insert.
                 e1 (subst (cons 2 bna) (assoc 2 e1) e1) ; put in the name of the anonymous block
                 e1 (rem_codes e1)                       ;remove the entname and the handle
                 e1 (subst (cons 66 0) (assoc 66 e1) e1)
           );setq

          (entmake e1)                                   ;make the insert

          (setq na (entlast));setq

          (shift_xref_insbase na (nth n lst3))

          (command "_.explode" na)                       ;explode it
          (while (wcmatch (getvar "cmdnames") "*EXPLODE*")
              (command "")
          );while
          (if (not (entget na))
             (progn
                (setq ss (acet-ss-new na));setq
                (setq bna3 (strcat bna3 ",`" bna))        ;purge it out later
                (if (not (equal n j))
                   (progn
                      (acet-ss-visible ss 1)                    ;Make the ents stealth.
                      (setq bna (acet-block-make-anon ss nil)         ; Define the anonymous block
                      );setq                               ;such that it's non-nested.
                   );progn
                );if
              );progn then the explode operation was a success.
              (progn
                (setq ss nil
                      ss (ssadd)
                      ss (ssadd na ss)
                );setq
                (if (not flag)
                    (setq flag bna);then save the first non-exploded anonymous block name
                );if
                (if (not (equal n j))
                    (setq bna (acet-block-make-anon ss nil))      ; Define a nested anonymous block cuz we
                );if                                   ;couldn't explode the insert of the last one
              );progn else
          );if
          (if (not (equal n j))                      ;if this isn't the last one,
              (command "_.erase" ss "")              ;then remove the ents used to create block.
              (progn
                                                     ; Else turn the stuff back on cuz this is
                 (if (not flag)                      ;the last time through.
                    (progn
                       (acet-ss-visible ss 0);then
                    );progn
                    (progn
                       (setq  na (tblobjname "block" flag)
                              e1 (entget na)
                              lst (list e1)
                       );setq
                       (while (setq na (entnext na))
                          (setq  e1 (entget na)
                                 e1 (subst (cons 60 0) (assoc 60 e1) e1)
                                 lst (append lst (list e1))
                           );setq
                       );while
                       (while lst
                          (entmake (car lst))
                          (setq lst (cdr lst));setq
                       );while
                       (entmake '((0 . "ENDBLK")))
                       (command "_.move" ss "" "0,0" "0,0");Done for highlighting reasons.
                    );progn else need to redefine the first block
                );if
              );progn else make the ents visible again
          );if
          (setq n (+ n 1));setq
       );repeat

       (if flag2
           (acet-ucs-cmd (list "_p"))
       );if
       (if (not (equal bna3 ""))
          (progn
             (setq bna3 (substr bna3 2)
                   bna3 (acet-str-to-list "," bna3)
              );setq
              (foreach x bna3
                  (acet-table-purge "block" x T)
              )
          );progn then
       );if
     );progn
  );if
  ; return the selection set of the cloned entities
  ss
);defun extract_clone
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shift_xref_insbase ( ss na / bna p1 e1 e2 )
 
(if (and ss
         na
         (setq e1 (entget na));setq
         (setq bna (cdr (assoc 2 e1)));setq
         (setq e2 (entget (tblobjname "block" bna)))
 
         (equal 4 (logand 4 (cdr (assoc 70 e2))))
         (setq p1 (cdr (assoc 10 e2)))
         (not (equal p1 '(0.0 0.0 0.0)))
    );and
    (command "_.move" ss "" "0,0,0" (acet-geom-vector-scale p1 -1.0))
);if
 
);defun shift_xref_insbase
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;Dictionary notes:
;na  IMAGE
;na2  340 -> IMAGEDEF (missing in xref)
;na3          330 -> DICTIONARY1
;na4                  330 -> DICTIONARY2 (3  AND 350)
;na5                          "ACAD_IMAGE_DICT" 350 -> DICTIONARY1
;na6                          "ACAD_IMAGE_VARS" 350 -> "RASTERVARIABLES"
;na7                                                      DICTIONARY2
;na8                  350 -> IMAGEDEF
;na9          330 -> IMAGEDEF_REACTOR
;na10                 330 -> IMAGE
;na11 360 -> IMAGEDEF_REACTOR
;
; group {102 330 102}
; leader 340
; mline  340
; hatch  330
;
;mline has a mlstyle of group 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun image_make ( na e1 / na2 e2 na3 e3 na4 e4 p1 p2 ina ina2)
 (setq   p1 (acet-geom-image-bounds na)
         p2 (trans
             (cadr p1)
             0 1)
         p1 (trans
              (car p1)
              0 1)
        na2 (cdr (assoc 340 e1))
         e2 (entget na2)                        ;the imagedef
        ina (cdr (assoc 1 e2))                  ;the image name w/full path
        na3 (cdr (assoc 330 e2))                ;owner dict
         e3 (entget na3)
       ina2 (cdr (assoc 3 (member (cons 350 na2) (reverse e3))));image name w/out path
        na4 (namedobjdict)                      ;current drawings' dictionary
         e4 (dictsearch na4 "ACAD_IMAGE_DICT")  ;image dict
 );setq
 (if (member (cons 3 ina2) e4)
     (setq ina ina2);then use the short name instead.
 );if
 
 (setq na (entlast))
 (command "_.-image" "_a" ina p1 p2 p2);command
 
 (if (equal na (entlast))
     (progn
      (alert
        (acet-str-format "Problem attaching image: %1\nThe file may not be on AutoCAD's search path"  ina )
      );alert
      (exit)
     );progn
     (progn
      (setq e2 (entget (entlast))
            e3 (append (list (assoc -1 e2)) e1)
            e3 (subst (assoc 5 e2) (assoc 5 e1) e3)
            e3 (subst (assoc 340 e2) (assoc 340 e1) e3)
            e3 (subst (assoc 360 e2) (assoc 360 e1) e3)
      );setq
      (entmod e3)
      (entupd (cdr (assoc -1 e3)))
      (acet-ss-visible (ssadd (entlast) (ssadd)) 1)
     );progn else
 );if
);defun image_make
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun select_nested ( msg / errno_sel lst ss ss2 na e1 n a lst2)
 
 ;local function
 (defun errno_sel ( msg / a)
  (setvar "errno" 0)
  (while (and (not (setq a (nentsel msg)))
              (equal 7 (getvar "errno"))
         );and
    (setvar "errno" 0)
  );while
  a
 );defun errno_sel
 
 (setvar "cmdecho" 0)
 (acet-ss-clear-prev)
 (setvar "highlight" 1)
 (command "_.select")
 (while (setq lst (errno_sel msg))
  (setq na (car lst)
        e1 (entget na)
  );setq
 
  (if (and (not (equal (length lst) 4))
           (not (equal "ATTRIB" (cdr (assoc 0 e1))))
      );and
      (progn
       (if (equal "VERTEX" (cdr (assoc 0 e1)))
           (command (cadr lst));then get the polyline header by directly selecting
           (setq ss nil
                 ss (ssadd)
                 ss (ssadd (car lst) ss)
           );setq
       );if
      );progn then it's non-nested.
      (progn
       (command "")
       (setq ss2 (ssget "_p"))
 
       (if (and (equal "ATTRIB" (cdr (assoc 0 e1)))
                (not (equal (length lst) 4))
           );and
           (progn
            (setq lst (append lst
                              (list nil)
                              (list nil) ;;;(list (list (ssname (ssget "_p") 0)))
                      );append
            );setq
           );progn then do special stuff for the attrib
       );if
       (setq ss (extract_clone lst))
 
       (if (and ss
                (> (sslength ss) 0)
           );and
           (setq lst2 (append lst2 (list ss)))
       );if
       (setvar "cmdecho" 0)
       (acet-ss-clear-prev)
       (setvar "highlight" 1)
       (command "_.select")
       (if (and ss2
                (> (sslength ss2) 0)
           );and
           (command ss2)
       );if
      );progn else it's nested
  );if
  (if (and ss
           (> (sslength ss) 0)
      );and
      (command ss)
  );if
 );while
 
 (while (wcmatch (getvar "cmdnames") "*SELECT*") (command ""));while
 (setq ss (ssget "_p"))
 
 (setvar "cmdecho" 0)
 (setvar "highlight" 0)
 (acet-ss-clear-prev)
 (command "_.select")
 (setq n 0)
 (repeat (length lst2)
 (setq a (nth n lst2))
  (command a)
 (setq n (+ n 1));setq
 );repeat
 (command "")
 (setq lst2 nil)
 (setq ss2 (ssget "_p"));nested ents
 
(list ss ss2)
);defun select_nested
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun do_voodo ( msg do flt / flag la ss ss2 n cleanup emarker )
 (acet-error-init (list (list    "cmdecho" 0
                               "highlight" 1
                                 "ucsicon" (getvar "ucsicon")
                                  "osmode" 0
                               "ucsfollow" 0
                                "limcheck" 0
                         );list
                         0 ;T
                         '(cleanup ss2)
                   );list
 );acet-error-init
 
 ;;;Local function
 (defun cleanup ( ss2 / la )
  (acet-sysvar-set '("cmdecho" 0))
  (if (not ss2)
      (setq ss2 (acet-ss-new emarker))
  );if
  (if (and ss2
           (> (sslength ss2) 0)
           (entget (ssname ss2 0))
      );and
      (progn
       (if (setq la (acet-layer-locked (getvar "clayer"))) ;just in case of transparent layer changes
           (command "_.layer" "_unlock" (getvar "clayer") "")
       );if
       (acet-ss-entdel ss2)
       (if la
           (command "_.layer" "_lock" (getvar "clayer") "")
       );if
      );progn
  );if
  (if (and flag
           (equal (type flag) 'STR)
      );and
      (command "_.-layer" "_lock" flag "")
  );if
  (acet-sysvar-restore)
 );defun cleanup
 
 (setq emarker (entlast)
            la (getvar "clayer")
          flag (acet-layer-unlock-all)
 );setq
 (setq  ss (select_nested msg)
       ss2 (cadr ss);nested only
        ss (car ss) ;all.
 );setq
 (if (and ss
          (> (sslength ss) 0)
          (or (not flt)
              (progn
               (setq n (sslength ss))
               (command "_.select" ss "")
               (if (and (setq ss (ssget "_p" flt))
                        (not (equal n (sslength ss)))
                   );and
                   (princ (acet-str-format "\n%1 objects were invalid for %2" (itoa (- n (sslength ss))) do))
               );if
               ss
              );progn
          );or
     );and
     (progn
      (setvar "cmdecho" 0)
      (setvar "highlight" 1)
      (command (strcat "_." do) ss)
      (setvar "cmdecho" 1)
      (if (wcmatch (getvar "cmdnames")
                   (strcat "*" (xstrcase do) "*")
          )
          (command "")
      );if
      (while (wcmatch (getvar "cmdnames")
                      (strcat "*" (xstrcase do) "*")
             )
       (command pause)
      );while
     );progn
 );if
 
 (cleanup ss2)
 (if (and ss2
          (> (sslength ss2) 0)
     );and
     (progn
      (if (not ss)
          (princ (acet-str-format "\nNo valid objects selected for %1.\n"  do ))
      );if
     );progn
 );if
 
 (if flag (command "_.-layer" "_lock" flag ""));if
 (acet-error-restore)
 
 (princ)
);defun do_voodo
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rem_codes ( e1 / na2 na3 e2)
 (setq e1 (acet-list-assoc-remove -1 e1)
       e1 (acet-list-assoc-remove 5 e1)
       e1 (acet-list-assoc-remove 102 e1)
       e1 (acet-list-assoc-remove 67 e1)
       e1 (remx_codes e1)
 );setq
 (if (not (assoc 62 e1))
     (setq e1 (append e1 (list '(62 . 256))));setq then force bylayer
 );if
 (if (not (assoc 6 e1))
     (setq e1 (append e1 (list '(6 . "BYLAYER"))));setq then force bylayer
 );if
 (cond
  ((equal "ATTRIB" (cdr (assoc 0 e1)))
   (setq e1 (acet-list-assoc-remove 3 e1)
         e1 (subst '(0 . "ATTDEF") '(0 . "ATTRIB") e1)
         e1 (append e1 (list (cons 3 (cdr (assoc 1 e1)))))
         e1 (acet-list-assoc-remove 100 e1)
   );setq
  )
  ((equal "HATCH" (cdr (assoc 0 e1)))
   (setq e1 (subst '(71 . 0) (assoc 71 e1) e1)
         e1 (subst '(97 . 0) (assoc 97 e1) e1)
         e1 (acet-list-assoc-remove 330 e1)
   );setq
  )
  ((equal "MLINE" (cdr (assoc 0 e1)))
   (setq   e2 (dictsearch (namedobjdict) "ACAD_MLINESTYLE")
          na2 (cdr (assoc -1 e2))
          na3 (cdr (assoc 350 (member (assoc 3 e2) e2)))
           e1 (subst (cons 340 na3) (assoc 340 e1) e1)
   )
  )
  ((equal "MTEXT" (cdr (assoc 0 e1)))
   (setq e1 (acet-list-assoc-remove 50 e1))
  )
 );cond
 e1
);defun rem_codes
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remx_codes ( e1 / lst a b c d n na2 e2)
;
(setq   d (cdr (assoc 0 e1))
      lst (list '(6 . "LTYPE")
                '(7 . "STYLE")
                '(8  . "LAYER")
          );list
);setq
(if (or (equal "DIMENSION" d)
        (equal "TOLERANCE" d)
        (equal "LEADER" d)
    );or
    (setq lst (append lst (list '(3 . "DIMSTYLE"))));setq then deal with dimstyles
);if
(setq n 0);setq
(repeat (length lst)
(setq a (nth n lst)
      b (cdr a)
      a (car a)
      c (cdr (assoc a e1))
);setq
(if (and c
         (setq na2 (tblobjname b c))
         (setq e2 (entget na2))
         (equal 16 (logand 16 (cdr (assoc 70 e2))))
    );and
    (progn
 
     ;yank it out for now.
     (if (equal "STYLE" b)
         (setq e1 (subst (cons 7 (getvar "textstyle")) (assoc 7 e1) e1));setq
         (setq e1 (acet-list-assoc-remove a e1))
     );if
     ;(if (equal 32 (logand 32 (cdr (assoc 70 e2))))
     ;    (progn
     ;     ;bind it?
     ;    );progn
     ;    (setq e1 (rem_code a e1));yank it
     ;);if
    );progn then xref dependant symbol table entry.
);if
(setq n (+ n 1));setq
);repeat
 
e1
);defun remx_code
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:btrim ( / flt )
 (setq flt '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") (0 . "TOLERANCE")
              (0 . "ARC") (0 . "CIRCLE") (0 . "LINE") (0 . "LWPOLYLINE")
              (-4 . "<AND")
               (0 . "POLYLINE")
               (-4 . "<NOT") (-4 . "&") (70 . 112) (-4 . "NOT>")
              (-4 . "AND>")
              (0 . "ELLIPSE") (0 . "SPLINE") (0 . "LEADER") (0 . "REGION")
              (0 . "IMAGE") (0 . "HATCH") (0 . "MLINE")
              (0 . "XLINE") (0 . "RAY")
             (-4 . "OR>")
            )
 );setq
 (do_voodo "\nSelect cutting edges: " "trim" flt)
);defun c:btrim
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:bextend ( / flt )
 (setq flt '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") (0 . "TOLERANCE")
              (0 . "ARC") (0 . "CIRCLE") (0 . "LINE") (0 . "LWPOLYLINE")
              (-4 . "<AND")
               (0 . "POLYLINE")
               (-4 . "<NOT") (-4 . "&") (70 . 112) (-4 . "NOT>")
              (-4 . "AND>")
              (0 . "ELLIPSE") (0 . "SPLINE") (0 . "LEADER") (0 . "REGION")
              (0 . "IMAGE") (0 . "HATCH") (0 . "MLINE")
              (0 . "XLINE") (0 . "RAY")
             (-4 . "OR>")
            )
 );setq
 (do_voodo "\nSelect edges for extend: " "extend" flt)
);defun c:bextend


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAHuvDv/GD99ZvExp3iUiviXsH1Uv5uXpYMsnKJ
;;; k61haSBgdOL94FZiCO93Yqr7VFkSJ7EqQXyRpPPtToEm3634+YjH21/uFjgyjVZn
;;; 55OHMOJl83MO29SGVIPyr5BHCZxYCeZyuLiSSaswpNyrq+30xKpcWTN2jkGg7YpH
;;; fMijuokJrcVs+73G+0FCr4tZFypLWqOaFAeJReg8s6pb+atozUtH4XJb5kM9L91F
;;; SqMIVrefvnofvD8pZj5t7pEx0hDMhDe2mgRGdHsVxFBGCB5z4sAeBTUT9jXIeG1V
;;; r0J30L8XrUYaLdcTnI2MYMPoKgtT/ecvwK1sfoSEXyTLa3+Vdi9iENg99zXXy6Ua
;;; CHfqctD+kMxC6exbcvM1No03WGYZxQradrpEVoQ67GB1Ci5N+nuba+6ej/fzubnl
;;; fgKF/2itEs0M4RliyvC517QCqgKaBjGiM1zJfCWHCZ7xHc32Nj+lRyXBgpEVwa7s
;;; lOcgGbQLICVhxkcw8uR/Kvypb5Megg6DbVPi55AebKuCGE9UsmJrM7I6CiI+/Wth
;;; v7eRj+M3Hco0R6rNqab+6EjcDTiGDo6c6esyCbAS6AP4iLJZPoA2YwXg3UK93HIS
;;; RLe5ja/E32+/h4YDb9RrFmU5nz/ty8jM1TC775Hrjx+xoF6RtYhWLisV/iswZOVC
;;; cniOXA==
;;; -----END-SIGNATURE-----