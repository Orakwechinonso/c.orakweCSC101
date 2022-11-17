;;;    FASTSEL.LSP
;;;    Created 7/21/97 by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:fastsel ( / old_err ss ss2 n na )
 (setq old_err *error*)
 (defun *error* ( a / )
  (princ a)
  (setq *error* old_err)
  (princ)
 );defun
 
(fsmode_init)
(princ "\nUse 'FSMODE to control chain selection.")
(princ (strcat "\nFSMODE = " #fsmode))
(setq ss2 (fs_get_current_sel)
       ss (fastsel)
);setq
 
(if (and ss
         (princ (strcat (itoa (sslength ss))
                        " object(s) found."
                )
         )
         (not (equal (getvar "cmdnames") ""))
    );and
    (command ss);then pass in the selection set
    (progn
     (if (and ss
              (equal 1 (getvar "pickfirst"))
         );and
         (progn
          (if (not ss2)
              (setq ss2 ss)
              (progn
               (setq n 0)
               (repeat (sslength ss)
                (setq na (ssname ss n));setq
                (if (not (ssmemb na ss2))
                    (setq ss2 (ssadd na ss2))
                );if
                (setq n (+ n 1));setq
               );repeat
              );progn then combine the previously gripped stuff with
                      ;the selection set returned from fastsel
          );if
          (sssetfirst ss2 ss2)
 
         );progn else just set a grip-ed selection set.
         (princ "\nNothing found")
     );if
    );progn else
);if
 
(setq *error* old_err)
(princ "\nExiting Fastsel")
(princ)
);defun c:fastsel
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:fs ()
 (c:fastsel)
);defun c:fs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:fsmode ( / old_err fsmode )
 (setq old_err *error*)
 (defun *error* ( a / )
  (princ a)
  (setq *error* old_err)
  (princ)
 );defun
 
(fsmode_init)
(initget "ON OFf")
(if (setq fsmode (getkword (strcat "\nFASTSEL chain selection <" #fsmode ">: ")));setq
    (progn
     (setq #fsmode (xstrcase fsmode))
     (setenv "BNS_FSMODE" #fsmode)
    );progn then
);if
 
(setq *error* old_err)
(princ)
);defun c:fsmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fsmode_init ()
 (if (not #fsmode)
     (setq #fsmode (getenv "BNS_FSMODE"))
 );if
 (if (and (not (equal "ON" #fsmode))
          (not (equal "OFF" #fsmode))
     );and
     (progn
      (setq #fsmode "OFF");setq
      (setenv "BNS_FSMODE" #fsmode)
     );progn then
 );if
);defun fsmode_init
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fastsel ( / flt na fsmode lst2 ss px px2 z j lst a b lst3 lst4
                       n c d ss2 ss4 ss5
               )
 
(setq    flt '(
                (0 . "LINE") (0 . "POLYLINE") (0 . "LWPOLYLINE") (0 . "CIRCLE")
                (0 . "ARC") (0 . "ATTDEF") (0 . "TEXT") (0 . "MTEXT")
                (0 . "ELLIPSE") (0 . "IMAGE") (0 . "SPLINE") (0 . "POINT")
                (0 . "INSERT") (0 . "3DFACE") (0 . "TRACE") (0 . "SOLID")
              )
          na (bns_fast_sel "\nSelect touching object: " flt)
         flt (append '((-4 . "<OR")) flt '((-4 . "OR>")))
      fsmode "ON"
);setq
(if na
    (progn
     (setq  lst2 (list na)
              ss (ssadd na (ssadd))
              px (acet-geom-pixel-unit)
             px2 (* px 0.75)
               z 0
               j 0
     );setq
     (while (and (< j (length lst2))
                 (equal fsmode "ON")
            );and
      (setq fsmode #fsmode)
      (setq na (nth j lst2));setq
      (setq  lst (acet-list-remove-adjacent-dups (acet-geom-object-point-list na (/ px 2.0)))
               a (car lst)
               b (cadr lst)
      );setq
      (if b
          (setq lst3 (list (polar a (+ (angle b a) (/ pi 2.0)) px2));list
                lst4 (list (polar a (- (angle b a) (/ pi 2.0)) px2));list
          );setq then
      );if
      (setq n 0)
      (repeat (max 0 (- (length lst) 1))
       (setq a (nth n lst)
             b (nth (+ n 1) lst)
             c (polar b (- (angle a b) (/ pi 2.0)) px2)
             d (polar b (+ (angle a b) (/ pi 2.0)) px2)
       );setq
       (if (not (equal c (last lst3) 0.00001))
           (setq lst3 (append lst3 (list c)));setq then
       );if
       (if (not (equal d (last lst4) 0.00001))
           (setq lst4 (append lst4 (list d)));setq then
       );if
       (setq n (+ n 1));setq
      );repeat
      (setq ss2 (f_on_screen lst flt))
      (setq ss4 (f_on_screen lst3 flt))
      (setq ss5 (f_on_screen lst4 flt))
      (if ss2
          (progn
           (setq n 0)
           (repeat (sslength ss2)
            (setq na (ssname ss2 n))
            (if (not (member na lst2))
                (setq lst2 (append lst2 (list na))
                        ss (ssadd na ss)
                );setq
            );if
            (setq n (+ n 1));setq
           );repeat
          );progn
      );if
      (if ss4
          (progn
           (setq n 0)
           (repeat (sslength ss4)
            (setq na (ssname ss4 n))
            (if (not (member na lst2))
                (progn
                 (setq lst2 (append lst2 (list na))
                         ss (ssadd na ss)
                 );setq
                );progn then
            );if
            (setq n (+ n 1));setq
           );repeat
          );progn
      );if
      (if ss5
          (progn
           (setq n 0)
           (repeat (sslength ss5)
            (setq na (ssname ss5 n))
            (if (not (member na lst2))
                (progn
                 (setq lst2 (append lst2 (list na))
                         ss (ssadd na ss)
                 );setq
                );progn then
            );if
            (setq n (+ n 1));setq
           );repeat
          );progn
      );if
      (setq j (+ j 1))
     );while
 
    );progn then
);if
 
ss
);defun fastsel
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;select the seed ent.
(defun bns_fast_sel ( msg flt / filter_check na)
 ;local function
 (defun filter_check ( na flt / e1 a n flag)
  (cond
   ((not na) (setq flag nil))
   ((not flt) (setq flag T))
   (T
    (setq e1 (entget na));setq
    (setq n 0)
    (while (and (not flag)
                (< n (length flt))
           );and
     (setq a (nth n flt));setq
     (if (member a e1)
         (setq flag T);setq then got a match for the filter
     );if
     (setq n (+ n 1));setq
    );while
   )
  );cond close
  flag
 );defun filter_check
 
(if (not (equal (substr msg 1 1) "\n"))
    (setq msg (strcat "\n" msg))
);if
(while (not na)
 (setvar "errno" 0)
 (while (or (and (not (setq na (car (entsel msg))))
                 (equal 7 (getvar "errno"))
            );and
            (and na
                 (not (filter_check na flt))
            );and
        );or
   (if (equal 7 (getvar "errno"))
       (princ "\n0 found")
       (progn
        (if na
            (princ (strcat "\n*Invalid* Must select "
                           "LINE, POLYLINE, LWPOLYLINE, CIRCLE, ARC, ATTDEF, TEXT,"
                           "MTEXT, ELLIPSE, or IMAGE object.\n"
                   );strcat
            );princ
        );if
       );progn
   );if
   (setvar "errno" 0)
 );while
 (cond
  ((equal (getvar "errno") 52) ;enter
   (setq na 99);
  )
 );cond close
);while
(if (equal na 99) (setq na nil))
na
);defun bns_fast_sel
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fs_get_current_sel ( / ss)
(if (and (equal 1 (getvar "pickfirst"))
         (cadr (ssgetfirst))
    );and
    (setq ss (cadr (ssgetfirst)));then something is already selected so get it.
);if
ss
);defun fs_get_current_sel
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;returns a list of points on screen if the first two lists do not
;contain segments that intersect each other.
;
(defun f_on_screen ( lst flt / vd p1 p2 p3 p4 lst2 lst3 n a b c d
                               x1 x2 x3 x4 pnt j ss ss2 na pnt2 dst dlst
                   )
 
(setq  vd (trans (getvar "viewdir") 1 0 T)
       p1 (acet-geom-m-trans (acet-geom-view-points) 1 vd)      ;variables p1, p2, p3, and  p4 are corner points
       p3 (cadr p1)                       ; of the current view
       p1 (car p1)
       p2 (list (car p3) (cadr p1));list
       p4 (list (car p1) (cadr p3));list
      dst (distance (getvar "extmin") (getvar "extmax"))
      lst (acet-geom-m-trans lst 1 vd)
        a (car lst)                 ;the first point in lst expressed in view coords.
        c (list (car a) (cadr a))
);setq
 
(if (and (<= (car c) (car p3))    ;if the first point is on screen then add it to lst2
         (<= (cadr c) (cadr p3))
         (>= (car c) (car p1))
         (>= (cadr c) (cadr p1))
    );and
    (setq lst2 (list a));setq
);if
 
(setq n 0)
(repeat (max (- (length lst) 1)
             0
        )
 (setq  a (nth n lst)             ;the first point
        c (list (car a) (cadr a)) ;the same point without the z
        b (nth (+ n 1) lst)       ;the second point
        d (list (car b) (cadr b)) ;ditto with no z
       x1 (inters p1 p2 c d)      ;check for intersections
       x2 (inters p2 p3 c d)
       x3 (inters p3 p4 c d)
       x4 (inters p4 p1 c d)
 );setq
 (if (or x1 x2 x3 x4)
     (progn             ;then intersection(s) were found
      (setq dlst nil)   ;Now build a list of sublist containing the
                        ;the distance from the intersecting point to point 'a'
                        ; and 'a' the point it's self.
      (if x1
          (setq dlst (append dlst (list (list (distance x1 c) x1))));setq
      );if
      (if x2
          (setq dlst (append dlst (list (list (distance x2 c) x2))));setq
      );if
      (if x3
          (setq dlst (append dlst (list (list (distance x3 c) x3))));setq
      );if
      (if x4
          (setq dlst (append dlst (list (list (distance x4 c) x4))));setq
      );if
      (setq dlst (acet-list-isort dlst 0)) ;sort the list of sublists based on distance from 'a'
      (setq j 0)
      (repeat (length dlst)                              ;then add them one at a time to lst2
       (setq pnt (nth j dlst)                            ;the sub-list (dist, point)
             pnt (cadr pnt)                              ;the point
             pnt (list (car pnt) (cadr pnt) (* -1.0 dst));now get ready to create a segment
            pnt2 (list (car pnt) (cadr pnt) dst)         ;that is normal to the view and very long
             pnt (inters a b pnt pnt2 nil)               ;check for 3d intersect to get true
                                                         ;location
       );setq
       (if (and pnt
                (not (equal pnt (last lst2)))
           );and
           (setq lst2 (append lst2 (list pnt)));setq
       );if
      (setq j (+ j 1));setq
      );repeat
     );progn then find the intersection closest to a
     (setq dlst nil);else no intersections
 );if
 (if (and (<= (car d) (car p3))
          (<= (cadr d) (cadr p3))
          (>= (car d) (car p1))
          (>= (cadr d) (cadr p1))
          (not (equal b (last lst2)))
     );and
     (setq lst2 (append lst2 (list b)));setq then
 );if
 (if dlst
     (progn
      (setq lst2 (acet-geom-m-trans lst2 vd 1)
            lst3 (append lst3 (list lst2))
            lst2 nil
      );setq
      (if (and (<= (car d) (car p3))
               (<= (cadr d) (cadr p3))
               (>= (car d) (car p1))
               (>= (cadr d) (cadr p1))
               (not (equal b (last lst2)))
          );and
          (setq lst2 (append lst2 (list b)));setq then
      );if
     );progn then
 );if
(setq n (+ n 1));setq
);repeat
(if (and lst2
         (setq lst2 (acet-geom-m-trans lst2 vd 1))
         (not (member lst2 lst3))
    );and
    (setq lst3 (append lst3 (list lst2)));setq then
);if
 
(setq ss2 (ssadd))
(setq n 0)
(repeat (length lst3)
 
(if (and (> (length (nth n lst3)) 1)
         (setq ss (ssget "_f" (nth n lst3) flt));setq
    );and
    (progn
 
     (setq j 0)
     (repeat (sslength ss)
      (setq na (ssname ss j))
      (if (not (ssmemb na ss2))
          (setq ss2 (ssadd na ss2));setq then
      );if
     (setq j (+ j 1));setq
     );repeat
    );progn
);if
(setq n (+ n 1));setq
);repeat
 
ss2
);defun f_on_screen


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgC0gjJohjTsKPk6Apf50bcYITCSL8RyCutFEVh6
;;; 810YNOR7BrRg6gDwEhYU9kbMs2uzj4LRLjGQ74yyuBcvITIWfc34o/Gz7oBcZSo3
;;; thmHKt50PDgRyDqapig0qQmmGjt2VEcZ1x4Z4QUwQAQeqQg+DZFaQCVwaGXTkpho
;;; 8sL3/zsBG5LWJiWrreLuGg+tuTLFWnySnbSrlQmI9EyIZohRGId2CbOM2oyhjFcK
;;; d5SqPvAyx/3Vuc2XZZr+Mp2gHta8ZpLHb6pY8gTUyK2gZIa77LfHBjA1G/DztFMn
;;; Z8UzMO01TLncBdvD7Z6VB+Z3rrPwAYGxj3wdVQP39zF+GC4XjG9sezDKLayf2LVk
;;; 2ifVOytuQdA1lOEoeq5E2BWsogFl+SVG+0scfrErg9O0f+voTsOR2dbgrDh4f5ZO
;;; py7vtk5DzXZgGP4PkQOy9bsvxNdJmZqohzSmwUhhAh1HTjvu6UbjiMpqVy1TUDeP
;;; GhaXVkAEaFL2rA7IwFvXvjQDlRlVzerBsITdtxuoyD6RysIlTwL0MyPGxD8TG7hu
;;; lGHW7uC1JWrifg/HcSNaUZCNt0l618/DVcunFcYrXQLAWufYNjXIc+rH4p+lhjeR
;;; TFxDW+88ROyQGQQyaDZ03TQIaVulz6Ywkjyfzp8PT8kL+7ahGSls6pnS4+ie86zA
;;; aaZZJg==
;;; -----END-SIGNATURE-----