;;
;;;
;;;    BLOCKTOXREFSup.LSP - Randy Kintzley
;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a block name and a file name and replaces all references to the block with xref
;references. 
;if the prg flag is true the block will be purged.
;
(defun acet-block-to-xref ( bna fna prg / tmp na bna2 j n tmp2 msg lst lalst lamap origlays lk e1 )
 
 (setq bna2 (acet-filename-ext-remove (acet-filename-path-remove fna)))
 (cond 
  ((acet-str-equal bna bna2)
      ;; then bna and bna2 are equal so need to rename bna to a temp name
      (setq tmp bna) ;save the original name
   
      ;; get a unique block name
      (setq n 1);setq
      (while (and (setq bna (strcat tmp "-OLD" (itoa n)));setq
                  (tblobjname "block" bna)
             );and
       (setq n (+ n 1));setq
      );while
      (command "_.rename" "_block" tmp bna)
  );cond #1 bna and bna2 are equal so need to rename bna to a temp name
 
  ((and (setq na (tblobjname "block" bna2))
        (setq e1 (entget na))
        (/= 4 (logand 4 (cdr (assoc 70 e1))))
   );and
      (setq tmp2 bna2)
      (setq n 1);setq
      (while (and (setq bna2 (strcat tmp2 "-" (itoa n)));setq
                  (tblobjname "block" bna2)
             );and
       (setq n (+ n 1));setq
      );while
      (setq msg (acet-str-format 
                 "\nThere is already a standard block with the same name as the specified xref: %1." tmp2
                )
            msg (strcat msg (acet-str-format "\nUsing %1 instead." bna2))
      );setq
      (acet-alert msg)
      (setq fna (strcat bna2 "=" fna))
  );cond #2
 );cond close
(acet-ui-progress-init "Converting blocks to xrefs" 12)
 (setq origlays (acet-table-name-list "layer"))
(acet-ui-progress-safe 1)
 (setq na (entlast))
 (acet-safe-command T T (list "_.-xref" "_a" fna "0,0" "0.00000001" "0.00000001" "0"))
 (if (not (equal na (entlast)))
     (progn
      (setq lk (acet-layer-unlock-all))
      (entdel (entlast))
      (command "_.redraw")	;; force an update
(acet-ui-progress-safe 2)
      (if (or prg
              (= 1 (getvar "visretain"))
          );or
          (progn
(acet-ui-progress-safe 3)
           (setq lst (acet-block-referenced-tables bna nil))	;; get a list of table references so that they can be purged.
 
(acet-ui-progress-safe 4)
           ;; If visretain is on, then after replacing, map the old layer settings to the 
           ;; newly imported xref layers
           (if (= 1 (getvar "visretain"))
               (setq lamap (acet-blocktoxref-local-to-xref-layer-map bna2))
           );if
(acet-ui-progress-safe 5)
          );progn then
      );if
(acet-ui-progress-safe 6)
      (setq j (acet-block-replace bna bna2))
(acet-ui-progress-safe 8)
      (if lk
          (command "_.-layer" "_lock" lk "")
      );if
(acet-ui-progress-safe 9)
      (if lst
          (progn
           (if (and (= 1 (getvar "visretain"))
                    lamap
                    (setq lalst (cdr (assoc 8 lst)))		;; referenced layers
               );and
               (acet-blocktoxref-apply-xref-layer-props lalst origlays lamap)
           );if
(acet-ui-progress-safe 10)
           (if prg
               (acet-block-referenced-tables-purge lst)
           );if
(acet-ui-progress-safe 12)
          );progn then
      );if
     );progn then xref was sucessful
     (princ "\nXref operation failed.")
 );if
 
 (if (and tmp 					;; original block was renamed 
          (tblobjname "block" bna)		;; and it's still around
          (not (tblobjname "block" tmp))	;; and no block with the original name is defined
     );and
     (command "_.rename" bna tmp);; so rename it back to its original name
 );if
 
(acet-ui-progress-done)
 
 
 j
);defun acet-block-to-xref
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Takes an xref name and returns a loist of pairs: ((local xref) (local xref) (local xref) ...)
;
(defun acet-blocktoxref-local-to-xref-layer-map ( xrefname / lst xla locla lst2 loclst )
 (setq xrefname (xstrcase xrefname)
            lst (bns_tbl_match "layer" 
                                (list (cons 2 (strcat xrefname "|*")) 
                                      '(-4 . "&") 
                                      (cons 70 (+ 16 32))
                                )
                );bns_tbl_match
            lst (mapcar '(lambda (x) 
                           (xstrcase (cdr (assoc 2 x)))
                         )
                        lst
                )
         loclst (bns_tbl_match "layer" 
                               (list '(-4 . "<NOT") '(-4 . "&") (cons 70 (+ 16 32)) '(-4 . "NOT>"))
                );bns_tbl_match
         loclst (mapcar '(lambda (x) 
                          (xstrcase (cdr (assoc 2 x)))
                         )
                         loclst
                )
 );setq
 (foreach xla lst
  (if (setq locla (acet-blocktoxref-local-layer-name xla loclst))
      (setq lst2 (cons (list locla xla) lst2));setq then
  );if
 );foreach
 
 (reverse lst2)
);defun acet-blocktoxref-local-to-xref-layer-map
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Takes an xref layer name and attempts to find a local layer that was created as a
; result of a bind.
;
(defun acet-blocktoxref-local-layer-name ( xla loclst / xrefname layername lst n a b flag )
 
 (setq  xla (xstrcase xla)
          n (acet-str-find "|" xla)
   xrefname (substr xla 1 (- n 1))
  layername (substr xla (+ n 1))
          b (strcat xrefname "$*$" layername)
 );setq
 
 (setq n 0)
 (while (and (< n (length loclst))
             (not flag)
        );and
  (setq a (nth n loclst))
  (if (wcmatch a b)
      (setq flag a) 
  );if
 (setq n (+ n 1));setq
 );while
 
 (setq n 0)
 (while (and (< n (length loclst))
             (not flag)
        );and
  (setq a (nth n loclst))
  (if (equal a layername)
      (setq flag a)
  );if
 (setq n (+ n 1));setq
 );while
 
 flag
);defun acet-blocktoxref-local-layer-name
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes:
;lalst-  a list of referenced layers
;origlays - a list of original layers that existed prior to xref attach operation.
;lamap - layer map ((local xref) (local xref) (local xref) ...)
;
;This function applies the properties of the local layers to new xref layers.
;
(defun acet-blocktoxref-apply-xref-layer-props ( lalst origlays laymap / la a )
 (setq    lalst (mapcar 'xstrcase lalst)
       origlays (mapcar 'xstrcase origlays)
 );setq
 (foreach la lalst
  (if (and (setq a (assoc la lamap))
           (not (member (cadr a) origlays))
      );and
      (acet-blocktoxref-copy-layer-props (car a) (cadr a));; then new xref layer so copy local props to new xref layer
  );if
 );foreach
 
);defun acet-blocktoxref-apply-xref-layer-props
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;Copy the properties from la to la2
;
(defun acet-blocktoxref-copy-layer-props ( la la2 / a b e1 e2 gc gclst )
 (setq    e1 (entget (tblobjname "layer" la))
          e2 (entget (tblobjname "layer" la2))
           a (cdr (assoc 70 e2))
           a (logand a (+ 16 32))
           b (cdr (assoc 70 e1))
          e2 (subst (cons 70 (logior a b)) (assoc 70 e2) e2)
       gclst (list 62 6 290 370 390)
 );setq
;(if (acet-str-equal (cdr (assoc 2 e1)) "xref1$0$3dpoly")
;    (progn
;     (print e1)
;     (print e2)
;     (getstring "hey")
;    );progn then
;);if
 (foreach gc gclst
  (if (setq a (assoc gc e1))
      (progn
       ;; if it's a linetype with a mangled named ($#$) resulting from a bind, then try to get a local 
       ;; non-mangled version of the same linetype.
       (if (and (= gc 6)
                (setq b (xstrcase (cdr a)))
                (wcmatch b "*$*$*")
                (or (wcmatch b "*$#$*")
                    (wcmatch b "*$##$*")
                    (wcmatch b "*$###$*")
                );or
                (setq b (last (acet-str-to-list "$" b)))
                (/= b "")
                (or (tblobjname "ltype" b)
                    (progn
                     (acet-safe-command T T (list "_.ltype" "_load" b "acad.lin" ""))
                     (tblobjname "ltype" b)
                    );progn
                );or
           );and
           (setq a (cons (car a) b));setq then use the non-mangled version
       );if
       (setq e2 (subst a (assoc gc e2) e2))
      );progn then
  );if
 );foreach
 (entmod e2)
);defun acet-blocktoxref-copy-layer-props
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-block-referenced-tables-purge ( tblst / gclst lst x gcode tbl name ) 
 ;; purge order
 (setq gclst '((2 . "block")
               (3 . "dimstyle")
               (8 . "layer")
               (6 . "ltype")
               (7 . "style")
              )
 );setq
 (princ "\nPurging...")
 (princ "\n")
 (repeat 2 	;; make two passes on the purge to make sure we get it all.
 
  (foreach x gclst
   (setq  gcode (car x)
         tbl (cdr x)
         lst (cdr (assoc gcode tblst))
   );setq
   (foreach name lst
     (if (or (/= gcode 8)
             (and (/= "0" name)
                  (not (acet-str-equal "DEFPOINTS" name))
             );and
         );or
         (acet-table-purge tbl name T)
     );if
   );foreach
 
  );foreach
 
 );repeat
 (princ "done.\n")
 
);defun acet-block-referenced-tables-purge
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a block name and returns a list of sublists containing symbol table references.
;i.e.
;( ("layer" name1 name2 name3 ...)
;  ("block" name1 name2 name3 ...)
;  ...
;)
(defun acet-block-referenced-tables ( bna tblst / na e1 )
 (if (not tblst)
     (setq tblst (list '(8)			;; layer
                       (list 2 bna)		;; blockname
                       '(7)			;; style
                       '(3)			;; dimstyle
                       '(6)			;; ltype
                 );list
     );setq
 );if
 (setq na (tblobjname "block" bna)
       e1 (entget na)
 );setq
 (while (and (setq tblst (acet-block-referenced-tables-ent e1 tblst))
             (setq na (entnext na))
             (setq e1 (entget na))
        );and
 );while
 tblst
);defun acet-block-referenced-tables
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-block-referenced-tables-ent ( e1 tblst / n lst a gcode tp lst2 b )
 (setq tp (cdr (assoc 0 e1)));setq
 (setq n 0)
 (repeat (length tblst)
  (setq lst (nth n tblst)
         gcode (car lst)
  );setq
  (if (setq a (cdr (assoc gcode e1)))
      (progn
       (cond
        ((= gcode 3)
         (if (and (/= tp "DIMENSION")
                  (/= tp "TOLERANCE")
                  (/= tp "LEADER")
             );and
             (setq lst nil);then not a dim
             (progn
              (if (= tp "DIMENSION")
                  (progn
                   (setq lst2 (assoc 2 tblst)
                            b (cdr (assoc 2 e1))
                   );setq
                   (if (not (member b lst2))
                       (setq tblst (acet-block-referenced-tables		;; recursive call
                                     b
                                     tblst
                                   )
                             tblst (acet-list-assoc-append (list 2 b) tblst)	;; add this anon block 
                       );setq then add the anonymous block referenced by the dim
                   );if
                  );progn then it's a dim so check for anonymous block references
              );if
             );progn else
         );if
        );cond #1 dimension
        ((and (= gcode 2)
              (not (member a lst))
         );and
         (if (/= tp "INSERT")
             (setq lst nil)
             (setq tblst (acet-block-referenced-tables a tblst));setq recursion excursion
         );if
        );cond #2
       );cond close
       (if (and lst
                (not (member a lst))
           );and
           (setq tblst (acet-list-assoc-append (list gcode a) tblst));setq
       );if
      );progn then
  );if
  (setq n (+ n 1));setq
 );repeat
 tblst
);defun acet-block-referenced-tables-ent
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes two block names and replaces all references to the first with references to 
;the second.
;returns the number of block references modified
;
(defun acet-block-replace ( bna bna2 / flt ss n j na e1 lst )
   (setq flt (list '(0 . "INSERT")
                    (cons 2 bna)
             )
          ss (ssget "_x" flt)
   );setq
   (setq j 0)
   (if ss
       (progn
        (setq n 0)
        (repeat (sslength ss)
         (setq na (ssname ss n)
               e1 (entget na)
               e1 (subst (cons 2 bna2) (assoc 2 e1) e1)
         );setq
         (if (entmod e1)
             (setq j (+ j 1))
         );if
         (setq n (+ n 1));setq
        );repeat
       );progn then got a selection set of inserts
   );if
   
   ;; get a list of nested block inserts
   (setq lst (bns_blktbl_match flt)) 
   (setq n 0)
   (repeat (length lst)
    (setq na (car (nth n lst))
          e1 (entget na)
          e1 (subst (cons 2 bna2) (assoc 2 e1) e1)
    );setq
    (if (entmod e1)
        (setq j (+ j 1))
    );if
    (setq n (+ n 1));setq
   );repeat       
 
 j
);defun acet-block-replace


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBEAfDmFAHdCR1tfUuT0BtrcA0r9iEPefDcn3qo
;;; 2Wj9i1Y6lkMrQrGERneamL1LXqQXbeXPAb9oDpICSu6xm6yCoulV4Epdkc8PajZw
;;; MGnF4xCUjPMQNfPq/xEg90Hh5hV7YOT4C8p6jWk52AZY+XzIkGROL6D9vKtqk/H8
;;; Xd56XQIF032AJ71Cxn0NPuUNr+JaHyRxnzhUINAlOhWIrvbGRCFJI2yWjPq8snNo
;;; blud4/mS3/W+Ubgj9UWpzIkq/9ta3wDH71yDz4Fe2jmM4wzXnQ7kWbGe7w/EYQTm
;;; +z3tTMc3mkMutOcy98bg5IYjr1whQIjGnwhp90xE1yKk+f5hxUWXfC2Em/WC95Th
;;; QvBGeJMIJIky/dyPw6zq/jkt2waylbhQi5Lo4rgbcykBjbBagC+ogvCWOVvhv9dq
;;; H0TPqrS2dPkZzQ9tdLGTC/qpZAg0AKrTI8LcWnv5IvLIo7nk7/vU1U3FZPZx05vF
;;; IO7ZOgJYAr6lKKws5GQBe6BcdbMvC7U2k3P9uXcLC3eKi+Aq+ryoEXOUhOCTtrzp
;;; gYPukdPDFRN/2PY+ozgyQQee6hvwbSRCTMMSqoM/y+RarUfRCz3PsUCIr6mn0Q8k
;;; 42cBUC8jBTayxQ+QIsAUcH0E8yN5WQscRKIlWfW43zmIJ81XtpSllc+7mj+/3C8h
;;; tAUuQQ==
;;; -----END-SIGNATURE-----