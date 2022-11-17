;;;
;;;    MPEDIT.LSP
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
 
;;;    Multiple objects can be selected for the equivalent of the pedit operation
;;;    on polylines.  Objects that are not polylines will be filtered out of the
;;;    selection set.
;;;
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;--------------------------------------------------------------------------
;;; Command-line entry point for mpedit
 
(defun c:mpedit ( / ss plines ss2 )
 (acet-error-init
  (list (list   "cmdecho" 0
              "highlight" nil
              "plinetype" 2
               "limcheck" 0
		"osmode"  0
		
        );list
        0  ;use undo marks but do not roll back on a cancel
  );list
 );acet-error-init
 
 (setq plines (ssget ":l" '(
        (-4 . "<OR")
         (0 . "ARC")
         (0 . "LINE")
         (-4 . "<AND")
          (0 . "*POLYLINE")
          (-4 . "<NOT") (-4 . "&") (70 . 88) (-4 . "NOT>") ;8 16 64 not a 3dpoly or 3d/pface mesh
         (-4 . "AND>")
         (0 . "LWPOLYLINE")
        (-4 . "OR>")
                  )
              )
 );setq
 (if plines
     (setq plines (acet-ss-filter-current-ucs plines T))
 );if
 (if plines
     (setq plines (car (acet-ss-filter (list plines '(("CURRENTUCS")) T))))
 );if
 (princ "\n")
 (if plines
     (setq ss (convert plines));setq
 );if
 (if (and ss
          (> (sslength ss) 0)
          plines
     );and
     (mpedit ss) ;after conversion, plines sset is duplicated in ss
     (progn
      (if (not plines)
          (princ "\nNothing selected.")
          (princ "\nNo valid objects selected.")
      );if
     );progn else
 );if
 
 (acet-error-restore)
);defun c:mpedit
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mpedit functionality and switch cases based on kword input operation
 
(defun mpedit ( plines / opt newWidth ss flt fuz st na ss2 )
 
  (while (/= opt "eXit")
   (initget 0 "Open Close Join Ltype Decurve Fit Spline Width eXit _Open Close Join Ltype Decurve Fit Spline Width eXit")
   (setq opt (getkword "\nEnter an option [Open/Close/Join/Width/Fit/Spline/Decurve/Ltype gen/eXit] <eXit>: "))
   ;Changed code below to a cond structure to improve readability R.K.
 
   (if (and opt 
            (not (equal opt "eXit"))
       );and
       (acet-undo-begin)
       (acet-undo-end)
   );if
 
   (cond
    ((not opt) (setq opt "eXit"))
    ((= opt "Open") (chgplopen plines)) ;open
    ((= opt "Close") (chgplclose plines));close
    ((= opt "Ltype") (chgltgen plines)) ;ltgen
    ((= opt "Decurve") (chgdecurve plines));decurve
    ((= opt "Fit") (chgfit plines))
    ((= opt "Spline") (chgspline plines))
    ((= opt "Width")
     (initget 69)
     (setq newWidth (getdist "\nEnter new width for all segments: "))
     (chgplwidths plines newWidth)
    );width option
    ((= opt "Join")
     (setq flt '((-4 . "<OR")
                  (0 . "LINE")
                  (0 . "ARC")
                  (-4 . "<AND")
                   (0 . "*POLYLINE")
                   (-4 . "<NOT") (-4 . "&") (70 . 89)  (-4 . "NOT>") ;1 8 16 64
                  (-4 . "AND>")
                 (-4 . "OR>")
                )
           flt (list flt
                     "\n1 object is invalid for join."
                     "\n%1 objects are invalid for join."
               );list
           flt (list flt
                     (list "LAYERUNLOCKED")
                     (list "CURRENTUCS")
               );list
            ss (car (acet-ss-filter (list plines flt T)))
     );setq
 
     (acet-autoload (list "pljoin.lsp" "(acet-pljoin-get-fuzz-and-mode)"))
     (acet-autoload (list "pljoin.lsp" "(acet-pljoin ss st fuz)"))
 
     (if ss
         (progn
          (setvar "highlight" 0)
          (setq fuz (acet-pljoin-get-fuzz-and-mode2)
                 st (cadr fuz)
                fuz (car fuz)
                 na (entlast)
          );setq
          (acet-pljoin2 ss st fuz)
          (setq ss2 (acet-ss-new na))
          (setq plines (acet-ss-union (list plines ss2)))
         );progn then
         (princ "\nNo valid objects to join.")
     );if
    );cond Join option
   );cond close
  );while
);defun mpedit
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pline width change
 
(defun chgplwidths (plines newWidth / count ent subEntity currVertex)
 (setq count 0)
 (while (< count (sslength plines))
  (setq ent (entget (ssname plines count)))
   (if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
   (command "_.pedit" (ssname plines count) "_width" newWidth "_exit")
    (progn ;polylines
      (setq subEntity (entnext (ssname plines count)))
      (setq currVertex (entget subEntity))
      (while (not (equal "SEQEND" (cdr (assoc 0 currVertex))))
      (setq currVertex (subst (cons 40 NewWidth) (assoc 40 currVertex) currVertex))
      (setq currVertex (subst (cons 41 NewWidth) (assoc 41 currVertex) currVertex))
      (entmod currVertex)
      (setq subEntity (entnext (cdr (assoc -1 currVertex))))
            (setq currVertex (entget subEntity))
      );while
      (entupd (ssname plines count))
    );progn
   );if
  (setq count (1+ count))
 );while
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pline modification to close
(defun chgplclose (plines / count )
 (setq count 0)
 (while (< count (sslength plines))
  (command "_.pedit" (ssname plines count) "_close" "_exit")
  (setq count (1+ count))
 );while
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pline modification to open
(defun chgplopen (plines / count)
 (setq count 0)
 (while (< count (sslength plines))
  (command "_.pedit" (ssname plines count) "_open" "_exit")
  (setq count (1+ count))
 );while
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pline vertex linetype generation switch
 
(defun chgltgen (plines / count new70 opt ent)
 (setq count 0)
  (initget 0 "ON OFF eXit _ON OFF eXit")
  (setq opt (getkword "Full PLINE linetype? [ON/OFF/eXit] <eXit>: "))
  (if opt opt "eXit")
   (if (= opt "ON")
  (while (< count (sslength plines))
    (setq ent (entget (ssname plines count)))
    (setq new70 (cons 70 (logior 128 (cdr (assoc 70 ent)))))
    (setq ent (subst new70 (assoc 70 ent) ent))
    (entmod ent)
    (setq count (1+ count))
   );while
   );if on
   (if (= opt "OFF")
  (while (< count (sslength plines))
    (setq ent (entget (ssname plines count)))
    (setq new70 (cons 70 (boole 6 128 (cdr (assoc 70 ent)))))
    (setq ent (subst new70 (assoc 70 ent) ent))
    (entmod ent)
    (setq count (1+ count))
   );while
   );if off
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pline decurve
(defun chgdecurve (plines / count)
 (setq count 0)
 (while (< count (sslength plines))
  (command "_.pedit" (ssname plines count) "_decurve" "_exit")
  (setq count (1+ count))
 );while
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pline curve fit
 
(defun chgfit (plines / count)
 (setq count 0)
 (while (< count (sslength plines))
  (command "_.pedit" (ssname plines count) "_fit" "_exit")
  (setq count (1+ count))
 );while
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pline spline fit
(defun chgspline (plines / count)
 (setq count 0)
 (while (< count (sslength plines))
  (command "_.pedit" (ssname plines count) "_spline" "_exit")
  (setq count (1+ count))
 );while
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert arcs and lines to polylines
;;; ss is retained as a duplicate of the plines selection set because
;;; after conversion, new handles are assigned to what were arcs and lines
(defun convert ( plines / ss count opt )
 (if (> (sslength plines) 0)
     (progn
      (initget 0 "Yes No _Yes No")
      (setq opt (getkword "Convert Lines and Arcs to polylines? [Yes/No] <Yes>: "))
     );progn then
 );if
 (if (not opt)
     (setq opt "Yes")
 )
 (if (and (= opt "Yes")
          (> (sslength plines) 0)
     );and
     (progn ;if yes -- convert lines and arcs to polylines
      (acet-undo-begin)
      (setq ss (ssadd))
      (setq count 0)
      (while (< count (sslength plines))
       (if (or (equal (assoc 0 (entget (ssname plines count))) '(0 . "ARC"))
               (equal (assoc 0 (entget (ssname plines count))) '(0 . "LINE"))
           );or
           (progn
            (command "_.pedit" (ssname plines count) "_yes" "_exit")
            (ssadd (entlast) ss)
           );progn true
           (ssadd (ssname plines count) ss)
       );if
       (setq count (1+ count))
      );while
      (acet-undo-end)
     );progn yes
     (progn ;if no -- do not convert
      (setq ss plines)
      (setq count 0)
      (while (< count (sslength ss))
       (if (or (equal (assoc 0 (entget (ssname ss count))) '(0 . "ARC"))
               (equal (assoc 0 (entget (ssname ss count))) '(0 . "LINE"))
           );or
           (progn
            (ssdel (ssname ss count) ss)
            (setq count (1- count))
           );progn true
       );if
       (setq count (1+ count))
      );while
     );progn no
 );if
 (if (and ss
          (equal (type ss) 'PICKSET)
          (equal 0 (sslength ss))
     );and
     (setq ss nil)
 );if
ss
)


(acet-autoload2	'("PLJOINSUP.LSP"	(acet-pljoin2 ss st fuzz)))
(acet-autoload2	'("PLJOINSUP.LSP"	(acet-pljoin-get-fuzz-and-mode2)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCqf4STmjqoCrud0izR9iEIYh5fbrgOHe4R2QgC
;;; JIKiDdeUl3rNz329SyNAcSRuRqPXVUMhYjUyuFe703aMc9ZS/nusNHjCnOTKR4KE
;;; BcQwqhJR+4E6iG+e/guySUHeLbxDBobw8J5Dy3axpPwQgy4FByBKcPRFEdC9Xidw
;;; urBc6vQ62gC5QxlahDju15bOIQ/cCjIRBufD6JS4lynPKCoMxmyObpLLDuBu75we
;;; kGSO0W1z6KHydT5R48FRoJS4QAiNkZPpYlY8OvYEOH4S1RBTj8w2S5yHWapz//tf
;;; Idxv69mRH1vnf4Q1fWXGcyEb5bwmaSk2FUZm+Zp6bwufjkxqN6BJAsbXUl+vkc4A
;;; Hjus4KNmJUrSMCZgpHjVMIqP7lW3LrEiyr+I5opiwTCMxnF6QlPhRHFi+O35o6PB
;;; fuVtZlvFkvrpa7qimtAqNdzRgrZ92Pa2r2h431Gm1XPAE378HHDLydoeX2aiX80U
;;; GT0suyvv9oecue1FlULwccbERZgyTBjkPqiS4eYHIK7i54pC+kbkvMJJ44gtsQ9K
;;; OvUHT5yEjMpRsEHv+pJ0733Ra9H8dsovWjizPlcLTgFTAUSBs/V0oT7KqlwbnFjD
;;; ezyHV0xp8gEn/5kE8xRCbFCDiXPAjBaAaXws1xtqbv/6AC9vb130Wtpfa5oT4Brb
;;; U58KXg==
;;; -----END-SIGNATURE-----