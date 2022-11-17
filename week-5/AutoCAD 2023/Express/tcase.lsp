;;
;;  Tcase.lsp - Changes case of selected text, attdefs, attributes, dimension text,
;;               mtext, arcalignedtext and rtext.
;;                    
;;
;;  Copyright © 1999-2006 by Autodesk, Inc.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TCASE
; changes the case of selected annotation objects.
;
(defun c:tcase ( / lst )
 (acet-error-init (list '("cmdecho" 0) T))
 (if (setq lst (acet-tcase-ui nil))
     (acet-tcase (car lst) (cadr lst))
 );if
 (acet-error-restore)
);defun c:tcase
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:-tcase ( / lst )
 (acet-error-init (list '("cmdecho" 0) T))
 (if (setq lst (acet-tcase-ui T))
     (acet-tcase (car lst) (cadr lst))
 );if
 (acet-error-restore)
);defun c:-tcase
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;If the cmdline arg is true then command will be command line line driven.
;
(defun acet-tcase-ui ( cmdline / flt ss lst )
 (setq flt '((-4 . "<OR") 
              (0 . "TEXT")
              (0 . "ATTDEF")
              (0 . "MTEXT")
              (0 . "DIMENSION")
              (0 . "RTEXT")
              (0 . "ARCALIGNEDTEXT")
              (-4 . "<AND") (0 . "INSERT") (66 . 1) (-4 . "AND>")
             (-4 . "OR>")
            )
 );setq
 (if (setq ss (ssget "_:L" flt))
     (progn
      (if (or cmdline
              (= 4 (logand 4 (getvar "cmdactive")))
              (= 0 (getvar "cmddia"))
          );or
          (setq lst (acet-tcase-ui-cmd))
          (setq lst (acet-tcase-ui-dlg))
      );if
      (if lst
          (setq lst (cons ss lst))
      );if
     );progn then
 );if
 lst
);defun tcase-ui
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-tcase-ui-cmd ( / ans def lst )
 
 (setq def (acet-getvar '("ACET-TCASE-MODE")))
 (if (not def)
     (setq def "Upper")
 );if
 (initget "Sentence Lower Upper Title toGgle")
 (setq ans (getkword 
            (acet-str-format "\nSelect case [Sentence/Lower/Upper/Title/toGgle] <%1>: "
                             def
            )
           );getkword
 );setq
 (if (not ans)
     (setq ans def)
 );if
 (acet-setvar (list "ACET-TCASE-MODE" ans 3)) ;; store it in the drawing and in the profile
 
 (list ans)
);defun acet-tcase-ui-cmd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;acet-tcase-dlg
;returns one of the following strings if OK is pressed:
;"Sentence Lower Upper Title toGgle"
;Returns nil on cancel
;
(defun acet-tcase-ui-dlg ( / iv flag set_bit mode lst )
 
 
 (if (> (setq iv (load_dialog (getfileET "tcase.dcl")));setq
        0
     );test
     (progn
      (if (new_dialog "tcase" iv)
          (progn
           (setq mode (acet-getvar '("ACET-TCASE-MODE")))
           (if (not mode)
               (setq mode "UPPER")
               (setq mode (xstrcase mode))
           );if
 
           (cond
            ((= mode "UPPER")
             (set_tile "upper" "1")
            );cond #1
            ((= mode "LOWER")
             (set_tile "lower" "1")
            );cond #2
            ((= mode "SENTENCE")
             (set_tile "sentence" "1")
            );cond #3
            ((= mode "TITLE")
             (set_tile "title" "1")
            );cond #4
            ((= mode "TOGGLE")
             (set_tile "toggle" "1")
            );cond #5
           );cond close
         
           (action_tile "upper"    "(setq mode \"upper\")")
           (action_tile "lower"    "(setq mode \"lower\")")
           (action_tile "sentence" "(setq mode \"sentence\")")
           (action_tile "title"    "(setq mode \"title\")")
           (action_tile "toggle"   "(setq mode \"toggle\")")
   
           (action_tile "accept" "(done_dialog 1)")
           (action_tile "cancel" "(done_dialog 0)")
           (action_tile "help" "(acet-help \"TCASE\")")
 
           (setq flag (start_dialog));setq
           (if (equal flag 1)
               (progn
                (acet-setvar (list "ACET-TCASE-MODE" (xstrcase mode) 3))
                (setq lst (list mode));setq
               );progn
               (setq mode nil)
           );if
          );progn then initialize the tiles and activate the dialog box
          (alert "Unable to display dialog box")
      );if new dialog
      (unload_dialog iv);unload it when done
     );progn then
     (alert "Unable to load dialog box");else
 );if load
 lst
);defun acet-tcase-ui-dlg
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-tcase ( ss mode / na e1 n x tp e2 flag frmt a )
 (setq n 0)
 (repeat (sslength ss)
  (setq na (ssname ss n)
        e1 (entget na)
  );setq
  (if (= 1 (cdr (assoc 66 e1)))
      (setq flag T)
      (setq flag nil)
  );if
  (while e1
   (setq tp (cdr (assoc 0 e1)))
   (cond
    ((or (= tp "TEXT")
         (= tp "ATTDEF")
         (= tp "ATTRIB")
         (= tp "ARCALIGNEDTEXT")
         (= tp "DIMENSION")
         (= tp "RTEXT")
     );or
        (setq e2 nil)
        (if (and (or (= tp "ATTDEF") (= tp "ATTRIB"))
                 (_matts_util na)
            )
           (progn
                ; Special case handling for multiline text attributes
                (setq x (_matts_util na 2))
                (if (= (type x) 'LIST)
                    (progn
                         (setq x (car x))
                         (setq a (cdr x))
                         (setq frmt (acet-mtext-format-extract a)
                               a (car frmt)
                               frmt (cadr frmt)
                         );setq
                         (setq a (acet-tcase-change-string a mode)
                              a (acet-mtext-format-apply a frmt)
                         );setq
                         (setq x (cons (car x) a))
                         (_matts_util na 3 x)
                    )
                )
           )
           (progn
              ; Process single line attributes using entmod
              (foreach x e1
                (if (or (= (car x) 1)
                        (= (car x) 3)
                    );or
                    (setq x (cons (car x) 
                                  (acet-tcase-change-string (cdr x) mode)
                            );cons
                     );setq then modify the case
                );if
                (setq e2 (cons x e2))
              );foreach
              (entmod (reverse e2))
           )
        );if ATTDEF/ATTRIB & multiline
    );cond #1
    ((= tp "MTEXT")
	;; first get the entire string 
        ;; then strip formatting and apply case changes
        ;; re-apply formating
        ;; place string back into elist and entmod
  
        (setq a "")
        (foreach x e1
         (if (or (= (car x) 1)
                 (= (car x) 3)
             );or
             (setq a (strcat a (cdr x)));setq then
         );if
        );foreach
        (setq frmt (acet-mtext-format-extract a)
                 a (car frmt)
              frmt (cadr frmt)
        );setq
        (setq a (acet-tcase-change-string a mode)
              a (acet-mtext-format-apply a frmt)
        );setq
        
        (setq e2 nil)
        (foreach x e1
         (if (or (= (car x) 1)
                 (= (car x) 3)
             );or
             (setq x (cons (car x) 
                           (substr a 1 (strlen (cdr x)))
                     );cons
                   a (substr a (+ (strlen (cdr x)) 1))
             );setq then 
         );if
         (setq e2 (cons x e2))
        );foreach
        (entmod (reverse e2))
    );cond #2
   );cond close
   (if flag
       (progn
        (if (= tp "SEQEND")
            (progn
             (entmod e1)
             (entmod (entget (ssname ss n)))
             (entupd (ssname ss n))
             (setq e1 nil)
            );progn then
            (progn
             (if (setq na (entnext na))
                 (setq e1 (entget na))
             );if 
            );progn
        );if
       );progn then
       (setq e1 nil)
   );if
  );while
  (setq n (+ n 1));setq
 );repeat
 
);defun tcase


(acet-autoload2	'("tcaseSup.lsp"	(acet-mtext-format-apply str flst)))
(acet-autoload2	'("tcaseSup.lsp"	(acet-mtext-format-extract str)))
(acet-autoload2	'("tcaseSup.lsp"	(acet-tcase-change-string a mode)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAGGdlzMcqlFO7n2CVzBYp2TdhsfQePCLSVx1rJ
;;; 9AQ1yCHVjeAJPw3DEZ0TwF/6r/aTuO+rSri2zXk9sLV4wn7oirJ+T+sLmBVxsIFT
;;; BorACw/+O2W7idOMBoURVVOzOBMdWB18vAAH24GbEkTiFs5a0mEGQ0lBaUH9dK5r
;;; rq3J4JXBTxiuMrxsJ0LMQkcUVSTYufn+YScKKlpeP8Erd1+NUv2cYc4Qv/7yYKPU
;;; 8N+dHTm440w1MclZjj5kT9N5PeuOYT2UwnDB6k7AmpRTyPR+JIbW86y1+TWd4bqX
;;; XyrsSGybrn4RBJ0ejDuAcNDQj4KWNxGWklMdEhuBrTDjxFg6gtaVj0mzcTrwjck3
;;; e59Ybn9bwDSLbip307JVTQ7wYXKCu6xPXL23zJZqemjK8p4GRexhmJt9q+tabwpZ
;;; TwSvb4fXNKdiTxYbSsvUSrmBelWe5YmyiiLsiACvBLrkREafM6EPVd4H2+evsf7G
;;; LfaBSWt30g6spksTyHUNEul0sCYdCt8nfnCRRwgnVSGRJ5SVIYc2azuh1do+Pqop
;;; z3/7VYzF0uR8hquA5p77SG754PlRemfKrnDR6scWVvTV4p5iRJ4CgudtvdTgwKUL
;;; e76rr5MRypVOpVEBfjyAjkVdHeKKefsYVaduZDbZ7p6Md1GF7f8vXxHDAfiwQVJJ
;;; c2p7bA==
;;; -----END-SIGNATURE-----