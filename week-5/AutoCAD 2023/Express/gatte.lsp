;;
;;;
;;;    GATTE.LSP
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
 
(Defun C:gatte ( /
     N      ;selection set counter
     CC     ;changed counter
     BN     ;block name
     TG     ;tag name
     ESel   ;entity pick/name/list
     EL     ;entity list
     EN     ;entity name
     PASS   ;loop pass flag
     TAGL   ;list of valid tags for a block
     TAGS   ;String of valid tags for a block
     TAGT   ;Temp tag list
     ;TAG    ;tag name in loop
     TMP    ;temporary variable
     SS1    ;selection set of insert objects
     XX X   ;flag and counter
     OLDCC  ;previous count of changes for update test
     A      ;entity information in change loop
     FL LA  ;frozen layer check variables
     na b
     )
 
   (acet-error-init
      (List
         (List "cmdecho" 0)
         T     ;flag. True means use undo for error clean up.
      ) ;list
   );acet-error-init
   (sssetfirst nil nil)
   ;;
   (Setq n 0
         cc 0
   )
   (while (null Pass)
      (initget "Block _Block")
;;      (setq ESel (entsel "\nBlock name/<select block or attribute>: "))
      (setq ESel (entsel "\nSelect block or attribute [Block name]: "))
      (cond
        ((null ESel) (setq Pass 'T BN nil))
        ((= (type ESel) 'LIST) ;;pick selection
           (setq EL (entget (car ESel)))
           (if (= (cdr (assoc 0 EL)) "INSERT")
               (setq BN (cdr (assoc 2 EL))
                     Pass 'T
                     ESel (nentselp (cadr ESel))
                     EL (entget (car Esel))
                     TG (if (= (cdr (assoc 0 EL)) "ATTRIB")
                            (cdr (assoc 2 EL))
                            nil
                        )
               )
               (prompt "\nSelected item not an INSERT.")
           );end if
        );end second conditional for picking attrib
        ((and (= (type ESel) 'STR) (= ESel "Block"))
           (setq BN (getstring "\nEnter block name: "))
           (if (tblsearch "BLOCK" BN)
              (setq Pass 'T)
              (prompt "\nInvalid block name.")
           )
        );end third conditional
      );the conditional statement ends
   ) ;;end of Block Name entry.
 
   (if BN (progn
     (setq Pass nil
           EN (cdr (assoc -2 (tblsearch "BLOCK" BN)))
     )
     (while EN
        (setq EL (entget EN))
        (if (= (cdr (assoc 0 EL)) "ATTDEF")
           (setq TAGL (cons (cdr (assoc 2 EL)) TAGL)))
        (setq EN (entnext EN))
     )
   )) ;;end if BN progn
   (if TG (setq Pass 'T))
 
   (if TAGL
     (progn
       (setq TAGS (car TAGL)
             TAGT (cdr TAGL)
       )
       (foreach TAG TAGT
         (setq TAGS (strcat TAGS " " TAG))
       )
     )
   )
 
   (while (and TAGS (null Pass))
      (initget TAGS)
      (prompt (strcat "\nKnown tag names for block: " TAGS))
      (setq ESel (nentsel "\nSelect attribute or type attribute name: "))
 
      (cond
        ((= (type ESel) 'STR)
           (setq ESel (xstrcase ESel))
           (if (member ESel TAGL)
             (setq Pass 'T
                   TG    ESel
             )
             (prompt "\nInvalid attribute name.")
           )
        )
        ((= (type ESel) 'LIST) ;;pick selection
           (setq TG (cdr (assoc 2 (entget (car ESel)))))
           (if TG
             (setq Pass 'T)
           )
        )
      );the conditional statement ends
   ) ;;end of Attribute Name entry.
 
 
   (if (and BN (null TAGL))
      (setq BN (prompt "\nThe block selected has no attributes!")))
   (If (And BN TG)
      (Progn
         (prompt (acet-str-format "\nBlock: %1   Attribute tag: %2" BN TG))
         (Setq
            NA (GetString T "\nEnter new text: ")
            SS1 (SsGet "_X"
                 (List
                    (Cons 0 "INSERT")
                    (Cons 2 bn)
                    (Cons 66 1)
                 )
              )
            N (If SS1 (SsLength SS1) 0)
         )
         (initget 0 "Yes No _Yes No")
         (setq TMP
           (getkword
             (acet-str-format "\nNumber of inserts in drawing = %1  Process all of them? [Yes/No] <Yes>: " (itoa N))))
         (if (and TMP (= TMP "No"))
            (setq SS1 (ssget (list (cons 0 "INSERT")
                                   (cons 2 BN)
                                   (cons 66 1)))
                  N (if SS1 (sslength SS1) 0)
            )
         )
         (if (> N 0) (Princ "\nPlease wait..."))
         (setq x 0)
         (repeat N
            (setq A (ssname SS1 x)
                  B (entget A)
                  la (cdr (assoc 8 B))      ;layer name from object
                  fl (tblsearch "LAYER" la) ;table entry for layer
                  fl (cdr (assoc 70 fl))    ;layer status flag
            )
            (if (/= fl 65) ;if layer not frozen
               (progn
                  (setq XX 1
                        oldcc cc)
                  (while XX
                     (setq
                        B (EntGet (EntNext (CDR (Assoc -1 B))))
                     )
                     (If (= (CDR (Assoc 0 B)) "SEQEND")
                        (Setq xx Nil)
                        (Progn
                           (If (= (CDR (Assoc 2 b)) tg)
                              (Progn
                                 (Setq B (subst (Cons 1 NA) (assoc 1 B) B)
                                       CC (1+ CC)
                                 )
                                 (EntMod B)
                              ) ;progn
                           ) ;if
                        ) ;progn
                     ) ;if
                  ) ;while
                  (If (/= cc oldcc) (EntUpd a))
               ) ;progn
            ) ;if
            (Setq X (1+ X))
         ) ;repeat
         (If (/= 1 cc)
           (princ (acet-str-format "\n%1 attributes changed." (itoa cc)))
           (princ (acet-str-format "\n%1 attribute changed." (itoa cc)))
         )
      ) ;progn
   )
   (acet-error-restore)
   (Princ)
) ;defun


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDPAIPmtHRiawpkKog7XLeOlNxMcyQZfjCr5g7q
;;; w/dGLIZlVvlEeSzubDcYyAXj0/15adF1h/l9m1p2LpQLHrmGek1R1vSweefEWlLT
;;; FUvC4HCrpx8HULfivKO4Bwv21kYauaGrJF+CIGA4n5lHcByMspX82wqxf8mqCB5Z
;;; u7O8BxM+SJBqUzx6j7rUv/TNS04MsVwm45wZu2m/1zvrNIK9Q414gPag8pzH9/jM
;;; 0nQpJUIqRFVcMs+pmVBoX6AqYBfteqi8KEIX+QzQJAVvpnqZfTnRTmSW9c894/Jm
;;; qOpiwOdNauISF3xMMNvwm9Ds71zEGstSZmtmpinbFnPpe3JdMZnOs7qcTFbjF+tw
;;; gYyZ3/qCOdXn06vM8HOO89U4+zn2ngc9+xAdlzj0REZMQ4DMrJEYGvsI9gJke035
;;; 1rgAET2nf1fbf4zOYscUsohriLTho2OrxWaCFO98PTA4nJ7lPLRcRWebHeUJ26h+
;;; DTfqki9BI9KjI0jTrRwnvYrWgB5pWvq3HxKm7VFzX9EDiEVzGSdPOcojdyUN4Hkf
;;; kemODdRfS3rxo03TOlmmjpfYFY3mf294BiaX/Gml1ntAEkrSg6yVOrLvcJn2Soi6
;;; rcL5ES5aoQigprBv9LodqTqeCTy9HDN1Lh04kgI9OWk1ANZLSMBxcg8TGi0jgYgH
;;; czL4og==
;;; -----END-SIGNATURE-----