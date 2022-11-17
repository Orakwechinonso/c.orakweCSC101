;;;
;;;
;;;    BURST.LSP
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
 
(Defun C:BURST (/ item bitset bump att-text lastent burst-one burst
                  BCNT BLAYER BCOLOR ELAST BLTYPE ETYPE PSFLAG ENAME )
 
   ;-----------------------------------------------------
   ; Item from association list
   ;-----------------------------------------------------
   (Defun ITEM (N E) (CDR (Assoc N E)))
   ;-----------------------------------------------------
   ; Error Handler
   ;-----------------------------------------------------
 
  (acet-error-init
    (list
      (list "cmdecho" 0
            "highlight" 1
      )
      T     ;flag. True means use undo for error clean up.
    );list
  );acet-error-init
 
 
   ;-----------------------------------------------------
   ; BIT SET
   ;-----------------------------------------------------
 
   (Defun BITSET (A B) (= (Boole 1 A B) B))
 
   ;-----------------------------------------------------
   ; BUMP
   ;-----------------------------------------------------
 
   (Setq bcnt 0)
   (Defun bump (prmpt)
      (Princ
         (Nth bcnt '("\r-" "\r\\" "\r|" "\r/"))
      )
      (Setq bcnt (Rem (1+ bcnt) 4))
   )
 
   ;-----------------------------------------------------
   ; Convert Attribute Entity to Text Entity or MText Entity
   ;-----------------------------------------------------
 
   (Defun ATT-TEXT (AENT / ANAME TENT ILIST INUM)
      (setq ANAME (cdr (assoc -1 AENT)))
      (if (_MATTS_UTIL ANAME)
         (progn
            ; Multiple Line Text Attributes (MATTS) -
            ; make an MTEXT entity from the MATTS data
            (_MATTS_UTIL ANAME 1)
         )
         (progn
            ; else -Single line attribute conversion
            (Setq TENT '((0 . "TEXT")))
            (ForEach INUM '(8
                            6
                            38
                            39
                            62
                            67
                            210
                            10
                            40
                            1
                            50
                            41
                            51
                            7
                            71
                            72
                            73
                            11
                            74
                           )
               (If (Setq ILIST (Assoc INUM AENT))
                   (Setq TENT (Cons ILIST TENT))
               )
            )
            (Setq
               tent (Subst
                       (Cons 73 (item 74 aent))
                       (Assoc 74 tent)
                       tent
                    )
            )
            (EntMake (Reverse TENT))
         )
      )
   )
 
   ;-----------------------------------------------------
   ; Find True last entity
   ;-----------------------------------------------------
 
   (Defun LASTENT (/ E0 EN)
      (Setq E0 (EntLast))
      (While (Setq EN (EntNext E0))
         (Setq E0 EN)
      )
      E0
   )
 
   ;-----------------------------------------------------
   ; See if a block is explodable. Return T if it is, 
   ; otherwise return nil
   ;-----------------------------------------------------
 
   (Defun EXPLODABLE (BNAME / B expld)
      (vl-load-com)
      (setq BLOCKS (vla-get-blocks 
                     (vla-get-ActiveDocument (vlax-get-acad-object)))
       )
      
      (vlax-for B BLOCKS (if (and (= :vlax-false (vla-get-islayout B))
                                  (= (strcase (vla-get-name B)) (strcase BNAME)))
                      (setq expld (= :vlax-true (vla-get-explodable B)))
           )
       )
       expld
    )


   ;-----------------------------------------------------
   ; Burst one entity
   ;-----------------------------------------------------
 
   (Defun BURST-ONE (BNAME / BENT ANAME ENT ATYPE AENT AGAIN ENAME
                     ENT BBLOCK SS-COLOR SS-LAYER SS-LTYPE mirror ss-mirror
                     mlast)
      (Setq
         BENT   (EntGet BNAME)
         BLAYER (ITEM 8 BENT)
         BCOLOR (ITEM 62 BENT)
         BBLOCK (ITEM 2 BENT)
         BCOLOR (Cond
                   ((> BCOLOR 0) BCOLOR)
                   ((= BCOLOR 0) "BYBLOCK")
                   ("BYLAYER")
                )
         BLTYPE (Cond ((ITEM 6 BENT)) ("BYLAYER"))
      )
      (Setq ELAST (LASTENT))
      (If (and (EXPLODABLE BBLOCK) (= 1 (ITEM 66 BENT)))
         (Progn
            (Setq ANAME BNAME)
            (While (Setq
                      ANAME (EntNext ANAME)
                      AENT  (EntGet ANAME)
                      ATYPE (ITEM 0 AENT)
                      AGAIN (= "ATTRIB" ATYPE)
                   )
               (bump "Converting attributes")
               (ATT-TEXT AENT)
            )
         )
      )
         (Progn
            (bump "Exploding block")
            (acet-explode BNAME)
            ;(command "_.explode" bname)
         )
      (Setq
         SS-LAYER (SsAdd)
         SS-COLOR (SsAdd)
         SS-LTYPE (SsAdd)
         ENAME    ELAST
      )
      (While (Setq ENAME (EntNext ENAME))
         (bump "Gathering pieces")
         (Setq
            ENT   (EntGet ENAME)
            ETYPE (ITEM 0 ENT)
         )
         (If (= "ATTDEF" ETYPE)
            (Progn
               (If (BITSET (ITEM 70 ENT) 2)
                  (ATT-TEXT ENT)
               )
               (EntDel ENAME)
            )
            (Progn
               (If (= "0" (ITEM 8 ENT))
                  (SsAdd ENAME SS-LAYER)
               )
               (If (= 0 (ITEM 62 ENT))
                  (SsAdd ENAME SS-COLOR)
               )
               (If (= "BYBLOCK" (ITEM 6 ENT))
                  (SsAdd ENAME SS-LTYPE)
               )
            )
         )
      )
      (If (> (SsLength SS-LAYER) 0)
         (Progn
            (bump "Fixing layers")
            (Command
               "_.chprop" SS-LAYER "" "_LA" BLAYER ""
            )
         )
      )
      (If (> (SsLength SS-COLOR) 0)
         (Progn
            (bump "Fixing colors")
            (Command
               "_.chprop" SS-COLOR "" "_C" BCOLOR ""
            )
         )
      )
      (If (> (SsLength SS-LTYPE) 0)
         (Progn
            (bump "Fixing linetypes")
            (Command
               "_.chprop" SS-LTYPE "" "_LT" BLTYPE ""
            )
         )
      )
   )
 
   ;-----------------------------------------------------
   ; BURST MAIN ROUTINE
   ;-----------------------------------------------------
 
   (Defun BURST (/ SS1)
      (setq PSFLAG (if (= 1 (caar (vports)))
                       1 0
                   )
      )
      (Setq SS1 (SsGet (list (cons 0 "INSERT")(cons 67 PSFLAG))))
      (If SS1
         (Progn
            (Setvar "highlight" 0)
            (terpri)
            (Repeat
               (SsLength SS1)
               (Setq ENAME (SsName SS1 0))
               (SsDel ENAME SS1)
               (BURST-ONE ENAME)
            )
            (princ "\n")
         )
      )
   )
 
   ;-----------------------------------------------------
   ; BURST COMMAND
   ;-----------------------------------------------------
 
   (BURST)
 
  (acet-error-restore)
 
);end defun


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCWrY00vyQLqK8YRUmNCiGXhKLycBATghBhVZ1l
;;; GzlDLdmCrOcpp1DOFGAz7soQUsqnGUAfJuO4bHU9lBVSJvg7rtafTGGjv2G1e8+8
;;; IVbeprzDOCa4ksz0plnCJrl2Fq0MXjuBSODnLfrffcmW8UvlBwX05ScNhijKraN7
;;; qctQFnaeIMnV9MZ7qolyVwUaazTqcYSWUWO9EON6OtZI+A/KLWRgQ+xlyZJlPz0s
;;; F81UBI05voWO4LMSmJ65p/GY8KNtKvzcT/lxvSAcL+6ghqfBcqet/jyQos+9EH5k
;;; kHyL6Z7duuq05fmBkFNU9W3raBmnNSn9bPcv/JEjX5uD/zOVxv3Ulybr6zI6cMtv
;;; dRLKVf4WkIophYDH7JpGaxHKe0IlMLSa9cIBaEcaJlY1xWScyw8fGuc8mbYOECK2
;;; Oxrlo1+fU2+yl0mVcflvELS3v4QHZBbmG/pLFWDe0ndVa5tJEGekOSYW9UPpjeXV
;;; hgzhY4Dztacp/V36WQ3EnYR/kSSnL104ADhnBKmRPZ0hWIwgbxIqqcxW+T9q2v/L
;;; lPYRsxOWICXtYq2V3FacJN6H+QzDxKF3m9Hfqb4loKtdLPIIDmWcU1mxneOYsXdd
;;; 7nHVMTNRVbPfP4bkkiBxVz/4TxrCh7dBl+Y+UOk1Bql5OiWJ267X10JtnhqM612Q
;;; DmCNcw==
;;; -----END-SIGNATURE-----