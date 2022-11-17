;;
;;;
;;;    By Dominic Panholzer
;;;
;;;    TXTEXP.LSP
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
;;;
;;;  External Functions:
;;;
;;;     ACET-ERROR-INIT           --> ACETUTIL.FAS   Intializes bonus error routine
;;;     ACET-ERROR-RESTORE        --> ACETUTIL.FAS   Restores old error routine
;;;     ACET-GEOM-ZOOM-FOR-SELECT --> ACETUTIL.FAS   Zoom boundry to include points given
;;;     ACET-LAYER-LOCKED         --> ACETUTIL.FAS   Checks to see if layer is locked
;;;     ACET-GEOM-PIXEL-UNIT      --> ACETUTIL.FAS   Size of pixel in drawing units
;;;     ACET-GEOM-TEXTBOX         --> ACETUTIL.FAS   Returns the textbox for any text
;;;     ACET-GEOM-MIDPOINT        --> ACETUTIL.FAS   Returns midpoint between two points
;;;     ACET-GEOM-VIEW-POINTS     --> ACETUTIL.FAS   Returns corner points of screen or viewport
;;;     ACET-STR-FORMAT           --> ACETUTIL.ARX   String builder
;;;     ACET-WMFIN                --> ACETUTIL.FAS   Brings in WMF file
;;;
 
(defun c:txtexp (/ grplst getgname blknm FLTR GLST GDICT SS VIEW UPLFT TMPFIL TBX
                   TMPFIL CNT PT1 PT2 ENT TXT TXTYP PTLST ZM LOCKED GNAM vpna vplocked)
  (acet-error-init
        (list
         (list   "cmdecho" 0
                 "highlight" 1
                 "osmode" 0
                 "Mirrtext" 1
                 "limcheck" 0
         )
         T
        )
  )
 
; --------------------- GROUP LIST FUNCTION ----------------------
;   This function will return a list of all the group names in the
;   drawing and their entity names in the form:
;   ((<ename1> . <name1>) ... (<enamex> . <namex>))
; ----------------------------------------------------------------
 
  (defun acet-txtexp-grplst (/ GRP ITM NAM ENT GLST)
 
    (setq GRP  (dictsearch (namedobjdict) "ACAD_GROUP"))
    (while (setq ITM (car GRP))       ; While edata item is available
      (if (= (car ITM) 3)             ; if the item is a group name
        (setq NAM (cdr ITM)           ; get the name
              GRP (cdr GRP)           ; shorten the edata
              ITM (car GRP)           ; get the next item
              ENT (cdr ITM)           ; which is the ename
              GRP (cdr GRP)           ; shorten the edata
              GLST                    ; store the ename and name
                  (if GLST
                    (append GLST (list (cons ENT NAM)))
                    (list (cons ENT NAM))
                  )
        )
        (setq GRP (cdr GRP))          ; else shorten the edata
      )
    )
    GLST                              ; return the list
  )
 
; ------------------- GET GROUP NAME FUNCTION --------------------
;   This function returns a list of all the group names in GLST
;   where ENT is a member. The list has the same form as GLST
; ----------------------------------------------------------------
 
  (defun acet-txtexp-getgname (ENT GLST / GRP GDATA NAM NLST)
    (if (and GLST (listp GLST))
      (progn
        (foreach GRP GLST
          (setq GDATA (entget (car GRP)))
          (foreach ITM GDATA                   ; step through the edata
            (if (and
                  (= (car ITM) 340)            ; if the item is a entity name
                  (eq (setq NAM (cdr ITM)) ENT) ; and the ename being looked for
                )
              (setq NLST                       ; store the ename and name
                      (if NLST
                        (append NLST (list (cons (car GRP) (cdr GRP))))
                        (list (cons (car GRP) (cdr GRP)))
                      )
              )
            )
          )
        )
      )
    )
    NLST
  )
 
; ----------------------------------------------------------------
;                          MAIN PROGRAM
; ----------------------------------------------------------------
 
  (if (and                                                ; Are we in plan view?
        (equal (car (getvar "viewdir")) 0 0.00001)
        (equal (cadr (getvar "viewdir")) 0 0.00001)
        (> (caddr (getvar "viewdir")) 0)
      )
 
    (progn
 
      (prompt "\nSelect text to be EXPLODED: ")
 
      (Setq FLTR    '((-4 . "<AND")
                        (-4 . "<OR")                      ; filter for mtext and text
                          (0 . "MTEXT")
                          (0 . "TEXT")
                        (-4 . "OR>")
                        (-4 . "<NOT")
                          (102 . "{ACAD_REACTORS")        ; and not leader text
                        (-4 . "NOT>")
                      (-4 . "AND>")
                     )
            GLST     (acet-txtexp-grplst)                             ; Get all the groups in drawing
            GDICT    (if GLST
                       (dictsearch (namedobjdict) "ACAD_GROUP")
                     )
            SS       (ssget  FLTR)
            CNT      0
      )
      ;; filter out the locked layers
      (if SS
        (setq SS (car (bns_ss_mod SS 1 T)))
      ) ;if
 
      ;; if we have anything left
      (if SS
        (progn
          (setq CNT 0)                                 ; Reset counter
          (while (setq ENT (ssname SS CNT))            ; step through each object in set
 
            (and
              GLST                                     ; if groups are present in the drawing
              (setq GNAM (acet-txtexp-getgname ENT GLST))          ; and the text item is in one or more
              (foreach GRP GNAM                        ; step through those groups
                (command "_.-group" "_r"               ; and remove the text item
                  (cdr GRP) ENT ""
                )
              )
            )
 
            (setq TBX (acet-geom-textbox (entget ENT) 0))   ; get textbox points
 
            (setq TBX (mapcar '(lambda (x)
                                 (trans x 1 0)         ; convert the points to WCS
                               )
                        TBX
                      )
            )
 
            (setq PTLST (append PTLST TBX))            ; Build list of bounding box
                                                       ; points for text items selected
 
            (setq CNT (1+ CNT))                        ; get the next text item
          ); while
 
          (setq PTLST (mapcar '(lambda (x)
                                 (trans x 0 1)         ; convert all the points
                               )                       ; to the current ucs
                      PTLST
                    )
          )
 
          (if (setq ZM (acet-geom-zoom-for-select PTLST))          ; If current view does not contain
            (progn                                     ; all bounding box points
              (setq ZM
                (list
                  (list (- (caar ZM) (acet-geom-pixel-unit))     ; increase zoom area by
                        (- (cadar ZM) (acet-geom-pixel-unit))    ; one pixel width to
                        (caddar ZM)                    ; sure nothing will be lost
                  )
                  (list (+ (caadr ZM) (acet-geom-pixel-unit))
                        (+ (cadadr ZM) (acet-geom-pixel-unit))
                        (caddr (cadr zm))
                  )
                )
              )
              (if (setq vpna (acet-currentviewport-ename))
                  (setq vplocked (acet-viewport-lock-set vpna nil))
              );if
              (command "_.zoom" "_w" (car ZM) (cadr ZM))  ; zoom to include text objects
            )
          )
 
          (setq VIEW     (acet-geom-view-points)
                TMPFIL   (strcat (getvar "tempprefix") "txtexp.wmf")
                PT1      (acet-geom-midpoint (car view) (cadr view))
                PT2      (list (car PT1) (cadadr VIEW))
          )
 
          (if (acet-layer-locked (getvar "clayer"))       ; if current layer is locked
            (progn
              (command "_.layer" "_unl" (getvar "clayer") "")  ; unlock it
              (setq LOCKED T)
            )
          )
 
          (command "_.mirror" SS "" PT1 PT2 "_y"
                   "_.WMFOUT" TMPFIL SS "")
 
          (if (findfile tmpfil)                           ; Does WMF file exist?
            (progn
              (command "_.ERASE" SS "")                   ; erase the orignal text
              (setq ss (acet-wmfin TMPFIL))               ; insert the WMF file
              (command "_.mirror" ss "" PT1 PT2 "_y")
            ) ;progn
          ) ;if
 
 
          (if LOCKED
            (command "_.layer" "_lock" (getvar "clayer") "") ; relock if needed
          ) ;if
 
          (if ZM (command "_.zoom" "_p"))              ; Restore original view if needed
          (if vplocked 
              (acet-viewport-lock-set vpna T) ;re-lock the viewport if needed.
          );if
          (prompt (acet-str-format "\n%1 text object(s) have been exploded to lines."  CNT))
          (prompt "\nThe line objects have been placed on layer 0.")
        )
      )
    )
    (prompt "\nView needs to be in plan (0 0 1).")
  );if equal
  (acet-error-restore)                                  ; Retsore values
  (princ)
)


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCG8yHq15uUY+24Gt2YX3lNZLSbpSWeqr08bF2p
;;; Vz5yltNbs8xHa7EQCmzm9d3EnsoTsvmdCNwz8740yB0TuAhkSP6PLBjoTdeXSkAc
;;; uNVOz4fcKsCK4MuRgqdikbIsXntZC4JYWPxZ0VA+fsWdcDBlztOVjNXvH7b6RfxU
;;; pVtaqNPvlRK94UdXqpnFIaC3UJDiS3PF4nGZdaJFhFxKcxP0+6KXXYnZRd/jZUYk
;;; fvYgFszI/RwAOwj/78L4lQubRf8HumgIwh06DTG1eu8/Hlz4AfcCbBXltXjfRdnz
;;; 1nBSnCIA64ILUTbVNq4fidOokk6fgBwCwu0EupMS+AUKKx8no/DT5xmyFEF/y4Nq
;;; cphQkHG1Bo6d4f0IDz+4aGpbBBRl2ClCWeVLcLrLIVvDbKlDYCDYGPN7tlVZl1Ih
;;; +V3+lemRcdOZOSeius2v0gtPkvsipbmAdiP+9ObT+Y+mqZtlLEsxhQwuc9hOVwlw
;;; nX8HnGntEiZGPI4LXxYzMSVU4FqQjVNH86DOuMFVI6+EyQAc+2P/ky7D0GzcVRZc
;;; qi1eOdyxB1U7Q9gwiObi9wIZS+/rWXuW7d37nnzKTgp7VyJeQxHWlktfRj5DUYHN
;;; jkMOWpk3weVcCnoP1HBZl4Eu+7hIdy2FRFg73B+K3PQsKcLO+kN5NNb+HjrOsfVq
;;; rRGkTg==
;;; -----END-SIGNATURE-----