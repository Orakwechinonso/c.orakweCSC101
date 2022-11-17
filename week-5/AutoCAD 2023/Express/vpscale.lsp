;;;
;;;    VPSCALE.LSP
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
 
;    find the scale of a viewport relative to paper space
;    Carl Bethea  11 April 91
;
;     Paul Vine   20 April 1999   Ported to 2000.
;
;--- paper -------------------------------------------------
; returns T if in paper space
(defun paper ()
   (> 2 (getvar "cvport")(getvar "tilemode")) ; port=1 & tile=0
)
;
;--- getx --------------------------------------------------
; return <nth> dotted pair of the extended entity data
; from an entity association list <data>
;
(defun getx (n data)
(nth n (cdadr (assoc -3 data)))
)
;
;
;
;--- c:vpscale ----------------------------------------------
; get the xp scale factor of a pspace viewport
;
(defun c:vpscale (/ ent data cvsize cvhgt units vpna flag)
 (cond
  ((not (equal 0 (getvar "tilemode")))
   (princ "\n  Command not allowed unless TILEMODE is set to 0  ") 
  )
  ((and (/= 1 (getvar "cvport"))
        (setq vpna (acet-currentviewport-ename))
        (equal 1 (logand 1 (cdr (assoc 90 (entget vpna)))))
   )
   (princ "\n  Command not allowed in perspective view  ") 
  )
  (T
 
      (acet-error-init
        (list
          (list "cmdecho" 0
                "luprec" (getvar "luprec")
                "dimzin" 8
          )
          T     ;flag. True means use undo for error clean up.
 
        );list
      );acet-error-init
 
 
      (if (paper)
        ;(setq ent (car (entsel "\nSelect edge of viewport: ")))
 
       ;;Added the following code to replace the above line.  Irregularly shaped floating viewports actuall
       ;;consist fo two entities (a pline and a viewport) with reactors on each other to point to each other
       ;;so a simple (entsel) returned a pline instead of a viewport. Had to uise the built-in filtering
       ;;capability of 'acet-ui-single-select' to get around this problem.
       (progn
          (while (not flag)
           (princ "\nSelect edge of viewport.")
           (setq ent (acet-ui-single-select '((0 . "viewport")) T )) ;setq
           (if (and ent
                    (= 1 (logand 1 (cdr (assoc 90 (entget ent)))))
               )
               (progn
                 (princ "\nViewports with perspective view on are not allowed.")
                 (setq flag nil)
               );progn
               (setq flag T)
           );if
          );while
        );progn
        (setq ent (acet-currentviewport-ename))
      )
      (cond
        ((and
            ent
            (setq data (entget ent '("ACAD")))
            (= "VIEWPORT" (acet-dxf 0 DATA))
         );and
          (setq cvhgt  (acet-dxf 41 DATA)  ; viewport height
                cvsize (cdr (getx 6 data))    ; viewsize from extended data
          )
          (prompt "\nPS:MS == ")
          (cond
            ((< cvsize cvhgt)
              (princ (rtos (/ cvhgt cvsize) 2))
              (princ ":1")
            )
            (T (princ "1:")
              (princ (rtos (/ cvsize cvhgt) 2))
            )
          );cond
          (setq units (getvar "lunits"))
          (setvar "luprec" 8)
          (cond
            ((= units 4)
              (prompt (strcat "\nViewport Scale: " (rtos (/ 12 (/ cvsize cvhgt))) " = 1'-0\""))
            )
            ((= units 3)
              (prompt (strcat "\nViewport Scale: 1\" = " (rtos (/ cvsize cvhgt))))
            )
          )
        )
        (T (prompt " no viewport found."))
      );cond
      (acet-error-restore)                                  ; Retsore values
  )
 );cond close 
  (princ)
);c:vpscale


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBmH4EqsmddIheM0AmN96aDUgWnc6jW8q6ZpeBd
;;; yvALmbfGIKDSgo1+GDt4Djrez/4BKthQ2cpZDY5wcHArRgqtYNbgSMJMoevdxapy
;;; PW+HZnYPW6CLEvEch64s3uLsZmtL5EA1wrVIGgtWAfge2DMoA65sOzGxcdRQCUr6
;;; 6rBVcu8xOoKxMI37RTSesqjVGC95pl8HumzhpK8yGoG4TZDU8AB5wcy8xVlzk8uv
;;; ZWlzY2OD/cgumWuGyvlFa607Agwu6fBhWsA96ngeEWYHzTO09H/FtrTt0crlIwz/
;;; bGAEG1GWLlbfgcxxNYUEjdmH7XNVPxB+KSnylxng1IIYUiFpUrDRSDDqmnypqxDb
;;; zc0TSbFM5DFeOKQjNIaAincSTL2ycztYI+bl212ELW+72J5Q2/j79zJ1da9pp89A
;;; lALPPiXmRttglYtFzc46d4i4Usf6Vax2v7Imw50uWJM31tzZJ4ljIirYuqH5V0lo
;;; 0g4HRTeqAIESXRHGhQp25SBMbpRJZ4KuP4xCj6XHHVtqBtofYa2hLayZVcnFlYdI
;;; Hb5b9lsnoYDIgh3JUlVXwAXlFtl0ZXOPqOo+FA7njj88uQIeArG2B+jFGNrvgk/K
;;; m+qx3knwG+nrkqCXqD7PJzmt4gyYS/LlD3YUE7RN62d3blVTVVN3/XvRK+SpoSa9
;;; 0IrC4Q==
;;; -----END-SIGNATURE-----