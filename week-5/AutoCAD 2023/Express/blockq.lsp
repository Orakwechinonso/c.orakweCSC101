;;
;;;
;;;    BLOCKQ.LSP
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
 
;;;   DESCRIPTION
;;;
;;;   (listb <block name> <entity type>)
;;;   LISTB walks through the entities in a block definition. It also lets
;;;   you specify only one entity type to report from the definiton. For
;;;   instance, (listb "myblock" "attdef") will display only the attribute
;;;   definitons in the block. To list all of the entities in the block,
;;;   supply a NIL argument for <entity type>, as in (listb "myblock" nil).
;;;
;;;   C:BLOCK? serves as a front-end for LISTB. It lets you either supply a
;;;   block name or pick an insterted block. Then you can specify an entity
;;;   type to search for, or accept the default to list all entities in
;;;   the definition.
;;;
;;;-- listb ------------------------------------------------
;;;   list the entities in a block definition <bname>
;;;
 
 
(defun listb (bname etype / data wait)
 
 
   ;; wait for key press
   ;; if ESC, then stop
   (defun wait ()
      (print data)
      (grread (grread T)); clear the buffer
      (terpri)
      (if (and
             (setq data (entnext (acet-dxf -1 data)))
             (/= 27 (cadr (grread)))
          )
          (setq data (entget data '("*")))
          (setq data nil)
      )
   );wait
 
   ;; begin the main program
   (textscr)
   (prompt "\nPress ESC to exit or any key to continue.")
   (terpri)
;;   (print (setq data (tblsearch "block" bname)))
   (if (setq data (tblsearch "block" bname))
     (print data)
   )
   (terpri)
   (if (setq data (acet-dxf -2 data))   ; get first entity
     (setq data (entget data '("*")))   ; get assoc list
   )
 
;;   (setq data (acet-dxf -2 data)               ; get first entity
;;         data (entget data '("*"))   ; get assoc list
;;   )
   (if etype (setq etype (xstrcase etype)))
   (while data
      (cond
         (etype
            (if (= etype (acet-dxf 0 data))
                (wait)
                (setq data
                   (if (setq data (entnext (acet-dxf -1 data)))
                       (entget data '("*"))
                   )
                )
            );if
         );etype
         (T (wait))
      );cond
   );while
   (princ)
)
;;;
;;;-- c:block? -----------------------------------------------
;;;   display a block definition,
;;;   optionally show only certain components
;;;
(defun c:block? (/ old_err bname etype data)
 (setq old_err *error*)
 (defun *error* ( a / )
  (print a)
  (setq *error* old_err)
  (princ)
 );defun
 
 
   (if (= "" (setq bname
         (getstring "\nEnter block name <Return to select>: ")
       ))
       (if (setq bname (entsel "Select a block: "))
           (if (and
                  (setq  data (entget (car bname)))
                  (or (= "INSERT" (acet-dxf 0 data))
                      (= "DIMENSION" (acet-dxf 0 data))
                  )
               );and
               (setq bname (acet-dxf 2 data))
               (setq bname nil)
           );if
       );if
   );if
   (cond
      (bname
         (if
            (= "" (setq etype
                  (getstring "\nEnter an entity type <Return for all>: ")
            ))
            (setq etype nil)
         );if
         (listb bname etype)
      )
      (T  (print " no block found."))
   );cond
 (setq *error* old_err)
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCzVkfeufh4TtnQr3Lv6ZUvwEPvbkWyasUkvHHX
;;; zZSlhMzpLq1+2ox6MftoWhupTb/Vn2/kjxR9OKXSzTL52JDnQa3nE5Yh8JoU2mH2
;;; wqNta9IWVixxjAUMpuuV+oXtDWFQSy3HjuhQw1Mjm2ZKFklvvh7mX9UKQCH4E/WY
;;; WNLclGF61BIA8fI/5EiJ4J+raJJK4SPqTgRrjYM4iNV4uvXfWn6fakCdYICejIMj
;;; y8fPdQkb7+RyJ/M1phAM9ym1j0ZX6+pwihhAyOT9XMW1/drExoi/wx358THiiYyU
;;; VOgLwjbVg8xexXM6bhXYLJqiPEHFms8Vsw30g67y55ACyuNKig+zxTvZDbITPlbz
;;; ptfgkt6eF16Bj24pO696mnQPPQY4eA26Om7DBcrjzP0/nroAlhnWcyCAnlJs2XMr
;;; 3uw2xBvTWInr83oTsm1ZPvVzwY382OEumfsl91b8GV6NwejV4mhjd8wojSx84kLY
;;; dShCM8paVqiHuw4N4WL5ba+eobO3l+cDzQuWKCC4F7MvKKmSOofBQRfg3NPXnUFA
;;; YwQrb2bkLZGzlw7cZ4a1d6AGx/7fg88+ylWa2vHt5gfLXTrn7tlPxkYVOs/Hsa0U
;;; 8OvcoxkQeIyk5MN1kbltVCVUsB8j+C6SkADaX9grzQWuQEaKZAwpozxHqFuVMAdB
;;; eb0/ng==
;;; -----END-SIGNATURE-----