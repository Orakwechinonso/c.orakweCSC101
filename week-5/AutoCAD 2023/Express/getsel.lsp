;;
;;;
;;;    GETSEL.LSP
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
 
(defun c:GETSEL (/ LAY    ;; Layer of the selected entity
                   ENT    ;; Entity type of seleted entity
                   SS     ;; Selection set
                   SSLST  ;; Filter list
                   cspace ;; current space
                )
 
  (acet-error-init
         (list
           (list "cmdecho" 0
                 "expert"  0
           )
           T     ;flag. True means use undo for error clean up.
         )       ;list
  );acet-error-init
  (sssetfirst nil nil)
 
  ;;(setq LAY (car(entsel "\nSelect Object on layer to Select from <*>: ")))
  (setq LAY (car (entsel "\nSelect an object on the Source layer <*>: ")))
 
  (if LAY
    (setq LAY  (cdr(assoc 8 (entget LAY)))
          SSLST (list (cons 8 LAY ))
    )
  )
 
  ;;(setq ENT (car(entsel "\nSelect type of entity you want <*>: ")))
  (setq ENT (car(entsel "\nSelect an object of the Type you want <*>: ")))
 
  (if ENT
    (progn
      (setq ENT  (cdr(assoc 0 (entget ENT))))
      (if SSLST
         (setq SSLST (append (list (cons 0 ENT )) SSLST))
         (setq SSLST (list (cons 0 ENT )))
      )
    )
  )
  (if SSLST
    (progn
      (cond
        ((and LAY ENT)
          (prompt (acet-str-format "\nCollecting all %1 objects on layer %2..." ENT LAY))
        )
        (LAY
          (prompt (acet-str-format "\nCollecting ALL objects on layer %1..."  LAY ))
        )
        (ENT
          (prompt (acet-str-format "\nCollecting all %1 objects in the drawing..."  ENT ))
        )
        (T
          (prompt "\nCollecting all objects in the drawing...")
        )
      )
      (setq SS (ssget "_X" SSLST))
    )
    (progn
      (setq SS (ssget "_X"))
    ) ;progn
  )
  (if SS
    (progn
      (setq SS (sslength SS))
      (if (> SS 0)
        (if (= SS 1)
          (prompt
            (acet-str-format "\n%1 object has been placed in the active selection set." (itoa SS))
          )
          (prompt
            (acet-str-format "\n%1 objects have been placed in the active selection set." (itoa SS))
          )
        ) ;if
        (prompt"\nNothing selected.")
      )
    ) ;progn
  ) ;if
 
  (acet-error-restore)
 
  (princ)
 
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgB0h2gEwHpwkz5gYWHFxrMl5q7ICACW1GgZ75Q3
;;; FNuH89wwjwb5B1+meoKRkUy8qBUESDxLcq7DaqyjELMdwO7C5KVk0HBZCbWT437I
;;; INEUGbTzPYku/N0wSZ3/l3WgUHZLnoYkIwEFIGyHTNDlrjMwdwO8lI8Wq0ek7SAS
;;; 5mI+i+PM+R66lFy+/q7o+l7RVp+EOY8Qd8Ke0oa7jjI8cS0aW4EEf9ADonf3l753
;;; TSsLLQBunYRGY8qOsO4pOM6MgPVqB2gsuLLnCEiFB16d7sWYSCuOimKzRy7Xezo0
;;; 0UxPJat4KGfbwSncIA9Ntlr3HxA37a+yyLniaL4uYm8eP9KLiP2oyLQ7pd7uY1OO
;;; X4Am1YbgPXZ8pte9jfM34NHfpdoWTgcCvOXz1cBH6eKumXsBEt72K0+zlehzTU/X
;;; TcXnV3fsgYqDvMq9pOcX4cHcIZa2jOLbBxEJz43hvoCKOooaiRzyVa261icn7Baq
;;; 1x4+eLTJGTznlRFTxv1widcnA3jc9UPRm84FQj+D0vFPX1RwCOJpcH3VxZn8k0tH
;;; o/iy74k45UD1NuKpYqBCpirkD1SPcy/frS6Ui9mC9GX/phJAUkyY6xXe+VKzfF6J
;;; m5oPp5Fri4hyCmtzAYK56/EuUz1j9yzGWdKRUXrXSTNu8WyMj2W84QpnXB7MthiY
;;; AtGjWQ==
;;; -----END-SIGNATURE-----