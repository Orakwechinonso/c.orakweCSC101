;;;                                                                    ;
;;;  ACDBEX.LSP                                                        ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998, 1999    ;
;;;  by Autodesk, Inc. All Rights Reserved.                            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;                                                                    ;
;;;  Incorporation of any part of this software into other software,   ;
;;;  except when such incorporation is exclusively for your own use    ;
;;;  or for use by others in your organization in the performance of   ;
;;;  their normal duties, is prohibited without the prior written      ;
;;;  consent of Autodesk, Inc.                                         ;
;;;                                                                    ;
;;;  Copying, modification and distribution of this software or any    ;
;;;  part thereof in any form except as expressly provided herein is   ;
;;;  prohibited without the prior written consent of Autodesk, Inc.    ;
;;;                                                                    ;
;;;  AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      ;
;;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           ;
;;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       ;
;;;  INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          ;
;;;  WILL BE UNINTERRUPTED OR ERROR FREE.                              ;
;;;                                                                    ;
;;;  Restricted Rights for US Government Users.  This software         ;
;;;  and Documentation are provided with RESTRICTED RIGHTS for US      ;
;;;  US Government users.  Use, duplication, or disclosure by the      ;
;;;  Government is subject to restrictions as set forth in FAR         ;
;;;  12.212 (Commercial Computer Software-Restricted Rights) and       ;
;;;  DFAR 227.7202 (Rights in Technical Data and Computer Software),   ;
;;;  as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       ;
;;;  Parkway, San Rafael, California 94903.                            ;
;;;                                                                    ;

;;;--------------------------------------------------------------------;
;;; General Note:  THIS FILE IS A MEMBER OF THE REAC-TST PROJECT       ;
;;;--------------------------------------------------------------------;
;;; This file contains various reactor AcDbReactors test               ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  ACDB-CALLBACK                                     ;
;;;                                                                    ;
;;;    Description:  This is a stub function. Its duties are to report ;
;;;                  the values of the arguments passed to it.         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;                  (AcDb-callback reactor arg-list)                  ;
;;;--------------------------------------------------------------------;
(defun AcDb-callback (reactor arg-list / ent)
  (princ "\nAcDb reaction ")
  (princ (VLR-current-reaction-name))
  (if (car arg-list)
    (princ "\nevent in the current graphical data base")
    (princ "\nevent in the other graphical data base")
    )
  (princ "\nAcDb callback second argument: ")
  (print (cadr arg-list))
  (terpri)
)

(setq ar (VLR-AcDb-reactor))    ;; create an object reactor 
                                ;; without data or callbacks

;;;--------------------------------------------------------------------;
;;;       Function:  VLR-FULL-TRACE                                    ;
;;;                                                                    ;
;;;    Description:  This function traces a reactor.                   ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           reactor =  a valid vlr object reactor. Filled in by the  ;
;;;                      calling reactor.                              ;
;;; callback-function =  a function to be invoked                      ;
;;;                      during a reactor event                        ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                  (vlr-full-trace ar 'AcDb-callback)                ;
;;;--------------------------------------------------------------------;
(defun vlr-full-trace (reactor callback-function)
  (foreach name	(VLR-Reaction-Names reactor)
    (VLR-Reaction-Set reactor name callback-function)
  )
  reactor
)


;;examples
;;(vlr-full-trace ar 'AcDb-callback)
;;(vlr-full-trace ar 'VLR-trace-reaction)

;;(vlr-remove ar)

;;;--------------------------------------------------------------------;
;;;       Function:  C:COPYSELF-TEST                                   ;
;;;                                                                    ;
;;;    Description:  This function traces the reactor named ar.        ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    create-same-copy-reactor                        ;
;;;                    add-circle                                      ;
;;;                    reactor-make-same-radius-color                  ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value: a valid vlr reactor object.                        ;
;;;                                                                    ;
;;;          Usage: (C:ACDBEX-TST) or ACDBEX-TST from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:ACDBEX-TST ()
  (if ar
    (vlr-full-trace ar (function AcDb-callback))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:STOP-ACDBEX-TST                                 ;
;;;                                                                    ;
;;;    Description:  This function removes the reactor named ar.       ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value: a valid vlr reactor object.                        ;
;;;                                                                    ;
;;;          Usage: (C:STOP-ACDBEX-TST) or STOP-ACDBEX-TST from        ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:STOP-ACDBEX-TST ()
  (if ar
    (vlr-remove ar)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:ACDBEX-INFO                                     ;
;;;                                                                    ;
;;;    Description:  This function displays a simple help on the       ;
;;;                  ACAD Command: prompt.                             ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:ACDBEX-INFO) or ACDBEX-INFO from                ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:ACDBEX-INFO ()
  (terpri)
  (princ "This test shows how to trace")
  (princ "\nall modification of ACAD graphical data base.")
  (princ "\nYou will see all ACDB reactors calls and their arguments")
  (princ "\nin the Visual Lisp trace window and ACAD console.")
  (princ "\nFor test run ACDBEX-TST command.")
  (princ
    "\nTo stop test run  STOP-ACDBEX-TST command."
  )
  (terpri)
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "ACDBEX-TST" "ACDBEX-INFO" "STOP-ACDBEX-TST")
	     *REACT-TEST-COMMANDS-INFO*))

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDA/Mxvuo+rHoNgOl7RgFyXI46F9VKmb7SAFKRa
;;; If6U4tFfPewO0Nt72lhaXMBZxIS1knP/Um5GNxBPEL5JxsoYYECctIhGvFJFJ0np
;;; Wk+teN94Cf7d3rt5frPFywGfhRsec1NXqJRRLEppNA//qbbeME6qiAGj8yDb0f91
;;; f/YjrKEVEsCYMQ4oLt8c0WIE2dvmSweudPdC28RxAE/5pefy2OqdEKLQ38eWF6hR
;;; qAMJ+gizAj7Pg0weq7iOL5r2Uy6wvXrw9UvWuq+hH4Y06VAzhkvUsGxhQROfkD/i
;;; 7U11FnjRFX5tyUqb2hcRdKmUzGY21FjydehvFrDoaScZGeGAN+JUXEgQk8u5V78/
;;; 2LBpaF8unOXEs8SLb6/2vzCISeKC/Sg4zP3x/GdM9BafMIuDOTugFyjNtHM//yPw
;;; H0M6sQLiaectfUbfTUrxxq13uQy4jivcxlteNuEWMzTjwbPX6pWqzrM4hwhBTr4n
;;; HKoO/axdS+kmpnpZy84cGnUbRiAmUQuv5GyPL/t5akGuZg4mJ2A63Yc3Yke4Xxl0
;;; 2yKdNVjLwVzak18cwZZT/mDsAQpwBe3DqYjdFFXSellywtNfArRiPFxaD3jSj7/s
;;; D5tSeAc/2mr0xx9bZ+wH93TNgTSSzAOygvPzqywsatdYPSWwbddMBnEGIk7fYRNF
;;; kO2f3Q==
;;; -----END-SIGNATURE-----