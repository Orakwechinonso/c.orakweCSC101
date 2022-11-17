;;;                                                                    ;
;;;  TMATRIX.LSP                                                       ;
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
;;;  This file demonstrates the use of vla-TransformBy to modify       ;
;;;  AutoCAD drawing entities                                          ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model functions here
(vl-load-com)

;;;--------------------------------------------------------------------;
;;;       Function:  none                                              ;
;;;                                                                    ;
;;;    Description:  a sample load and execute upon loading.           ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  Vla Object                                        ;
;;;                                                                    ;
;;;          Usage: Load and evaluate                                  ;
;;;--------------------------------------------------------------------;
;|
vla-TransformBy
Moves, scales, or rotates an object given a 4 x 4 transformation matrix.
The following table demonstrates the transformation matrix 
configuration, where R = Rotation, and T = Translation:
'(
  (R00 R01 R02 T0)
  (R10 R11 R12 T1)
  (R20 R21 R22 T2)
  (0.0 0.0 0.0 1.0)
 )
|;

(setq IAcadApplication (vlax-get-acad-object)
      ActiveDocument (vla-get-ActiveDocument IAcadApplication)
      ModelSpace (vla-get-ModelSpace ActiveDocument)
)

;Rotation Matrix: 90 Degrees about point 0,0,0
(setq ma1 '((0.000000  -1.000000  0.000000  0.000000)
	    (1.000000  0.000000  0.000000  0.000000)
	    (0.000000  0.000000  1.000000  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))
;Scaling Matrix: scale by 1.707107 about point 5,5,0
(setq ma2 '((1.707107  0.000000  0.000000  5.000000)
	    (0.000000  1.707107  0.000000  5.000000)
	    (0.000000  0.000000  1.707107  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))
;Translation Matrix: move an entity by 10,10,0
(setq ma3 '((1.000000  0.000000  0.000000  10.000000)
	    (0.000000  1.000000  0.000000  10.000000)
	    (0.000000  0.000000  1.000000  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))
;Scaling Matrix: scale by 10 at point 0,0,0
(setq ma4 '((10.000000  0.000000  0.000000  0.000000)
	    (0.000000  10.000000  0.000000  0.000000)
	    (0.000000  0.000000  10.000000  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))
;Scaling Matrix: scale by 10 at point 2,2
(setq ma5 '((10.000000  0.000000  0.000000  -18.000000)
	    (0.000000  10.000000  0.000000  -18.000000)
	    (0.000000  0.000000  10.000000  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))

;; New for AutoCAD 2000 - Create and Fill A Variant Array for the Transforms
(setq clr 0)
(foreach x (list 'ma1 'ma2 'ma3 'ma4 'ma5)
  (progn
    (setq translen (length (eval x)))
    (setq TransDataA (vlax-make-safearray vlax-vbDouble (cons 0 (1- translen)) (cons 0 (1- translen))))
    (vlax-safearray-fill TransDataA (eval x))
    (setq TransData (vlax-make-variant TransDataA (logior vlax-vbarray vlax-vbDouble)))
    (setq line (vla-AddLine ModelSpace (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point '(10.0 10.0 0.0))))
    (vla-TransformBy line TransData)
    (vla-Put-Color line (setq clr (1+ clr)))
    (vla-Update line)
   )
)

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDRTlpSxrTLUDmaova/jOfx2yrlbMt8M9p+hXc/
;;; 3OcrkMs2UzNVIBcpeBYYx/it8b6O4o19KDbxzLEX5zp8U0WmYWPqs+X0yWwWNrko
;;; 8osQ3p679e+Xk5q7PQKSXsyxStvP6v0ylHBsfdih1cJX/j7qoh/pPiiRVlNn5qs/
;;; xYeqzCtB8SDbCBGs3FBUdSwHomu+jAdWHYnpotcDVzYhaw4uN3gSv1vo3s1wbQY1
;;; NiqqAupf1f9EnrEAgKhL1zkDxmXE4OdX7ps+nYoM6iJR8WjmhvvghYmZKZTzI1y/
;;; wSjzSXPEotHh9HdqPlON40B4NYiiXXfjoRR4PS/cWUbqDoiU109k4gXtt3UR/6MC
;;; umuWS6BLEY2f11D2AREyO2tpK9c9tSqrvVHj4OogaG+vDzQPX/ScVKG86bBxUxIJ
;;; eyf+dXT3I+ZzSIqFKOGJ64scMkQMLOZW7OWkbmU+n5pzMaViKZOSUVv7slAXVkQA
;;; r9P2BeZ3PQJKn3f4YklGOEdJkwPTkdVCkPu2+MXgMQkE6u+q1GcNVSWHCMsKj2om
;;; NvVf3KFsHAh6GeDvlb7o3GXMFYLyfa0PPzwTPUaOQtS8solVa3Oc9pNBTs+u7cJG
;;; ofO2AunzwWlji8DzFfdp4TbtU7Wx0lx9zoVgQhaZ5sM0mz3DfFUOCxLVcv0+8SFg
;;; JKBtiw==
;;; -----END-SIGNATURE-----