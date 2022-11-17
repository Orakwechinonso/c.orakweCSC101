;;;                                                                    ;
;;;  LISPDATA1.LSP                                                     ;
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
;;;  This file demonstrates the capability of Visual LISP to store     ;
;;;  AutoLISP data to a drawing.                                       ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model functions here.
(if (car (atoms-family 1 '("vl-load-com"))) (vl-load-com))

;;;--------------------------------------------------------------------;
;;;       Function:  C:LISPDATA-TEST                                   ;
;;;                                                                    ;
;;;    Description:  Command which demonstrates the capability of      ;
;;;                  Visual LISP to store AutoLISP data to a drawing.  ;
;;;                  This is done in two ways:  First, by              ;
;;;                  storing data in a global dictionary               ;
;;;                  named "MY-DICT". Second, by storing data in an    ;
;;;                  AutoCAD circle entity.                            ;
;;;                                                                    ;
;;;              The ActiveX methods demonstrated are:                 ;
;;;                  (vlax-ldata-put <str/entity> <key> <data>)        ;
;;;                  (vlax-ldata-get <str/entity> <key> [<default>])   ;
;;;                  (vlax-ldata-delete <str/entity> <key>)            ;
;;;                  (vlax-ldata-list <str/entity>)                    ;
;;;                  (vlax-ldata-test <ldata>)                         ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:LISPDATA-TEST) or LISPDATA-TEST from the ACAD   ;
;;;                 Command: prompt.                                    ;
;;;--------------------------------------------------------------------;
(defun C:LISPDATA-TEST (/	 aApp	  aDoc	   key1	    val1
			modelSpace	  circleObj	    x
			handle	 entname  key2	   val2
		       )
  (princ "Visual LISP LispDATA demonstration")
  (setq	aApp (vlax-get-acad-object)
	aDoc (vla-get-ActiveDocument aApp)
  )

  ;; Add LDATA in global dictionary
  (princ "\nStep 1. LDATA in global named dictionary:\n")
  (setq	key1 "Key1"
	val1 '("Key1 value" 1 1.1 k1)
  )

  ;; Check value to fit in LDATA
  (or (vlax-ldata-test val1)
      (exit)
  )

  (vlax-ldata-put "MY-DICT" key1 val1)
  (princ "Dictionary: adding   -> ")
  (prin1 val1)
  (terpri)

  (setq x (vlax-ldata-get "MY-DICT" key1 "None"))
  (princ "Dictionary: getting  <- ")
  (prin1 x)
  (terpri)

  (setq x (vlax-ldata-list "MY-DICT"))
  (princ "Dictionary: listing  == ")
  (prin1 x)
  (terpri)

  ;; Get default value for no LDATA:
  (vlax-ldata-delete "MY-DICT" key1)
  (setq x (vlax-ldata-get "MY-DICT" key1 "NO VALUE"))
  (princ "Dictionary: deleted  -- ")
  (prin1 x)
  (terpri)


  ;; Add LDATA in Entity's xdictionary
  (princ "\nStep 2. LDATA in Entity's xdictionary:\n")
  (setq	modelSpace (vla-get-ModelSpace aDoc)
	circleObj  (vla-AddCircle modelSpace (vlax-3d-point '(10.0 10.0 0.0)) 10.0)
	handle	   (vla-get-Handle circleObj)
	entname	   (entlast)
  )
  (setq	key2 "Key2"
	val2 (list "Key2 value" 2 2.2 'k2 handle circleObj entname)
  )

  ;; Check value to fit in LDATA
  (or (vlax-ldata-test val2)
      (exit)
  )

  (vlax-ldata-put circleObj key2 val2)
  (princ "Entity: adding   -> ")
  (prin1 val2)
  (terpri)

  (setq x (vlax-ldata-get circleObj key2 "None"))
  (princ "Entity: getting  <- ")
  (prin1 x)
  (terpri)

  (setq x (vlax-ldata-list circleObj))
  (princ "Entity: listing  == ")
  (prin1 x)
  (terpri)

  ;; Get default value for no LDATA:
  (vlax-ldata-delete entname key2)
  (setq x (vlax-ldata-get entname key2 "NO VALUE"))
  (princ "Entity: deleted  -- ")
  (prin1 x)
  (terpri)

  ;; Clean up DWG
  (vla-erase circleObj)

  (princ "\nLDATA demonstration complete")
  (princ)
)

(princ "\nTo test: C:LISPDATA-TEST")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAxs6t91C5zyW58Fp8J/IVTipsq0sO4LikHq0oZ
;;; wTqbVXingUkbVE9Cp7/A1L6bMYQMMBVOvqzW6m21uobNvhCWz6ED3IoK7n5MFtpW
;;; ueQ2SUxISagFNJaSsmqiIuOPCO0w75Zr3P97rjOFjl8ofRe0JAeFGwfopun5ywkO
;;; nmrbgNX4Klbp+xH9wuTXe4Iqwj2Y4TXVUBCXAnwKoFjj5hrR8nXU3r+SqzbgGmX4
;;; Ilt9m2qJ8qrtDCAI/yN6UCNRdcnSVpEnjmg3ctuQCkVjFBbnORnSiZzsTyl+8WaL
;;; 2AfLLWRO3PXYABxc9THxLr7nh+zI6SlcdxSL1l4N3Z34lTBka9bJ9g4cNqMSxBeb
;;; g/naPHVa6sezQH0ZDO4Te9FPCenqguBJ82oFcJ2aBb53m4FzQi1MiRbyFyqyLok/
;;; nb/887+MPk1ZbJQawbQ084wzQZyDfgQCHGjG68Hdh8lMuwjB6CUX4NyfiAuCCziK
;;; U/1yDECH5leeaLmEwhUZokUZ5gb0/d0uXizCd7N62GYAfrtDfOguX9BCmK2ZZbl5
;;; FRkfv7+BHy6oWCnH/IpQdXyAFrfuh8WpRnvOzRscaD8H4xYk4sMhZPUPxrW5yWh5
;;; 8AgYaeW8QHHCY31r/3LxVsspvyp4OyrXOqwVvLG52TwctGGcWk1oKTZBSIiv4DAv
;;; 2TgKtw==
;;; -----END-SIGNATURE-----