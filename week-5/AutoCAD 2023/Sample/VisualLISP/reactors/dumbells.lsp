;;;                                                                    ;
;;;  DUMBELLS.LSP                                                      ;
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
;;;     This file contains an example that enables the                 ;
;;;	creation of two red circles with the same radius               ;
;;;	with a red line connecting their centers. If you               ;
;;;	change the radius of a circle the adjacent circle              ;
;;;	will match the radius of the modified circle. If you           ;
;;;	move the position of a line end or the line itself,            ;
;;;	the circles will adjust to the end points of the line.         ;
;;;	Run the DUMBELLS-TST command to see this test.                 ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  _REACTOR-MAKE-ASSOCIATION-PROPERTIES              ;
;;;                                                                    ;
;;;    Description:  This function makes an association between        ;
;;;                  object1 and object2 with the supplied             ;
;;;                  association data.                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             obj1 = a valid vla object.                             ;
;;;             obj2 = a valid vla object.                             ;
;;; association-list = an association list.                            ;
;;;                                                                    ;
;;; Returned Value:  A valid vla object.                               ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (_reactor-make-association-properties                          ;
;;;           vla-Object1                                              ;
;;;           vla-Object1                                              ;
;;;           properties-List )                                        ;
;;;--------------------------------------------------------------------;
(defun _reactor-make-association-properties
					    (obj1
					     obj2
					     association-list
					     /
					     new-value
					     property-from
					     property-to
					    )
  (if (and
	(eq 'VLA-OBJECT (type obj2))
	(vlax-write-enabled-p obj2)	;; test if object can be modified
	(vlax-read-enabled-p obj1)	;; test if object can be read
      )
    (foreach association association-list
      (if
	(and
	  (setq property-from (vlax-ldata-get obj1 association))
	  (setq property-to (vlax-ldata-get obj2 association))
	  (vlax-property-available-p obj1 property-from)
	  (vlax-property-available-p obj2 property-to)
	  (not				;; don't modify if equal
	    (equal
	      (setq new-value (vlax-get obj1 property-from))
	      (vlax-get obj2 property-to)
	    )
	  )
	)
	 (vlax-put obj2 property-to new-value)
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  _REACTOR-MAKE-ASSOCIATION-PROPERTIES-LIST         ;
;;;                                                                    ;
;;;    Description:  This function makes an association list using     ;
;;;                  _reactor-make-association-properties              ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      _reactor-make-association-properties          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         notifier = a valid vla object.                             ;
;;;         obj-list = a list of vla object.                           ;
;;;    property-list = an association list.                            ;
;;;                                                                    ;
;;; Returned Value:  The last modified vla object.                     ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (_reactor-make-association-properties-list                     ;
;;;           notifier                                                 ;
;;;           obj-list                                                 ;
;;;            '("start-association"                                   ;
;;;              "end-association"                                     ;
;;;               "Radius-association") )                              ;
;;;--------------------------------------------------------------------;
(defun _reactor-make-association-properties-list
       (notifier obj-list property-list)
  (foreach obj obj-list
    (_reactor-make-association-properties
      notifier
      obj
      property-list
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-CIRCLE-LINE-POINT                         ;
;;;                                                                    ;
;;;    Description:  This function is a call back function to an       ;
;;;                  editor reactor.                                   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      _reactor-make-association-properties-list     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         notifier = a valid vla object filled by the reactor        ;
;;;         obj-list = a list of vla object filled by the reactor      ;
;;;           arg-list = filled by the reactor.                        ;
;;;                                                                    ;
;;; Returned Value:  The last modified vla object.                     ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (_reactor-make-association-properties-list                     ;
;;;           notifier                                                 ;
;;;           obj-list                                                 ;
;;;           properties-List )                                        ;
;;;--------------------------------------------------------------------;
(defun reactor-circle-line-point (notifier reactor arg-list)
  (_reactor-make-association-properties-list
    notifier
    (VLR-Data reactor)
    '("start-association" "end-association" "Radius-association")
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:DUMBELLS-TST                                    ;
;;;                                                                    ;
;;;    Description:  This function creates a line and two circles.     ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    reactor-circle-line-point                       ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid reactor object such as:                   ;
;;;                 #<VLR-Object-reactor>                              ;
;;;                                                                    ;
;;;          Usage: (C:DUMBELLS-TST) or DUMBELLS-TST from              ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:DUMBELLS-TST
       (/ circle1 circle2 line obj-lst reactor acadModel)
  (setq
    acadModel (vla-get-ModelSpace
		(vla-get-ActiveDocument (vlax-get-acad-object))
	      )
    line1     (vla-addLine acadModel (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point'(0.0 50.0 0.0)))
    circle1   (vla-addCircle acadModel (vlax-3d-point '(0.0 0.0 0.0)) 20)
    circle2   (vla-addCircle acadModel (vlax-3d-point '(0.0 50.0 0.0)) 20)
    obj-lst   (list circle1 circle2 line1)
  )
  (foreach vla-obj obj-lst
    (vla-put-Color vla-obj acRed)
  )
  (vlax-ldata-put line1 "start-association" "StartPoint")
  (vlax-ldata-put line1 "end-association" "EndPoint")
  (vlax-ldata-put circle1 "start-association" "Center")
  (vlax-ldata-put circle2 "end-association" "Center")
  (vlax-ldata-put circle1 "Radius-association" "Radius")
  (vlax-ldata-put circle2 "Radius-association" "Radius")
  (setq	reactor
	 (VLR-Object-reactor
	   obj-lst
	   obj-lst
	   '((:vlr-modified . reactor-circle-line-point))
	 )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:DUMBELLS-INFO                                   ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:DUMBELLS-INFO) or DUMBELLS-INFO from            ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:DUMBELLS-INFO ()
  (terpri)
  (princ
    "\nThis example creates two red circles with the same "
  )
  (princ
    "\nradius and a red line connecting their centers. If you "
  )
  (princ
    "\nchange the radius of a circle the adjacent circle "
  )
  (princ
    "\nwill match the radius of the modified circle. If you"
  )
  (princ
    "\nmove the position of a line end or the line itself, "
  )
  (princ
    "\nthe circles will adjust to the end points of the line. "
  )
  (princ "\nRun the DUMBELLS-TST command to see this test.")
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "DUMBELLS-TST" "DUMBELLS-INFO")
	     *REACT-TEST-COMMANDS-INFO*
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCHKLkMLdf7Jrgw776YWAfO4zjMPCsNlIbMnVMQ
;;; mI5HjlG5/6OLAfvCg/egq+jnBde3B/Y1MI0UTgyGWl+uLU1GIM4D7S3S53qkHiwT
;;; MF5mhXhhmQreABKBaTaUI9mEUM8i/WiEF1mBIpCojbbVdqIF2MQZDVs26Jl8V+0d
;;; kRSFuzDOFFJMm6HBHdOvxqL65o+uAU+oGNaMO/R/Cwlhxnp4sc8S3oJY3je+zAig
;;; Sxfjf1Pd4NeP0sMNbpEIFpiPC7JLGdT/wdWuvLQOBned7UX9UuXYOyAwQ+2b/bgv
;;; pzj1mrIupSz0Z1zgLhdb7i+h+hfgVZc0OdKwo4lPhGqiEFmmZI5eGcnJKT60Z5Mj
;;; RxPubCbyZGbTJODGzHNIpYc4rE2AUCF7beO3AiXg4BR5sdo1u+sb+WOcK9on0SPE
;;; U0cCjOBWCfm6HGYtm8jiQkObAnciBcYN+kUlvMiDFtKMJQeUBB1DTIPTKkO5xuM3
;;; UqiVKwP12071H8AeHnAoDNA/kbyQZodo1miXqKFCdjXqMNKP6duFI1JomokB1vhY
;;; 2SS1EUY6tq/+pV+jowKjFfuKDdPdYrLqmRczg1lTKoLLWrRx+IHw8nDhcRLN2egX
;;; qZ/Fl1vtZwMspgL/cbQFzxQKL8N1Kmm9IJQ0FgQvSOwS3ATl1YsWPtMMVU9bUoNd
;;; i0+SQw==
;;; -----END-SIGNATURE-----