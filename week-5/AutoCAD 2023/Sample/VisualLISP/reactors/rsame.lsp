;;;                                                                    ;
;;;  RSAME.LSP                                                         ;
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
;;; General Note:  THIS FILE IS A MEMBER OF THE RCTR-TST PROJECT       ;
;;;--------------------------------------------------------------------;
;;; This file contains various reactor utilities to make objects share ;
;;; equal propertise. All modification will be made after              ;
;;; :vlr-modified notification has been received.                      ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SAME-PROPERTIES                              ;
;;;                                                                    ;
;;;    Description:  This function is used to modify two               ;
;;;                  vla objects to share the same properties.         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            obj1 = a valid vla object to be used as the source      ;
;;;                   object to get properties from.                   ;
;;;            obj2 = a valid vla object to be used as the target      ;
;;;                   objects to place the properties from obj1.       ;
;;;   property-list = a list of properties to be modified.             ;
;;;                                                                    ;
;;; Returned Value:  A vla object with updated properties.             ;
;;;		                                                       ;
;;;          Usage:                                                    ;
;;;                (make-same-properties                               ;
;;;                      obj1                                          ;
;;;                      obj2                                          ;
;;;                      property-list)                                ;
;;;--------------------------------------------------------------------;
(defun make-same-properties
			    (obj1 obj2 property-list / new-value)
  (if (and
	obj2
	(eq 'VLA-OBJECT (type obj2))
	(vlax-write-enabled-p obj2)	; test if object can be modified
	(vlax-read-enabled-p obj1)	; test if object can be read
      )
    (foreach property property-list
      (if
	(and
	  ;;(vlax-property-available-p obj1 property)
	  ;;(vlax-property-available-p obj2 property)
	  (not				; don't modify if equal
	    (equal
	      (setq new-value (vlax-get obj1 property))
	      (vlax-get obj2 property)
	    )
	  )
	)
	 (vlax-put obj2 property new-value)
      )
    )
  )
)
;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SAME-PROPERTIES-LIST                         ;
;;;                                                                    ;
;;;    Description:  This function is used to modify a collection of   ;
;;;                  vla objects to share the same properties.         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      make-same-properties                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   Filled in by the reactor invoked.                ;
;;;         arg-list  = a list of arguments.                           ;
;;;                   Filled in by the reactor invoked.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (make-same-properties-list                          ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun make-same-properties-list (notifier obj-list property-list)
  (foreach obj obj-list
    (make-same-properties notifier obj property-list)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SAME-RADIUS                                  ;
;;;                                                                    ;
;;;    Description:  This function is used as a call back function to  ;
;;;                  an event. It is responsible in modifying the      ;
;;;                  radius of a circle.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      make-same-properties-list                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   Filled in by the reactor invoked.                ;
;;;       arg-list  = a list of arguments.                             ;
;;;                   Filled in by the reactor invoked.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (make-same-radius                                   ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun make-same-radius	(notifier reactor arg-list)
  (make-same-properties-list
    notifier
    (VLR-Data reactor)
    '("Radius")
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-SAME-REACTOR                               ;
;;;                                                                    ;
;;;    Description:  This creates a duplicate modified event for a     ;
;;;                  list of vla-objects.                              ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         obj-list  a valid list of vla objects.                     ;
;;;        reaction = a valid function to invoke as a call back.       ;
;;;                                                                    ;
;;; Returned Value:  A vlr object reactor.                             ;
;;;                  such as:                                          ;
;;;			#<VLR-Object-reactor>                          ;
;;;		                                                       ;
;;;          Usage:  Where ac1 and ac2 are valid vla-object and        ;
;;;                  reaction is a function call back.                 ;
;;;                 (setq r                                            ;
;;;                     (create-same-reactor (list ac1 ac2)            ;
;;;                       'reactor-save-center-color))                 ;
;;;--------------------------------------------------------------------;
;;!! redefined in RUTILS.LSP
(defun create-same-reactor (obj-list reaction)
  (VLR-Object-reactor
    obj-list	;; owners
    obj-list	;; user data - recivers
    (list (cons :vlr-modified reaction))
  )
)

;;EOF
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgC2aKzd1Gdon8sxWuKR7NG2Yv61rSgQpnuNimPT
;;; zMsXegayy9uW9l8sbTezq1qvJMXEF7kRlmUgjn9bp+j+4ucYgyQskKpG8zlUlA1r
;;; Rr/0hHC4n2FTbOUeSpWMPHU8Mf5DNhZg9WBnK+74mo2DDzAQKX6vTmfkQzt34k05
;;; klgVAnipaujEUO52qfH7eB+YlfW3ws5BmlYx7MQ7qTeiG01NKLZOlgyEeAckdA6O
;;; do1KGHvPkVwswvgY3+QH0zoxDefzXPEtySPIVpFdth5aNJVUd8Xo23eBweiSoKS9
;;; ZpPKAEiM84Yb0YJHT96Zg4HOVNOC+F1r5oyzdLPGSKeUur0qILiusCKvny3qqG5b
;;; 3wYbTxDs1VfQA5qsunyqqzGPxpRYqcYyxoItuHCAAucwXOTcohbn+OwfZRPaYtc4
;;; uIoHnGiAOp4qHf+SiA8yTYMU/YopMzp6Gy+dUvetGSr0PxRtVXp/Ep9C7kOEz/ug
;;; 23WA2PncJbY/L7gbsU9OVFq7e/P+4+XuMTFXHnJDBagJs9G1qAD9+XwPBMgTY3wq
;;; C/UC9TjZ5rDXQL+JBQaiHQe3D9sb36awUtm+Uj/nABuc+7gKOYWoGzpNi2VDdJ19
;;; A4f8twp5a90v5U94e8ooc9Gm1P7Vj9MfRI0tkXwIq7rVvq+dL9OeybWC/ezCt6rg
;;; JMyDFw==
;;; -----END-SIGNATURE-----