;;;                                                                    ;
;;;  OBJEX.LSP                                                         ;
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
;;;     This file contains a reactor test.                             ;
;;;	In this test three circles of the same radius                  ;
;;;	will be created and will have different colors                 ;
;;;	Try to change one circle and the others will                   ;
;;;	change their radius.                                           ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  PREPARE-FOR-OBJEX-TST                             ;
;;;                                                                    ;
;;;    Description:  This function makes 3 circles with equal radius   ;
;;;                  in model space.                                   ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (prepare-for-OBJEX-TST)                          ;
;;; Note: This function creates the global variables:                  ;
;;;       [ ac1 ac2 ac3 acadapp acaddoc acadmodel ]                    ;
;;;       They are allowed to be global for the                        ;
;;;       perusal of their values.                                     ;
;;;--------------------------------------------------------------------;
(defun prepare-for-OBJEX-TST ()
  (setq acadApp (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadApp))
  (setq acadModel (vla-get-ModelSpace acadDoc))

;;; Create 3 cirles
  (setq ac1 (vla-AddCircle acadModel (vlax-3d-point '(10.0 10.0 0.0)) 10.0))
  (vla-put-Color ac1 1)
  (setq ac2 (vla-AddCircle acadModel (vlax-3d-point '(5.0 5.0 0.0)) 5.0))
  (vla-put-Color ac2 2)
  (setq ac3 (vla-AddCircle acadModel (vlax-3d-point '(3.0 3.0 0.0)) 3.0))
  (vla-put-Color ac3 3)

  (command "_.ZOOM" "_EXTENTS")
  (command "_regen")
)

;;; define some helpers
;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-THE-SAME-RADIUS                              ;
;;;                                                                    ;
;;;    Description:  This function makes the 3 circles have the same   ;
;;;                  radius.                                           ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            obj1 = a valid vla circle object.                       ;
;;;            obj2 = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:   a valid vla circle object.                       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (make-the-same-radius                            ;
;;;                           vla-circle-object1                       ;
;;;                           vla-circle-object2                       ;
;;;                  )                                                 ;
;;;--------------------------------------------------------------------;
(defun make-the-same-radius (obj1 obj2)
  (if (and
	obj2
	(vlax-write-enabled-p obj2)	;; test if object can be modified
	(vlax-read-enabled-p obj1)	;; test if object can be read
      )
    (VLA-PUT-RADIUS obj2 (vla-get-radius obj1))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SAME-RADIUS-REACTION                         ;
;;;                                                                    ;
;;;    Description:  This function is a call back function invoked     ;
;;;                  from a reactor.                                   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     make-the-same-radius                           ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (make-same-radius-reaction                       ;
;;;                           notifier                                 ;
;;;                           reactor                                  ;
;;;                           arg-list                                 ;
;;;                  )                                                 ;
;;;--------------------------------------------------------------------;
(defun make-same-radius-reaction (notifier reactor arg-list)
  (make-the-same-radius notifier (VLR-Data reactor))
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:OBJEX-TST                                       ;
;;;                                                                    ;
;;;    Description:  This function aids in the creation of a circle    ;
;;;                  object which will contain reactors for:           ;
;;;                  COPY, MIRROR or ARRAY                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    create-same-copy-reactor                        ;
;;;                    add-circle                                      ;
;;;                    reactor-make-same-radius-color                  ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid reactor object such as:                   ;
;;;                 #<VLR-Object-reactor>                              ;
;;;                                                                    ;
;;;          Usage: (C:OBJEX-TST) or OBJEX-TST from                    ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:OBJEX-TST ()
  (prepare-for-OBJEX-TST)
  (setq	r1
	 (VLR-Object-reactor
	   (list ac1)
	   ac2
	   '((:vlr-modified . make-same-radius-reaction))
	 )
  )
  (setq	r2
	 (VLR-Object-reactor
	   (list ac2)
	   ac3
	   '((:vlr-modified . make-same-radius-reaction))
	 )
  )
  (setq	r3
	 (VLR-Object-reactor
	   (list ac3)
	   ac1
	   '((:vlr-modified . make-same-radius-reaction))
	 )
  )
  (vla-put-Color ac1 acRed)  ;touch of the circles to fire reactors the first time
)


;;;;;;; now take a look at ACAD console
;;;;;;; change radius of the ac1 (red one) 
;;;; both ac2 and ac3 will change there radii


;;To remove reactor
;;(vlr-remove r1)
;;(vlr-remove r2)
;;(vlr-remove r3)

;;;;; try ACAD again
;;; Now All circles radii independent again
;;; You can restore contraints by adding reactors back
;;(vlr-add r1)
;;(vlr-add r2)
;;(vlr-add r3)



;;;;;;;;;;;;;;;;;;;;; make them persistent!
;;(vlr-pers r1)
;;(vlr-pers r2)
;;(vlr-pers r3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; to trace all events coming to circles
;|
(setq rr
       (VLR-Object-reactor
	 (list ac1 ac2 ac3)
	 nil
	 nil
	 ))

(defun vlr-full-trace (reactor)
  (foreach name (VLR-Reaction-Names reactor)
    (VLR-Reaction-Set reactor name 'VLR-trace-reaction)))

(vlr-full-trace rr)
|;

;;;--------------------------------------------------------------------;
;;;       Function:  C:OBJEX-INFO                                      ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:OBJEX-INFO) or OBJEX-INFO from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:OBJEX-INFO ()
  (textscr)
  (princ "\nIn this test three circles of the same radius")
  (princ "\nwill be created and will have different colors ")
  (princ "\nTry to change one circle and the others will ")
  (princ "\nchange their radius.")

)



;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "OBJEX-TST" "OBJEX-INFO")
	     *REACT-TEST-COMMANDS-INFO*))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCe1OXNCUVxeXizjA6+rdLpEfHKjV/BZb0Ol/T2
;;; /mDfU5NXgtEis9zOCbWxlTIbKAUuCW7PswOxs+tNzM8aAxp3Bjh/ZrLUZXGVZxNe
;;; MGx8AT0jEezb2PTlCILdGUiSyxqR2tN05092F3RXoGBiUUtNMuSchZZD6d0JLRVm
;;; 4V3AwarbRZZGWmqS7zuRl04HzLzwXmh/+WfSDHlOxript32v5oZHIfi9FLEM4NOl
;;; qkAH8oE7yCwrxCUWrcI6EHRwerOqFkeMSa8F7CWvWlAJuU7124tLs3Y+j3ueVAQm
;;; 1sunfdM7jCLqiDWS8S6BS4N0KFMoQxxBxw3jfLK9bsiw0ZevDgaPyRcljq13SATt
;;; tB/sWQAx1I9yunECCxcIfJ5XL9CLJhl+kM45oA+roBp6BRFavNNXyjuodkVfB24g
;;; czJK14rDrn/WCJgRTUwkYFM1RU2D5dt+tc0dwyoq/dwpAYqFrJBVdM2wciUUfgR+
;;; NYlyfOKuYFZx4BpuKAziuU3iN0JoSDGm5HWSzUAPw42xU9jcyc/w8c5tpocsbs9f
;;; jbgfhSO0/eX0QIrXtP2SdL7FZyVH7HxEuZ/SH0d6Mjjgtr1oRE9b9ZsMKyjk956E
;;; a+jyZDs9y5Dw+MJhETwIRD+CQBYbBnVDrjru6LPbk+Ck7LXEYwyy7BTvyvf75/g9
;;; d5Vx8A==
;;; -----END-SIGNATURE-----