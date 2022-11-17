;;;                                                                    ;
;;;  RTRANSL.LSP                                                       ;
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
;;; This file contains various reactor utilities                       ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-TRANSLATE-CENTER                          ;
;;;                                                                    ;
;;;    Description:  This function is used within a :vlr-modified      ;
;;;                  reactor event.                                    ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     subtract-vector                                ;
;;;                     translate-vla-circle                           ;
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
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;     (reactor-translate-center notifier reactor arg-list)           ;
;;;--------------------------------------------------------------------;
(defun reactor-translate-center
				(notifier   reactor    arg-list
				 /	    property   from
				 to	    tr-vect    transl-obj
				)
  (if (vlax-read-enabled-p notifier)
    (progn
      (setq from    (vlax-ldata-get notifier "Center")
	    to	    (vlax-get notifier "Center")
	    tr-vect (subtract-vector to from)
      )
      (if
	(and from
	     (not (equal from to))
	)
	 (foreach obj (vlr-data reactor)
	   (translate-vla-circle obj tr-vect)
	 )
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  TRANSLATE-VLA-CIRCLE                              ;
;;;                                                                    ;
;;;    Description:  This function is used as a move method for an     ;
;;;                  object.                                           ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     add-vector                                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             obj = a valid vla object.                              ;
;;;     translation = a valid translation list as returned from        ;
;;;                   add-vector. Such as (7 7 7)                      ;
;;;                                                                    ;
;;; Returned Value:  A valid vla object.                               ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (translate-vla-circle obj '( 7 7 7 )             ;
;;;--------------------------------------------------------------------;
(defun translate-vla-circle (obj translation / old-center new-center)
  (if (and
	obj
	(eq 'VLA-OBJECT (type obj))
	(vlax-write-enabled-p obj) ;; test if object can be modified
      )
    (progn
      (setq old-center (vla-get-center obj)
	    new-center (add-vector old-center translation)
      )
      (vlax-ldata-put obj "Center" new-center)
      ;; It is important to store new-center before the object is moved.
      ;; Because after moving, this object will fire notifications to   
      ;; its associated objects. Note: updating the new center position 
      ;; property will not move other circles. Only the translation of  
      ;; the first object will cause translations of all other objects. 
      (vla-move obj old-center (vlax-3d-point new-center))
    )
  )
)


;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-TRANSLATE-REACTOR                          ;
;;;                                                                    ;
;;;    Description:  This function is used as a reactor constructor.   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     reactor-save-center                            ;
;;;                     reactor-translate-center                       ;
;;;                     save-object-properties                         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         cla-lst = a list of vla circle objects.                    ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (create-translate-reactor cla-lst )              ;
;;;--------------------------------------------------------------------;
(defun create-translate-reactor	(cla-lst / reactor)
  (function reactor-save-center)
  (function reactor-translate-center)
  (foreach obj cla-lst
    (save-object-properties obj '("Center"))
  )
  (setq	reactor
	 (VLR-Object-reactor
	   cla-lst		;; owners
	   cla-lst		;; recievers
	   (list
	     (cons :vlr-objectClosed 'reactor-save-center)
	     (cons :vlr-modified 'reactor-translate-center)
	   )
	 )
  )
  reactor
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SQUARE-CL                                    ;
;;;                                                                    ;
;;;    Description:  This function creates 4 circles.                  ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;     start-point = a valid 3d point   (list of three reals)         ;
;;;             rad = a real number.                                   ;
;;;                                                                    ;
;;; Returned Value:  A valid list of vla circle objects.               ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (make-square-cl start-point rad)                 ;
;;;--------------------------------------------------------------------;
(defun make-square-cl
       (start-point rad / acadApp acadDoc acadModel ac-lst pi2 i)
  (setq acadApp   (vlax-get-acad-object)
        acadDoc   (vla-get-ActiveDocument acadApp)
        acadModel (vla-get-ModelSpace acadDoc)
        pi2       (/ pi 2.0)
        i         0
  )
  (while (< i 4)
    (setq ac-lst      (cons (vla-AddCircle acadModel (vlax-3d-point start-point) rad) ac-lst)
          i           (1+ i)
          start-point (polar start-point (* pi2 i) rad)
    )
  )
  ac-lst
)


;;;--------------------------------------------------------------------;
;;;       Function:  C:RTRANSL-TST                                     ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for the            ;
;;;                  position of the first circle and creates          ;
;;;                  3 additional circles. The circle centers          ;
;;;                  will form a square.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    create-translate-reactor                        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid reactor object such as:                   ;
;;;                 #<VLR-Object-reactor>                              ;
;;;                                                                    ;
;;;          Usage: (C:RTRANSL-TST) or RTRANSL-TST from                ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:RTRANSL-TST (/ pnt rad)
  (initget 1)
  (setq pnt (getpoint "\nSelect center of the first circle: "))
  (initget 103)	  ;; (+ 1 2 4 32 64)
  (setq rad (getdist pnt "\n Select radius: "))
  (create-translate-reactor (make-square-cl pnt rad))
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:RTRANSL-INFO                                    ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:RTRANSL-INFO) or RTRANSL-INFO from              ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:RTRANSL-INFO ()
  (princ
    "\nFour circles will be created. These circles will be moved together"
  )
  (princ "\nif you move one of them.")
  (princ
    "\n(You will be asked to select center and radius of the circle.)"
  )
  (princ "\nFor test call RTRANSL-TST command")
  (terpri)
  (princ)
)


;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "RTRANSL-TST" "RTRANSL-INFO")
	     *REACT-TEST-COMMANDS-INFO*
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCmSH/pT/sBxMbG/yTbVmH4vNrL+mSP+OdzvQ1f
;;; bWMg9U1rD0ccbcpnXatKCQFKaRKg7b3Gb1XqDWNYFXSGsgWTog+oy7B5ZzkCyDfQ
;;; UKOgBCf36K1pEz4p29oxtqTvjZyrnyAode0GXR34BRgQSfIxFvf2Kar9dXBxPTtW
;;; pXEG//ZhB+qgh39SL6POFQmCONpmJT0ugmfZ9A1SOcYPnydwe4Yhdbt/fnKXgfXu
;;; JVHU8RgFtFVg0/LgMtV7KuCFEfo6+ORHENAgdqp8vjH8EPme42caxFOMHtl3QMvv
;;; Q//jW2Dc6LN3BtYH2muSpFazh2LNZBoct52vkJWCsKN8jEwUpdZxIjN1UK7BpRr5
;;; mJgFR2RIBYTt77h0rSlwlx2qCbxumZfnCfQLNlVZMPz7Z0anEiaBm8B4bIEn+SM7
;;; e69lSeVxoGOc6xo1EZ5qPd5m2Vdw4Br1xnAWa5EBCIZhWLPZqG4mwNTRzlIC/pww
;;; o3iExSROTOMGntjDTo0q/YgNwrFeS7jOQrjYMbsW351/l7rb/bHayD5MdjlOd/9V
;;; 16pVB2L5ZH7SR4QI2z6uheUB6+E4Jwgh6lO1yO12uheIJbKvzjkSPcqJu/pbbn9C
;;; dKvJ0BAusdAXqMmwoxkUFhFY0vYLS1fiiFQj6OmpVKigYvMFlS7OZJxW5fU1ZahK
;;; v/lYhw==
;;; -----END-SIGNATURE-----