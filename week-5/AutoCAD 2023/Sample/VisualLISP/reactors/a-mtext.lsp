;;;                                                                    ;
;;;  A-MTEXT.LSP                                                       ;
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
;;;  This file defines two C: commands: C:A-MTEXT-TEST and             ;
;;;  C:A-MTEXT-INFO.  The C:A-MTEXT-TEST command display using         ;
;;;  reactors to link a CIRCLE entity with MTEXT.  When the circle     ;
;;;  radius changes, the MTEXT is modified to display the new radius.  ;
;;;  Conversely, when the MTEXT radius value changes, the radius of    ;
;;;  the circle will be modified and the entity updated.               ;
;;;                                                                    ;
;;;  For a description of the entire project and a listing of the      ;
;;;  AutoCAD commands defined within it, see the source code file      ;
;;;  REAC-TST.PRJ.                                                     ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-MTEX                                       ;
;;;                                                                    ;
;;;    Description:  This function creates an Mtext object.            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;       inspoint = a valid vla object reactor.                       ;
;;;          width = a real number denoting the width of the text.     ;
;;;           text = A string value.                                   ;
;;;                                                                    ;
;;; Returned Value:  returns the status of not-release. If not-release ;
;;;                  is nil then the return value is T otherwise nil.  ;
;;;                                                                    ;
;;;          Usage: (create-mtex '(0 0 0 ) 2.0 "Hello")                ;
;;;--------------------------------------------------------------------;
(defun create-mtex
       (inspoint width text / acadApp acadDoc acadMode vla-mtext)
  (if (not (= (type text) 'STR))
    (setq text "TEXT")
  )
  (setq	acadApp	  (vlax-get-acad-object)
	acadDoc	  (vla-get-activedocument acadApp)
	acadModel (vla-get-modelspace acadDoc)
	vla-mtext (vla-addmtext acadModel inspoint width text)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-MTEX-FOR-CIRCLE                            ;
;;;                                                                    ;
;;;    Description:  This function creates a Circle object.            ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          set-mtext-params-for-circle               ;
;;;                          create-mtex                               ;
;;;                          update-parameter                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;     vla-circle = a valid vla circle object.                        ;
;;;                                                                    ;
;;; Returned Value:  returns a vla mtext object.                       ;
;;;                                                                    ;
;;;          Usage: (create-mtex-for-circle vla-circle-Object)         ;
;;;--------------------------------------------------------------------;
(defun create-mtex-for-circle
       (vla-circle / inspoint width text vla-mtext)
  (set-mtext-params-for-circle vla-circle)
  (setq vla-mtext (create-mtex inspoint width text))
  (update-parameter vla-mtext "height" height)
  vla-mtext
)

;;;--------------------------------------------------------------------;
;;;       Function:  SET-MTEXT-PARAMS-FOR-CIRCLE                       ;
;;;                                                                    ;
;;;    Description:  This function creates various global values.      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;     vla-circle = a valid vla circle object.                        ;
;;;                                                                    ;
;;; Returned Value:  returns the width                                 ;
;;;                                                                    ;
;;;          Usage: (set-mtext-params-for-circle vla-circle-Object)    ;
;;;--------------------------------------------------------------------;
(defun set-mtext-params-for-circle (vla-circle)
  (if (and (= (type vla-circle) 'VLA-OBJECT)
	   (vlax-read-enabled-p vla-circle)
      )
    (setq inspoint (vla-get-center vla-circle)
	  width	   (vla-get-radius vla-circle)
	  text	   (rtos width)
	  height   (/ width 3)
	  width	   (/ width 4)
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  SET-CIRCLE-PARAMS-FOR-MTEXT                       ;
;;;                                                                    ;
;;;    Description:  This function creates various global values.      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      vla-mtext = a valid vla mtext object.                         ;
;;;                                                                    ;
;;; Returned Value:  returns the center                                ;
;;;                                                                    ;
;;;          Usage: (set-circle-params-for-mtext vla-mtext=Object)     ;
;;;--------------------------------------------------------------------;
(defun set-circle-params-for-mtext (vla-mtext)
  (if (and (= (type vla-mtext) 'VLA-OBJECT)
	   (vlax-read-enabled-p vla-mtext)
      )
    (setq rad	 (atof (vla-get-textstring vla-mtext))
	  center (vla-get-insertionpoint vla-mtext)
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-PARAMETER                                  ;
;;;                                                                    ;
;;;    Description:  This function updates a value to a parameter.     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        vla-obj = a valid vla object.                               ;
;;;      par-name  = a string denoting a parameter name.               ;
;;;      par-value = a value denoting the intended value for par-name  ;
;;;                                                                    ;
;;; Returned Value:  returns a vla object                              ;
;;;                                                                    ;
;;;          Usage: Where height is 4.0                                ;
;;;                 (update-parameter vla-mtext "height" height)       ;
;;;--------------------------------------------------------------------;
(defun update-parameter	(vla-obj par-name par-value)

(if (eq (type par-value) 'VARIANT)
   (if (> (vlax-variant-type par-value) 8192)
     (setq par-value (vlax-safearray->list (vlax-variant-value par-value)))
   )
 )
(if (and (= (type vla-obj) 'VLA-OBJECT)
	   (vlax-write-enabled-p vla-obj)
	   (not (equal (vlax-get vla-obj par-name) par-value))
      )
    (vlax-put vla-obj par-name par-value)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-MTEX-FOR-CIRCLE                            ;
;;;                                                                    ;
;;;    Description:  This function is called from the reactor call     ;
;;;                  back function reactor-circle->mtext.              ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          set-mtext-params-for-circle               ;
;;;                          update-parameter                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vla-mtext  = a valid vla mtext  object.                    ;
;;;         vla-circle = a valid vla circle object.                    ;
;;;                                                                    ;
;;; Returned Value:  Returns the text string value for                 ;
;;;                  the mtext object                                  ;
;;;                                                                    ;
;;;          Usage: (update-mtex-for-circle                            ;
;;;                              vla-circle-Object                     ;
;;;                              vla-mtext-Object                      ;
;;;                                 )                                  ;
;;;--------------------------------------------------------------------;
(defun update-mtex-for-circle
       (vla-mtext vla-circle / inspoint width height text)
  (if (set-mtext-params-for-circle vla-circle)
    (progn
      (update-parameter vla-mtext "InsertionPoint" inspoint)
      (update-parameter vla-mtext "width" width)
      (update-parameter vla-mtext "height" height)
      (update-parameter vla-mtext "TextString" text)
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-CIRCLE-FOR-MTEX                            ;
;;;                                                                    ;
;;;    Description:  This function is called from the reactor call     ;
;;;                  back function reactor-mtext->circle.              ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          set-circle-params-for-mtext               ;
;;;                          update-parameter                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;     vla-mtext  = a valid vla mtext  object.                        ;
;;;     vla-circle = a valid vla circle object.                        ;
;;;                                                                    ;
;;; Returned Value:  returns the center of the circle.                 ;
;;;                                                                    ;
;;;          Usage: (update-circle-for-mtex                            ;
;;;                              vla-circle-Object                     ;
;;;                              vla-mtext-Object                      ;
;;;                                 )                                  ;
;;;--------------------------------------------------------------------;
(defun update-circle-for-mtex (vla-circle vla-mtext / rad center)
  (if (set-circle-params-for-mtext vla-mtext)
    (progn
      (if (and rad (> rad 0))
	(update-parameter vla-circle "Radius" rad)
      )
      (update-parameter vla-circle "Center" center)
    )
  )
)

;;;--------------------------------------------------------------------;
;;; reactors                                                           ;
;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-CIRCLE->MTEXT                             ;
;;;                                                                    ;
;;;    Description:  This function will be called during a             ;
;;;                  :vlr-modified event.                              ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          update-mtex-for-circle                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;--------------------------------------------------------------------;
(defun reactor-circle->mtext (notifier reactor arg-list)
  (update-mtex-for-circle (vlr-data reactor) notifier)
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-MTEXT->CIRCLE                             ;
;;;                                                                    ;
;;;    Description:  This function will be called during a             ;
;;;                  :vlr-modified event.                              ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          update-circle-for-mtex                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;--------------------------------------------------------------------;
(defun reactor-mtext->circle (notifier reactor arg-list)
  (update-circle-for-mtex (vlr-data reactor) notifier)
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:A-MTEXT-TEST                                    ;
;;;                                                                    ;
;;;    Description:  This function aids in the creation of a circle    ;
;;;                  object and an Mtext object. Both are linked to    ;
;;;                  one another. If you move, reduce or increase the  ;
;;;                  circle the mtext object updates. If you move the  ;
;;;                  mtext object the circle moves.  The text value    ;
;;;                  displayed is the radius value of the circle.      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    reactor-circle->mtext                           ;
;;;                    reactor-mtext->circle                           ;
;;;                    create-mtex-for-circle                          ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A list of two valid reactor objects. The first is ;
;;;                  the circle reactor the sencod the mtext reactor.  ;
;;;                   (#<VLR-Object-reactor> #<VLR-Object-reactor>)    ;
;;;                                                                    ;
;;;          Usage: (C:A-MTEXT-TEST) or C:A-MTEXT-TEST from            ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:A-MTEXT-TEST (/ vla-circle vla-mTxT r1 r2)
  (function reactor-circle->mtext)
  (function reactor-mtext->circle)
  (setq vla-circle (add-circle))
  (if vla-circle
    (progn
      (vla-put-color vla-circle acred)
      (setq vla-mTxT (create-mtex-for-circle vla-circle)
	    r1	     (vlr-object-reactor
		       (list vla-circle)
		       vla-mTxT
		       '((:vlr-modified . reactor-circle->mtext))
		     )
	    r2	     (vlr-object-reactor
		       (list vla-mTxT)
		       vla-circle
		       '((:vlr-modified . reactor-mtext->circle))
		     )
      )
      (list r1 r2)
    )
  )
(princ)
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:A-MTEXT-INFO                                    ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:A-MTEXT-INFO ) or A-MTEXT-INFO from             ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:A-MTEXT-INFO ()
  (terpri)
  (princ
    "\nThis test binds a circle (colored red) with MTEXT."
  )
  (princ
    "\nYou will be asked to select center and radius of the circle."
  )
  (princ
    "\nWhen the circle radius changes, the MTEXT also changes."
  )
  (princ
    "\nAnd visa versa, when you change the number value of the MTEXT"
  )
  (princ "\nthe radius will be changed.")
  (princ "\n\nRun A-MTEXT-TEST command to test.")
  (terpri)
  (princ)
)


;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "A-MTEXT-TEST" "A-MTEXT-INFO")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAtUz30TwxyYclaKOLne/dQmnz0JAYUMbsV/jeH
;;; zM7dG2m5fyhwFYh4EHgVxnIJ+M0rKrSyBW7xIjZ6gpD5eCqCb8XusaEB4+PJtFw0
;;; e3TvVQazJP3UvmHRxaptR1hHYwj7PhMvwBoDKWbfRH/94a8MBVJc4SlvVNTLozNS
;;; Hf5QQyk2Dk0kUrUxeN1wLFN/3SO7AcubObh3MA4BtsEKLgeERDb9qeTafhwQL8wN
;;; 80GWpFUt0ZAOy8yA5WDFWMU7RYKgxJc7hFSfzusgri6c5X22Q5/Ew0qD45Hzu3dt
;;; fLJfNpSt1lM1ZXit2ZQRFmgF+7a4fSohGu3jMixLGpirCVYmJsioE/Jk1C4kiMyP
;;; x/IrIsVLQSt5UIoYWFeuJe8fdMH+hr+Ef8JR+2lDyyxsuepIQLYPsZ+j2IH2N2Xe
;;; hCI61YALGOtCPVHtwOarRoKKd195XtsPpY2g5yXjqAtW/FtLwRBGMaxqmgcnsrah
;;; KibPRdasaeTw++rNXfUa7Ka4jPLt/S5cgLU66MJVCK0YrHnLbplZ3WDj+tDaAnby
;;; 4YGlubQk4pG/grK+cblz+JBRJa1y6Ye4oePqF7pq6hdStkzLajFnylO2a5Y9sgej
;;; SjPQUWynjPys9o3NF7S1Fptk1+yXD6Wnpz6hZi/mlV300lJE9796FUyEjsXoafe7
;;; SNZegw==
;;; -----END-SIGNATURE-----