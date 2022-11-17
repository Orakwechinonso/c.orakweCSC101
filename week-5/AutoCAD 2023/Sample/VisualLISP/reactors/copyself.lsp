;;;                                                                    ;
;;;  COPYSELF.LSP                                                      ;
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
;;;       Function:  CREATE-SAME-COPY-REACTOR                          ;
;;;                                                                    ;
;;;    Description:  This function copies a reaction to all objects    ;
;;;                  contained in the argument obj-list.               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      add-object-to-data                            ;
;;;                      reaction                                      ;
;;;                      add-object-to-data                            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        obj-list = a valid list of vla-objects to be modified with  ;
;;;                   the new reaction call back function in           ;
;;;                   the reaction argument.                           ;
;;;        reaction = a call back function to a vlr reactor object.    ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (create-same-copy-reactor                                      ;
;;;         '(vla-Object1 vla-Object1 )                                ;
;;;             'myfunction)                                           ;
;;;--------------------------------------------------------------------;
(defun create-same-copy-reactor	(obj-list reaction)
  (function add-object-to-data)
  (vlr-object-reactor
    obj-list				;;owners
    obj-list				;;user data - receivers
    (list (cons :vlr-modified reaction)
	  (cons :vlr-copied (function add-object-to-data))
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  ADD-OBJECT-TO-DATA                                ;
;;;                                                                    ;
;;;    Description:  This function will be called inside               ;
;;;                  :vlr-copied reaction for this reaction arg-list   ;
;;;                  contains only one element -  entname of new       ;
;;;                  object because we can't make VLA object from      ;
;;;                  entname inside this reaction we first store       ;
;;;                  it to DATA                                        ;
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
;;;     (add-object-to-data notifier reactor arg-list)                 ;
;;;--------------------------------------------------------------------;
(defun add-object-to-data (notifier reactor arg-list)
  (vlr-data-set
    reactor
    (cons (car arg-list) (vlr-data reactor))
  )
  ;|
  ;; We need to make the reactor sensitive to
  ;: vlr-openedForModify reaction.  We will update reactors
  ;; owners and data inside update-object-in-reactor callback.
  ;; Unfortunatively this doesn't work
  (vlr-reaction-set
    reactor
    :vlr-openedForModify
    'reactor-update-object-in-reactor
  )
|;
  ;; To overcome the problem noted above, we will update the reactors' owner
  ;; and data properties during the :vlr-commandEnded callback.
  (if (null *editor-updating-reactor*)
    (setq *editor-updating-reactor* (vlr-editor-reactor))
  )
  (if (not (vlr-added-p *editor-updating-reactor*))
    (vlr-add *editor-updating-reactor*)
  )
  (vlr-data-set
    *editor-updating-reactor*
    (cons reactor (vlr-data *editor-updating-reactor*))
  )
  (vlr-reaction-set
    *editor-updating-reactor*
    :vlr-commandended
    (function editor-update-object-in-reactor)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-UPDATE-OBJECT-IN-REACTOR                  ;
;;;                                                                    ;
;;;    Description:  This function updates the reactor's object        ;
;;;                  property according to the arg-list                ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      update-object-in-reactor                      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object.                              ;
;;;         reactor = a valid vlr object reactor.                      ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:  	We need to make the reactor sensitive to       ;
;;;			vlr-openedForModify reaction.  We will update  ;
;;;			reactors owners and data inside                ;
;;;			update-object-in-reactor callback.             ;
;;;			Unfortunatively this doesn't work!             ;
;;;--------------------------------------------------------------------;
(defun reactor-update-object-in-reactor	(notifier reactor arg-list)
  (if (update-object-in-reactor reactor)
    ;; If all etity was replaced by VLA objects then
    ;; make reactor insensitive to :vlr-openedForModify reaction
    ;; not to catch this reaction any more
    (vlr-reaction-set reactor :vlr-openedformodify nil)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  EDITOR-UPDATE-OBJECT-IN-REACTOR                   ;
;;;                                                                    ;
;;;    Description:  This function updates the reactor's object        ;
;;;                  property according to the arg-list                ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      update-object-in-reactor                      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         reactor = a valid vla object reactor. Filled in by the     ;
;;; 		      calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: Note: Intended to be called as a call              ;
;;;		    back function within a reactor.                    ;
;;;		    (editor-update-object-in-reactor                   ;
;;;				vla-reactor-Object                     ;
;;;					arg-list )                     ;
;;;--------------------------------------------------------------------;
(defun editor-update-object-in-reactor (reactor arg-list / new-data)
  (foreach obj-reactor (vlr-data reactor)
    (if	(not (update-object-in-reactor obj-reactor))
      (setq new-data (cons obj-reactor new-data))
    )
  )
  (vlr-data-set reactor new-data)
  (if (null new-data)
    (vlr-remove *editor-updating-reactor*)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-OBJECT-IN-REACTOR                          ;
;;;                                                                    ;
;;;    Description:  This function updates the reactor's object        ;
;;;                  property.                                         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         reactor = a valid vla object reactor                       ;
;;;                                                                    ;
;;; Returned Value:  returns the status of not-release. If not-release ;
;;;                  is nil then the return value is T otherwise nil.  ;
;;;                                                                    ;
;;;          Usage: (update-object-in-reactor vla-reactor-Object)      ;
;;;--------------------------------------------------------------------;
(defun update-object-in-reactor	(reactor / vlaobj new-data not-release)
  ;;edit reactor on fly!
  (foreach obj (vlr-data reactor)
    (cond ((eq (type obj) 'ename)
	   (cond ;; now we can try to make VLA object from entity
		 ((setq vlaobj (vlax-ename->vla-object obj))
		  ;; to make it notify other circles add it to list of owners
		  (setq new-data (cons vlaobj new-data))
		  (vlr-owner-add reactor vlaobj)
		 )
		 (t
		  (setq	new-data    (cons obj new-data)
			not-release t
		  )
		 )
	   )
	  )
	  (t (setq new-data (cons obj new-data)))
    )
  )
  ;; to make it recieve notification from others circles
  ;; add it to the list of recievers
  (vlr-data-set reactor (reverse new-data))
  (not not-release)
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:COPYSELF-TEST                                   ;
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
;;;          Usage: (C:COPYSELF-TEST) or COPYSELF-TEST from            ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:COPYSELF-TEST (/ vla-obj reactor)
  (setq vla-obj (ADD-CIRCLE))
  (if vla-obj
    (progn
      (vla-put-color vla-obj acred)
      (setq reactor (create-same-copy-reactor
		      (list vla-obj)
		      (function reactor-make-same-radius-color)
		    )
      )
    )
  )
  reactor
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:COPYSELF-INFO                                   ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:COPYSELF-INFO) or COPYSELF-INFO from            ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:COPYSELF-INFO ()
  (terpri)
  (textscr)
  (princ
    "\nThis test demonstrates a special reactor for a circle.  You will"
  )
  (princ
    "\nbe prompted to define the center point and radius for a circle,"
  )
  (princ
    "\nwhich is initially colored red.  A reactor is bound to it."
  )
  (princ
    "\nAny subsequent circles created by COPY, MIRROR or ARRAY of the"
  )
  (princ
    "\noriginal circle, will be updated to match the radius and color"
  )
  (princ "\nof the original circle.")
  (princ "\n\nRun COPYSELF-TEST to run the demonstration.")
  (terpri)
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "COPYSELF-TEST" "COPYSELF-INFO")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBrij/NV6aRlz6MJTxNBplTnLyc7tUOY9koW9DY
;;; Uxlyo2FGlDapenDT5vWpfHNd1KYLutOsLV9W0lKIEFk4al8AM4BXystGhDYT+v94
;;; BBiVSFftLFDgTTdW+Ub0JBMvnyVmGXjvuHBxtytO5yh5vRk92h66xYQVV9h7JoXK
;;; m0r6n9pSOkaLFDdCVgrRACGRll86cOGN54zP3j9cTB32LKSIGcajxkgQdHaTVI4s
;;; cOoMyfbsL0Xm7G01RzyixseXKYjgQNaApNetlMcECkT1FQj3GWs44ZGPZMo5W+vY
;;; Fb2CYEV7Hhu1j7YPxiatKRrgYIN4z+KeZWNfQlKuR6vHk9vHTIrLobo6zi/Zwfl6
;;; uqaFkHyXt0v/52C48AvyRTUbNXVEaxv9xXu9gsGwehJtVPTdD4MqPQAUUrwdMjrn
;;; yreGMO54ZjsBrn4bWiDcG9HSXG6oew5cSqftrW8NiVGFJF/v+IaDpzDE2HS78k+0
;;; 1ns+ORxP8CnuzsrOKXkXl6qZ6w3kgszLYgfy87bsSnk/eimYK6cULzlehqCe/Ei8
;;; uD5qayi1Zx+bdUtfmS+YmPlD2sVPJxtBmYCNyjXl6e1hItIR1FNcrNJUSCH9yq1U
;;; N2FUp0sNbhjAKrqH+r61aOkWjcouh4pLXjRM6Ces5soJ8wwK8VRXV8Lb+5DSy6Hc
;;; Xgrliw==
;;; -----END-SIGNATURE-----