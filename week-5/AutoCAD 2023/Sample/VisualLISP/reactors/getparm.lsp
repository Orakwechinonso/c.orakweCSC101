;;;                                                                    ;
;;;  GETPARM.LSP                                                       ;
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
;;; This file contains various DCL functions which are called from     ;
;;; the external file RCTR.LSP                                         ;
;;;--------------------------------------------------------------------;
;;; Globals defined:
;;;  *GetParams-dlg-position*
;;;  *dcl-file-name*

;;;--------------------------------------------------------------------;
;;;       Function:  LOAD TIME EVALUATED                               ;
;;;                                                                    ;
;;;    Description:  Make the global variable that contains the dialog ;
;;;                  startup screen position.                          ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a string.                                         ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;--------------------------------------------------------------------;
(SETQ *GetParams-dlg-position* '(-1 -1)) ;;default startup position


;;;--------------------------------------------------------------------;
;;;       Function:  LOAD TIME EVALUATED                               ;
;;;                                                                    ;
;;;    Description:  Make the global variable that contains the dialog ;
;;;                  name to load.                                     ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a string.                                         ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;--------------------------------------------------------------------;
(if (= 3 (VL-STRING-MISMATCH "RTS" (_VLISP-VERSION) nil nil t))
  ;; in IDE, assume standard Visual Lisp directory structure
  (setq	*dcl-file-name*
	 (findfile
	   (strcat (VL-FILENAME-DIRECTORY (strcase (findfile "vl.arx")))
		   "/sample/VisualLisp/reactors/getparm.dcl"
	   )
	 )
  )
  nil
)
;; not in IDE or file not found, the default path will be used.	
(if (= nil *dcl-file-name*)
  (setq *dcl-file-name* "getparm.dcl")
  nil
)


;;;--------------------------------------------------------------------;
;;;       Function:  GETPARAMS-DLG-CALLBACK                            ;
;;;                                                                    ;
;;;    Description:  This function returns the values selected by the  ;
;;;                  user from the main dialog run-GetParams-dlg.      ;
;;;                                                                    ;
;;;                  Note: This function is not used.                  ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            bool = a value T or nil. This argument if T will save   ;
;;;                   the current dialog position upon exiting. If     ;
;;;                   nil the function will make a list from the       ;
;;;                   current value of $x and $y.                      ;
;;;            code = an integer that denotes what information needs   ;
;;;                   to be returned.                                  ;
;;;                                                                    ;
;;;                   If code value is:                                ;
;;;                    12 = Return the present value of color          ;
;;;                    20 = Return the present value of circle-number  ;
;;;                    30 = Return the present value of radius         ;
;;;                    50 = Return the present value of                ;
;;;                         *use-persistent-reactor*                   ;
;;;                                                                    ;
;;; Returned Value:   the value of the code argument.                  ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (GetParams-dlg-callback T 20)                                  ;
;;;--------------------------------------------------------------------;
(DEFUN GetParams-dlg-callback (bool code)
  (COND
	((= code 12) (SETQ color (ATOI $value)))
	((= code 20) (SETQ circle-number (ATOI $value)))
	((= code 30) (SETQ radius (ATOF $value)))
	((= code 50) (SETQ *use-persistent-reactor* 
	             (not *use-persistent-reactor*)))
  )
  (IF bool
    (SETQ *GetParams-dlg-position* (DONE_DIALOG code))
    (SETQ *GetParams-dlg-position* (LIST $x $y))
  ) ;_ end of if
  code
)

;;;--------------------------------------------------------------------;
;;;       Function:  GETPARAMS-DLG-POPUPS                              ;
;;;                                                                    ;
;;;    Description:  This function is responsible in placing default   ;
;;;                  values in various edit fields within the dialog.  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:   nothing of importance.                           ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (GetParams-dlg-popups)                           ;
;;;--------------------------------------------------------------------;
(DEFUN GetParams-dlg-popups ( / db)
  (SET_TILE "key-Color" (ITOA color))
  (SET_TILE "key-Number" (ITOA circle-number))
  (SET_TILE "key-Radius" (RTOS radius))
  (SET_TILE "key-Persistent-reactors" 
            (if *use-persistent-reactor* "1" "0"))
  (mode_tile "key-Get-radius" (if aCurve 0 1))
  (mode_tile "accept" (if (and radius aCurve) 0 1))
)

;;;--------------------------------------------------------------------;
;;;       Function:  RUN-GETPARAMS-DLG                                 ;
;;;                                                                    ;
;;;    Description:  This function returns the values selected by the  ;
;;;                  user from the main dialog run-GetParams-dlg.      ;
;;;                                                                    ;
;;;                  Required Functions: none                          ;
;;;                                                                    ;
;;;                  Required variables with values:                   ;
;;;                                                                    ;
;;;                            color         = an integer value        ;
;;;                            circle-number = an integer value        ;
;;;                            radius        = a real number           ;
;;;                                                                    ;
;;;                                                                    ;
;;;                    Note: The variables noted above                 ;
;;;                    are globalized within function                  ;
;;;                    the calling function call-GetParams-dlg         ;
;;;                    and is located in RCTR.LSP.                     ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:   none                                             ;
;;;                                                                    ;
;;; Returned Value:   0 if the user pressed the cancel button.         ;
;;;                   1 if the user pressed the ok button.             ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                  (run-GetParams-dlg)                               ;
;;;--------------------------------------------------------------------;
(DEFUN run-GetParams-dlg ( /   dcl_id	tmp	what_next )
  (IF (NULL
	(SETQ dcl_id (load_dialog *dcl-file-name*))
      )
    (alert "Unable to load DCL file")
  )
  (SETQ	what_next 1000)
  (WHILE (> what_next 1)
    (IF	(NOT (NEW_DIALOG
	       "DLG_GetParams"
	       dcl_id
	       ""
	       *GetParams-dlg-position*
	     )
	)
      (alert "Unable to create new dialog")
    )
    (GetParams-dlg-popups)
    (SETQ what_next (START_DIALOG))
    (COND
      ((= what_next 10)
       (setq aCurve (select-a-curve))
      )
      ((= what_next 40)
       (setq radius (get-radius (vlax-curve-getStartPoint aCurve)))
      )
    )
  )
  (unload_dialog dcl_id)
  what_next
)


;;; EOF

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBgINIESy9oLKci2gFtNLXObK/+rwWR9MwK7HJj
;;; fm6fGV9V2dkbbjB/aRyjs4K0LZvUn+VVcYFK+lhCnBpZ1Scf+Zly93qMzw9ucBH2
;;; QZ7QCBvKvzsN29XVnCjVJJTTtC5ZwAWkNV6lNMvfi0D1GU+9LzzMkQbyUgEwUTnq
;;; 9QkkjcHVnntavHLnf0YeXTp3AjbWyztSDisXARQP7XhegeidIJCp9OfeaGTaWmRE
;;; fvs8zqKvSyWtCPJu8FWTZlRq+2CB1x2WYaSs4m6XMnnFEUZXuUWeTbiTQ1zTM1pM
;;; nS9HUNW4hZFfNdH+V6YS0yVV/PGyd5akVNr3ShWKVWyG54dKs8zEYbfrtXYbQ1x9
;;; T85j7N63U6efnWOrBcnra7nanwfLQtxOC/MMENRti3hnNZgmZOVRylHgRKOZtY8h
;;; u4tsntKe10WaeyDzd56eC/ub4Fogg3n4ht0QpXLD6M+9GOoWMoVzestdJkPiJ9Bw
;;; R6+m8YsH4HMxRhwf8m6enO5ztORQw55uB0TYKU4KyzUgfNSbeNOzTPtqhMQqR7Rz
;;; sjwMmh+Nq6qZotwyih80eLkJXJqLEVuXWQPn0RVl9huJLH0o3axgBFiJpg/1WoN1
;;; wjaI+0GEfuW7bCRwh1NQhtEbR39DaAO3UxNqT1u7yKfJxJYsp2/f+UsGkFBvxNB0
;;; vcXHvA==
;;; -----END-SIGNATURE-----