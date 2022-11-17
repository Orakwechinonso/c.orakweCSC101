;;;                                                                    ;
;;;  TIMESTAMP.LSP                                                     ;
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
;;; General Note:                                                      ;
;;;  This exercise demonstrates the ability to create a command using  ;
;;;  the functions vlax-add-cmd and vlax-reg-app.                      ;
;;;  With this exercise the novice can create commands without the use ;
;;;  of prefixing a function with a c: .                               ;
;;;  No demand-loading commands are associated with this exercise.     ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
;;;  This file demonstrates adding an automatic plot time-stamp.       ;
;;;  By attaching a reactor to AutoCAD's internal PLOT command, a      ;
;;;  drawing can be time-stamped every time it is plotted.  In this    ;
;;;  example, a TEXT entity will be created at the 0,0,0 coordinate    ;
;;;  and will show the time-stamp text.                                ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model and reactor functions here
(vl-load-com)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-TIMESTAMP                                  ;
;;;                                                                    ;
;;;    Description:  Creates a text object with the plot date.         ;
;;;                  Used as a portion of a call back procedure within ;
;;;                  the timestamp-callback reactor function.          ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A Vla Text Object                                 ;
;;;                                                                    ;
;;;          Usage: (create-timestamp)                                 ;
;;;--------------------------------------------------------------------;
(defun create-timestamp	(/ acadapp acaddoc mspace)
  (setq	acadapp	(vlax-get-acad-object)
	acaddoc	(vla-get-ActiveDocument acadapp)
	mspace	(vla-get-ModelSpace acaddoc)
  )
;;; Add our TimeStamp Text here
  (setq	TextObj	(vla-addText
		  mspace
		  "Plot date & time"
		  (vlax-3d-point '(0.0 0.0 0.0))
		  2.5
		)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-TIMESTAMP                                  ;
;;;                                                                    ;
;;;    Description:  Creates a text object with the plot date.         ;
;;;                  Used as a portion of a call back procedure within ;
;;;                  the timestamp-callback reactor function.          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      timestamp-object = A vla text object.                         ;
;;;                                                                    ;
;;; Returned Value:  The updated timestamp-object as a vla text object ;
;;;                                                                    ;
;;;          Usage: (update-timestamp vlaTextObject)                   ;
;;;--------------------------------------------------------------------;
(defun update-timestamp	(timestamp-object / now)
  (setq now (menucmd "M=$(edtime,$(getvar,date),D MON YY - HH:MM)"))
  (cond	((or (not timestamp-object)
	     (vlax-erased-p timestamp-object)
	 )
	 (setq timestamp-object (create-timestamp))
	)
  )
  (if (vlax-write-enabled-p timestamp-object)
    (vla-put-TextString timestamp-object now)
  )
  timestamp-object
)

;;;--------------------------------------------------------------------;
;;;       Function:  TIMESTAMP-CALLBACK                                ;
;;;                                                                    ;
;;;    Description:  Function which is invoked when the plot command   ;
;;;                  is issued.                                        ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          retrieve-timestamp                        ;
;;;                          update-timestamp                          ;
;;;                          save-timestamp                            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;                                                                    ;
;;;      calling-reactor = A vlr reactor object which is Automatically ;
;;;                        passed to this function from within         ;
;;;                        Visual LISP                                 ;
;;;                        when the PLOT command is issued.            ;
;;;                                                                    ;
;;;         command-list = A list of uppercase strings which is        ;
;;;                        Automatically passed to this function from  ;
;;;                        within Visual LISP and contains a list of   ;
;;;                        commands as a list.                         ;
;;;                        when the PLOT command is issued.            ;
;;;                                                                    ;
;;; Returned Value:  The updated timestamp-object as a vla text object ;
;;;                                                                    ;
;;;          Usage: (update-timestamp vlaTextObject)                   ;
;;;--------------------------------------------------------------------;
(defun timestamp-callback (calling-reactor command-list / time-stamp)
  ;; Debuging princ's here
  ;;(princ calling-reactor)
  ;;(princ command-list)
  (cond	((member "PLOT" command-list)
	 (setq time-stamp (retrieve-timestamp)
	       time-stamp (update-timestamp time-stamp)
	 )
	 (save-timestamp time-stamp)
	)
  )
  (princ)
)

;;;--------------------------------------------------------------------;
;;;       Function:  SAVE-TIMESTAMP                                    ;
;;;                                                                    ;
;;;    Description:  Saves user defined data associated with the custom;
;;;                  timestamp text object.                            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      timestamp-object = A vla text object.                         ;
;;;                                                                    ;
;;; Returned Value:  a vla text object                                 ;
;;;                                                                    ;
;;;          Usage: (save-timestamp vlaTextObject)                     ;
;;;--------------------------------------------------------------------;
(defun save-timestamp (timestamp-object)
  (vlax-ldata-put "TIME-STAMP" "TEXT-OBJECT" timestamp-object)
)

;;;--------------------------------------------------------------------;
;;;       Function:  RETRIEVE-TIMESTAMP                                ;
;;;                                                                    ;
;;;    Description:  Retrives the defined data associated with the     ;
;;;                  custom timestamp text object.                     ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a vla text object                                 ;
;;;                                                                    ;
;;;          Usage: (retrieve-timestamp)                               ;
;;;--------------------------------------------------------------------;
(defun retrieve-timestamp ()
  (vlax-ldata-get "TIME-STAMP" "TEXT-OBJECT")
)

;;;--------------------------------------------------------------------;
;;;       Function:  REGISTER-TIMESTAMP                                ;
;;;                                                                    ;
;;;    Description:  Registers time stamp as a valid ACAD command      ;
;;;                  without the need to define a c: function.         ;
;;;                  Once REGISTER-TIMESTAMP is registered with ACAD   ;
;;;                  It can be used without having to explicitly load  ;
;;;                  during an ACAD startup. This function also        ;
;;;                  guarantees the reactor is present and enabled.    ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          Initialize-TimeStamp-Reactor              ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a user defined function                           ;
;;;                  such as: #<USUBR @0285f6f8 REGISTER-TIMESTAMP>    ;
;;;                                                                    ;
;;;          Usage: (register-timestamp)                               ;
;;;--------------------------------------------------------------------;
(defun register-timestamp ()
  (princ "\nRegistering: TimeStamp ")

;;; incase the reactor was removed
  (Initialize-TimeStamp-Reactor)
  (princ)
)

;;;--------------------------------------------------------------------;
;;;       Function:  UNREGISTER-TIMESTAMP                              ;
;;;                                                                    ;
;;;    Description:  Un-registers the time stamp as a valid ACAD       ;
;;;                  command. In this case the user would have to      ;
;;;                  explicidly call the UNREGISTER-TIMESTAMP command. ;
;;;                  This function also disables all reactors          ;
;;;                  associated with the timestamp object.             ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a user defined function                           ;
;;;                  such as: #<USUBR @0285f6f8 REGISTER-TIMESTAMP>    ;
;;;                                                                    ;
;;;          Usage: (unregister-timestamp)                             ;
;;;--------------------------------------------------------------------;
;;; This function when called will change the timestamp
;;; from demand loading when acad starts.
;;; 
(defun unregister-timestamp ()
  (princ "Command has been Unregistered\n")

;;; Remove the reactors which have a value of "TimeStamp Reactor" as
;;; part of their data. We do this just in case there is more than one.
  (if (EditorReactorList)
    (foreach EditorReactor (EditorReactorList)
      (if (eq (type EditorReactor) 'VLR-Editor-Reactor)
	(progn

	  (princ (vlr-data EditorReactor))
	  (if (= "TimeStamp Reactor" (car (vlr-data EditorReactor)))
	    (progn
	      (princ "\nRemoving: ")
	      (princ EditorReactor)
	      (vlr-remove EditorReactor)
	    )
	  )
	)				;end progn
      )
    )
    (princ "\nNo EditorReactors in Drawing !")
  )
  (princ)
)

;;;--------------------------------------------------------------------;
;;;       Function:  EDITORREACTORLIST                                 ;
;;;                                                                    ;
;;;    Description:  This function scans all editor reactors.          ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  Returns a list of all editor reactors.            ;
;;;                  Such as:                                          ;
;;;                  (#<VLR-Editor-reactor> #<VLR-Editor-reactor>)     ;
;;;                                                                    ;
;;;          Usage: (editorReactorList)                                ;
;;;--------------------------------------------------------------------;
;;; Returns a list of all editor reactors
;;; 
(defun editorReactorList ()
  (car (vlr-reactors :VLR-Editor-Reactor))
)

;;;--------------------------------------------------------------------;
;;;       Function:  REMOVE-TIMESTAMP-COMMANDS                         ;
;;;                                                                    ;
;;;    Description:  This function removes all time stamp command      ;
;;;                  created with vlax-add-cmd.                        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  Returns a list consisting of two elements. The    ;
;;;                  first if present and T denotes the command        ;
;;;                  "Register-TimeStamp" was removed succesfully. Nil ;
;;;                  is returned if the command was not present. The   ;
;;;                  second if present and T denotes the command       ;
;;;                  "UnRegister-TimeStamp" was removed succesfully.   ;
;;;                  Nil is returned if the command was not present.   ;
;;;                  Such as:                                          ;
;;;                  (T T)                                             ;
;;;                                                                    ;
;;;          Usage: (Remove-TimeStamp-Commands)                        ;
;;;--------------------------------------------------------------------;
(defun Remove-TimeStamp-Commands ()
  (list
    (vlax-remove-cmd "Register-TimeStamp")
    (vlax-remove-cmd "UnRegister-TimeStamp")
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  INITIALIZE-TIMESTAMP-REACTOR                      ;
;;;                                                                    ;
;;;    Description:  This function creates a TimeStamp reactor which   ;
;;;                  called when an ACAD command has been invoked.     ;
;;;                  Furthermore, This function places a text value of ;
;;;                  "TimeStamp Reactor" within the reactor data to    ;
;;;                  distinguish this editor reactor.                  ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          timestamp-callback                        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  Returns a vlr reactor object.                     ;
;;;                  Such as:                                          ;
;;;                  #<VLR-Editor-reactor>                             ;
;;;                                                                    ;
;;;          Usage: (Initialize-TimeStamp-Reactor)                     ;
;;;--------------------------------------------------------------------;
;;; Initializes the reactor
(defun Initialize-TimeStamp-Reactor ()
  (vlr-Editor-reactor
    (list "TimeStamp Reactor")		; identify this as a timestamp reactor
    '((:vlr-commandEnded . timestamp-callback))
  )
)
;;; ----------------------------------------------------------------------
;;; Description and usage of this function:
;;;
;;; Events to be triggered upon Loading...

;;; 1. 	Just in case the commands "Register-TimeStamp" and
;;; 	"UnRegister-TimeStamp" are defined lets remove them.
;;; 	If a particular command does not exist the call to vlax-remove-cmd
;;; 	will return nil otherwise it returns T if the functions exist. 

(setq removeResult (Remove-TimeStamp-Commands))


;;; 2.	This will be evaluated at load time also.
;;; 	Place the register timestamp command name.
;;; 	In this fashion you will have defined a new
;;; 	command called "Register-TimeStamp"
;;; 	almost equivalent to "C:Register-TimeStamp"

(vlax-add-cmd
  "Register-TimeStamp"
  ;; command name
  (function register-timestamp)
  ;; function to excecute
)

;;; 3.	This will be evaluated at load time also.
;;; 	Place the unregister timestamp command name.
;;; 	In this fashion you will have defined a new
;;; 	command called "UnRegister-TimeStamp"
;;; 	almost equivalent to "C:UnRegister-TimeStamp"
(vlax-add-cmd
  "UnRegister-TimeStamp"
  ;; command nanme
  (function unregister-timestamp)
  ;; function to excecute
)


;;; 4.	This will be evaluated at load time also.
;;; 	Print a command usage for the user
(progn
  (princ
    "\nTime Stamp Loaded:\nTo start use Register-TimeStamp. \nTo disable use UnRegister-TimeStamp."
  )
  (princ)
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDI54v0w5f1cfVr5Xm2kic0BZuckGBWVLtoB5Tg
;;; 2D0IcWv136FOMzl26P1VonD3hOJF+ZzHk7TgRS2Ja50v7ZGhtbKludIjLgosZOHR
;;; VbCMVGMIkWgovwuzoNLsrOQKnGghdkFP0RTYAdYatHoQzJlwblgGbi0FMnMR0ZuC
;;; hLb6GXvzb8umBxvdsWGilaygThyp46+UY4jyEw7Fw6ypibSeiT3uNAGUeibJVK3X
;;; EZAaOHeHWErqY1B61Q+PXPmCHU3ITmfHsKjHdhIEsLCen0YbMEKgP1iDACFCPsB6
;;; 5EevTemezyvFL0Crey0EG1nDjkFzRpA12j1/xqXMfZu10S/ARG6kavFHS9RyuxCl
;;; JhjiN7C51VjoSL9NIF0cuIzYBVTB2o2APZkqkhq4XFSzR8Dca2Z1OeFqG+sMWUse
;;; zyBlMfwb13HZ1kHLeGTwt1jPY0L5nQGPh2vN9POu4KMeD5pBuf9MILJORLqR9NZN
;;; K5ZIuSpi//ilS3Lx4IW6cv6TPBY4jzJgQX8WQAJYHe/e3qd+dLRonJQDvkewOO2z
;;; fpSfwIObfwDPK99wjyDX6DxT55a4X4vOQu0tI1bAWLNgNLd9nituStfimDg5nJ7M
;;; My7wr52JnsJ7gp7fyRJvZldJ4rV6qiDEPNpzCSO4uqk0P+kAl/Ustn7/WgHtlpnH
;;; 1hKdHg==
;;; -----END-SIGNATURE-----