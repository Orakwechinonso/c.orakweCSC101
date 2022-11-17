;;;
;;;  VLX_FILE.LSP
;;;
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
;;;
;;;
;;; This VLX application demonstrates that variables can be exchanged
;;; between application and document namespaces and between application
;;; namespace and session blackboard.
(vl-load-com)

(vl-doc-export 'import_objs)
(defun C:import_objs ()
  (setq	thisDoc	      (vla-get-ActiveDocument (setq app (vlax-get-acad-object)))
	docs	      (vla-get-documents app)
	thisDocName   (vla-get-name thisDoc)
	thisDocBlocks (vla-get-blocks thisDoc)
	thisMS	      (vla-get-modelspace thisDoc)
  )


  (setq exclude_docs (list thisDocName))

  ; Retrieve and increment counter from active document's namespace.
  ; Counter reflects number of times this function has been invoked.
  (setq import_objs_cntr (vl-doc-set 'mdi-vlx-cntr
					 (1+ (vl-doc-ref 'mdi-vlx-cntr))))
   

  (while (equal "Y" (progn
		      (initget 0 "Y N")
		      (getkword "\nImport a transfer set into active document? [Y|N] ")
		      )
		)
    ; prompt user to identify an open document
    (setq message (strcat "\nCurrently in drawing "
			  thisDocName
			  ".\nXREF in drawing from open document:"
		  )
    )

    (setq
      chosenDoc	   (vla-item docs
			     (choose_item docs message exclude_docs)
		   )
      chosenName   (vla-get-name chosenDoc)

      chosenFile   (strcat "trans" (itoa import_objs_cntr) chosenName)

      chosenSelSet (vla-item (vla-get-SelectionSets chosenDoc)
			     "mdi-vlx"
		   )
    )
    (terpri)
    (princ chosenFile)

    (vla-wblock chosenDoc chosenFile chosenSelSet)

    				;clear chosenSelSet for next loop pass

    (princ
      (strcat "\n" (vla-get-fullname chosenDoc) " is selected\n")
    )

    (setq exclude_docs (cons chosenName exclude_docs))

    (setq InsertPt
	   (vlax-3d-point (getpoint "\nChoose insertion point.\n"))
    )
    (setq importBlock
	   (vla-InsertBlock
	     thisMS
	     InsertPt
	     chosenfile
	     1
	     1
	     1
	     0
	   )
    )

    (vl-file-delete chosenFile)
    (vla-regen thisDoc acActiveViewport)
    (vla-ZoomAll (vlax-get-acad-object))
  )
)

(defun choose_item
       (collection msg excludeGrp / obj index keywds curName)
       ;|
  Allows user to identify an item in a collection, excluding members of "excludeGrp".
  excludeGrp is a list of names which might coincide with the names of items
  in the ActiveX "collection".  The user is prompted to select one number from a menu of acceptable
  collection items.  The index of the item is then returned.
  |;
  (if (or (not (listp excludeGrp))
					; collection not a vla-object
      )
    (quit)
  )

  (terpri)
  (princ msg)
  (terpri)

  (setq	index 0
	keywds ""
  )
  (terpri)
  (vlax-for obj	collection
    (if	(not (member (setq curName (vla-get-name obj))
		     excludeGrp
	     )
	)
      (progn
	(princ (strcat "[" (itoa index) "] " curName))
	(terpri)
	(setq keywds (strcat keywds (itoa index) " "))
      )
    )
    (setq index (1+ index))
  )

  (initget 1 keywds)
  (atoi (getkword "\nEnter number for an open :"))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBQKRs13tGnUHwG81ws4/dZhYpofdZpMmbTqBXm
;;; NCgfrP6Mk/jpOfTVIJ7KTTJ3iHFYYRYngArEZaFAGT8UaOH6c+cl+7ybgCi4eZaj
;;; 4DxzRRZl9pfKNQmPqvp9vw8RJkeHFAYLBT9dauQFqwwaTg3KBz4iJWZlS090C+vf
;;; 4qVNXjvUqrJ0A4aRSTqRR2cRKh86voHbW3aFBose+0DChyNHkoo4w0hXmjtfLYHZ
;;; Y0b7n0Ps8rSbmoY/n8xwTMUHggTFxxJlG1arNL9u82tp3OX5FoMgm8ttFWxh+qFV
;;; d8zvi4wKWNN34s4gWji2eimdjgVoCMUaRWzGTDOWkCD0tRd1kn1Tdx51pE7pHnd6
;;; Br5Xzp33Fh/IR4taox03On2sdTQtQo3J7S0X5zKPUTWOismRBJbq3eH+GOYqKQ+4
;;; aFT/d3RVdEAqzeJdDGA5XubAcofoKITeUbTtKkh8oWXSTXu3cpzVuA7OibodQx87
;;; FBc882Jt/KYujKPlyERUdO2B0rbL2maqZFpARJEzXqmWapzPYLV4MYCwoiCZWbMI
;;; kIGPiPMKNYDKoO8A9RBZMXrLBol9bF99MbXgLwZPtCf9LF9PIKuIDe43mw3BlzRG
;;; krhluJezdXv7gtIt913WuNlEpWZFGdTQjqzuVEqt6JQzhD+K4YqwSZpXRGv5Ovg2
;;; G9a/zQ==
;;; -----END-SIGNATURE-----