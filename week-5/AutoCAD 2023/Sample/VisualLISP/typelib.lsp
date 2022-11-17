;;;                                                                    ;
;;;  TYPELIB.LSP                                                       ;
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
;;;  This file shows how to import type library files and how to use   ;
;;;  them.                                                             ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  First of all we have to init the ActiveX interface.               ;
;;;--------------------------------------------------------------------;
(vl-load-com)


;;;--------------------------------------------------------------------;
;;;  Import the Microsoft WinWord type library. You have to replace    ;
;;;  the path with the path to your WinWord type library file.         ;
;;;  Import the type library only if it is not already loaded.         ;
;;;--------------------------------------------------------------------;
(if (equal nil mswc-wd100Words) ; check for a WinWord constant
  (vlax-import-type-library
    :tlb-filename "c:/program files/Microsoft Office/msword8.olb"
    :methods-prefix "mswm-"
    :properties-prefix "mswp-"
    :constants-prefix "mswc-"
  ) ;_ end of vlax-import-type-library
) ;_ end of if

;;;--------------------------------------------------------------------;
;;;  After importing the type library file you can use the Apropos     ;
;;;  Window to see the added VisualLisp functions.                     ;
;;;  Go to the "View" menu, select "Apropos Windows..." and enter      ;
;;;  "mswm-" in the edit box to get a list of all WinWord methods.     ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the Model Space portion of the active drawing.  This  ;
;;;  variable, named *ModelSpace* will be created at load time.        ;
;;;--------------------------------------------------------------------;
(setq *ModelSpace*
  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
) ;_ end of setq

;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the AutoCAD application object.  This variable, named ;
;;;  *AcadApp* will be created at load time.                           ;
;;;--------------------------------------------------------------------;
(setq *AcadApp*
  (vlax-get-acad-object)
) ;_ end of setq


;;;--------------------------------------------------------------------;
;;;  This is the main function. It will iterate the model space and    ;
;;;  collect all Text entities. Then it gets the text strings and adds ;
;;;  them to a newly created Microsoft WinWord document.               ;
;;;--------------------------------------------------------------------;
(defun c:ExtractText ( / msw docs doc ent pgs pg range
		         text varTextpos arrayTextpos textinfo)
  ; Get the Microsoft WinWord application object
  (setq msw (vlax-get-object "Word.Application.8"))
  (if (equal nil msw)
    (progn
      ; WinWord is not running. Start it.
      (setq msw (vlax-create-object "Word.Application.8"))
      (vla-put-visible msw 1)
    )
  )
  (if (/= nil msw)
    (progn
      ;; Get the WinWord's document collection object.
      ;; The Application object of WinWord is accessed by the
      ;; vla-get-xxx functions. For some ActiveX properties and methods
      ;; there is no generated VisualLisp function. In this case you have
      ;; to use the vlax-get-property / vlax-put-property and
      ;; vlax-invoke-method functions.
      ;; Example: For the CommandBars property of the WinWord application
      ;; object there is no VisualLisp function, but you can get this
      ;; object the following way:
      ;; (setq ComBars (vlax-get-property msw "CommandBars"))
      (setq docs (vla-get-documents msw))
      ; Add a new document
      (setq doc (mswm-add docs))
      ; Get the paragraphs of the document (to do some formatting)
      (setq pgs (mswp-get-paragraphs doc))
      ; Now iterate the AutoCAD model space and export
      ; every text entity to WinWord.
      (vlax-for ent *ModelSpace*
	(if (equal (vla-get-ObjectName ent) "AcDbText")
	  (progn
	    ; Get some information from the text entity
	    (setq text (vla-get-TextString ent)
		  textpos (vla-get-InsertionPoint ent)
		  arrayTextpos (vlax-variant-value textpos)
		  textinfo (strcat
			     (rtos (vlax-safearray-get-element arrayTextpos 0) 2 2)
			     ", "
			     (rtos (vlax-safearray-get-element arrayTextpos 1) 2 2)
			     ", "
			     (rtos (vlax-safearray-get-element arrayTextpos 2) 2 2)
			   )
            ) ;_ end of setq
	    ; Print some info (with formatting)
	    ; 1) Get the last paragraph
	    (setq pg (mswp-get-last pgs))
	    ; 2) Get a range object
	    (setq range (mswp-get-range pg))
	    ; 3) Do some formatting
	    (mswp-put-bold range 1)
	    (mswp-put-underline range mswc-wdUnderlineSingle)
	    ; 4) Show the info text
	    (mswm-InsertAfter range (strcat "AcDbText at position " textinfo "\n"))
	    ; Now show the text string (from the ACAD text entity)
	    (setq pg (mswp-get-last pgs))
	    (setq range (mswp-get-range pg))
	    (mswp-put-bold range 0)
	    (mswp-put-underline range mswc-wdUnderlineNone)
	    (mswm-InsertAfter range (strcat text "\n\n"))
	  ) ;_ end of progn
	) ;_ end of if AcDbText
      ) ;_ end of vlax-for
    ) ;_ end of progn
    (princ "\nNo Microsoft WinWord found.\n")
  ) ;_ end of if (/= nil msw)
  (princ)
)


;;; Display a message to let the user know the command name
(princ "\nType ExtractText to export all text entities to Microsoft WinWord.\n")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgA4xC2CWVnTv3XONe2sknVmdZ07gNo/IEh1FjE/
;;; DLfs8mZLiKMmoP9F8xN6+E031Q9Lti3T/qT3a8HO3Zu0v1QLSEF23NDwVUetxXxp
;;; mjQG7nzNYjuJxCiz3563d4y/waxM0lXcBdfHJneIAD5Vt13uqEDHIFBLlIP9eQoP
;;; AWqU1GZuJj+kw9X9eyLuyff+0rT/J/AMEF7V/9paKH8gKp1HYVbO35DgVvsuUK3H
;;; 4IGkZFXvOJc1SF1POwR4SGykAA1/tor6vys9Sy4sH2K4YSTZ+lRzWWbnu6VF4+SF
;;; NXakh6i1ZM93kc+WL1b0MrD5YjA6nuYf6EGCHCSBnkdTBHiZQRGwM9NtTePjEQLu
;;; d08Qxxr0PSPA5i2qFP6377EWQsLh+5qmhl0lF/1PQ78+uZDMlNmYffmzscRz4XAn
;;; g6NWu1zTIM7nUnKajbEUVsA8vFYYrF5GJjnpSx1RNJjK+RNAG4hI3qfclUo+lLx1
;;; jqHYwk6uYAkwYyJse+I4ZGy2xnnmWdXvCpPha4B75ou244ESCh2z5i0o3oN/R0VJ
;;; FSK5pFXlV3TNbxng4+T/mwM0kvom5dZPsTS5+J1Ch7Hw31+NGkQw8II9RT0Wx3kJ
;;; TcgSHaE+m3ExMiR+hWp3T4CZgE5JJcs2xdR8W/70CCZhNQxiVQXK1Hym9ZHiG4kM
;;; H12FrQ==
;;; -----END-SIGNATURE-----