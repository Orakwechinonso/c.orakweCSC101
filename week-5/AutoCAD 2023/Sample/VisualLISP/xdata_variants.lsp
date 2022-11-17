;;;                                                                    ;
;;;  XDATA_VARIANTS.LSP                                                ;
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
;;;  This file shows how to use the new VARIANT and SAFEARRAY          ;
;;;  variables.                                                        ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  First of all we have to init the ActiveX interface.               ;
;;;--------------------------------------------------------------------;
(vl-load-com)


;;;--------------------------------------------------------------------;
;;;  Register an application name for the XData.                       ;
;;;--------------------------------------------------------------------;
(regapp "VLAX_SAMPLE")

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
;;;  For the ActiveX Getxxx functions, we need to define a global      ;
;;;  variable which "points" to the AutoCAD Utility object.  This      ;
;;;  variable, named *AcadApp* will be created at load time.           ;
;;;--------------------------------------------------------------------;
(setq *AcadUtility*
  (vla-get-Utility (vla-get-ActiveDocument *AcadApp*))
) ;_ end of setq

;;;--------------------------------------------------------------------;
;;;  This function builds a Lisp list from the two parameters.         ;
;;;  The first parameter is a safearray and contains shorts. These     ;
;;;  shorts are the DXF group codes.                                   ;
;;;  The second parameter is a safearray and contains variants. These  ;
;;;  variants contains the DXF values.                                 ;
;;;--------------------------------------------------------------------;
(defun GetList (DxfTypes DxfValues / LispList Counter Code VarValue Value)
  ;; Get Array bounds
  (if (/= DxfTypes nil)
    (progn
      (setq ListList nil)
      ;; Get the dimension of the safearray
      (setq lBound (vlax-safearray-get-l-bound DxfValues 1)
	    uBound (vlax-safearray-get-u-bound DxfValues 1)
	    Counter lBound)
      (while (<= Counter uBound)
	(setq Code     (vlax-safearray-get-element DxfTypes Counter)
	      VarValue (vlax-safearray-get-element DxfValues Counter)
	      Counter (1+ Counter))
	;; VarValue contains the variant, but we need the Lisp value of it
	(setq Value (vlax-variant-value VarValue))
	;; Create the list
	(setq LispList (append LispList (list (cons Code Value))))
      ) ;_ end of while
    ) ;_ end of progn
    (setq LispList nil)
  ) ;_ end of if
  LispList
) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;  This function builds two VARIANTS from the two parameters.        ;
;;;  The first parameter is a list specifying the DXF group codes, the ;
;;;  second list specifies the DXF values.                             ;
;;;  After converting the parameters into safearrays, this function    ;
;;;  creates two variants containing the arrays.                       ;
;;;--------------------------------------------------------------------;
(defun BuildArrays (DxfTypes dxfValues / ListLength Counter
		                         Code VarValue
		                         ArrayTypes ArrayValues
		    			 VarTypes VarValues Result)
  ;; Get length of the lists
  (setq ListLength (1- (length DxfTypes)))
  ;; Create the safearrays for the dxf group code and value
  (setq ArrayTypes (vlax-make-safearray vlax-vbInteger (cons 0 ListLength))
	ArrayValues (vlax-make-safearray vlax-vbVariant (cons 0 ListLength)))
  ;; Set the array elements
  (setq Counter 0)
  (while (<= Counter ListLength)
    (setq Code (nth Counter DxfTypes)
	  VarValue (vlax-make-variant (nth Counter DxfValues)))
    (vlax-safearray-put-element ArrayTypes Counter Code)
    (vlax-safearray-put-element ArrayValues Counter VarValue)
    (setq counter (1+ counter))
  ) ;_ end of while
  ;; Create the two VARIANTs
  (setq VarTypes  (vlax-make-variant ArrayTypes)
	VarValues (vlax-make-variant ArrayValues))
  ;; Create a (Lisp) list which contains the two safearrays and
  ;; return this list.
  (setq Result (list VarTypes VarValues))
  Result
) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;  This function uses the ActiveX function GetEntity to let the user ;
;;;  select an entity.                                                 ;
;;;--------------------------------------------------------------------;
(defun VlaxSelectEntity (/ VlaxEntity ent)
  (setq ent (car (entsel "\nSelect Entity: ")))
  (setq VlaxEntity (vlax-ename->vla-object ent))
  VlaxEntity
) ;_ end of defun


;;;********************************************************************;
;;;     Function: C:GetXData     The Main function                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function lets the user select an entity. Then   ;
;;;               it extracts the XData of the selected entity using   ;
;;;               AutoCAD's ActiveX interface.                         ;
;;;********************************************************************;
(defun C:GetXData (/ VlaxEntity Point Types Values xdatas)
  ;; Let the user select an entity.
  (setq VlaxEntity (VlaxSelectEntity))
  (if (/= VlaxEntity nil)
    (progn
      ;; Get the XData
      (vla-getXData VlaxEntity '"" 'Types 'Values)
      ;; Types is a safearray which contains the DXF group codes
      ;; and Values is a safearray which contains the data values.
      ;; Types contains shorts and Values contains Variants.
      ;; Let us extract the information
      (setq xdatas (GetList Types Values))
      ;; Now print the 'normal' Lisp list
      (princ "\nXData attached to the selected entity:\n")
      (print xdatas)
      (princ "\n")
    ) ;_ end of progn
  ) ;_ end of if
  (princ)				; exit quietly
) ;_ end of defun


;;;********************************************************************;
;;;     Function: C:SetXData     The Main function                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function lets the user select an entity.        ;
;;;               Then it attaches an XData string to the entity using ;
;;;               AutoCAD's ActiveX interface.                         ;
;;;********************************************************************;
(defun C:SetXData (/ VlaxEntity String DxfTypes DxfValues)
  ;; Let the user select an entity
  (setq VlaxEntity (VlaxSelectEntity))
  (if (/= VlaxEntity nil)
    (progn
      ;; Get the string to attach
      (setq String (getstring T "Enter XData string: "))
      (if (/= String nil)
	(progn
	  ;; Create two safearrays for the ActiveX method SetXData
	  (setq xdatas (BuildArrays '(1001 1000) (list "VLAX_SAMPLE" String)))
	  ;; Extract the two variants from the returned (Lisp) list
	  (setq DxfTypes (nth 0 xdatas)
		DxfValues (nth 1 xdatas))
	  ;; Set the Xdata
	  (vla-setXData VlaxEntity DxfTypes DxfValues)
          (princ "\nXData attached to the selected entity.\n")
          (princ "\nUse 'GetXData' to get the XData.")
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of progn
  ) ;_ end of if
  (princ)				; exit quietly
) ;_ end of defun


;;; Display a message to let the user know the command name
(princ "\nType SetXData to attach XData to an entity.")
(princ "\nType GetXData to show the XData attached to an entity.")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCywEYw0J9Hnq3MUBidDPz17LdgQGiyyjSqnLqb
;;; t+5RA+oz5z7aImo0A8yZ3p1Gg705h5Qfkpjgs9JNKyj9ccfQvLOxi70sgwAfHSfa
;;; sVmgrmdGksx8V00LIvw6M/MloEXNERNhSnTmBwdPWOCFgmrn/r11epJoEDl4nwcr
;;; 3CLC2SQCgfRxiVbjZeKG7cAZjvqTlqmR3AvaZUhO2q/3BUnUKy5GQYfDp7De+POW
;;; zrRp4hWBVRqqwgcFoQxlyaKnDvh8gSE5/u0XGTygqnOUFKm74fLNihawFteo9//i
;;; q3VRtP1I7fx8HGImE6E8EuovUwxbBZj4f2fT36T/yGUddws6wLc7ZrUk/3MkM/OO
;;; t3AV5q/0OIbmWrAXbZUNQdRKENsJPhfF34kCFHOMFgsK/+1Ct49i8sdVzEOg8PFd
;;; ECP+oUxn3TDqRMBfoXVLFemyIJkpf44YwoP4HWQTUoVqdU64oiSup8ygeUTAC+VU
;;; W/4WxIKYN5Squ+NhdB2buLnv6SuF7i8etLicthrWF6MAUYaI0wbwuJKs5bn/Aoz1
;;; pyT3GsLp+OqBPOgmTv23ik5DS5EIyz+NkKC7kY/nWAMWzQZxdW9wU04+ZMczWc+x
;;; /DL4D4TtkOGgnWsbK5SZn3Oo4BqyohJqz3EPVAVPlpqKQbpAKGSARGiQtmbJqbjJ
;;; nvE1vw==
;;; -----END-SIGNATURE-----