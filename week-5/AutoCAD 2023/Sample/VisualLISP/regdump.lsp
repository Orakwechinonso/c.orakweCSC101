;;;                                                                    ;
;;;  REGDUMP.LSP                                                       ;
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
;;;         Functions defined:                                         ;
;;;	                registry-tree-dump                             ;
;;;	                dump-registered-apps                           ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
;;;  This file demonstrates several registry-xxxx functions and how    ;
;;;  they can be used to create user defined registry functions.       ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model functions here
(if (car (atoms-family 1 '("vl-load-com"))) (vl-load-com))

;;;--------------------------------------------------------------------;
;;;       Function:  REGISTRY-TREE-DUMP                                ;
;;;                                                                    ;
;;;    Description:  This function dumps the registry contents for     ;
;;;                  a specific key.                                   ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             rkey    = Registry Key Name                            ;
;;;             indent  = indent by a string value.                    ;
;;;                       This value can be nil. If nil it             ;
;;;                       defaults to "" other this indent value is    ;
;;;                       incremented internally as required.          ;
;;;                                                                    ;
;;; Returned Value:  A consed list denoting:                           ;
;;;                  (Number-of-registry-decendants                    ;
;;;                     . Number-of-Keys)                              ;
;;;                  Note: This is used internally.                    ;
;;;                                                                    ;
;;;          Usage: (registry-tree-dump                                ;
;;;                     "HKEY_CURRENT_USER\\Software" ""               ;
;;;                     )                                              ;
;;;--------------------------------------------------------------------;
;;;
;;;
(defun registry-tree-dump (rkey indent / vs ks)
  (if (equal "\\" (substr rkey (strlen rkey)))
    (setq rkey (substr rkey 1 (1- (strlen rkey))))
    )
  (or indent (setq indent ""))

  (princ (strcat indent "Key: " rkey "\n"))
  (if (setq vs (vl-registry-descendents rkey t))	; value names
    (progn
      (princ indent)
      (princ "- values:\n")
      (foreach v (vl-sort vs '<)
	(princ indent)
	(princ (strcat
		 "  "
		 (if (equal v "") "@" v)
		 ": " ) )
	(prin1 (vl-registry-read rkey v))
	(terpri)
	) )
    (progn
      (princ indent)
      (princ "- no values\n")
      )
    )
  (if (setq ks (vl-registry-descendents rkey))		; subkey names
    (progn
      (princ indent)
      (princ "- subkeys:\n")
      (setq rkey (strcat rkey "\\")
	    indent (strcat indent "  ") )
      (foreach k (vl-sort ks '<)
	(registry-tree-dump (strcat rkey k) indent)
	) )
    (progn
      (princ indent)
      (princ "- no subkeys\n")
      )
    )
(cons (length ks) (length vs))
 )

;;;--------------------------------------------------------------------;
;;;       Function:  DUMP-REGISTERED-APPS                              ;
;;;                                                                    ;
;;;    Description:  This function dumps the registry database         ;
;;;                  subtree for every application that is registered  ;
;;;                  to the current ACAD .                             ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          registry-tree-dump                        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (dump-registered-apps)                             ;
;;;--------------------------------------------------------------------;
(defun dump-registered-apps (/ AcadApp AppNames)
  ;; find registry key for current AutoCAD version
  (setq AcadApp "HKEY_LOCAL_MACHINE\\Software\\Autodesk\\AutoCAD"
	AcadApp (strcat AcadApp "\\"
			(vl-registry-read AcadApp "CurVer") )
	AcadApp (strcat AcadApp "\\"
			(vl-registry-read AcadApp "CurVer")
			"\\Applications" ) )
  ;; get list of registered applications
  (setq AppNames (VL-SORT (vl-registry-descendents AcadApp) '<))
  
  ;; dump registry subtree for every application
  (foreach app AppNames
    (princ (strcat "\n=== " app " registry subtree dump\n"))
    (setq app (vl-registry-read (strcat AcadApp "\\" app) "REGPATH"))
    ;; app starts with "\\\\" - Visual LISP does not like this
    (setq app (substr app 3)) ;; So we remove some \\.
    (registry-tree-dump app nil)
    )
  (length AppNames)
  )

;;;--------------------------------------------------------------------;
;;;                                                                    ;
;;; Note:                                                              ;
;;;  To start this function cut and paste the function call below.     ;
;;;     (dump-registered-apps)                                         ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAQGAbjQH+43MxBC/b0ZaPrUxeqjugnIYHUhoIn
;;; SSlCdmWNYPCoMEwmxgTNwkiYpXyeNDxoFonojNyHvtYCZrpsI2tBAccOYD5BfMUf
;;; zxsI3BxuSwjc/T7DM0HGi7BS/ycyDjtZYFLguTval2zlP1e1pXGadYnlsqdH8E8u
;;; kcDdWkFPVi+YCritpN57CS4UGZWtrHnD+u29AqnqHXwTG3lZa4AOFp2wUBC+aG0P
;;; GVIqjx0gvCcYaS/R917JPqf352dS7VuVpwmbj3oBCs+N/C4xfeL+HtKsT9YmM7o1
;;; gxUfd584s9paejhFdjxN3ryDi3b9LbsFN2iVp7apwdwJBlESK4rOul8HtXbkN8U+
;;; 3Rw827wMIgTen3jsQyNDTVKolMQkZCogmwTkhbIsXI2gRAdSXvjPXSGef/RTydgh
;;; mFbC2iveOwldQSDulhkh+nZ7nMQKHCuuU6aaIpdMCaH+mLVwQOBo/qhViunSO2Tw
;;; UvvaJeCblhYLToI/hQVaufpSbeZlKq1ok2118/JQQWWN1ABQrpW5o3b/uFa0ZSZs
;;; gIsu35wtWWaGxAV95fZbcWpSWjc9nhgBo900lDSGuxgvesvh4ilErGwga5oMCK6p
;;; 0ATjUhHbfZ+/3oH/Pu2fYOBOtSZ8wE57ktzwSGYZYbtowjIB5NnUnKAIp2oWaqSA
;;; DF4dcg==
;;; -----END-SIGNATURE-----