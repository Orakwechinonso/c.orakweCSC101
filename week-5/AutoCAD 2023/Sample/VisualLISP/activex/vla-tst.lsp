;;;                                                                    ;
;;;  VLA-TST.LSP                                                       ;
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

;;; Load the AutoCAD 2000 COM object model functions here
(vl-load-com)

;;;--------------------------------------------------------------------;
;;; General Note: The command function c:vla-tst requires a function   ;
;;;               named get-utime located in file al-tst.lsp           ;
;;; ******  THIS FILE IS A MEMBER OF THE VLA-TST PROJECT.  ********    ;
;;;--------------------------------------------------------------------;
;;;  This file demonstrates adding 2000 circles using ActiveX          ;
;;;  Automation. Each circle's visible property is then modified       ;
;;;  to visible and then invisible. Then each circle is erased         ;
;;;  individually. An elapsed timer is displayed after the creation    ;
;;;  and deletion of the circles created.                              ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  VLA-TST                                           ;
;;;                                                                    ;
;;;    Description:  This function creates 2000 circles with           ;
;;;                  equidistant offsets using ActiveX Automation.     ;
;;;                  Each circle's visible property is then modified   ;
;;;                  to visible and then invisible. Then each circle   ;
;;;                  is erased individually.                           ;
;;;                                                                    ;
;;;      Arguments:  None                                              ;
;;;                                                                    ;
;;; Returned Value:  last erased vla object                            ;
;;;                                                                    ;
;;;          Usage: (vla-tst)                                          ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun vvla-tst (/ acadApp acadDoc mSpace i plObj offs
		nPoint lwh2 lwh j saPntLst plen pntlist varPntLst ent
	       )
  (Setq	acadApp	    (vlax-get-acad-object)
	acadDoc	    (vla-get-ActiveDocument acadApp)
	mSpace	    (vla-get-ModelSpace acadDoc)
	offs	    (car (getvar "snapunit"))
  )
(setq lwh2 (/ (setq lwh 5.0) 2.0))

  (princ "Creating 2000 LWPolylines.\n")

  ;; Create 2000 Polylines in Model space
  (setq i 0)
   (setq saPntLst (vlax-make-safearray vlax-vbDouble '(0 . 7)))
   (while (< i 2000)
   (setq plen  (length
    (setq pntlist
	   (list (* -1.0 lwh2) (* -1.0 lwh2)
                  lwh2 (* -1.0 lwh2)
                  lwh2 lwh2
                 (* -1.0 lwh2) lwh2
           )
   )
   ))
   (setq j 0)
   (while (< j plen)
     (vlax-safearray-put-element saPntLst j (nth j pntlist))
     (setq j (1+ j))
   )
   (setq varPntLst (vlax-make-variant saPntLst (logior vlax-vbArray vlax-vbDouble)))
     ;; creates an LightWeightPolyline object in model space
    (setq plObj (vla-AddLightWeightPolyline mSpace varPntLst))
    (vla-Put-Closed plObj acTrue)
    (vla-Update plObj)
    (setq lwh2 (/ (setq lwh (+ lwh offs)) 2.0))
    (setq i (1+ i))
 )

  (princ "Changing 2000 LWPolylines to Red Color.\n")

  ;; Change color
  (vlax-for ent mSpace
    (vla-put-Color ent 1)
     (vla-Update ent)
  )

    (princ "Erasing 2000 LWPolylines.\n")

  ;; Erase all objects
  (vlax-for ent mSpace
    (vla-Erase ent)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:VLA-TST                                         ;
;;;                                                                    ;
;;;    Description:  This keeps track of the elapsed time from the     ;
;;;                  creation of 2000 circles to their erasure.        ;
;;;                                                                    ;
;;;      Arguments:  None                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                  Elapsed time in seconds from when the drawing was ;
;;;                  opened.                                           ;
;;;                                                                    ;
;;;          Usage: (C:VLA-TST) or VLA-TST from the Command: prompt.   ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun c:vla-tst (/ t0 t1 cmde blipm osm asm)
;;; Drawing Limits, Zoom, OSMODE, and VIEWRES may all significantly affect
;;; the times it takes for these functions to return.
  (command "VIEWRES" "Y" "1000")
  (command "LIMITS" "-750,-750" "750,750")
  (command "ZOOM" "W" "-750,-750" "750,750")
  (setq t0 (get-utime))
  (setq cmde (getvar "CMDECHO"))
  (setq blipm (getvar "BLIPMODE"))
  (setq osm (getvar "OSMODE"))
  (setq asm (getvar "AUTOSNAP"))
  (setq plm (getvar "PLINETYPE"))
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (setvar "OSMODE" 0)
  (setvar "AUTOSNAP" 0)
  (setvar "PLINETYPE" 2)
;; Testing function place
  (vvla-tst)
  (setvar "CMDECHO" cmde)
  (setvar "BLIPMODE" blipm)
  (setvar "OSMODE" osm)
  (setvar "AUTOSNAP" asm)
  (setvar "PLINETYPE" plm)
  (setq t1 (get-utime))
  (princ "\n; Time (secs): ")
  (princ (- t1 t0))
  (terpri)
  (princ)
)

(princ "\n; To test: AL-TST, VLA-TST, (c:al-tst), or (c:vla-tst)\n")
(princ)

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDAcdrRhYFJ9zcsHd2Odp37oQ93n2EbMusPZ/xS
;;; Oep7HP0N0qSfSpHQ3//KatgSI8omZ+7OFJDPPn9+JgLbQu7X+mlFHqj3wX7kGkTN
;;; 4nFpkEgwyCTog/AdSZ//EB8wkHeB35XQN38qpMlkbD1a2UrYrROwmWUsJB0ARP0G
;;; Ett5jjzydexKy9TmfVcqF9PiNyHcGou3Fk5JnsVNFePyavUOAABWCoddk+Y4Z++D
;;; CzwiW6+0zSGes8+vjGqxnQpdTIn0Ici6jekkLbikXg2FksMbb/QYpvKXST1+VsNi
;;; pigs1jHYiKiG3J9Y0XPpcQvbhdzN4oT+V9HesEb2d9qUWJ6azFGmMwsaCYY7Bpd/
;;; w0QpaelCXOXBUvZ1SHzUTO4xngfL0tRq2ZzR9qdTnurtStueAup9WG5A37O4yuCD
;;; jGTSsN+1uWHixlnep7zdAjMnodT6KqRd7UyIorLcXJ/jKH9ajlG5yfxjuN2FrDme
;;; Rl8BXfn2MZtFYCqqFVRgg/ndKm9tb4eczseL/tqcegaKy3YG02kL8g9uCvS2csY/
;;; G1c7ApeCXHlw5IC1nYeyPSaKASP0/7ExCoXLKzlnaHeQLTOycHFVzhZ9derXOIWW
;;; dJFxD3zCZhKKXvn48aUAbZNv7qYHQ3yH//L46jrL/usBt15CiZ2wvnWspeVeQNUE
;;; IsY7Fw==
;;; -----END-SIGNATURE-----