;;;                                                                    ;
;;;  RTIE.LSP                                                          ;
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
;;; This file contains various utilities                               ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  POINT-AT-DISTANCE-ON-CURVE                        ;
;;;                                                                    ;
;;;    Description:  This function is used to modify a collection of   ;
;;;                  vla objects to share the same properties.         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;      proportion = a valid real number                              ;
;;;                                                                    ;
;;; Returned Value:  A point.                                          ;
;;;		                                                       ;
;;;          Usage:                                                    ;
;;;                (point-at-distance-on-curve                         ;
;;;                      vla-curve-object                              ;
;;;                      proportion)                                   ;
;;;--------------------------------------------------------------------;
(defun point-at-distance-on-curve (aCurve proportion)
  (vlax-curve-getPointAtDist
    aCurve
    (+
      (* (- 1 proportion)
	 (vlax-curve-getDistAtParam
	   aCurve
	   (vlax-curve-getStartParam aCurve)
	 )
      )
      (* proportion
	 (vlax-curve-getDistAtParam
	   aCurve
	   (vlax-curve-getEndParam aCurve)
	 )
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-CIRCLES-ON-CURVE                             ;
;;;                                                                    ;
;;;    Description:  Calculate position of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                        point-at-distance-on-curve                  ;
;;;                        make-a-circle                               ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;          radius = a valid real number                              ;
;;;  num-of-circles = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A list of points.                                 ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(make-circles-on-curve                                 ;
;;;                       aCurve                                       ;
;;;                       radius                                       ;
;;;                       num-of-circles                               ;
;;;              )                                                     ;
;;;              or                                                    ;
;;;            (make-circles-on-curve vla-curve 1.0 5)                 ;
;;;--------------------------------------------------------------------;
(defun make-circles-on-curve (aCurve	     radius
			      num-of-circles /
			      res-circles    proportion
			      ctrpt	     i
			      num-of-Intervals
			     )
  (setq	i 0
	num-of-Intervals
	 (if (vlax-curve-isClosed aCurve)
	   num-of-circles
	   (1- num-of-circles)
	 )
	num-of-circles (1- num-of-circles)   ;we starts from 0
  )
  (if (= 0 num-of-Intervals)
    (setq num-of-Intervals 1))
  (while (<= i num-of-circles)
    (setq
      proportion  (/ (float i) num-of-Intervals)
      ctrpt	  (point-at-distance-on-curve aCurve proportion)
      res-circles (cons
		    (make-a-circle ctrpt radius proportion)
		    res-circles
		  )
      i		  (1+ i)
    )
  )
  res-circles
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-A-CIRCLE                                     ;
;;;                                                                    ;
;;;    Description:  Calculate position of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;          radius = a valid real number                              ;
;;;       n-circles = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A list of vla circle objects                      ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(make-a-circle                                         ;
;;;                       vla-Curve-Object                             ;
;;;                       radiusOfCircles                              ;
;;;                       numberOfCircles                              ;
;;;              )                                                     ;
;;;              or                                                    ;
;;;		(make-a-circle pt1 1.0 5)                              ;
;;;--------------------------------------------------------------------;
;;; redefined in CTIE.LSP!!!
(defun make-a-circle (ctrpt radius proportion / new-circle)
  (setq	new-circle
	 (vla-AddCircle acadModel (vlax-3d-point ctrpt) radius)
  )
  (vlax-ldata-put new-circle "proportion" proportion)
  new-circle
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-POSITION-REACTION                          ;
;;;                                                                    ;
;;;    Description:  This function updates the position of each        ;
;;;                  circle associated with the reactor.               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                         update-position                            ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;         reactor = a valid real number                              ;
;;;        arg-list = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A list of vla circle objects                      ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;		(update-position-reaction                              ;
;;;                       aCurve                                       ;
;;;                       reactor                                      ;
;;;                       arg-list                                     ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
;;; redefined in CTIE.LSP!!!
(defun update-position-reaction	(aCurve reactor arg-list)
  (foreach circle (vlax-ldata-get aCurve (vlr-data reactor))
    (update-position aCurve circle)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-POSITION                                   ;
;;;                                                                    ;
;;;    Description:  This function updates the position of a circle    ;
;;;                  according its proportion property and the         ;
;;;                  the curve object.                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;         aCircle = a valid vla circle object                        ;
;;;                                                                    ;
;;; Returned Value:  A list of vla circle object.                      ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(update-position                                       ;
;;;                       aCurve                                       ;
;;;                       aCircle                                      ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
;;; redefined in CTIE.LSP!!!
(defun update-position (aCurve aCircle / old-center new-center)
  (if
    (and aCircle
	 (vlax-write-enabled-p aCircle)
	 (not
	   (equal (setq old-center (vla-get-center aCircle))
		  (setq	new-center
			 (point-at-distance-on-curve
			   aCurve
			   (vlax-ldata-get aCircle "proportion")
			 )
		  )
	   )
	 )
    )
     (vla-put-center aCircle (vlax-3d-point new-center))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CIRCLES-TIED-TO-CURVE                             ;
;;;                                                                    ;
;;;    Description:  Calculate position of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                         make-circles-on-circle                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;          radius = a valid real number                              ;
;;;       n-circles = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr object reactor object.                ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(circles-tied-to-curve                                 ;
;;;                       aCurve                                       ;
;;;                       radius                                       ;
;;;                       n-circles                                    ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
;;; redefined in CTIE.LSP!!!
(defun circles-tied-to-curve (aCurve radius circl-number)
  (function update-position-reaction)	; prevent name drop
  (setq circles-lst (make-circles-on-curve aCurve radius circl-number))
  (vlax-ldata-put aCurve "circles" circles-lst)
  (VLR-Object-reactor
    (list aCurve)
    "circles"
    '((:vlr-modified . update-position-reaction))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAhiYOu9YvDndzir84OmlMuNT2P0loVVrtAtAX9
;;; CIS82kXtWy2lQR5zVtiK+450yYoNlSvn42wWzIwcs6FuF4ZltutnrLm11WgApuRa
;;; pNX440oNOAqzgW6PSYNBU/UZSPkDJudrTejSdHJGOcu1GIyMzIS3y4yBuh94PXX/
;;; SiCA0jKSIP66w7BlcE3rWiZEfckbvJzGoxbC1KtYqP11nXk762fLXbhh8ma5YVel
;;; vPbrLBvP/XBToVV+rwcgWQ+u8qvQmJPkEVLKPyLAXGJ1Itd77X+iGBaT841qHUPk
;;; +amS3+fAkxrWLdGGSgOKlrPdENSv0ofkB1SEsMlVhT042HDLZ1czSEjrgl060cRF
;;; rvDd/kNV/reYDgPEHXel6nNLT1IpTMETGvrMIgShB4qkgfSckw7F6MhczrQ51gJh
;;; qJWWfvnLBgUeMMlqI5/QRx8PU7QqEPvWCxMYJMsJkMmvCDnp4ou6sHwCPoo06n9M
;;; gnaH6LekvwgmMnSthAKuoaKlJbBTUxXSiWVk8OGfczZEnYOoen0k96yOL23NFTsv
;;; ETwwAf0cQKSgE5BhW0sqKvX1Pr1njvvUaFew5BtgsuhPIqmLipXTyvfo6KeCySeZ
;;; bpU5YlZ0mIzS9z5jLLIC8qh/KULbCGXe+zwo8eB88HytrnJyWYcwz6mM4GDvPV85
;;; EaTKNQ==
;;; -----END-SIGNATURE-----