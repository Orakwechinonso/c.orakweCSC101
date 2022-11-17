;;;                                                                    ;
;;;  CTIE.LSP                                                          ;
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
;;; This file contains various reactor and functions.                  ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;       Function:  GET-MODEL-SPACE                                   ;
;;;                                                                    ;
;;;    Description:  This function test if the global variable         ;
;;;                  *current-model-space* is set. If it is the        ;
;;;                  current value of *current-model-space* is         ;
;;;                  returned. Otherwise the value of the global       ;
;;;                  variable *current-model-space* is created.        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A vla model space object                          ;
;;;                  is returned such as:                              ;
;;;                  #<VLA-OBJECT IAcadModelSpace 027a34c0>            ;
;;;                                                                    ;
;;;          Usage: (get-model-space)                                  ;
;;;--------------------------------------------------------------------;
(defun get-model-space (/ tmp)
  (cond (*current-model-space* *current-model-space*)
        ((and (setq tmp (vlax-get-acad-object))
              (setq tmp (vla-get-activedocument tmp))
              (setq tmp (vla-get-modelspace tmp))
         )
         (setq *current-model-space* tmp)
        )
        (t nil)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-POINT-AT-PROPORTION-ON-CURVE                  ;
;;;                                                                    ;
;;;    Description:  Calculate positiom of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;      proportion = a valid integer                                  ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A list of points.                                 ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(get-point-at-proportion-on-Curve                      ;
;;;                       vla-Curve-Object                             ;
;;;                             proportion )                           ;
;;;--------------------------------------------------------------------;
(defun get-point-at-proportion-on-Curve
       (aCurve proportion / str-par end-par param)
  (setq	str-par	(vlax-curve-getStartParam aCurve)
	end-par	(vlax-curve-getEndParam aCurve)
	param	(+ (* str-par (- 1 proportion)) (* end-par proportion))
  )
  (vlax-curve-getPointAtParam aCurve param)
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-CIRCLES-ON-CIRCLE                            ;
;;;                                                                    ;
;;;    Description:  Calculate positiom of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;          radius = a valid real number                              ;
;;;       n-circles = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A list of points.                                 ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(make-circles-on-circle                                ;
;;;                       vla-Curve-Object                             ;
;;;                       radiusOfCircles                              ;
;;;                       numberOfCircles                              ;
;;;              )                                                     ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun make-circles-on-circle (aCurve	     radius
			       n-circles     /
			       res-circles   proportion
			       posn	     index
			       num-of-Intervals
			      )
  (setq	index 0
	num-of-Intervals n-circles
	n-circles (1- n-circles)
  )
  (if (= 0 num-of-Intervals)
    (setq num-of-Intervals 1)
  )
  (while (<= index n-circles)
    (setq
      proportion  (/ (float index) num-of-Intervals)
      posn	  (get-point-at-proportion-on-Curve aCurve proportion)
      res-circles (cons
		    (make-a-circle posn radius proportion)
		    res-circles
		  )
      index	  (1+ index)
    )
  )
  res-circles
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-A-CIRCLE                                     ;
;;;                                                                    ;
;;;    Description:  Calculate positiom of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla arc object.                          ;
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
(defun make-a-circle (posn radius proportion / new-circle)
    (setq new-circle
	 (vla-AddCircle (get-model-space) (vlax-3d-point posn) radius)
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
(defun update-position (aCurve aCircle / old-center new-center)
  (if
    (and aCircle
	 (vlax-write-enabled-p aCircle)
	 (not
	   (equal (setq old-center (vla-get-center aCircle))
		  (setq	new-center
			 (get-point-at-proportion-on-Curve
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
;;;    Description:  Calculate positiom of circles on a curve          ;
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
(defun circles-tied-to-curve (aCurve radius circl-number)
  (setq circles-lst (make-circles-on-circle aCurve radius circl-number))
  (vlax-ldata-put aCurve "circles" circles-lst)
  (VLR-Object-reactor
    (list aCurve)
    "circles"
    (list
      (cons :vlr-modified (function update-position-reaction))
    )
  )
)

;;;;;; To use test this file
;;;	1. draw a circle on the screen
;;;	2. Cut and paste the code below
;|
(setq e (circles-tied-to-curve
          (vlax-ename->vla-object (entlast))
          (getdist "\nRadius Distance:")
          (getint "\nNumber of Circles:")
        )
)
|;
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBkA67VMqxM8GLgclPFL+emI9mu5Qy+XkkvpMz4
;;; Vg10q5em3yxV9tBnKQ6bXc0ggIid3tg63c77KSKk0iSeqC1a1LNueAeEx9COXYty
;;; RMaHsWhrBGgt9+/+DBsOIX9xIxXrKUmj/Bk3E4WrbJpu+VHpDMnP5W0fxlFKQsIT
;;; U1LZJ3tzVJ6cj0SPwA2w9XoUxBaKfHw+eqMipo19CcHwweDfFbSsVoxx6KAbqc9h
;;; wq5lLADQiJ7v5dlYlleUhNrsKY7QTngEuy8djP1CIk5Wzr/nAfDbTs71KIBPRnrB
;;; cEzmhWlEuMMtoELthHlO+JjaRlHDkCjaM2fKkMg8qQLjJcNgV7w/xu9GiHJnRgaR
;;; l5LI6cUrynV0LcbbBXfYGWkr4o6vP+V9t82Fiq0vI+cBk3YQa9QL0Ay2iXvzAEH6
;;; HFPCTZyCU8KR9QPOYfjk0qrq9weo07Vk1LrZ2gAF8FGed032LMHrwBQtdwOrneOR
;;; 823QF2oltAj1GzTjHHyJsWVJt4NXKmv+CI7se5abAzszogEyoM5Ab+o+kSEzKd8t
;;; PkoEpiqe9CduaMI7pLRD6guMQU81io7Ekt9cb4DnMzkuRfQvc+Uvdf2/a8L7WiCE
;;; hsGlUQy9aqwTR4EW/oymnotyKcWGF1mBdvknX/dHsrA2KEsWZA/8m13T/iOPVYnZ
;;; 9aJQow==
;;; -----END-SIGNATURE-----