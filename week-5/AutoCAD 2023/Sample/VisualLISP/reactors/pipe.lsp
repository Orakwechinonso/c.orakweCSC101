;;;                                                                    ;
;;;  PIPE.LSP                                                          ;
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
;;;    This file contains a demonstration of a path based on a         ;
;;;    polyline.                                                       ;
;;;                                                                    ;
;;;    Creates a path - polyline and a base curve - circle.            ;
;;;                                                                    ;
;;;    The 3d figure will be created as an extrusion of the circle     ;
;;;    along the polyline (they are colored red). When you change the  ;
;;;    path or the circle the 3d figure will be updated.               ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  GET-VECTOR-ALONG                                  ;
;;;                                                                    ;
;;;    Description:  This function performs a subtraction of the       ;
;;;                  startPoint and EndPoint of a vla line object.     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            line = a valid vla line object.                         ;
;;;                                                                    ;
;;; Returned Value:  A vector list.                                    ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (get-vector-along vla-line-Object )              ;
;;;--------------------------------------------------------------------;
(defun get-vector-along	(line / from to)
  (setq	from (vla-get-startPoint line)
	to   (vla-get-EndPoint line)
  )
  (mapcar '- to from)
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-PIPE                                         ;
;;;                                                                    ;
;;;    Description:  This function extrudes a circle in the direction  ;
;;;                  of a path line. Note: Line and circle can not be  ;
;;;                  coplanar.                                         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     get-model-space                                ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            line = a valid vla line object.                         ;
;;;          circle = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  A list of a vla 3d Solid Object. Such as:         ;
;;;                  (#<VLA-OBJECT IAcad3DSolid 02d23f2c>)             ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (make-pipe vla-line-Object vla-circle-object)    ;
;;;--------------------------------------------------------------------;
(defun make-pipe
       (line circle / mSpace region-list normal-vector exrude-list)
  (vla-move circle
	    (vlax-3d-point (vlax-curve-getstartPoint circle))
	    (vlax-3d-point (vlax-curve-getstartPoint line))
  )
(setvar "ISOLINES" 25)
(setq circleAa (vlax-make-safearray vlax-vbObject '(0 . 0)))
(vlax-safearray-put-element circleAa 0 circle)
(setq circleA (vlax-make-variant circleAa (logior vlax-vbarray vlax-vbObject)))  

  (setq	mSpace	    (get-model-space))
  (setq	region-list (vlax-safearray->list
		      (vlax-variant-value
			(vla-AddRegion mSpace circleA)
		      )
		     )
  )
  (setq	exrude-list (mapcar
		      (function
			(lambda	(region)
			  (vla-AddExtrudedSolidAlongPath mSpace region line)
			)
		      )
		      region-list
		    )
  )
  (foreach region region-list
    (if	(not (vlax-erased-p region))
      (vla-Erase region)
    )
  )
  exrude-list
)

;;;--------------------------------------------------------------------;
;;;       Function:  PROPERTY-CHANGED-P                                ;
;;;                                                                    ;
;;;    Description:  This function serves as a predicate. Testing for  ;
;;;                  the integrity of the data retreived from the      ;
;;;                  object to be the same as the supplied property.   ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vla-obj = a valid vla object.                              ;
;;;        property = a property list to compare.                      ;
;;;                                                                    ;
;;; Returned Value:  T if the property has changed. Nil otherwise.     ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (property-changed-p vla-line-Object prop-List)     ;
;;;--------------------------------------------------------------------;
(defun property-changed-p (vla-obj property)
  (and (eq 'VLA-OBJECT (type vla-obj))
       (vlax-read-enabled-p vla-obj)
       (vlax-property-available-p vla-obj property)
       (not (equal (vlax-get vla-obj property)
		   (vlax-ldata-get vla-obj property)
	    )
       )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-PIPE-CIRCLE                               ;
;;;                                                                    ;
;;;    Description:  This function will be called inside a             ;
;;;                  :vlr-modified event.                              ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     reactor-pipe-line                              ;
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
;;;     (reactor-pipe-circle notifier reactor arg-list)                ;
;;;--------------------------------------------------------------------;
(defun reactor-pipe-circle (notifier reactor arg-list)
  (reactor-pipe-line
    (vlax-ldata-get notifier "line")
    reactor
    arg-list
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-PIPE-LINE                                 ;
;;;                                                                    ;
;;;    Description:  This function will be called inside a             ;
;;;                  :vlr-modified event and is invoked by             ;
;;;                  reactor-pipe-circle reactor call back. Its        ;
;;;                  purpose is to modify the reactor which            ;
;;;                  was invoked.                                      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     property-changed-p                             ;
;;;                     change-pipe-list                               ;
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
;;;       (reactor-pipe-line notifier reactor arg-list)                ;
;;;--------------------------------------------------------------------;
(defun reactor-pipe-line (notifier reactor arg-list)
  (if (or t
	  (property-changed-p notifier "StartPoint")
	  (property-changed-p notifier "EndPoint")
      )
    ;;    (change-pipe notifier)
    ;|{|;
    (progn
      (if (null *editor-pipe-updating-reactor*)
	(setq *editor-pipe-updating-reactor* (VLR-Editor-reactor))
      )
      (if (not (VLR-added-p *editor-pipe-updating-reactor*))
	(vlr-add *editor-pipe-updating-reactor*)
      )
      (VLR-Data-Set
	*editor-pipe-updating-reactor*
	(cons notifier (VLR-Data *editor-pipe-updating-reactor*))
      )
      (vlr-reaction-set
	*editor-pipe-updating-reactor*
	:vlr-commandEnded
	(function change-pipe-list)
      )
    )
    ;|}|;
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CHANGE-PIPE-LIST                                  ;
;;;                                                                    ;
;;;    Description:  This function will be called inside a             ;
;;;                  :vlr-modified event and is invoked by             ;
;;;                  reactor-pipe-circle reactor call back. Its        ;
;;;                  purpose is to modify the reactor which            ;
;;;                  was invoked.                                      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     change-pipe                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;                 (change-pipe-list reactor arg-list)                ;
;;;--------------------------------------------------------------------;
(defun change-pipe-list	(reactor arg-list)
  (foreach pipe	(VLR-Data reactor)
    (change-pipe pipe)
  )
  (VLR-Data-Set reactor nil)
)

;;;--------------------------------------------------------------------;
;;;       Function:  CHANGE-PIPE                                       ;
;;;                                                                    ;
;;;    Description:  This function will modify the pipe created        ;
;;;                  from the path line.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     make-pipe                                      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vla-obj = a valid vla object.                              ;
;;;                                                                    ;
;;; Returned Value:  A valid vla object.                               ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (change-pipe vla-obj)                              ;
;;;--------------------------------------------------------------------;
(defun change-pipe (vla-obj)
  (if (and vla-obj (not (vlax-erased-p vla-obj)))
    (progn
      (foreach extrudion (vlax-ldata-get vla-obj "extrude-list")
	(if (not (vlax-erased-p extrudion))
	  (vla-Erase extrudion)
	)
      )
      (if (not (vlax-erased-p vla-obj))
	(vlax-ldata-put
	  vla-obj
	  "extrude-list"
	  (make-pipe vla-obj (vlax-ldata-get vla-obj "circle"))
	)
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-PIPE-REACTOR                                 ;
;;;                                                                    ;
;;;    Description:  This function will modify the pipe created        ;
;;;                  from the path line.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     change-pipe                                    ;
;;;                     reactor-pipe-line                              ;
;;;                     reactor-pipe-circle                            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            line = a valid vla line object.                         ;
;;;          circle = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr object reactor                        ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (make-pipe-reactor                                 ;
;;;                        vla-line-object vla-circle-Object)          ;
;;;--------------------------------------------------------------------;
(defun make-pipe-reactor (line circle)
  (vlax-ldata-put line "circle" circle)
  (vlax-ldata-put circle "line" line)
  (change-pipe line)
  (list
    (VLR-Object-reactor
      (list line)
      nil
      (list (cons :vlr-modified (function reactor-pipe-line)))
    )
    (VLR-Object-reactor
      (list circle)
      nil
      (list (cons :vlr-modified (function reactor-pipe-circle)))
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-PIPE-BASE                                     ;
;;;                                                                    ;
;;;    Description:  This function is responsible for building an      ;
;;;                  ActiveX circle object for the pipe base.          ;
;;;                                                                    ;
;;;                  Note: It's possible use (entsel).                 ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     get-model-space                                ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid vla circle object.                        ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (get-pipe-base)                                    ;
;;;--------------------------------------------------------------------;
(defun get-pipe-base (/ obj)
  (setq	obj (VLA-addCIRCLE
	      (get-model-space)
	      (vlax-3d-point '(5.0 5.0 0.0))
	      5
	    )
  )
  (vla-put-Normal obj (vlax-3d-point '(0.0 0.0 1.0)))
  (vla-put-Color obj acred)
  obj
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-PIPE-EXTRUDE                                  ;
;;;                                                                    ;
;;;    Description:  This function is responsible for building an      ;
;;;                  ActiveX object for the pipe extrusion.            ;
;;;                                                                    ;
;;;                  Note: It's possible use (entsel).                 ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     get-model-space                                ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid vla polyline object.                      ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (get-pipe-extrude)                                 ;
;;;--------------------------------------------------------------------;
(defun get-pipe-extrude (/ obj Points ptlstlen PointDataA PointData)

(setq Points (mapcar 'float '(0 0 0  0 10 0  -7 23 0  -10 30 0)))
(setq ptlstlen (length Points))
(setq PointDataA (vlax-make-safearray vlax-vbDouble (cons 0 (1- ptlstlen))))
(vlax-safearray-fill PointDataA Points)
(setq PointData (vlax-make-variant PointDataA (logior vlax-vbarray vlax-vbDouble)))

  (setq obj (vla-Addpolyline
              (get-model-space)
              ;; all points need to be reals
              PointData
            )
  )
  ;;; all normals need to be reals
  (vla-put-Normal obj (vlax-3d-point '(0.0 1.0 0.0)))
  (vla-put-Color obj acred)
  obj
)


;;;--------------------------------------------------------------------;
;;;       Function:  C:PIPE-TST                                        ;
;;;                                                                    ;
;;;    Description:  This function aids in the creation of a circle    ;
;;;                  object and a path which will create a "smart"     ;
;;;                  pipe able to be modified.                         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    get-pipe-extrude                                ;
;;;                    get-pipe-base                                   ;
;;;                    make-pipe-reactor                               ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid vla object reactor.                       ;
;;;                                                                    ;
;;;          Usage: (C:PIPE-TST) or PIPE-TST from                      ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:PIPE-TST (/ line circle)
  (setq line (get-pipe-extrude))
  (setq circle (get-pipe-base))
  (if (and line circle)
    (make-pipe-reactor line circle)
  )
(princ)
  )

;;;--------------------------------------------------------------------;
;;;       Function:  C:PIPE-INFO                                       ;
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
(defun C:PIPE-INFO ()
  (textscr)
    (princ "\nThis file contains a demonstration of a path based on a polyline.")
    (princ "\nCreates a path - polyline and a base curve - circle. ")
    (princ "\n")
    (princ "\nThe 3d figure will be created as an extrusion of the circle ")
    (princ "\nalong the polyline (they are colored red). When you change the ")
    (princ "\npath or the circle the 3d figure will be updated.")
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "PIPE-TST" "PIPE-INFO")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCB3urMSMr/TnYYrZF4kFUiJM+Pt4mCTaT3QWZF
;;; pSuEQcbDfnu6kYqC1ZYLVRzk1FRMVFqDcecuTW7Kn30md5WrWabiq7qPOBYBw0mA
;;; 1sDFuok80Ominl/Mw30JPNbkiwS+UrgWpzkbDiAJKwOQaM3DQIVXu+jONBZdYILX
;;; mwvcbZitXJpvG8PTpdpZtOrhOR+wGsxal0qFJSiTRCZ4vvMp+HStLh1UFMrkIImm
;;; Ccqn3NPIejdDYduo672eNE6waDaE7jbjeBngWhPhpAGc20e5b4x3lqM9iz98slC+
;;; og7vhg+vET387aScW2mjsQctaVqK6RRYD37PxEv/hTQucairnQ2F8yDAzWdoLt2q
;;; Lr4rMSYlvVuJgU6xwEuXypymfs32wHIG+H9+dDtWwYUu8CK/9VrkBoqavp9XonWt
;;; uA4B7t37EZ0lOxypsfwh4jwwwn/liUZIydgXti/wOSkl5oqGHj0LX4z7YsiACcWJ
;;; zk8zkiUCOcqqUet5uiy9m9TLscyPvXg61W6geIpyNU3+wd/OESYDAcxYsmlCia8Y
;;; RHcNlRLZnJcD4C8TDhAV8jPGC93Hw64zUwL84I5Vi1ffK5JeF1EJ4WdiXDvHwmCM
;;; LyiVhZ36CBRFRDMudchCAVjNtwt+htCx2mLDEwjzugDeD5a+6mLcRtFmNFXUD2yh
;;; m/t/bQ==
;;; -----END-SIGNATURE-----