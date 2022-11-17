;;;                                                                    ;
;;;  GRAFUN.LSP                                                        ;
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
;;;  This file demonstrates adding a 3D mesh utilizing activeX methods ;
;;;  and native AutoLISP command function. The Mesh is produced by     ;
;;;  applying a function to the beginning coordinate for the mesh,     ;
;;;  a length in Y direction for the mesh, and a length in the x       ;
;;;  for the mesh.                                                     ;
;;;  coordinate.                                                       ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;; General Note:  ACAD user timer must be ON                          ;
;;; (command "_.time" "_ON" "")                                        ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model functions here
(vl-load-com)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-UTIME                                         ;
;;;                                                                    ;
;;;    Description:  GET-UTIME converts a fraction of a day into       ;
;;;                  seconds by multiplying the result from            ;
;;;                  (getvar "tdusrtimer") and 86400.0.                ;
;;;                                                                    ;
;;;                  Example:                                          ;
;;;                     (getvar "tdusrtimer") returns a fraction of    ;
;;;                     one day. So... (getvar "tdusrtimer") might     ;
;;;                     return: 0.138439                               ;
;;;                     In order to return elapsed second we determine ;
;;;                     Seconds in One Hour:                           ;
;;;                                     (* 60.00 60.00) = 3600.0       ;
;;;                     And seconds in One 24 Hour period:             ;
;;;                                     (* 24 3600.0) = 86400.0        ;
;;;                                                                    ;
;;;      Arguments:  None                                              ;
;;;                                                                    ;
;;; Returned Value:  Returns a real number whose meaning is:           ;
;;;                  Elapsed time in seconds from when the drawing was ;
;;;                  opened.                                           ;
;;;                                                                    ;
;;;          Usage: (get-utime)                                        ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
;;; returns the user time
(defun get-utime ()
  (* 86400 (getvar "tdusrtimer"))
  )
;;;--------------------------------------------------------------------;
;;;       Function:  AL-GRAFUN                                         ;
;;;                                                                    ;
;;;    Description:  Draws a mesh utilizing the AutoLISP Command       ;
;;;                  function according to the arguments supplied.     ;
;;;                  This function prints elapsed time in minutes      ;
;;;                  and seconds during processes and returns a list   ;
;;;                  points which were used to create the 3D mesh.     ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          get-utime                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;                  Fun = Function to apply.                          ;
;;;                  xy0 = Lower left corner of rectangular mesh.      ;
;;;                  xy1 = Upper right corner of rectangular mesh.     ;
;;;                  dx  = Density factor for x direction.             ;
;;;	                       The smaller the number denser the mesh. ;
;;;                  dy  = Density factor for y direction.             ;
;;;	                       The smaller the number denser the mesh. ;
;;;                                                                    ;
;;; Returned Value:  A list points which were used to create           ;
;;;                  the 3D mesh.                                      ;
;;;                                                                    ;
;;;Dislayed Values: Displays time of evaluation in the                 ;
;;;                 following format:                                  ;
;;;                 (TT == NCT + DMT)                                  ;
;;;                 Where:                                             ;
;;;                 TT  = Total elapsed time.                          ;
;;;                 NCT = Elapsed time from start of function which    ;
;;;                       draws the mesh until the function has        ;
;;;                       finished its number crunching.               ;
;;;                 DMT = Elapsed time of the mesh drawing portion     ;
;;;                       after number crunching has finished.         ;
;;;                                                                    ;
;;;          Usage: (al-grafun fun xy0 xy1 dx dy)                      ;
;;;--------------------------------------------------------------------;
(defun aal-grafun (fun xy0 xy1 dx dy
	/ x0 y0 x1 y1 x_coord y_coord z_coord cx cy pts ids t0 t1 t2 t3 ce bm os)
  (setq t0 (get-utime))
  (setq x0 (car xy0)
	y0 (cadr xy0)
	x1 (car xy1)
	y1 (cadr xy1)
	)
  (setq x_coord x0
	cx 0)
  (while (<= x_coord x1)
    (setq y_coord y0
	  cy 0 )
    (while (<= y_coord y1)
      (setq z_coord (fun x_coord y_coord))
      (setq pts (cons (list x_coord y_coord z_coord) pts))
      (setq y_coord (+ y_coord dy)
	    cy (1+ cy) )
      )
    (setq x_coord (+ x_coord dx)
	  cx (1+ cx) )
    )
  (setq pts (reverse pts))
  (setq t1 (get-utime))

  (setq ce (getvar "CMDECHO"))
  (setq bm (getvar "BLIPMODE"))
  (setq os (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (setvar "OSMODE" 0)
  (command "_.3dmesh" cx cy)
  (FOREACH p pts (command p))
  (setq pts (entlast))
  (command "_.CIRCLE" '(5.0 5.0 0.0) 5.0)
  (setvar "CMDECHO" ce)
  (setvar "BLIPMODE" bm)
  (setvar "OSMODE" os)
  (setq t2 (get-utime))

  ;(command "")
  
  (princ "\n; Time: ")
  (princ (list (- t2 t0) '== (- t1 t0) '+ (- t2 t1)))
  pts
  )
;;;--------------------------------------------------------------------;
;;;       Function:  VL-GRAFUN                                         ;
;;;                                                                    ;
;;;    Description:  Draws a mesh utilizing Visual LISP Automation     ;
;;;                  extensions (known as activeX methods) according   ;
;;;                  to the arguments supplied.                        ;
;;;                  This function prints elapsed time in minutes      ;
;;;                  and seconds during processes and returns a list   ;
;;;                  points which were used to create the 3D mesh.     ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          get-utime                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;                  Fun = Function to apply.                          ;
;;;                  xy0 = Lower left corner of rectangular mesh.      ;
;;;                  xy1 = Upper right corner of rectangular mesh.     ;
;;;                  dx  = Density factor for x direction.             ;
;;;	                       The smaller the number denser the mesh. ;
;;;                  dy  = Density factor for y direction.             ;
;;;	                       The smaller the number denser the mesh. ;
;;;                                                                    ;
;;; Returned Value:  A list points which were used to create           ;
;;;                  the 3D mesh.                                      ;
;;;Dislayed Values: Displays time of evaluation in the                 ;
;;;                 following format:                                  ;
;;;                 (TT == NCT + DMT)                                  ;
;;;                 Where:                                             ;
;;;                 TT  = Total elapsed time.                          ;
;;;                 NCT = Elapsed time from start of function which    ;
;;;                       draws the mesh until the function has        ;
;;;                       finished its number crunching.               ;
;;;                 DMT = Elapsed time of the mesh drawing portion     ;
;;;                       after number crunching has finished.         ;
;;;                                                                    ;
;;;          Usage: (vl-grafun fun xy0 xy1 dx dy)                      ;
;;;--------------------------------------------------------------------;
(defun avl-grafun (fun xy0 xy1 dx dy
	/ x0 y0 x1 y1 x_coord y_coord z_coord cx cy pts t0 t1 t2 t3 *ModelSpace*)
  (setq t0 (get-utime))
  (setq x0 (car xy0)
	y0 (cadr xy0)
	x1 (car xy1)
	y1 (cadr xy1)
	)
  (setq x_coord x0
	cx 0)
  (while (<= x_coord x1)
    (setq y_coord y0
	  cy 0 )
    (while (<= y_coord y1)
      (setq z_coord (fun x_coord y_coord))
      (setq pts (vl-list* z_coord y_coord x_coord pts))
      (setq y_coord (+ y_coord dy)
	    cy (1+ cy) )
      )
    (setq x_coord (+ x_coord dx)
	  cx (1+ cx) )
    )
  (setq pts (reverse pts))

;; New for AutoCAD 2000 - Create and Fill A Variant Array for the Mesh Points
  (setq ptlstlen (length pts))
  (setq PointDataA (vlax-make-safearray vlax-vbDouble (cons 0 (1- ptlstlen))))
  (vlax-safearray-fill PointDataA pts)
  (setq PointData (vlax-make-variant PointDataA (logior vlax-vbarray vlax-vbDouble)))
;; New for AutoCAD 2000 - Create and Fill A Variant Array for the Mesh Points

  (setq t1 (get-utime))
  (setq *ModelSpace* (vla-get-ModelSpace
		       (vla-get-ActiveDocument
		         (vlax-get-acad-object) )))
  
  (setq pmesh (vla-Add3Dmesh *ModelSpace* cx cy PointData) )
  (vla-Update pmesh)
  (vla-AddCircle *ModelSpace* (vlax-3d-Point '(5.0 5.0 0.0)) 5.0)
  (setq t2 (get-utime))
  (princ "\n; Time: ")
  (print (list (- t2 t0) '== (- t1 t0) '+ (- t2 t1)))
  pmesh
  )
;;;--------------------------------------------------------------------;
;;;       Function:  SQR                                               ;
;;;                                                                    ;
;;;    Description:  Function to return the square of a number.        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             x   = a valid number.                                  ;
;;;                   Note: no error checking is performed             ;
;;;                                                                    ;
;;; Returned Value: a number either real or integer.                   ;
;;;                 If an integer is passed an integer is returned.    ;
;;;                 If a real number is passed a real is returned.     ;
;;;                                                                    ;
;;;          Usage: (sqr 7) returns 49                                 ;
;;;                 (sqr 7.0) returns 49.00                            ;
;;;--------------------------------------------------------------------;
(defun sqr(x) (* x x))

;;;--------------------------------------------------------------------;
;;;       Function:  SPHE5                                             ;
;;;                                                                    ;
;;;    Description:  Function which calculates a half of sphere.       ;
;;;                  Where: C(5,5,0), R(5)                             ;
;;;                  and Z coordinate is:                              ;
;;; 		            Z = F(X,Y)                                 ;
;;;                                                                    ;
;;;                  This function provides a seed algorithim to       ;
;;;                  calculate 1/2 a sphere to utilize within          ;
;;;                  our 3dMesh creation functions:                    ;
;;;                     vl-grafun                                      ;
;;;                     al-grafun                                      ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          sqr                                       ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               x_coord  = X coordinate of point to determine the Z  ;
;;;                    coordinate for the mesh.                        ;
;;;               y_coord  = Y coordinate of point to determine the Z  ;
;;;                    coordinate for the mesh.                        ;
;;;                                                                    ;
;;; Returned Value:  Z coordinate.                                     ;
;;;                                                                    ;
;;;          Usage: (sphe5 x_coord y_coord)                            ;
;;;--------------------------------------------------------------------;
(defun sphe5 (x_coord y_coord)
  (sqrt (max 0.0 (- 25 (sqr (- x_coord 5)) (sqr (- y_coord 5)))))
  )

;;;--------------------------------------------------------------------;
;;;       Function:  AZVPOINT                                          ;
;;;                                                                    ;
;;;    Description:  This function changes the vpoint of the current   ;
;;;                  viewport to values represented in pt argument.    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               pt = A list of three numbers which represent view    ;
;;;                    coordinate and direction.                       ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (zvpoint '( 0 0 1))                                ;
;;;--------------------------------------------------------------------;
(defun zvpoint (pt / ce)
  (setq ce (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (COMMAND "_.ZOOM" "_E")
  (COMMAND "_.VPOINT" pt)
  (setvar "CMDECHO" ce)
  (princ)
  )
;;;--------------------------------------------------------------------;
;;;       Function:  C:AL-GF                                           ;
;;;                                                                    ;
;;;    Description:  This excecutes the native AutoLISP function to    ;
;;;                  create a 3D mesh.                                 ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          al-grafun                                 ;
;;;                          zvpoint                                   ;
;;;                          sphe5                                     ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (c:AL-GF) or AL-GF from the ACAD comand prompt.    ;
;;;--------------------------------------------------------------------;
(defun C:AL-GF ()
  (aAL-GRAFUN SPHE5 '(-1 -1) '(11 8) 0.2 0.2)
  (ZVPOINT '(10 10 3))
  )
;;;--------------------------------------------------------------------;
;;;       Function:  C:VLA-GF                                          ;
;;;                                                                    ;
;;;    Description:  This excecutes ActiveX Automation functions to    ;
;;;                  create a 3D mesh.                                 ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          vl-grafun                                 ;
;;;                          zvpoint                                   ;
;;;                          sphe5                                     ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (c:VLA-GF) or VLA-GF from the ACAD comand prompt.  ;
;;;--------------------------------------------------------------------;
;;; Use ActiveX Automation. 
;;; Returns a response similar to this:(your may vary)
;;;  "; Time: 
;;; (1.54 == 0.71 + 0.83) 
(defun C:VLA-GF ()
  (aVL-GRAFUN SPHE5 '(-1.0 -1.0) '(11.0 8.0) 0.2 0.2)
  (ZVPOINT '(10 10 3))
  )

;;;--------------------------------------------------------------------;
;;;                                                                    ;
;;; On Your Own:                                                       ;
;;;    Try out the following code fragment in the Visual LISP console  ;
;;;    after loading this file into the editor.                        ;
;;;  (VL-GRAFUN SPHE5 '(0 0) '(4 4) 0.2 0.2)                           ;
;;;--------------------------------------------------------------------;

;;; Print an aid for the command above.
(princ "; To test: AL-GF, VLA-GF, (c:al-gf), or (c:vla-gf) \n")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBHXlkYQpq3aUHfLlC6R5FBPTGtlSV8DNjPL12m
;;; rxKpP5pY+KnifoY7hutUyHn0Lj0sU8hwEExcb1GVfQvRhbpsPncUpPhLhFXy8veX
;;; pClYCd4ectUTU8xBmND8+/OWxSS7C6rq3MV1oVr+C4sebIbQ//Rj24N2T6LI6FJt
;;; D8eegfJ4FayEjE714/7hjNwwLc0ryrL/UB+F4+VdT9C9pMAYI4dSTr+Aue8Jd5eK
;;; DEEftZL2CC/yILPg7KtxvN8A9C6TkXuSBLSgrCWglIWC6P49ueb3XcEOtKdUL1/r
;;; y8YnoDLvnGJt2yaUDQbPmIDEUJanwwWOdQE7SxxX6bvBx7Owbno8WdIDEWw8cR5w
;;; 4jTFEUB48HcQyik9JEmV+LTHXUHiYYdOyF11nHvRC2l5apEHhyOEkFBYSFnbh0ha
;;; 2v01ciq6kTSOc7G24y5HLA0yvfwViT/yjCxl+QX++CtyevY9R5noptHqh9pnD95B
;;; t30crOrXOBIQi3GHxgHB/pI75zo5ctAN/lsBAGilL6fI5ANFPcbKBG6/oPHV9RAU
;;; PrILNtNqLmdTtUOcbVkge+s3EXTWBsCret1vRcqBYzD8H31PI59G78bhN/WMhwZO
;;; ChWVyVRngUqPu4OQseMbIW5M4zOqca25H1f+YcO5/BzWDU5uQFimsE3qR4wkR4JP
;;; /wlWYg==
;;; -----END-SIGNATURE-----