;;;                                                                    ;
;;;  DIMEX.LSP                                                         ;
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
;;;	This file contains an Active DIMENSION example.                ;
;;;	This program creates a red circle and an                       ;
;;;	associated dimension. You will be prompted                     ;
;;;	to select the center and radius of the circle.                 ;
;;;	When the circle radius changes, the MTEXT                      ;
;;;	dimension also changes. Conversely, when you                   ;
;;;	change the value of the MTEXT dimension, the                   ;
;;;	circle radius will change.                                     ;
;;;                                                                    ;
;;;                                                                    ;
;;; Active DIMENSION Radial                                            ;
;;; You first bind a CIRCLE or ARC with DIMENTION                      ;
;;; When the circle radius changes the MTEXT in the dimension changes  ;
;;; as well and when you change the number in MTEXT                    ;
;;; the radius will be changed                                         ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-DIM                                        ;
;;;                                                                    ;
;;;    Description:  This function creates a dimension object and      ;
;;;                  an mtext object.                                  ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         ins-pnt = a valid list of 3 reals.                         ;
;;;           width = width of the mtext.                              ;
;;;            text = string value for the mtext                       ;
;;;     coordinates = a list of numbers representing the leader        ;
;;;                   coordinates. such as:                            ;
;;;                   (38.3776 106.162 0.0 38.3776                     ;
;;;                     117.066 0.0 49.2812 127.97 0.0)                ;
;;;                                                                    ;
;;; Returned Value:  A list comprising of a vla-mtextobject and a      ;
;;;                  vla-eader-object. Such as:                        ;
;;;                    (vla-mtext vla-leader)                          ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (create-dim                                           ;
;;;                 '(49.2812 127.97 0.0)                              ;
;;;                    2.72591                                         ;
;;;                    10.9037                                         ;
;;;                   '(38.3776 106.162 0.0 38.3776                    ;
;;;                     117.066 0.0 49.2812 127.97 0.0)                ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun create-dim
		  (ins-pnt    width	 text	    Coordinates
		   /	      acadApp	 acadDoc    acadMode
		   vla-mtext ptlstlen PointDataA PointData
		  )
  (princ (list ins-pnt    width	 text	    Coordinates))
  (if (not (= (type text) 'STR))
    (setq text "TEXT")
  )

(setq ptlstlen (length Coordinates))
(setq PointDataA (vlax-make-safearray vlax-vbDouble (cons 0 (1- ptlstlen))))
(vlax-safearray-fill PointDataA Coordinates)
(setq PointData (vlax-make-variant PointDataA (logior vlax-vbarray vlax-vbDouble)))

  (setq	acadApp	   (vlax-get-acad-object)
	acadDoc	   (vla-get-ActiveDocument acadApp)
	acadModel  (vla-get-ModelSpace acadDoc)
	vla-mtext  (vla-AddMtext acadModel (vlax-3d-point ins-pnt) width text)
	vla-leader (vla-AddLeader
		     acadModel
		     PointData  ;;Coordinates
		     vla-mtext
		     acLineNoArrow
		   )
  )
  (list vla-mtext vla-leader)
)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-DIM-FOR-CIRCLE                             ;
;;;                                                                    ;
;;;    Description:  This updates a vla dimension object.              ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                       set-dim-params-for-circle                    ;
;;;                       create-dim                                   ;
;;;                       update-parameter-mdim                        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      vla-circle = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  A vla-dimension-object.                           ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (create-dim-for-circle                                ;
;;;                 vla-circle-object                                  ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun create-dim-for-circle
			     (vla-circle  /	      ins-pnt
			      width	  text	      vla-dim
			      vla-leader  center      height
			      Coordinates
			     )
  (set-dim-params-for-circle vla-circle)
  (setq vla-dim (create-dim ins-pnt width text Coordinates))
  (update-parameter-mdim (car vla-dim) "height" height)
  (update-parameter-mdim
    (cadr vla-dim)
    "Coordinates"
    Coordinates
  )
  vla-dim
)

;;;--------------------------------------------------------------------;
;;;       Function:  SET-DIM-PARAMS-FOR-CIRCLE                         ;
;;;                                                                    ;
;;;    Description:  This function sets the parameters required        ;
;;;                  to associate a dimension object to the circle.    ;
;;;                                                                    ;
;;;                  defines global parameters:                        ;
;;;                        center                                      ;
;;;                        width                                       ;
;;;                        text                                        ;
;;;                        ins-pnt                                     ;
;;;                        height                                      ;
;;;                        Coordinates                                 ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      vla-circle = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  coordinates to the leader object.                 ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (set-dim-params-for-circle                            ;
;;;                 vla-circle-object                                  ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun set-dim-params-for-circle
       (vla-circle / delta lead-pnt1 lead-pnt2 lead-pnt3)
  (if (and (= (type vla-circle) 'VLA-OBJECT)
	   (vlax-read-enabled-p vla-circle)
      )
       (progn
        (setq center  (vla-get-center vla-circle))
        (if (eq (type center) 'VARIANT)
            (if (> (vlax-variant-type center) 8192)
               (setq center (vlax-safearray->list (vlax-variant-value center)))
            )
        )
        (setq 	  width	      (vla-get-radius vla-circle)
	  delta	      (list 0 width 0)
	  lead-pnt1   center
	  lead-pnt2   (mapcar '+ center delta)
	  delta	      (list width width 0)
	  lead-pnt3   (mapcar '+ lead-pnt2 delta)
	  ins-pnt     lead-pnt3
	  text	      (RTOS width)
	  height      (/ width 3)
	  width	      (/ width 4)
	  Coordinates (append lead-pnt1 lead-pnt2 lead-pnt3)
      ))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  SET-CIRCLE-PARAMS-FOR-MDIM                        ;
;;;                                                                    ;
;;;    Description:  This function returns the radius of the circle    ;
;;;                  based on the value of vla-mtext-object.           ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;       vla-mtext = a valid vla mtext object.                        ;
;;;                                                                    ;
;;; Returned Value:  A real number.                                    ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (set-circle-params-for-mdim                           ;
;;;                 vla-mtext-object                                   ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun set-circle-params-for-mdim (vla-mtext)
  (if (and (= (type vla-mtext) 'VLA-OBJECT)
	   (vlax-read-enabled-p vla-mtext)
      )
    (setq rad (ATOF (vla-get-TextString vla-mtext)))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-PARAMETER-MDIM                             ;
;;;                                                                    ;
;;;    Description:  This function is responsible for updating a       ;
;;;                  vla-object's parameter.                           ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vla-obj = a valid vla object.                              ;
;;;        par-name = a parameter name.                                ;
;;;       par-value = a new parameter value                            ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (update-parameter-mdim                                ;
;;;                 vla-obj  par-name par-value                        ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun update-parameter-mdim (vla-obj par-name par-value)
  (if (and (= (type vla-obj) 'VLA-OBJECT)
	   (vlax-write-enabled-p vla-obj)
	   (not (equal (vlax-get vla-obj par-name) par-value))
      )
    (vlax-put vla-obj par-name par-value)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  LAST-THREE                                        ;
;;;                                                                    ;
;;;    Description:  This function reverses a list an retreives the    ;
;;;                  nth 2, nth 1 and nth 0.                           ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             lst = a list of elements.                              ;
;;;                                                                    ;
;;; Returned Value:  A list of three elements.                         ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (last-three                                           ;
;;;                 list-of-things                                     ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun last-three (lst / rev-lst varlst)
(setq varlst nil)
(if (eq (type lst) 'VARIANT)
   (if (> (vlax-variant-type lst) 8192)
     (progn
      (setq lst (vlax-safearray->list (vlax-variant-value lst)))
      (setq varlst T)
     )
    )
 )
  (setq rev-lst (reverse lst))
  (setq lst (list	(nth 2 rev-lst)
	(nth 1 rev-lst)
	(nth 0 rev-lst)
  ))
;; Maybe Not !! (if varlst (vlax-3d-point lst))
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-MTEXT-FOR-LEADER                           ;
;;;                                                                    ;
;;;    Description:  This function updates an mtext object to place    ;
;;;                  as a value to th eleader object.                  ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                       set-dim-params-for-circle                    ;
;;;                       update-parameter-mdim                        ;
;;;                       last-three                                   ;
;;;                       update-parameter-mdim                        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vla-lst = a list of vla objects. Whose first element is a  ;
;;;                   vla mtext object and the second is a vla circle  ;
;;;                   object.                                          ;
;;;      vla-leader = a valid vla leader object.                       ;
;;;                                                                    ;
;;; Returned Value:  An updated mtext object.                          ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (update-mtext-for-leader                              ;
;;;                 vla-lst  vla-leader                                ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun update-mtext-for-leader
			       (vla-lst	    vla-leader	/
				ins-pnt	    width	height
				text	    Coordinates	center
				vla-mtext   vla-circle
			       )
  (setq	vla-mtext  (car vla-lst)
	vla-circle (cadr vla-lst)
  )
  (if (set-dim-params-for-circle vla-circle)
    (progn
      (if (vlax-read-enabled-p vla-leader)
	(update-parameter-mdim
	  vla-mtext
	  "InsertionPoint"
	  (mapcar '+
		  (last-three (vla-get-Coordinates vla-leader))
		  (list 0 height 0)
	  )
	)
      )
      (update-parameter-mdim vla-mtext "width" width)
      (update-parameter-mdim vla-mtext "height" height)
      (update-parameter-mdim vla-mtext "TextString" text)
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-LEADER-FOR-CIRCLE                          ;
;;;                                                                    ;
;;;    Description:  This function updates the "coordinate" property   ;
;;;                  for the leader object.                            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      vla-leader = a valid vla leader object.                       ;
;;;      vla-circle = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  An updated leader object.                         ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (update-leader-for-circle                             ;
;;;                   vla-leader  vla-circle                           ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun update-leader-for-circle
				(vla-leader  vla-circle	 /
				 ins-pnt     width	 height
				 text	     Coordinates center
				)
  (if (set-dim-params-for-circle vla-circle)
    (update-parameter-mdim vla-leader "Coordinates" Coordinates)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-CIRCLE-FOR-RDIM                            ;
;;;                                                                    ;
;;;    Description:  This function updates the "radius" property       ;
;;;                  for a circle.                                     ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                        set-circle-params-for-mdim                  ;
;;;                        update-parameter-mdim                       ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      vla-circle = a valid vla circle object.                       ;
;;;      vla-leader = a valid vla leader object.                       ;
;;;                                                                    ;
;;; Returned Value:  An updated circle object.                         ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;              (update-circle-for-rdim                               ;
;;;                   vla-circle vla-leader                            ;
;;;               )                                                    ;
;;;--------------------------------------------------------------------;
(defun update-circle-for-rdim (vla-circle vla-mtext / rad center)
  (if (set-circle-params-for-mdim vla-mtext)
    (progn
      (if (and rad (> rad 0))
	(update-parameter-mdim vla-circle "Radius" rad)
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-CIRCLE->LEADER                            ;
;;;                                                                    ;
;;;    Description:  This function will be called inside               ;
;;;                  :vlr-modified event.                              ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                         update-leader-for-circle                   ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vla leader object.                        ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;       (reactor-circle->leader notifier reactor arg-list)           ;
;;;--------------------------------------------------------------------;
(defun reactor-circle->leader (notifier reactor arg-list)
  (update-leader-for-circle (vlr-data reactor) notifier)
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-LEADER->MTEXT                             ;
;;;                                                                    ;
;;;    Description:  This function will be called inside               ;
;;;                  :vlr-modified event.                              ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                         update-mtext-for-leader                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vla mtext object.                         ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;       (reactor-leader->mtext notifier reactor arg-list)            ;
;;;--------------------------------------------------------------------;
(defun reactor-leader->mtext (notifier reactor arg-list)
  (update-mtext-for-leader (vlr-data reactor) notifier)
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-RDIM->CIRCLE                              ;
;;;                                                                    ;
;;;    Description:  This function will be called inside               ;
;;;                  :vlr-modified event.                              ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                         update-circle-for-rdim                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vla circle object.                        ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;       (reactor-rdim->circle notifier reactor arg-list)             ;
;;;--------------------------------------------------------------------;
(defun reactor-rdim->circle (notifier reactor arg-list)
  (update-circle-for-rdim (vlr-data reactor) notifier)
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:DIMEX-TST                                       ;
;;;                                                                    ;
;;;    Description:  This function aids in the creation of a circle    ;
;;;                  object which will contain reactors for:           ;
;;;                  COPY, MIRROR or ARRAY                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    reactor-circle->leader                          ;
;;;                    reactor-rdim->circle                            ;
;;;                    reactor-leader->mtext                           ;
;;;                    add-circle                                      ;
;;;                    create-dim-for-circle                           ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A list of vlr object reactors:                    ;
;;;                (                                                   ;
;;;                  #<VLR-Object-reactor>                             ;
;;;                  #<VLR-Object-reactor>                             ;
;;;                  #<VLR-Object-reactor>                             ;
;;;                )                                                   ;
;;;                                                                    ;
;;;          Usage: (C:DIMEX-TST) or DIMEX-TST from                    ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:DIMEX-TST (/ vla-circle vla-dim vla-mtext vla-leader r1 r2 r3)
  (function reactor-circle->leader)
  (function reactor-rdim->circle)
  (function reactor-leader->mtext)
  (setq vla-circle (ADD-CIRCLE))
  (if vla-circle
    (progn
      (vla-put-Color vla-circle acRed)
      (setq vla-dim    (create-dim-for-circle vla-circle)
	    vla-mtext  (car vla-dim)
	    vla-leader (cadr vla-dim)
	    r1	       (VLR-Object-reactor
			 (list vla-circle)
			 vla-leader
			 '((:vlr-modified . reactor-circle->leader))
		       )
	    r2	       (VLR-Object-reactor
			 (list vla-leader)
			 (list vla-mtext vla-circle)
			 '((:vlr-modified . reactor-leader->mtext))
		       )
	    r3	       (VLR-Object-reactor
			 (list vla-mtext)
			 vla-circle
			 '((:vlr-modified . reactor-rdim->circle))
		       )
      )
      (list r1 r2 r3)
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:DIMEX-INFO                                      ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:DIMEX-INFO) or DIMEX-INFO from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:DIMEX-INFO ()
  (princ "\nActive DIMENSION example.")
  (princ "\nThis program creates a red circle and an associated dimension.")
  (princ "\nYou will be prompted to select the center and radius of the circle.")
  (princ "\nWhen the circle radius changes, the MTEXT dimension also changes.")
  (princ "\nConversely, when you change the value of the MTEXT dimension, the")
  (princ "\ncircle radius will change.")
  (princ "\n\nRun DIMEX-TST command for test.")
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "DIMEX-TST" "DIMEX-INFO")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBF7k7Q1z/9S++F7kIp1senSDB74X6iWeRlaSrv
;;; Is6jeaCetCPZruy9dmaWevNnlgFRQsbP4iW8SUUMl0lmpC7wHRljg0qcnjyR6lPw
;;; VhC48OT2K3CtbTHhk0vDd8G8NOLsyMZOKqUSqK12fvqAAWakbXxNYpatlwvViGqT
;;; 9i77vss3OTA65Id+o2uervDl1dma6NoLKlvIel5zf/pJCjGPzuvHkZ1C0so9aP/2
;;; hPCTBO4r7WTaGA8t9cJ0dn89lcZI1mwlXKRkp5otIlLArC9swIvkApdwnpwM0e2Q
;;; mGvp3SlYtKwLK6U/A3cumJZwC3ynU9z6MZQbVTkcOe9u+pBeA7VsMzDQ+IuPK3c2
;;; +x3bU04XjDoafLjDMcPkTnrTc6fl3cTFJN+4p8MAq5c1rFh6EqTElJPzcpb06/S/
;;; DcfHTj1rwkGNxXU8yS68CxZl5US0AztyfEHS7oiS75E22t7BNvIEGNHWJuSjDyYb
;;; sO4qkxRTlkNOSzW92hiHiIuCbsYwqY/WGvt+5fvgMpOkwYfjjKiwjtq8siVYizDw
;;; Bp7aLAUBpDW8VLRr7EFxddDo5KCh1kunEg3x7nptdVJ1kYPiqKswRbVlzqQK+4e0
;;; A5YMFdiFWbygHG6oLa6/IXEslqppLJbWXn843sfdJsEIuOseizx1oQAa2gi0HmNI
;;; OnFJUA==
;;; -----END-SIGNATURE-----