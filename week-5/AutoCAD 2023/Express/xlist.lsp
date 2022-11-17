;;;
;;;    XLIST.LSP
;;;    Copyright © 1999-2003 by Autodesk, Inc.
;;;
;;;    Your use of this software is governed by the terms and conditions of the
;;;    License Agreement you accepted prior to installation of this software.
;;;    Please note that pursuant to the License Agreement for this software,
;;;    "[c]opying of this computer program or its documentation except as
;;;    permitted by this License is copyright infringement under the laws of
;;;    your country.  If you copy this computer program without permission of
;;;    Autodesk, you are violating the law."
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;  ----------------------------------------------------------------
 
;;;  DESCRIPTION
;;;
;;;   XLIST
;;;   This routine lists certain properties of objects that are nested in
;;;   a block or xref.
;;;
;;;   (c:xlist) and (c:-xlist) call (XLIST) with a boolean kUseDialog turned on
;;; to use the dialog display or off for listing at the command line, respectively.
;;;
;;;   (XLIST)
;;;   This is the main function called from the command line.  This function
;;;   prompts the user to pick an object, determines if the selection is valid,
;;;   and if it is, calls GetList().  After calling this function, it calls (DisplayDialog)
;;;   if kUseDialog is true or (DisplayList) if it is false.
;;;
;;;   (GetList)
;;;   This function take the object handle as an argument and parses out the assoc
;;;   codes we are interested in, makes adjustments for the optional codes, converts
;;;   color numbers 1 through 8 back to their color names.  It calls a "vertex" a polyline
;;;   and an "insert" a block.
;;;
;;;   (DisplayDialog)
;;;   It loads XLIST.DCL and sets the keys to the results from GetList() and
;;;   invokes the appropriate dialog to display the results.  I have defined three
;;;   different dialogs depending on the type of object: a dialog to display blocks,
;;;   one for text and one for everything else.
;;;
;;;   (DisplayList)
;;;   Invokes the text screen and displays the results in list format.
;;;
;;;---------------------------------------------------------------------------;
 
;;(defun xlist_err ( /  )
(defun xlist_err (s)
  (setq *error* old_err)
  (command "_.undo" "_end")
  (if old_cmd (acet-set-CmdEcho old_cmd)) ; restore CMDECHO
  (princ)
  ;(princ "xlist_err was called.")
); exit quietly
 
(defun GetList ( / iNest eList   )
  (setq iNest (length (last ePick)))
 
;The next if statement handles block within blocks. iNest = 1 means no nesting. Since (nentsel) goes all the
;way to the root AutoCAD object we have to traverse back up to the top level of the nesting to get a block name.
  (if (= iNest 1)
    (setq eList (entget (car ePick))) ;then pull the list from the standard nentsel call.
    (setq eList (entget (nth (- iNest 2) (last ePick))))  ;else last last our way back up to the top block definition
  );end if
 
 
;Pull out the assoc codes.
  (setq sLayer (cdr (assoc 8 eList))
                sObjectType (cdr (assoc 0 eList))
        sLineType (cdr (assoc 6 eList))     ; This is optional, we check for it later.
                sColor (cdr (assoc 62 eList))
    sBlockname ""
    sStyleName ""
      ); end setq
 
 
;Check for no linetype override, in which case it is bylayer.
      (if (= sLineType nil) (setq sLineType "Bylayer"))   ;Tidy up the optional DXF codes for linetype
 
 
;If the object is a vertex, call a vertex a polyline
      (if (= "VERTEX" sObjectType) (setq sObjectType "POLYLINE"))
 
;If the object is a block, call an insert a block and find out the block name
  (if (= "INSERT" sObjectType)
    (progn
      (setq   sObjectType "BLOCK"
         sBlockname (cdr (assoc 2 eList))
      )
    );end progn
  );end if
 
;If the object is text or mtext, find out the style name
  (if (or (= "TEXT" sObjectType) (= "MTEXT" sObjectType))
    (setq sStyleName (cdr (assoc 7 eList)))
  );end if
 
; Sort out the colors and assign names to the first 8 plus bylayer and byblock
      (cond ( (= nil sColor) (setq sColor "Bylayer"))
          ( (= 0 sColor) (setq sColor "Byblock"))
            ( (= 1 sColor) (setq sColor "Red"))
            ( (= 2 sColor) (setq sColor "Yellow"))
    ( (= 3 sColor) (setq sColor "Green"))
            ( (= 4 sColor) (setq sColor "Cyan"))
            ( (= 5 sColor) (setq sColor "Blue"))
          ( (= 6 sColor) (setq sColor "Magenta"))
            ( (= 7 sColor) (setq sColor "White"))
            ( (= 256 sColor) (setq sColor "Bylayer"))
            (t (setq sColor (itoa sColor)))
      );end cond
 
;(princ (strcat sLayer sColor sObjectType sLinetype sThickness sStylename sBlockname))  ; for debugging purposes
 
); End GetList
 
 
;This fucntion displays the results in LIST form...
(defun DisplayList (  / )
  (textscr)
  (cond
    ((= "BLOCK" sObjectType)
      (princ    (acet-str-format  "\n\tObject:\t%1\n\tBlock name:\t%2" sObjectType sBlockname )
      );end princ
 
    );end this condition
    ((or (= "TEXT" sObjectType) (= "MTEXT" sObjectType))
      (princ (acet-str-format  "\n\tObject:\t%1\n\tStyle name:\t%2" sObjectType sStylename )
      );end princ
 
    );end this condition
    ( T
      (princ (acet-str-format  "\n\tObject:\t%1" sObjectType));end princ
    );end this condition
  ); end cond
 
  (princ (acet-str-format "\n\tLayer:\t\t%1\n\tColor:\t\t%2\n\tLinetype:\t%3" sLayer sColor sLinetype ) )
 
);end DisplayList
 
 
 
;This function displays the results in dialog form...
 
;Findfile for the dialog in case it isn't in the bonus/lisp/ directory
(defun DisplayDialog ( /  sAcad sAcadPath sDlgNameAndPath dcl_id  )
 
  (setq sAcad (findfile "acad.exe"))
  (setq sAcadPath (substr sAcad 1 (- (strlen sAcad) 8) ))
 
  (if (< (setq dcl_id (load_dialog (getfileET "xlist.dcl"))) 0)
  (progn
    (if (not (setq sDlgNameAndPath (findfileET "xlist.dcl")))
    (progn
      (alert  "Can't locate dialog definition file XLIST.DCL.\nCheck your support directories.")
      (exit)
    );end progn
    );end if
  );end progn
  );end if
 
;Load the dialog.  If the object is a block, load the block dialog; if it is a text entity, load the text dialog.
  (cond
    ((= "BLOCK" sObjectType) (if (not (new_dialog "xlistblock" dcl_id)) (EXIT)))
      ((or (= "TEXT" sObjectType) (= "MTEXT" sObjectType))(if (not (new_dialog "xlisttext" dcl_id)) (EXIT)))
    ( T (if (not (new_dialog "xlist" dcl_id)) (EXIT) ))
  ); end cond
        (set_tile "sLayer" (strcase sLayer T))
        (set_tile "sColor" sColor)
        (set_tile "sObjectType"  sObjectType )
        (set_tile "sLineType" sLineType )
  (set_tile "sBlockname" sBlockname)
  (set_tile "sStyleName" sStyleName)
 
;If we can't starts the dialog, then bail.
        (if   (= (start_dialog) nil)  (exit));
  (unload_dialog dcl_id);
  (princ)
 
); end DisplayDialog
 
 
 
(defun XLIST (  kUseDialog /  sLayer sObjectType sLineType sColor sBlockname
                              sStyleName ePick old_cmd old_err)
;capture existing settings
    (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
        old_err *error*
    *error* xlist_err
    )
 
  (acet-set-CmdEcho 0)                   ;;;turn command echo off

  (command "_.undo" "_be")
 
;The next while loop checks for null or invalid selection.
  (while (or
    (not (setq ePick (nentsel "\nSelect nested xref or block object to list: ")))
    (< (length ePick) 3)
    );end or
    (progn  (princ "\nObject was invalid or was not selected."))
  );end while
 
;Extract the information...
      (GetList)
 
;If we are calling from "xlist" use the dialog, else display at command line...
  (if kUseDialog (DisplayDialog) (DisplayList))
  (setq *error* old_err)
  (command "_.undo" "_end")
  (acet-set-CmdEcho old_cmd)                   ;;;restore CMDECHO
  ; princ "normal exit called.")
  (princ)
)
 
(defun c:-xlist ( / kUseDialog )
  (setq kUseDialog nil)
  (xlist kUseDialog)
)
(defun c:xlist ( / kUseDialog )
  (setq kUseDialog T)
  (xlist kUseDialog)
)


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAFf1vdiWiCLkZJegjDX/CDwzmXKDWSe8BZLxQg
;;; sVKu2+we/jJ+quUrgnlV1R5i3knV6pIN5WG1L9kSUJoGmzOz7uSsJmmCFzyv8GDU
;;; iHb0e4rvY15z3KUg7eeAAdbBIEbz8jRONThplQexFMnHlTFGTLcegK1wFcEKX43x
;;; p8FA/uigrnEPDQXxKjGkMMBss1KZQ5923bl+6CrqFZVTMSQPkxmfJmlGz9eXMc1N
;;; PUcLFwR2Qcn6dEQsC9RSNAqPBLyhPLv5/CbAwkhxVNhz3c11PKKy7XY8dIf0yfSx
;;; v5izX2G2NvBu2EiXjb2GLHIb6ofANkrV+IK4Qt7+1bW+QW69gQqbkX81n1wyir9Z
;;; k9vykugdYSfsXpA4SqppgDzPvV0vf7CvipNfJ3zoPlfesxMFfEytaN2VUnabiAi5
;;; qKp6lZGRXh9FlyjiFdSll68EaaJyR7EWz+JKMRSgL+xD4KyI42nPHgnJ4+lwvU52
;;; kfFWPPaDYV/kj4wnc1tmeQrdfQIpCGr0WveQd0YzAy0RILNvaoWzjMG1PUCaSC2h
;;; joxyC9soEOOfi7tmvf/+wHNmzTtCsM5ZPGpGfyVqpYAb5++gk7bCl4QRqeNgXj/U
;;; yzUQZ4kUyxxVdPfTCXfICi/b22n4MEI19kIgUl2VwaHMAXmMu2BdidsphwCdwbNF
;;; G/wO9Q==
;;; -----END-SIGNATURE-----