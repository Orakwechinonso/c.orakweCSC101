;;;    $Header: //streams/main/develop/global/src/coreapps/lpp/Samples/color-util.lsp#1 $ $Change: 360915 $ $DateTime: 2013/02/11 08:55:58 $ $Author: ongk $
; $NoKeywords: $
;;;    Copyright (C) 2003 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file contains lisp utilities for dealing with true color in AutoCAD.
;;
;; AutoCAD describes true color with 24 bit integer values where 
;;  - blue is the first byte
;;  - green is the second and
;;  - red is the third byte
;;
;;  (Careful... This is not exactly a RGB value. It is a BGR.)
;;
;; Within the scope of this file, the term "TrueColor" refers to one of these 24 bit
;; values. The term "ColorIndex" will be used to refer to the traditional AutoCAD 
;; colors that range from 0 through 256 (where 0 and 256 have the special meanings of
;; "ByBlock" and "ByLayer" respectively)
;;
;;-------------------------------------
;; Entity lists and dxf group codes:
;;-------------------------------------
;; DXF
;;  62  - ColorIndex values are still represented with the 62 dxf group code (same as in 
;;        previous AutoCAD releases).
;;
;;  420 - TrueColor values are associated with the new 420 dxf group code.
;;        For example: You might find (420 . 255) within the entity list of a blue entity.
;;
;;  430 - Contains a color book and color name description string. This is used only for
;;        informational purposes. Actual entity color is governed by 62 and/or 420 group 
;;        codes.
;;
;;Entity lists...
;;
;; Entget:
;;  No 62 pair present - If no 62 group code is present then the object's color is assumed to be 
;;                       ByLayer. In other words; No behavior change from previous releases. 
;;                       (The 420 and 430 pairs will not be present either)
;;
;;  62 only            - Object's color is by ColorIndex (also refered to as ByACI)
;;
;;  62 and 420         - Object's color is TrueColor and is fully described by the 420 group code. 
;;                       The 62 pair will contain the colorindex that most closely matches the true 
;;                       color.
;;
;;  62, 420 and 430    - Object color was chosen from a color book. The 430 contains a textual description
;;                       of the form "'colorbook$colorname". The 420 pair describes the 
;;                       corresponding truecolor and the 62 contains the closest matching color 
;;                       index.
;;                       
;;
;; Entmod and Entmake: 
;;  The 420 group code takes precedence over the 62 group code. That is; entmake or entmod will 
;;  use the 420 group code if it is present and will ignore the 62 group code in the provided 
;;  entity list. If a 420 is _not_ present in the entity list, then the 62 group code will 
;;  be used. The 430 group code is for informational purposes only and does not have
;;  any effect on displayed color.
;;
;;-------------------------------------
;; Two new built in lisp functions:  Acad_TrueColorDlg and Acad_TrueColorCli
;;-------------------------------------
;;
;;  Acad_TrueColorDlg is somewhat similar to the existing acad_colordlg function. 
;;        Signature:
;;          (acad_truecolordlg color [allowbylayer] [currentlayercolor])
;;
;;            Where "color" is a dotted pair that describes the default color. The first element 
;;            of the dotted pair must be one of the color dxf group codes (62, 420, or 430). 
;;            For example:
;;              (62 . ColorIndex)
;;              (420 . TrueColor)
;;              (430 . "colorbook$colorname")
;;
;;            If the optional [allowbylayer] parameter is present and non-nil, then the bylayer and 
;;            byblock buttons will be enabled. If this parameter is missing, the value defaults to 
;;            T and the bylayer/bylbock buttons will be enabled.
;;
;;            The optional [curLayerColor] parameter controls the color of the bylayer/byblock color
;;            in the dialog. This parameter is also a dotted pair like the first color parameter.
;;            
;;            Returns nil if the user canceled.
;;            On success, the function returns a list of one or more dotted pairs that fully describe the 
;;            chosen color. The last dotted pair in this list indicates, specifically, the color that was 
;;            chosen. Additional details on the return list follow...
;;
;;            ColorBook color - If the last item in the returned list is a 430 pair, then the specified 
;;                              color originates from a colorbook. (This returned list will also contain
;;                              a 420 pair that describes the corresponding truecolor and a 62 pair that 
;;                              describes the closest matching colorindex value.)
;;
;;            True Color      - If the returned list contains a 420 pair as the last item, then
;;                              a true color was specified (as "Red,Green,Blue"). The list will
;;                              also contain a 62 pair that indicates the closest matching colorindex.
;;                              (No 430 pair will be present)
;;
;;            Color Index     - If the last item in the list is a 62 pair, then a colorindex was chosen.
;;                              (no other dotted pairs will be present in the returned list)
;;
;;         Examples of usage: 
;;                 (acad_truecolordlg '(62 . 1))
;;                 (acad_truecolordlg '(420 . 2686760))
;;                 (acad_truecolordlg '(430 . "Color Book Sample File$Sample 0C"))
;;                 (acad_truecolordlg '(420 . 2686760) nil)
;;
;;            
;;
;;  Acad_TrueColorCli is the command line counterpart to acad_truecolordlg. It prompts at the command line 
;;                    for a color.
;;        Signature:
;;          (acad_truecolorcli color [allowbylayer] [alternatePrompt])
;;
;;            The first two parameters are identical to acad_truecolordlg, and the third parameter
;;            is an optional prompt string. If this string is omited, the default value is "New color ".
;;
;;            Some examples of lists returned by acad_truecolordlg and acad_truecolorcli:
;;             Color book color:  ((62 . 80) (420 . 2686760) (430 . "Color Book Sample File$Sample 0B"))
;;                   True color:  ((62 . 162) (420 . 2114527))
;;                  color index:  ((62 . 110))
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the red component of a TrueColor number
;;
(defun TrueColor-red-value ( c / )
 (lsh (fix c) -16)
);defun TrueColor-red-value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the green component of a TrueColor number
;;
(defun TrueColor-green-value ( c / r )
 (lsh (lsh (fix c) 16) -24)
);defun TrueColor-green-value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the blue component of a TrueColor number
;;
(defun TrueColor-blue-value ( c / )
 (lsh (lsh (fix c) 24) -24)
);defun TrueColor-blue-value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TrueColor-split - takes a TrueColor number and returns a list of integers the form '(red green blue)
;;
(defun TrueColor-split ( c / )
 (list (lsh (fix c) -16)
       (lsh (lsh (fix c) 16) -24)
       (lsh (lsh (fix c) 24) -24)
 )
);defun TrueColor-split

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TrueColor-make - takes TrueColor components (red green blue) and creates a single TrueColor integer
;;
(defun TrueColor-make ( r g b /  )
 (+ (lsh (fix r) 16) 
    (lsh (fix g) 8) 
    (fix b)
 )
);defun TrueColor-make

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TrueColor-to-ColorIndex - takes a TrueColor value and returns the closest AutoCAD color index (1-255)
;; NOTE: This is an approximation. The return color index is the AutoCAD color index that most 
;;       closely matches the provided TrueColor.
;;
(defun TrueColor-to-ColorIndex ( TrueColor / colorObj ci )
 (vl-load-com)
 (and (setq colorObj (vla-getinterfaceobject (vlax-get-acad-object) "AutoCAD.AcCmColor.16"))
      (not (vl-catch-all-error-p 
            (vl-catch-all-apply 'vla-setRGB (cons colorObj (TrueColor-split TrueColor)))
           )
      )
      (setq ci (vla-get-ColorIndex colorObj))
 );and
 ci
);defun TrueColor-to-ColorIndex

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ColorIndex-to-TrueColor - takes an AutoCAD color index (1 through 255) and returns the equivalent 
;; TrueColor value.
;;
(defun ColorIndex-to-TrueColor ( ci / colorObj TrueColor )
 (vl-load-com)
 (and (setq colorObj (vla-getinterfaceobject (vlax-get-acad-object) "AutoCAD.AcCmColor.16"))
      (>= ci 1)
      (<= ci 255)
      (not (vl-catch-all-error-p 
            (vl-catch-all-apply 'vla-put-ColorIndex (list colorObj ci))
           )
      )
      (setq TrueColor (TrueColor-make 
                          (vla-get-red   colorObj)
                          (vla-get-green colorObj)
                          (vla-get-blue  colorObj)
                      )
      );setq
 );and
 TrueColor
);defun ColorIndex-to-TrueColor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; colorname-to-truecolor - converts a string of the form "colorbook$colorname" to a TrueColor value.
;;
(defun colorname-to-truecolor ( colorbookandname / pos colorbook colorname colorObj TrueColor )
 (vl-load-com)
 (and (equal (type colorbookandname) 'STR)
      (setq pos (vl-string-search "$" colorbookandname))
      (setq colorbook (substr colorbookandname 1 pos)
            colorname (substr colorbookandname (+ pos 2))
      );setq
      (setq colorObj (vla-getinterfaceobject (vlax-get-acad-object) "AutoCAD.AcCmColor.16"))
      (not (vl-catch-all-error-p 
            (vl-catch-all-apply 'vla-SetColorBookColor (list colorObj colorbook colorname))
           )
      )
      (setq TrueColor (TrueColor-make 
                          (vla-get-red   colorObj)
                          (vla-get-green colorObj)
                          (vla-get-blue  colorObj)
                      )
      );setq
 );and
 TrueColor
);defun colorname-to-truecolor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TrueColor-to-RGBstring - Takes a TrueColor (24 bit value) and returns a string of the 
;;                          form: "Red,Green,Blue"
;;
(defun TrueColor-to-RGBstring ( TrueColor / rgb )
 (and truecolor
      (setq rgb (mapcar 'itoa (truecolor-split truecolor)))
      (setq rgb (strcat (car rgb) "," (cadr rgb) "," (caddr rgb)))
 )
 rgb
);defun TrueColor-to-RGBstring

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ssget-ColorIndex-filter - 
;; Takes an AutoCAD color index  (0 through 256) and 
;; returns a dxf group code list that can be used with (ssget ...) to
;; select all objects that "exactly" match the specified AutoCAD ColorIndex.
;; This allows you to filter for objects that are of a specific AutoCAD
;; ColorIndex (0-256) but are not assigned a true-color value.
;; 
;; For example:
;;  The following will return a selection set that contains "red" objects
;;  _and_ objects that are assigned a true color that is merely "close" to red.
;;
;;   (setq ss (ssget "_x" '((62 . 1))))
;;
;; If you do not want true-color objects and only want to select objects that are
;; explicitly set to color number 1 ("red") then you can use the following code.
;; This will return a selection set of all non-true-color entities that are color 1.
;;
;;   (setq ss (ssget "_x" (ssget-ColorIndex-filter 1)))
;;
;;
;;
(defun ssget-ColorIndex-filter ( ColorIndex / )
 (list (cons 62 ColorIndex) '(-4 . "<NOT") '(-4 . "*") '(420 . 0) '(-4 . "NOT>"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entlist-set-ColorIndex
;; Takes an entity list and an AutoCAD ColorIndex (0 through 256)
;; Returns the entity list with the specified color setting applied.
;; This returned list can then be used with (entmake ...) or (entmod ...).
;; NOTE: This function does _not_ call entnake or entmod. It only manipulates 
;;       the provided list and returns the resulting list.
;;
;; Backround:
;;  If a 420 group code pair (true color) is present in an entity list, entmake and entmod
;; will ignore the 62 group code (AutoCAD ColorIndex). In order to set a color index
;; using entmod or entmake, the provided list must not contain a 420 group code.
;;  This function allows you to apply a specific AutoCAD ColorIndex value to an entity list
;; by removing the 420 group code (if present) and inserting the specified ColorIndex with
;; a 62 group code pair.
;;
;; (provide a ColorIndex of 0 for ByBlock and 256 for ByLayer)
;;
(defun entlist-set-ColorIndex ( entlist ColorIndex / ci )
  (setq entlist (vl-remove (assoc 420 entlist) entlist))
  (if (setq ci (assoc 62 entlist))
      (setq entlist (subst (cons 62 ColorIndex) ci entlist))
      (setq entlist (append entlist (list (cons 62 ColorIndex))))
  );if
);defun entlist-set-ColorIndex


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgC6T06H7GLAYkDCkIzOs6kL2MAqt0dW5vvtTGKM
;;; 4xkP1dWkgUFjXc1XgQlh5gArUO0qB+pGN3m5dwI0bW9iLeh38G9U1NJYKfBk0GIF
;;; syGZCxYF/rvBG77wBrP1yc4e28qe4+dnzyETL6kAixahA23+by6NPMP93NkWbqDK
;;; bDhU2/E6hmCxpBEULgjd0r/tjR0oUUZdzmk0OH4V6lR7VJfiRDaaWrnx1TaUq6PO
;;; B9MdceP9/6hr/6IrtxFjlSpYqJorfZZsHBfsTovgwJGlWn5THITbkZ5vWH+F14jJ
;;; zldcC/dpNrqfiKSB4ydufjxOZNFkooOtwTF3IhwHP4SXa6mUR4eGb2cyUzLpHXr6
;;; cNEmKvc6RAc9iTr30I45ZXMP6w+LO/BkCVugqbrx2XS2DADGF1uRJlveYgyGdvcg
;;; MnXCfjMyNKb/PDMNhpCchUnTt9o2VbYBYj5daBg/1lyHfFrCN+OYbbtdnnlPqPAg
;;; 1L698c52bpIZZYQ2DJirXFqkJvF/9FzJyWlIKcV8vLV2MQhO8tgKZ5OA8YIROISz
;;; /N1ysYbc7Uvb6yx/88GGIrhiCFbOLCenjSZyH9kDJFeUK3F3J/Ahg2/jLyKcK6wB
;;; iRkJq6ZOdI1FyvRNHeHyKKobuqLZD5TaeVNVxH+NI7/984p1ymv0T+B2zSEbZVGS
;;; 5k0H3g==
;;; -----END-SIGNATURE-----