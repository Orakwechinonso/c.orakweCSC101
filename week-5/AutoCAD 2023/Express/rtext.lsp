;;
;;  rtext.lsp - RText acquisition and editing
;;
;;  Copyright © 1999 by Autodesk, Inc.
;;
;;  Your use of this software is governed by the terms and conditions of the
;;  License Agreement you accepted prior to installation of this software.
;;  Please note that pursuant to the License Agreement for this software,
;;  "[c]opying of this computer program or its documentation except as
;;  permitted by this License is copyright infringement under the laws of
;;  your country.  If you copy this computer program without permission of
;;  Autodesk, you are violating the law."
;;
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;  DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;  UNINTERRUPTED OR ERROR FREE.
;;
;;  Use, duplication, or disclosure by the U.S. Government is subject to
;;  restrictions set forth in FAR 52.227-19 (Commercial Computer
;;  Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;  (Rights in Technical Data and Computer Software), as applicable.
;;
;;  ----------------------------------------------------------------
;;
;;  DESCRIPTION
;;  RText creation and editing.
;;
;;  ----------------------------------------------------------------
 
;;
;;  RTEXT - insert RText object
;;
(defun C:RTEXT (/ fetch ans def tStyle tSize tRot done)
 
  (defun fetch (mode / cont lay vc)
    ;;  start from view center
    (setq vc (getvar "VIEWCTR"))
    ;;  pick text or file
    (if (setq cont (if (= mode 0)
                 (acet-ui-getfile "Select text file"
                              (acet-filename-ext-remove (getvar "DWGNAME"))
                              "txt;*" "Acet:RText" 1664)
                 (AcetRText:pickText "") ) )
      (progn
        ;;  create object
        (entmake (list '(0 . "RTEXT")
                       '(100 . "AcDbEntity")
                       '(100 . "RText")
                        (cons 10 (trans vc 1 0))
                        (cons 40 tSize)
                        (cons 7 tStyle)
                        (cons 50 tRot)
                        (cons 1 cont)
                        (cons 70 mode)
                        (cons 210 (acet-geom-cross-product (getvar "UCSXDIR")
                                                           (getvar "UCSYDIR") ) )
                 )
        )
        ;;  move to requested location
        (setq lay (AcetRText:unlockLayer nil))
        (princ "\nSpecify start point of RText: ")
        (command "_.MOVE" (entlast) "" vc pause)
        (while (wcmatch (getvar "CMDNAMES") "*MOVE*")
          (command pause)
        )
        (AcetRText:modify (entlast) T)
        (AcetRText:lockLayer lay)
      )
    )
  )
 
  (if (AcetRText:appload)
    (progn
      (acet-error-init '(("CMDECHO" 0  "LIMCHECK" 0  "ORTHOMODE" 0) T))
 
      (setq tStyle (getvar "TEXTSTYLE")
            tSize (getvar "TEXTSIZE")
            tRot (AcetRText:defRotation nil)
            def (getenv "AcetRText:type") )
      (if (or (not def)
              (and (/= "Diesel" def)
                   (/= "File" def) ) )
        (setenv "AcetRText:type" (setq def "File"))
      )
 
      (while (not done)
       (princ (acet-str-format "\nCurrent settings: Style=%1  Height=%2  Rotation=%3\n"
                               tStyle
                               (ai_rtos tSize)
                               (angtos tRot) ) )
        (initget "Style Height Rotation File Diesel _Style Height Rotation File Diesel")
        (setq ans (getkword (acet-str-format "Enter an option [Style/Height/Rotation/File/Diesel] <%1>: " def))
              ans (if ans ans def) )
        (cond
          ((= ans "Style")
            (setq tStyle (AcetRText:pickStyle tStyle))
            (setvar "TEXTSTYLE" tStyle)
          )
          ((= ans "Height")
            (setq tSize (AcetRText:pickHeight tSize))
            (setvar "TEXTSIZE" tSize)
          )
          ((= ans "Rotation")
            (setq tRot (AcetRText:pickRotation tRot))
            (AcetRText:defRotation tRot)
          )
          ((= ans "File")
            (fetch 0)
            (setq done T)
            (setq def "File")
          )
          ((= ans "Diesel")
            (fetch 1)
            (setq done T)
            (setq def "Diesel")
          )
        )
      )
      (setenv "AcetRText:type" def)
 
      (acet-error-restore)
    )
  )
)
 
 
;;
;;  RTEDIT - edit RText object
;;
(defun C:RTEDIT (/ ename)
  (if (AcetRText:appload)
    (progn
      (acet-error-init '(("CMDECHO" 0) T (if ename (redraw ename 4))))
 
      ;;  select one RText object, not on locked layer
      (if (setq ename (acet-ui-single-select '((0 . "RTEXT")) nil))
        (AcetRText:modify ename nil)
      )
 
      (acet-error-restore)
    )
  )
)
 
 
;;
;;  RTEXTAPP - set RText editing app
;;
(defun C:RTEXTAPP (/ app new)
  (if (AcetRText:appload)
    (progn
      (acet-error-init nil)
      ;;  locate current value
      (if (not (setq app (getenv "AcetRText:editor")))
        (setq app "") )
      ;;  pick new value
      (setq new (getstring T (acet-str-format
              "\nEnter RText editing application or . for system default <\"%1\">: "
              app ) ) )
      ;;  validate clear and default
      (cond
        ((= new ".")
          (setq new "") )
        ((= new "")
          (setq new app) )
      )
      ;;  set new value
      (setenv "AcetRText:editor" new)
      (acet-error-restore)
    )
  )
  (princ)
)
 
 
;;  verify that required apps are loaded
(defun AcetRText:appload ()
  (if (member nil (mapcar
                   '(lambda (x) (if (member x (arx)) T (arxload x nil)))
                   '("acetutil.arx" "rtext.arx") ) )
    (alert (acet-str-format "Could not load required ObjectARX modules."))
    T
  )
)
 
 
;;  modify RText object
(defun AcetRText:modify (ename setdef / ent lay tStyle tSize tRot ans done xv)
  (if (not ename)
    ;;  select one RText object, locked layer OK
    (setq ename (acet-ui-single-select '((0 . "RTEXT")) T))
  )
  (if (and ename
           (setq ent (entget ename)) )
    (progn
      (redraw ename 3)
      (setq lay (AcetRText:unlockLayer (cdr (assoc 8 ent)))
            xv (cdr (assoc 210 ent))
            tStyle (cdr (assoc 7 ent))
            tSize (cdr (assoc 40 ent))
            tRot (acet-geom-angle-trans (cdr (assoc 50 ent)) xv 1)
      )
      (while (not done)
        (princ (acet-str-format
                   "\nCurrent values: Style=%1  Height=%2  Rotation=%3\n"
                   tStyle (ai_rtos tSize) (angtos tRot) ) )
        (initget "Style Height Rotation Edit _Style Height Rotation Edit")
        (if (setq ans (getkword "Enter an option [Style/Height/Rotation/Edit]: "))
          (progn
            (cond
              ((= ans "Style")
                (setq tStyle (AcetRText:pickStyle tStyle))
                (setq ent (subst (cons 7 tStyle) (assoc 7 ent) ent))
                (redraw ename 4)
                (entmod ent)
                (entupd ename)
                (princ " ")
                (redraw ename 3)
                (if setdef
                  (setvar "TEXTSTYLE" tStyle)
                )
              )
              ((= ans "Height")
                (setq tSize (AcetRText:pickHeight tSize))
                (setq ent (subst (cons 40 tSize) (assoc 40 ent) ent))
                (redraw ename 4)
                (entmod ent)
                (entupd ename)
                (princ " ")
                (redraw ename 3)
                (if setdef
                  (setvar "TEXTSIZE" tSize)
                )
              )
              ((= ans "Rotation")
                (setq tRot (AcetRText:pickRotation tRot))
                (setq ent (subst (cons 50 (acet-geom-angle-trans tRot 1 xv))
                                 (assoc 50 ent)
                                 ent ) )
                (redraw ename 4)
                (entmod ent)
                (entupd ename)
                (princ " ")
                (redraw ename 3)
                (if setdef
                  (AcetRText:defRotation tRot)
                )
              )
              ((= ans "Edit")
                (AcetRText:edit ent)
                (entupd ename)
                (redraw ename 4)
                (entupd ename)
                (princ " ")
                (redraw ename 3)
                (setq ent (entget ename))
              )
            )
          )
          (setq done T)
        )
      )
 
      (AcetRText:lockLayer lay)
      (redraw ename 4)
    )
    (princ "\nNothing found")
  )
)
 
 
;;  edit given RText object
(defun AcetRText:edit (ent / cont dsl app mode)
  (setq cont (cdr (assoc 1 ent))
        mode (cdr (assoc 70 ent))
  )
 
  (if (and (/= 1 (logand 1 mode))
           (not (findfile cont)) )
    (progn
      (alert (acet-str-format "Cannot find RText file: %1" cont))
      (setq cont (acet-ui-getfile "Select text file" cont "txt;*" "Acet:RText" 1664) )
      (if (and cont
               (findfile cont) )
        (entmod (subst (cons 1 cont) (assoc 1 ent) ent))
      )
    )
    (progn
      (if (and (= 1 (logand 1 mode))
               (setq dsl (AcetRText:pickText cont)) )
        (entmod (subst (cons 1 dsl) (assoc 1 ent) ent))
        (progn
          (if (= 1 (logand 1 mode))
            (setq dsl "")
          )
        )
      )
    )
  )
 
  (if (not dsl)
    (progn
      ;;  pick editing app
      (if (or (not (setq app (getenv "AcetRText:editor")))
              (= app "") )
        (setq app (acet-filename-associated-app cont))
      )
      ;;  use Notepad if nothing else found
      (if (not app)
        (setq app "Notepad")
      )
      ;;  run editor
      (if (/= -1 (acet-sys-spawn 1 app cont))
        (entupd (cdr (assoc -1 ent)))
        (princ (acet-str-format "\nEditor failure."))
      )
    )
  )
 
  (princ "\nUpdates may not be apparent until next regen.")
)
 
 
;;  pick text height
(defun AcetRText:pickHeight (hgt / work a b)
  (setq hgt (if hgt hgt (getvar "TEXTSIZE"))
        work (ai_rtos hgt) )
 
  (initget 6)
  (setq work (getdist (acet-str-format "\nSpecify height <%1>: " work)))
  (if work work hgt)
)
 
 
;;  pick text style
(defun AcetRText:pickStyle (def / showStyles new done)
 
  (defun showStyles (/ pat sty)
    (setq pat (getstring T
                  (acet-str-format "\nEnter text style(s) to list <*>: ") )
          pat (if (= "" pat) "*" (xstrcase pat)) )
    (while (setq sty (tblnext "STYLE" (not sty)))
      (if (and (not (wcmatch (cdr (assoc 2 sty)) "*|*"))
               (wcmatch (cdr (assoc 2 sty)) pat) )
        (progn
          (princ (acet-str-format
                     "Style name: %1   Font files: %2\n"
                     (cdr (assoc 2 sty))
                     (cdr (assoc 3 sty)) ) )
          (princ (acet-str-format
                     "   Height: %1  Width factor: %2  Obliquing angle: %3\n"
                     (cdr (assoc 40 sty))
                     (cdr (assoc 41 sty))
                     (angtos (cdr (assoc 50 sty))) ) )
          (princ (acet-str-format "   Generation: %1\n\n"
                     (if (= 0 (cdr (assoc 70 sty)))
                       "Normal"
                       (acet-str-format "%1%2%3"
                         (if (/= 0 (logand 2 (cdr (assoc 71 sty))))
                             "Backwards " "")
                         (if (/= 0 (logand 4 (cdr (assoc 71 sty))))
                             "Upside-down " "")
                         (if (/= 0 (logand 4 (cdr (assoc 70 sty))))
                             "Vertical" "")
                       )
                     )
                  )
          )
        )
      )
    )
  )
 
  (if (and def
           (setq def (xstrcase def))
           (tblsearch "STYLE" def) )
    (setq new def)
    (setq new (getvar "tStyle")
          def new )
  )
 
  (while (not done)
    (setq new (getstring T (acet-str-format
                           "\nEnter name of text style or [?] <%1>: " new))
          new (if (= "" new) def (xstrcase new)) )
    (if (tblsearch "STYLE" new)
      (setq done T)
      (if (= new "?")
        (showStyles)
        (princ (acet-str-format "\nCannot find text style \"%1\"." new))
      )
    )
  )
 
  new
)
 
 
;;  pick text rotation
(defun AcetRText:pickRotation (ang / def)
  (setq def (if ang ang (AcetRText:defRotation nil))
        ang (getangle (acet-str-format
                "\nSpecify rotation angle of RText <%1>: " (angtos def)))
  )
  (if ang ang def)
)
 
 
;;  get/set default Text rotation
(defun AcetRText:defRotation (ang / lay ent)
  (setq ang (if ang (angtos ang (getvar "AUNITS") 8) "")
        ent (entlast) )
 
  ;;  unlock CLAYER if locked
  (setq lay (AcetRText:unlockLayer nil))
 
  ;;  insert dummy Text object to set internal default rotation
  (command "_.TEXT" "0,0")
  ;;  extra arg required if zero text height
  (if (= 0.0 (cdr (assoc 40 (entget (tblobjname "STYLE" (getvar "TEXTSTYLE"))))))
    (command "") )
  (command ang ".")
 
  ;;  check rotation on Text object
  (if (/= ent (entlast))
    (progn
      (setq ang (cdr (assoc 50 (entget (entlast)))))
      (entdel (entlast))
    )
    (setq ang 0.0)
  )
 
  ;;  re-lock CLAYER if necessary
  (AcetRText:lockLayer lay)
 
  ang
)
 
 
;;  unlock layer if locked, returns layer or nil
(defun AcetRText:unlockLayer (lay / lck)
  ;;  use CLAYER if none given
  (setq lay (if lay lay (getvar "CLAYER"))
        lck (acet-layer-locked lay) )
  (if lck
    (progn
      (command "_.-LAYER" "_UNLOCK" lay "")
      lay
    )
    nil
  )
)
 
 
;;  lock layer if previously locked
(defun AcetRText:lockLayer (lay)
  (if lay
    (command "_.-LAYER" "_LOCK" lay "")
  )
)
 
 
;;  pick contents
(defun AcetRText:pickText (str / work)
  (if (or (= 0 (getvar "CMDDIA"))
          (= 4 (logand 4 (getvar "CMDACTIVE"))) )
    (progn
      (setq str "")
      (while (/= work "")
        (setq work (getstring T (acet-str-format "\nEnter RText: ")))
        (if (/= work "")
          (setq str (strcat str (if (< 0 (strlen str)) "\r\n" "") work))
        )
      )
      str
    )
    (acet-ui-txted str "Edit RText")
  )
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAxUSZfkO8A6qMQ3+rSES1cQRnEhemE2gAA/EN1
;;; czQKD2BuPJwb/zJ/a9Op3lYDVP0lr/EfFMtjVZWUG9dyPCJfW3NB4G+O1hlpTDVH
;;; JKtgyHff3hYpsGYDqAPAtwhsrQ+cmIZaP9MG0vDTkPM7IFBFk6WydJYehOPvnGsT
;;; S4IBcEPKgbtMVY3J5S2pBuc1WQUd5V2Ev4JlubOgGpd49V54d+YeBt6HrGlDE1JQ
;;; jwYlG5ssWgb8ub8ds1ScOXEXthAm7tiID4RDCGaUrpw1nS1irlbUTQ9sQYqePHN3
;;; Vz/vzJurcarTkeTZCgwVfAH4ba2HU5mnbOBs/4Sss8blxLZ/iSGGICmjQRICxB9q
;;; dCQwDDtPWiXqrIY+oyjxkMMjSMJbew8cjX3R4LTUm5u/+XCRd5q8HkcmYYQ5a3nn
;;; g1PBb4nR1o5nlM//Fl6ejRwIJj9feUQwh3E+P9oLdX2UB1LGVWR0V/rxQEo+734R
;;; 6eTNuBVlP5heiMUtZdbjOfeYhX6JnD9K/AdCqNbW5rHoGKtfpGxWwfntcruvNPTI
;;; cWRYO+yOKVtCZGw4wNZO4wgYssq/1lwNjPCTq2P17z7exc6EG3fXI0mBWZV04qvQ
;;; xVhVw5Rp8/0qllapeOZvnshn5oQzkpxHQQppBpURvdRQJbWMB1vJH5dtSkMewt49
;;; hcVu3g==
;;; -----END-SIGNATURE-----