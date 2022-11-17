;;
;;;
;;;    SSX.LSP
;;;    Copyright © 1999 by Autodesk, Inc.
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
 
;;; DESCRIPTION
;;;                              SSX.LSP
;;;
;;;   "(SSX)" -  Easy SSGET filter routine.
;;;
;;;   Creates a selection set.  Either type "SSX" at the "Command:" prompt
;;;   to create a "previous" selection set or type "(SSX)" in response to
;;;   any "Select objects:" prompt.  You may use the functions "(A)" to add
;;;   entities and "(R)" to remove entities from a selection set during
;;;   object selection.  More than one filter criteria can be used at a
;;;   time.
;;;
;;;   SSX returns a selection set either exactly like a selected
;;;   entity or, by adjusting the filter list, similar to it.
;;;
;;;   The initial prompt is this:
;;;
;;;     Command: ssx
;;;     Select object/<None>: (RETURN)
;;;     >>Block name/Color/Entity/Flag/LAyer/LType/Pick/Style/Thickness/Vector:
;;;
;;;   Pressing RETURN at the initial prompt gives you a null selection
;;;   mechanism just as (ssx) did in Release 10, but you may select an
;;;   entity if you desire.  If you do so, then the list of valid types
;;;   allowed by (ssget "x") are presented on the command line.
;;;
;;;     Select object/<None>:  (a LINE selected)
;;;     Filter: ((0 . "LINE") (8 . "0") (39 . 2.0) (62 . 1) (210 0.0 0.0 1.0))
;;;     >>Block name/Color/Entity/Flag/LAyer/LType/Pick/Style/Thickness/Vector:
;;;
;;;   At this point any of these filters may be removed by selecting the
;;;   option keyword, then pressing RETURN.
;;;
;;;     >>Layer name to add/<RETURN to remove>: (RETURN)
;;;
;;;     Filter: ((0 . "LINE") (39 . 2.0) (62 . 1) (210 0.0 0.0 1.0))
;;;     >>Block name/Color/Entity/Flag/LAyer/LType/Pick/Style/Thickness/Vector:
;;;
;;;   If an item exists in the filter list and you elect to add a new item,
;;;   the old value is overwritten by the new value, as you can have only
;;;   one of each type in a single (ssget "x") call.
;;;
;;;--------------------------------------------------------------------------;
;;;
;;; Find the dotted pairs that are valid filters for ssget
;;; in entity named "ent".
;;;
;;; ssx_fe == SSX_Find_Entity
;;;
 
(defun ssx_fe (/ data fltr ent)
  (setq ent (car (entsel "\nSelect object <None>: ")))
  (if ent
    (progn
      (setq data (entget ent))
      (foreach x '(0 2 6 7 8 39 62 66 210) ; do not include 38
        (if (assoc x data)
          (setq fltr
            (cons (assoc x data) fltr)
          )
        )
      )
      (reverse fltr)
    )
  )
)
;;;
;;; Remove "element" from "alist".
;;;
;;; ssx_re == SSX_Remove_Element
;;;
(defun ssx_re (element alist)
  (append
    (reverse (cdr (member element (reverse alist))))
    (cdr (member element alist))
  )
)
;;;
;;; INTERNAL ERROR HANDLER
;;;
(defun ssx_er (s)                     ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (if (/= s "Function cancelled")
    (princ (acet-str-format "\nError: %1" s))
  )
  (if olderr (setq *error* olderr))   ; Restore old *error* handler
  (princ)
)
;;;
;;; Get the filtered sel-set.
;;;
;;;
(defun ssx (/ olderr fltr)
  (gc)                                ; close any sel-sets
  (setq olderr *error*
        *error* ssx_er
  )
  (setq fltr (ssx_fe))
  (ssx_gf fltr)
)
;;;
;;; Build the filter list up by picking, selecting an item to add,
;;; or remove an item from the list by selecting it and pressing RETURN.
;;;
;;; ssx_gf == SSX_Get_Filters
;;;
(defun ssx_gf (f1 / t1 t2 t3 f1 f2)
  (while
    (progn
      (cond (f1 (prompt "\nCurrent filter: ") (prin1 f1)))
      (initget
        "Block Color Entity Flag LAyer LType Pick Style Thickness Vector")
      (setq t1 (getkword
        "\nEnter filter option [Block name/Color/Entity/Flag/LAyer/LType/Pick/Style/Thickness/Vector]: "))
    )
    (setq t2
      (cond
        ((eq t1 "Block")      2)   ((eq t1 "Color")     62)
        ((eq t1 "Entity")     0)   ((eq t1 "LAyer")      8)
        ((eq t1 "LType")      6)   ((eq t1 "Style")      7)
        ((eq t1 "Thickness") 39)   ((eq t1 "Flag" )     66)
        ((eq t1 "Vector")   210)
        (T t1)
      )
    )
    (setq t3
      (cond
        ((= t2  2)  (getstring "\n>>Enter block name to add <RETURN to remove>: "))
        ((= t2 62)  (initget 4 "?")
          (cond
            ((or (eq (setq t3 (getint
              "\n>>Enter color number to add [?] <RETURN to remove>: ")) "?")
              (> t3 256))
              (ssx_pc)                ; Print color values.
              nil
            )
            (T
              t3                      ; Return t3.
            )
          )
        )
        ((= t2  0) (getstring "\n>>Enter entity type to add <RETURN to remove>: "))
        ((= t2  8) (getstring "\n>>Enter layer name to add <RETURN to remove>: "))
        ((= t2  6) (getstring "\n>>Enter linetype name to add <RETURN to remove>: "))
        ((= t2  7)
          (getstring "\n>>Enter text style name to add <RETURN to remove>: ")
        )
        ((= t2 39)  (getreal   "\n>>Enter thickness to add <RETURN to remove>: "))
        ((= t2 66)  (if (assoc 66 f1) nil 1))
        ((= t2 210)
          (getpoint  "\n>>Specify extrusion Vector to add <RETURN to remove>: ")
        )
        (T          nil)
      )
    )
    (cond
      ((= t2 "Pick") (setq f1 (ssx_fe) t2 nil)) ; get entity
      ((and f1 (assoc t2 f1))         ; already in the list
        (if (and t3 (/= t3 ""))
          ;; Replace with a new value...
          (setq f1 (subst (cons t2 t3) (assoc t2 f1) f1))
          ;; Remove it from filter list...
          (setq f1 (ssx_re (assoc t2 f1) f1))
        )
      )
      ((and t3 (/= t3 ""))
        (setq f1 (cons (cons t2 t3) f1))
      )
      (T nil)
    )
  )
  (if f1 (setq f2 (ssget "_x" f1)))
  (setq *error* olderr)
  (if (and f1 f2)
    (progn
      (princ (acet-str-format "\n%1 found. "  (itoa (sslength f2))))
      f2
    )
    (progn (princ "\n0 found.") (prin1))
  )
)
;;;
;;; Print the standard color assignments.
;;;
;;;
(defun ssx_pc ()
  (if textpage (textpage) (textscr))
  (princ "\n                                                     ")
  (princ "\n                 Color number   |   Standard meaning ")
  (princ "\n                ________________|____________________")
  (princ "\n                                |                    ")
  (princ "\n                       0        |      <BYBLOCK>     ")
  (princ "\n                       1        |      Red           ")
  (princ "\n                       2        |      Yellow        ")
  (princ "\n                       3        |      Green         ")
  (princ "\n                       4        |      Cyan          ")
  (princ "\n                       5        |      Blue          ")
  (princ "\n                       6        |      Magenta       ")
  (princ "\n                       7        |      White         ")
  (princ "\n                    8...255     |      -Varies-      ")
  (princ "\n                      256       |      <BYLAYER>     ")
  (princ "\n                                               \n\n\n")
)
;;;
;;; C: function definition.
;;;
(defun c:ssx ()
 (ssx)
 (princ)
)
(princ "\n\tType \"ssx\" at a Command: prompt or ")
(princ "\n\t(ssx) at any object selection prompt. ")


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgADPysRSL0VItX98OxzpAwzU5rnb69hWfQ6x2VB
;;; JHkDq/eKSCjuJqDQb/lfj9MDd72bECfudL9Tkb8JklRB9Wqmox60qD2YBlkCRDJn
;;; cv1byVM5HHba5G2VuQt0nTprbh/IyFKOy6eVodZQyxHEjZiupBrA4IQuLYDPmvl6
;;; JzU4WkLtDolwXmXElZaP6g+TbHZsKjhz85UIFGGz43G743zu+AC0A8mFr5Vh12yb
;;; nCloaXn0FU6E4b/RYJeCx7269ztgpqcY4eRR3DafYb5MpVLCYgXPiD1y9asvSCOM
;;; e2wuet3E5VmH1Luw3uxknXCQtftGAyddT1G7SzHlDcTislo4znv49D6V/FWh1brr
;;; d32CHExoO39tMrs3kttgAd5ziBGwoDSoAkJy+Q017DLYz07u0nyr4u7qVlMmwFC8
;;; JpULjyUj9G6xDImLC5m3iuG2xDYag+2Fu7iESy7E1MTnSP7JtZWdrwPYsBqxKl+n
;;; 43r7gX3Jpri9DiEjtEg0Cgw4nohAnspvQBVS/TQuqKVz9mt270+h9c41fygKlGsL
;;; EE3DnJ0H6NwV5ttoWFvdn51d8y2RIWwd9vOketbcIdQIyxKmyyJbR0QuQpZaerPz
;;; N0IY57FZLJkBacJew0L/N1VQRNkbbgSkyjkfdixkIWCuNmjt4ttnJHwa4bjhmRlx
;;; ljIVnA==
;;; -----END-SIGNATURE-----