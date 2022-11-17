; Next available MSG number is    29
; MODULE_ID LSP_3DARRAY_LSP_
;;;
;;;    3darray.lsp
;;;
;;;  Copyright 2022 Autodesk, Inc.  All rights reserved.
;;;
;;;  Use of this software is subject to the terms of the Autodesk license 
;;;  agreement provided at the time of installation or download, or which 
;;;  otherwise accompanies this software in either electronic or hard copy form.
;;;
;;;============================================================================
;;;  Functions included:
;;;       1) Rectangular ARRAYS (rows, columns & levels)
;;;       2) Circular ARRAYS around any axis
;;; 
;;;  All are loaded by: (load "3darray")
;;; 
;;;  And run by:
;;;       Command: 3darray
;;;                Select objects:
;;;                Rectangular or Polar array (R/P): (select type of array)


;;; ===================== load-time error checking ============================

  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       )
     )
     (exit)
  )

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

     (  (not (findtrustedfile "ai_utils.lsp"))                     ; find it
        (ai_abort "3DARRAY"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load (findtrustedfile "ai_utils.lsp") "failed"))            ; load it
        (ai_abort "3DARRAY" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "3DARRAY" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================
;;; 
;;;******************************** MODES ********************************
;;; 
;;; System variable save

(defun MODES (a)
  (setq MLST '())
  (repeat (length a)
    (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
    (setq a (cdr a))
  )
)

;;;******************************** MODER ********************************
;;; 
;;; System variable restore

(defun MODER ()
  (repeat (length MLST)
    (setvar (caar MLST) (cadar MLST))
    (setq MLST (cdr MLST))
  )
)

;;;******************************** 3DAERR *******************************
;;; 
;;; Standard error function

(defun 3DAERR (st)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (ai_setCmdEcho 0)

  (if (/= st "Function cancelled")
      (princ (strcat "\nError: " s))
  )
  (moder)                             ; Restore system variables
  (ai_setCmdEcho 0)
  (command "_.UNDO" "_E")
  (ai_undo_off)
  ; Restore CMDECHO without undo recording
  (ai_setCmdEcho _3da_oldCmdEcho)
  (setq *error* olderr)               ; Restore old *error* handler
  (princ)
)

;;;******************************* P-ARRAY *******************************
;;; 
;;; Perform polar (circular) array around any axis

(defun P-ARRAY (/ n af yn cen c ra)

  ;; Define number of items in array
  (setq n 0)
  (while (<= n 1)
    (initget (+ 1 2 4))
    (setq n (getint "\nEnter the number of items in the array: "))
    (if (= n 1)
      (prompt "\nNumber of items must be greater than 1")
    )
  )

  ;; Define angle to fill
  (initget 2)
  (setq af (getreal "\nSpecify the angle to fill (+=ccw, -=cw) <360>: "))
  (if (= af nil) (setq af 360))

  ;; Are objects to be rotated?
  (initget "Yes No")
  (setq yn (getkword "\nRotate arrayed objects? [Yes/No] <Y>: "))
  (if (null yn)
    (setq yn "Yes")
  )
  (setq yn (if (= yn "Yes") "_Y" "_N"))

  ;; Define center point of array
  (initget 17)
  (setq cen (getpoint "\nSpecify center point of array: "))
  (setq c (trans cen 1 0))

  ;; Define rotational axis
  (initget 17)
  (setq ra (getpoint cen "\nSpecify second point on axis of rotation: "))
  (while (equal ra cen)
    (princ "\nInvalid point. Second point cannot equal center point.")
    (initget 17)
    (setq ra (getpoint cen "\nPlease try again: "))
  )
  (setvar "UCSFOLLOW" 0)
  (setvar "GRIDMODE" 0)
  (command "_.UCS" "_ZAXIS" cen ra)
  (setq cen (trans c 0 1))

  ;; Draw polar array
  (command "_.ARRAY" ss "" "_P" cen n af yn)
  (command "_.UCS" "_p")
)

;;;******************************* R-ARRAY *******************************
;;; 
;;; Perform rectangular array

(defun R-ARRAY (/ nr nc nl flag x y z c el en ss2 e)

  ;; Set array parameters
  (while (or (= nr nc nl nil) (= nr nc nl 1))
    (setq nr 1)
    (initget (+ 2 4))
    (setq nr (getint "\nEnter the number of rows (---) <1>: "))
    (if (null nr) (setq nr 1))
    (initget (+ 2 4))
    (setq nc (getint "\nEnter the number of columns (|||) <1>: "))
    (if (null nc) (setq nc 1))
    (initget (+ 2 4))
    (setq nl (getint "\nEnter the number of levels (...) <1>: "))
    (if (null nl) (setq nl 1))
    (if (= nr nc nl 1)
      (princ "\nOne-element array, nothing to do.\nPlease try again")
    )
  )
  ;;
  ;; get environment variable "MaxArray", If unable to get, use
  ;; the default value of 100000. Value of 100000 is taken from
  ;; the value of MAX_ARRAY_DEFAULT  #defined in coresrc\array.c
  (if (= (getenv "MaxArray") nil)
    (progn 
	   (setq maxlimit 100000)
	)
	(progn
	  (setq maxlimit (atoi(getenv "MaxArray")))
	)
  )
  ;; ne - number of elements/entity.
  (setq ne (sslength ss))

  (if (< maxlimit (* nr nc nl ne))
  (progn
   (princ "\nThis would create ")
   (princ  (- (* nc nr nl ne) 1))
   (princ " objects, exceeding the limit of ")
   (princ maxlimit )
   (princ " objects imposed by the MaxArray environment setting.\n")
  )
  (progn
  (setvar "ORTHOMODE" 1)
  (setvar "HIGHLIGHT" 0)
  (setq flag 0)                       ; Command style flag
  (if (/= nr 1)
    (progn
    (initget (+ 1 2))
    (setq y (getdist "\nSpecify the distance between rows (---): "))
    (setq flag 1)
    )
  )
  (if (/= nc 1)
    (progn
    (initget (+ 1 2))
    (setq x (getdist "\nSpecify the distance between columns (|||): "))
    (setq flag (+ flag 2))
    )
  )
  (if (/= nl 1)
    (progn
    (initget (+ 1 2))
    (setq z (getdist "\nSpecify the distance between levels (...): "))
    )
  )
  (setvar "BLIPMODE" 0)

  (setq c 1)
  (setq el (entlast))                 ; Reference entity
  (setq en (entnext el))
  (while (not (null en))
    (setq el en)
    (setq en (entnext el))
  )

  ;; Copy the selected entities one level at a time
  (while (< c nl)
    (command "_.COPY" ss "" "0,0,0" (append (list 0 0) (list (* c z)))
    )
    (setq c (1+ c))
  )

  (setq ss2 (ssadd))                  ; create a new selection set
  (setq e (entnext el))               ; of all the new entities since
  (while e                            ; the reference entity.
    ; Don't add subentities
    (setq ed (entget e))
    (if (not (or (= (cdr (nth 1 ed)) "VERTEX")
                 (= (cdr (nth 1 ed)) "ATTRIB")
                 (= (cdr (nth 1 ed)) "SEQEND")))
       (ssadd e ss2)
    )
    (setq e (entnext e))
  )

  ;; Array original selection set and copied entities
  (cond
    ((= flag 1) (command "_.ARRAY" ss ss2 "" "_R" nr "1" y))
    ((= flag 2) (command "_.ARRAY" ss ss2 "" "_R" "1" nc x))
    ((= flag 3) (command "_.ARRAY" ss ss2 "" "_R" nr nc y x))
  )
  ) ;;; matching progn
  ) ;;; matching '(if (< maxlimit (* nr nc nl ne))'
)

;;;***************************** MAIN PROGRAM ****************************

(defun C:3DARRAY (/ olderr ss xx)
  (if (and (= (getvar "cvport") 1) (= (getvar "tilemode") 0))
    (progn
      (prompt "\n *** Command not allowed in Paper space ***\n")
      (princ)
    )
    (progn
      (setq olderr *error*
            *error* 3daerr
      )
      (*push-error-using-command*)
      (modes '("blipmode" "highlight" "orthomode" 
               "ucsfollow" "gridmode")
      )
      
      (setq _3da_oldCmdEcho (getvar "CMDECHO"))
      ; Change CMDECHO without undo recording
	  (ai_setCmdEcho 0)

      (ai_undo_on)                    ; Turn UNDO on
      (command "_.UNDO" "_GROUP")
      (graphscr)

      (ai_setCmdEcho _3da_oldCmdEcho)
      (setq ss nil)
      (while  (null ss)               ; Ensure selection of entities
          (setq ss (ssget))
          (if ss (setq ss (ai_ssget ss)))
      )
    
      (initget 0 "Rectangular Polar Circular")

      (setq xx (getkword "\nEnter the type of array [Rectangular/Polar] <R>:"))
      (cond 
        ((or (eq xx "Rectangular") 
             (eq xx nil))
          (r-array)
        )
        (T 
          (p-array)
        )
      )
      (ai_setCmdEcho 0)
      (moder)                         ; Restore system variables
      (command "_.UNDO" "_E")
      (ai_undo_off)                   ; Return UNDO to initial state
      ; Restore CMDECHO without undo recording
      (ai_setCmdEcho _3da_oldCmdEcho)
      (setq *error* olderr)           ; Restore old *error* handler
      (*pop-error-mode*)
      (princ)
    )
  )
)

(princ "  3DARRAY loaded.")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBTdfyubNqjlLSIzffycTniSgNKNMac7NrVh1MZ
;;; EeENcqdGb6auMTcEQPkM0ASBD+UwqkLTLDdU97sf8u4ogh0Gt+CENxbrvw4P/sfI
;;; +doQP1UgUkQQZXYBbv3lWWR/4P2w0kXKvBQpq5fVQ3jX2grkEh5kC0ntlAA4Fonv
;;; uflcMU1cFTE0GcUZnpz0lEU9JhJLuASQLl6mVz3JI53lyThPvREG/5nYlE9MPRse
;;; 8wtpsZpeV3uAgO+ZkltEcnGRaSrfB9ofj3fFfEe47Ml8NgT/s64zbW/crfJBTSBj
;;; VzPtHgtnqYMk0JxZKDN26l7Mu6uIpFJFIwVfQRqC2ASEItu8kXiDvw134tksISYx
;;; yvS/QI3N8fnOVxdNvqLJkPlgDOd8BuWXIMZjktvc11Zf4ayN1+vh3BCSfN7C3wm0
;;; 3dQDKqeZSiraywKbgZ+En7elBa3lrPndkemymcqhmD1hR55iFeRYG/VB3C7uuy3C
;;; vS4+Jdwlfqfj//rJuQaqfNZKstGj2G5kaebgZj1mVBjz8Aymh8dRa+3rqI6Z6z+o
;;; xMuRAGvukgQkihmwNuYY9mXY5+Wshx4hvUiQ2iK0bZkxtjJL/2StGyE4FrCOPTH/
;;; uah7TmsiCX2FjFhwm8Tzfm8TFswpi7a7f/T34l9ChkQcPGz1Adr/Ub8zE4OCvLVW
;;; tj99ng==
;;; -----END-SIGNATURE-----