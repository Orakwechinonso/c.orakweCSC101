; Next available MSG number is    12
; MODULE_ID EDGE_LSP_

;;;
;;;    edge.lsp            
;;;    
;;;  Copyright 2022 Autodesk, Inc.  All rights reserved.
;;;
;;;  Use of this software is subject to the terms of the Autodesk license 
;;;  agreement provided at the time of installation or download, or which 
;;;  otherwise accompanies this software in either electronic or hard copy form.
;;;
;;;
;;;
;;; --------------------------------------------------------------------------;
;;; DESCRIPTION
;;;
;;;   Interactive editor for changing the visibility of 3DFACE edges.           
;;;   Prompt:         "Display/<Select edge>: "                                 
;;;
;;;   Features:                                                                 
;;;    > "Display" -  Allows selective regeneration of 3DFACE's highlighting    
;;;                   invisible edges.                                          
;;;    >  Select   -  Reverses the visibility of each edge found.               
;;;
;;;   Note:                                                                     
;;;    >  EDGE uses Osnap MIDpoint as the center of a small crossing box        
;;;       when selecting edges.                                                 
;;;    >  Only edges displayed (or highlighted) can be modified.  (Use          
;;;       "Display" to display a 3DFACE.)                                       
;;;    >  Invisible edges will always be displayed if the system variable       
;;;       SPLFRAME is set to 1.                                                 
;;;
;;; --------------------------------------------------------------------------;


;;; Set CMDECHO without undo recording
(defun edge_setCmdEcho ( newVal / _oldEnvVal)
  ; Only do it if the value is different than the current value
  (if (/= newVal (getvar "CMDECHO"))
    (progn
      (setq _oldEnvVal (getenv "acedChangeCmdEchoWithoutUndo"))
      ; If not set yet, use 0 for default
      (if (not _oldEnvVal)
          (setq _oldEnvVal "0"))
      (setenv "acedChangeCmdEchoWithoutUndo" "1")
      (setvar "cmdecho" newVal)
      (setenv "acedChangeCmdEchoWithoutUndo" _oldEnvVal)
    )
  )
)

;;; ----------------------------- CREATE NEW *ERROR* -------------------------;

(defun edge-er (n) 
  (setq *error* lisp-er)
  (if (/= s "Function cancelled") 
    (princ (strcat "\nError: " n))
  ) 
  (edge_setCmdEcho 0)
  (command-s "_.UCS" "_P") 
  (setvar "osmode" o1)
  (setvar "gridmode" g1)
  (setvar "aperture" a1)
  (setvar "splframe" v1)
  (setvar "ucsfollow" u1)
  (setq n1 -1)
  (repeat (sslength faclst) 
    (redraw (entupd (ssname faclst (setq n1 (1+ n1)))))
  )  
  (command-s "_.UNDO" "_E")
  ;; Restore CMECHO without undo recording
  (edge_setCmdEcho _edge_oldCmdEcho)
  (edge_setCmdEcho _edge_oldCmdEcho)
  (prin1)
) 
;;; ---------------------------- COMMONLY USED MACROS ------------------------;

(defun getval (n e) 
  (cdr (assoc n e))
) 

(defun fltfac (ss / n1) 
  (setq n1 0)
  (if ss 
    (repeat (sslength ss) 
      (if (/= (getval 0 (entget (setq e1 (ssname ss n1)))) "3DFACE") 
        (ssdel e1 ss) 
        (setq n1 (1+ n1))
      )
    )
  ) 
  ss
) 

;;; ------------------------- FORCE DISPLAY OF ALL EDGES ---------------------;

(defun dsply (/ ss n1 t1) 
  (setvar "osmode" 0)
  (initget "All Select") 
  (setq ss (if (eq (getkword "\nEnter selection method for display of hidden edges [Select/All] <All>: ") "Select") 
             (fltfac (ssget)) 
             (ssget "_x" '((0 . "3dface")))
           ) 
        n1 -1)
  (setvar "osmode" 2)
  (cond (ss (princ "\n** Regenerating 3DFACE objects...") 
            (repeat (sslength ss) 
              (ssadd (setq t1 (ssname ss (setq n1 (1+ n1)))) faclst) 
              (shohdn (entget (entupd t1)))) 
            (princ "done.") T) 
    (T (princ "\nNo 3DFACE objects found.") nil)
  )
) 
;;; ----------------------------- SHOW HIDDEN EDGES --------------------------;

(defun shohdn (e / b1 p1 p2 p3 p4) 
  (setq b1 (getval 70 e))
  (mapcar '(lambda (j k) (set j (getval k e))) 
          '(p1 p2 p3 p4) 
          '(10 11 12 13)) 
  (if (= (logand b1 1) 1) 
    (grdraw p1 p2 c1 1)
  ) 
  (if (= (logand b1 2) 2) 
    (grdraw p2 p3 c1 1)
  ) 
  (if (= (logand b1 4) 4) 
    (grdraw p3 p4 c1 1)
  ) 
  (if (= (logand b1 8) 8) 
    (grdraw p4 p1 c1 1)
  )
) 

;;; --------------------------- GET ENTITY TO EDIT ---------------------------;

(defun getfce (pt / ll ur n1 ss e1 p1) 
  (setq p1 (trans pt 0 2) 
        ll (trans (polar P1 (/ (* pi 5) 4) h1) 2 0) 
        ur (trans (polar P1 (/ pi 4) h1) 2 0) 
        n1 0)
  (setvar "osmode" 0)
  (if (setq ss (ssget "_c" ll ur))
    (setq ss (fltfac ss))
  ) 
  (setvar "osmode" 2)
  ss
) 

;;; --------------------- MODIFY 3DFACE EDGE VISIBILITY FLAG -----------------;

(defun modfce (ss pt / n1 e1 e0 p0 b1 b2 b3) 
  (setq n1 0)
  (repeat (sslength ss) 
    (setq e1 (entget (ssname ss n1)) 
          e0 (getval -1 e1))
    (ssadd e0 faclst) 
    (mapcar '(lambda (j k) (set j (getval k e1))) 
            '(p1 p2 p3 p4) 
            '(10 11 12 13)) 
    (setq p0 (if (equal (distance p3 p4) 0 1e-8) 
               (mapcar '(lambda (j k l) (/ (+ j k l) 3)) p1 p2 p3) 
               (mapcar '(lambda (j k l m) (/ (+ j k l m) 4)) p1 p2 p3 p4)
             ))
    (setq b1 (getval 70 e1) 
          b2 (cond ((equal pt (inters p0 pt p1 p2) h1) 1) 
               ((equal pt (inters p0 pt p2 p3) h1) 2) 
               ((equal pt (inters p0 pt p3 p4) h1) 4) 
               ((equal pt (inters p0 pt p4 p1) h1) 8) 
               (T 0)
             ) 
          b3 (+ b1 (if (= (logand b1 b2) b2) 
                     (- b2) 
                     b2
                   )
             ) 
          e1 (shohdn (entmod (subst (cons 70 b3) (assoc 70 e1) e1))) 
          n1 (1+ n1))
  ) 
  T
) 
;;; ------------------------------ MAIN PROGRAM ------------------------------;

(defun c:EDGE (/ lisp-er s1 o1 g1 a1 v1 h1 u1 faclst c1 r1 t1 ss pt n e n1) 
  (setq lisp-er *error* 
        *error* edge-er 
        s1 (getvar "cmdecho") 
        _edge_oldCmdEcho s1
        o1 (getvar "osmode") 
        g1 (getvar "gridmode") 
        a1 (getvar "aperture") 
        v1 (getvar "splframe") 
        u1 (getvar "ucsfollow") 
        h1 (/ (getvar "viewsize") 100) 
        faclst (ssadd) 
        c1 7 
        r1 T)
   ; Set CMDECHO without undo recording
  (edge_setCmdEcho 0)
  (command "_.UNDO" "_GROUP") 
  (setvar "osmode" 2)
  (setvar "gridmode" 0)
  (setvar "aperture" 5)
  (setvar "splframe" 1)
  (setvar "ucsfollow" 0)
  (command "_.UCS" "_W") 
  (edge_setCmdEcho _edge_oldCmdEcho)
  (while r1 
    (initget "Display") 
    (setq t1 (getpoint "\nSpecify edge of 3dface to toggle visibility or [Display]: ") 
          r1 (cond ((eq t1 "Display") (dsply)) 
               ((eq (type t1) 'LIST) (setq ss (getfce t1))
                (cond ((null ss) (princ " No 3DFACE edges found.")) 
                  ((= (sslength ss) 0) (princ " Object is not a 3DFACE.")) 
                  (t (modfce ss t1))
                )
               ) 
               (T nil)
             ))
  ) 
  (edge_setCmdEcho 0)
  (command "_.UCS" "_P") 
  (setvar "osmode" o1)
  (setvar "gridmode" g1)
  (setvar "aperture" a1)
  (setvar "splframe" v1)
  (setvar "ucsfollow" u1)
  (setq n1 -1)
  (repeat (sslength faclst) 
    (redraw (entupd (ssname faclst (setq n1 (1+ n1)))))
  ) 
  (redraw) ; to remove highlighted screen graphics
  (command "_.UNDO" "_E") 
  ;; Restore CMECHO without undo recording
  (edge_setCmdEcho _edge_oldCmdEcho)

  (setq *error* lisp-er)
  (prin1)
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAaQVsOv0wHclCVAIrLucLwkgJR+Rkfg2XfWILZ
;;; lNAPHTsQCOEfu5D+ZaGabnIg7LXiyAc9vx6/u2Ygxgk2IaRkWYUcIQCWOMnFu6I4
;;; 3vyVLS/CZU+5PGtzJyHP5xUBfOhSjiY6FYieZd1XiFKRVHt1x9VEQeNIPMcicbB7
;;; MVyFeNJJhHFz5kCrGEIVZVYBxDU2HSMsXE5AZAELHZkZYcBZQ/EV+p/NpfubyCMp
;;; hRB/KMkwgV3u5/Ie0PVrU21pRCwJ02Br9YCCOKaUwp6IVaOHkRFJkr1taAo7UkF4
;;; CDQsxl2mFQdSrJpbmsvKdnamn35nzcd4dMvA//sixEYIGXDUjmlIjBCa2HtibGfF
;;; oAjSNc5dmhiJnDnxs1tl7tJybzxr7e3ZgXgafkQ9Raidc7tItGfnWxjQ832qlmEt
;;; cpnQDFcw3OH0HvsfBxuz7QJ2JZXcXyLQtYRG5drncenMLQAE5eVjsnm5g2vb43g2
;;; IBRxhA4SpUApcw/OctUyib0I+lSlDXRbJKaPkkoAq4zZwgzdRdL+HSN1K07NCX3H
;;; NaSBssRhX9bGp4z6jTdY8tulOaYb58oVGvrBV/KXSbpj5yaiuIg0wAmTbxr74nBD
;;; SDJi0dfDn4ecLvSCq4i9qpPGBcLRdDBPhcpi5gz1DFNI5D220nfBXeISSG4plvOo
;;; lJPO0Q==
;;; -----END-SIGNATURE-----