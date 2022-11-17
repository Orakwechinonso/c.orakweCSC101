;;
;;;
;;;    VPSYNC.LSP
;;;    Created 10/16/97 by Dominic Panholzer
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
 
 
(defun c:vpsync (/ is_perspective align_vp FLAG IN_MSPACE ANS VPM CNT DCNT VP_ELST VP_HLST EN LOOP SS)
 
 
; --------------- PERSPECTIVE CHECKING FUNCTION ------------------
; This function checks if a viewport is oin perspective view.
; It returns T if the vp is in perspective view else nil.
; ----------------------------------------------------------------
 
  (defun is_perspective (VP)
    (setq VP (entget VP '("ACAD"))                     ; Get viewport edata
          VP (cdr (car (cdr (assoc -3 VP))))           ; and its xdata
          VP (cdr (member (assoc 1070 VP) VP))         ; Strip out Extended data version #
          VP (cdr (assoc 1070 VP))                     ; and get the view mode
    )
    (equal 1 (logand 1 VP))                            ; Is the 1 bit set (perspective)?
  )
 
; ------------------- ALIGN VPORTS FUNCTION ----------------------
; This function takes a Master viewport entity name MVP and a
; list of viewport entities to be aligned to the master VP.
; ----------------------------------------------------------------
  (defun align_vp (VPM VP_ELST / PT1 PT2 VPM# VDIR PMS1 PMS2 CNT vplock na vp )
 
    (setq PT1 '(0 0)                                   ; set abitrary PS points for alignment
          PT2 '(1 1)
          VPM# (cdr (assoc 69 (entget VPM)))           ; get the master VP's number
    )
    (command "_.mspace")                               ; Get into MS
    (setvar "cvport" VPM#)                             ; Set the master VP current
    (setq VDIR (getvar "viewdir"))                     ; get the view direction
    (setq PMS1 (trans PT1 3 2)                         ; translate PT1 from PS to Display
          PMS1 (trans PMS1 2 0)                        ; and then from display to current ucs
          PMS2 (trans PT2 3 2)                         ; Do the same for PT2
          PMS2 (trans PMS2 2 0)
          CNT  0
    )
    (foreach na VP_ELST                                ; Go through the list of slave VPs
      (setq VP (cdr (assoc 69 (entget na))))           ; get its number
      (setvar "cvport" VP)                             ; set it current
      (setq vplock (acet-viewport-lock-set na nil))    ; unlock the viewport if needed
      (if (not (equal vdir
                      (getvar "viewdir")
                      0.000001
          )    );not equal
          (command "_.vpoint" (trans VDIR 0 1 T))      ; change the view direction if needed
      );if
      (alignspace PMS1 PMS2 PT1 PT2)                   ; align the points
      (if vplock
          (acet-viewport-lock-set na T)
      );if
    );foreach
  );defun
 
; ----------------------------------------------------------------
;                          MAIN PROGRAM
; ----------------------------------------------------------------
 
  (acet-error-init
    (list (list   "cmdecho" 0
                "regenmode" 1
          );list
          T
    );list
  );acet-error-init
 
  (acet-autoload (list "aspace.lsp" "(alignspace PT1 PT2 PT3 PT4)")) ; Autoload alignspace
 
  (cond
    ((not (equal 0 (getvar "tilemode")))
       (princ "\n  Command not allowed unless TILEMODE is set to 0  ")
    );cond #1
 
    ((and (setq FLAG (acet-viewport-next-pickable))                 ; If there is at least one
          (car FLAG)                                   ; non-perspective VP
     );and
 
     (if (> (getvar "cvport") 1)                       ; If MS is active
       (progn
         (setq IN_MSPACE (getvar "cvport"))            ; save the current VP
         (command "._pspace")                          ; and switch to PS
       )
     )
 
     ;(setq ANS  (car (entsel "\nSelect master viewport: "))
     ;;Added the following code to replace the above line.  Irregularly shaped floating viewports actuall
       ;;consist fo two entities (a pline and a viewport) with reactors on each other to point to each other
       ;;so a simple (entsel) returned a pline instead of a viewport. Had to uise the built-in filtering
       ;;capability of 'acet-ui-single-select' to get around this problem.
 
     (setq ANS (acet-ui-single-select '((0 . "viewport")) T ) ;setq
           LOOP T
     )
 
     (while LOOP
       (cond
         ((not ANS)
           (setq LOOP nil)
         )
         ((not (= "VIEWPORT" (cdr (assoc 0 (entget ANS))))) ; If selection is not a VP
           (prompt "\nInvalid selection. Try again.")       ; re-prompt the user
         )
         ((is_perspective ANS)
           (prompt "\n  Command may not be invoked on a viewport with perspective view  ")
         )
         ((= 0 (cdr (assoc 68 (entget ANS))))
           (prompt "\n  Command may not be invoked on a viewport which is turned off  ")
         )
         (T
           (setq VPM  ANS
                 LOOP nil
           )
           (redraw VPM 3)                              ; Highlight the Master VP
         )
       )
 
       (if LOOP
         (setq ANS  (car (entsel "\nSelect master viewport: ")))
       )
     )
 
     (if VPM
       (progn
         (prompt "\nSelect viewports to be aligned to master viewport.")
 
         (setq SS (ssget (list '(-4 . "<AND")
                               '(0 . "VIEWPORT")                                   ; Must be a viewport
                               (cons 410 (getvar "ctab"))
                               '(-4 . "<NOT") '(68 . 0)   '(-4 . "NOT>")           ;and not be turned off
                               '(-4 . "<NOT") '(-4 . "&") '(90 . 1) '(-4 . "NOT>") ;and not perspective
                               '(-4 . "AND>")
                          )
                  )
         )
 
         (if SS                                        ; If a valid selection was made
           (progn
             (setq CNT    0
                   DCNT   0
             )
 
             (while (setq EN (ssname SS CNT))          ; step through each viewport
               (if (is_perspective EN)                 ; check to see if it is in perspective
                 (setq DCNT (1+ DCNT))                 ; if so, increment the removed counter
                 (setq VP_ELST (append (list EN) VP_ELST)                 ; else add the VP to the list
                       VP_HLST (cons (cdr (assoc 5 (entget EN))) VP_HLST) ; and save its handle for storage
                 )
               )
               (setq CNT (1+ CNT))
             )
 
             (if (< 0 DCNT)
               (progn
                 (prompt (strcat "\n" (itoa (sslength SS)) " viewport(s) selected."))
                 (prompt (strcat "\n" (itoa DCNT) " perspective view viewport(s) removed from selection."))
               )
             )
           )
         )
         (redraw VPM 4)                                ; If no valid selection, Unhighlight Master VP
       )
     )
 
     (if (and VPM VP_ELST)                             ; If both Masetr VP and 'slave' VP(s) exist
       (progn
         (align_vp VPM VP_ELST)                        ; align them
         (setq VP_ELST (list VPM VP_ELST))
       )
     )
 
     (if IN_MSPACE                                     ; If MS was originally active
       (setvar "cvport" IN_MSPACE)                     ; return back to the original VP
       (command "_.pspace")                            ; else goto PS
     )
 
    );cond #2
    ((cadr flag)                                       ; If no no-perspective VP available
     (princ (cadr flag))                               ; display error message
    );cond #3
  );cond close
 
  (acet-error-restore)
  (princ)
)


(acet-autoload2	'("aspace.lsp"	(alignspace p1 p2 p3 p4)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAz+9xy/uelxc+E5ggpvbmoufxQk38xYWk5Ti2g
;;; 6lzf6uh9Iv/Uith3o0Keywnk6e2ElNVTlFmzm+ClvjBBWqkB96fDRvzhzjGe55CB
;;; BC2E0TzrCqK+rHbl2oLIV4xQ6U8VuQNvJxZoSlq4HlpBN33XZug5MNXV7wovllgr
;;; 5iLGKctznyhwj0DOJEb6YHo3g0lHCUSROsTbHh3hpggXPKaNtROY+rzsnOGhmmGi
;;; OftnkbC4nb50hLurTB/klKt5nNMGOlSTchp4FDY/ofN1JKuF+HqPxqSPoSfybOib
;;; lH6ZZIr9NID9hys+ZFE04FfUGNqVHsWzVa23N3fM/jnTYTkPi4wLaHlwsigmpeV1
;;; 1B5GwUvTw+UAzoBCwltL/g0z6mSIM2Rg1Nt7+yTNM31pcSUhxV89yI00I3bQ97E3
;;; 5RROyk4qoy/xmZ/i/qP4qt6nx7KaqEDwJ4rv5RhC/iD5UpzT6yFLDETHxRZP+Y6N
;;; 5CfKTtEEtXN1I/bUPcKsMqZKBGKFLXVJ3Sfb8M3efmqIUCyMkqRkg/ZK2oZnnNHf
;;; PfL91Q/2ZW3bbIXBZLruWme7hATWZMnHHdcZSHF39oEneSM9DYokgSrNkmElpqBn
;;; NvEBM4uMm8Zp73ZwDbY6r0rwsDM+niKn6n9v75xXFNxeKZaNMZSrZCqzghnOnTF8
;;; Yb8chg==
;;; -----END-SIGNATURE-----