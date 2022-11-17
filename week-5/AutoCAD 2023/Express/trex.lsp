;;;
;;;    TREX.LSP - Written by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TREX
;Trim and Extend combined!
;Pick to trim and shift+pick to extend.
;
(defun c:trex ( / )
 (acet-error-init
  (list '("cmdecho" 0)
         0 ;0 means place an undo begin and end mark but do not
           ;use undo to back up on an error event.
        '(if ss (acet-ss-redraw ss 4))	;; clear the redraw on any selected objects
  );list
 )
 (acet-trim-extend)
(acet-error-restore)
);defun c:trex
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-trim-extend ( / ss flt flag p1 errno lst n a u ulst endit )
 
(setq flt '((-4 . "<NOT")
             (-4 . "<OR")
              (0 . "INSERT")
              (0 . "ATTDEF")
              (0 . "DIMENSION")
              (0 . "SOLID")
              (0 . "3DSOLID")
              (0 . "3DFACE")
              (0 . "POINT")
              (-4 . "<AND")
               (0 . "POLYLINE")
               (-4 . "&") (70 . 80) ;16 64 / 3dmesh and pface mesh.
              (-4 . "AND>")
              (0 . "TRACE")
              (0 . "SHAPE")
             (-4 . "OR>")
            (-4 . "NOT>")
           )
);setq
 
(acet-trex-print-modes)
(princ "\nSelect cutting/boundary edges or press enter for implied.")
 
(if (setq ss (ssget))
    (progn
     (setq ss (acet-ss-filter
               (list ss
                     (list (list flt
                                 "\n1 object was invalid as a cutting/boundary edge."
                                 "\n%1 objects were invalid as cutting/boundary edges."
                           );list
                     );list
                     T
               );list
              );acet-ss-filter
           ss (car ss)
     );setq
     (if (not ss)
         (setq endit T)
     );if
    );progn then
);if

(while (and (not endit)
            (or
                (progn
                 (acet-ss-redraw ss 3)
                 (initget "Fence Undo Project Edge")
                 (setvar "errno" 0)
                 (setq p1 (entsel "\nPick to trim or Shift+Pick to extend [Project/Edge/Undo]: "));setq
                 (acet-ss-redraw ss 4)
                 p1
                );progn
                (equal (setq errno (getvar "errno"))
                       7
                );equal
            );or
       );and
 (setq flag (acet-sys-shift-down))
 
 (setq u 0)
 
 (cond
  ((equal p1 "Undo")
   (if ulst
       (progn
        (command "_.undo" (car ulst))
        (setq ulst (cdr ulst))
       );progn then
       (princ "\nCommand has been completely undone.")
   );if
   (setq u nil)
  );cond #1
 
  ((or (equal p1 "Project") 
       (equal p1 "Edge")
   );or
   (command "_.trim" "")
   (setvar "cmdecho" 1)
    (command (strcat "_" p1) pause)
   (setvar "cmdecho" 0)
   (acet-safe-command nil T (list "")) ;exit the trim command
   (setq u (+ u 1))
  );cond #2
 
  ((not (equal (getvar "errno") 7))
   (if (equal (type p1) 'LIST)
       (setq p1 (cadr p1))
   );if
   (if flag
       (command "_.extend")
       (command "_.trim")
   );if
   (if ss
       (command ss)
   );if
   (command "")
   (if (equal p1 "Fence")
       (progn
        (command nil)
        (setq u (+ u 1));setq
        (setq lst (acet-ui-fence-select))
        (setq flag (acet-sys-shift-down))
        (if lst
            (progn
             (if flag
                 (command "_.extend")
                 (command "_.trim")
             );if
             (if ss (command ss));if
             (command "" "_F")
             (setq n 0)
             (repeat (length lst)
              (if (setq a (nth n lst))
                  (command a)
              );if
             (setq n (+ n 1));setq
             );repeat
             (command "" "")
             (setq u (+ u 1))
            );progn then
        );if
       );progn then fence
       (progn
        (command p1 "")
        (setq u (+ u 1))
       );progn else point pick
   );if
  );cond #3
 
  ((equal (getvar "errno") 7)
   (princ "\nYou missed! Try again...")
   (setq u nil)  
  );cond #4
 );cond close
 (if (and u
          (> u 0)
     );and
     (setq ulst (cons u ulst))
 );if
);while

(acet-safe-command nil T (list "")) ;exit any active command

 
);defun acet-trim-extend
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;print the status of projmode and edgemode the same wy that trim and extend do.
(defun acet-trex-print-modes ( / a b )
 (setq a (getvar "projmode")
       b (getvar "edgemode")
 );setq
 (cond
  ((equal a 0) (setq a "Projmode = None"))
  ((equal a 1) (setq a "Projmode = UCS"))
  ((equal a 2) (setq a "Projmode = View"))
  (T (setq a ""))
 );cond close
 
 (cond
  ((equal b 0) (setq b "Edgemode = No extend"))
  ((equal b 1) (setq b "Edgemode = Extend"))
  (T (setq b ""))
 );cond close
 
 (setq a (strcat "\n(" a ", " b ")"))
 (princ a)
 
);defun acet-trex-print-modes


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDKU6MitRRcz/qn50XiipNkjhY+UEn/UtjQ9Hjt
;;; YS97iFDToTUz0mDIniIm5ym5ZRiQBLr3OmPRIVlBMFUyLfBvnaDU2cXirTeVMZIp
;;; Aw+F/12o7/4eQVwJliSMUzNvkkWwhPH1fQ3CAStRBRxfKVkN6CBOgVkmA5GdPiK8
;;; zA0R/5QpQIbwLgUEWZJa+F2xzbvcp/yFEsMdG2TuJwaKCP1JI6RtaXlhOTnuJjZb
;;; RbDZtRkjv5DmN0KQ+CJEzg+GOvDaZS+F02kqFBrR4syHatDN3lV2v0zthnaaKz6d
;;; tBSix0uEjO++1vqE3OrTjky9HYoER23gCexYs1MEoD5y9UkM8Ujp3Fy+ff7+9vZw
;;; P0tk99l3jnkO82iKpiE79fWoTEhET/1RUlPGYo4iIFmICadm37MMUR2ZRoqJFWZW
;;; w0ssOaMcouQRYIa6cG7cUDqjCJKJY4uJ8y9507fRYbNERGQO3uiO90Ed7CAGVo2A
;;; L7BVKlcPAfPtQrD9fIly8SYhf3jHWqU+IWk6YO7Z/AmfuBF4t6jPXl68O1ZldfUV
;;; Dn/K8Z2PfgRdmi1dv7imjT4hJcfcCaLT0y7wARFZzEUOevSYI5jq0+6x1ZLHYhbS
;;; sf8uCkmJJ3TGcSCast1/GZDHtQZSFluQ5iHD0HztMnaTuUsvrzKMJrtmozi5dieS
;;; HRYPHg==
;;; -----END-SIGNATURE-----