;;
;;;
;;;    TEXTFIT.LSP
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
 
 
(defun c:textfit (/ ename textent newend tmp start newpt val ltc_% ss txtsz)
 
  (acet-error-init
    (list
        (list  "cmdecho"    0
               "snapang"    0
              "limcheck"    0
              "orthomode"  1
        )
        T     ;flag. True means use undo for error clean up.
     ) ;list
  );acet-error-init
 
;;;End Error control
 
  (if (not (and
              (setq ss (ssget "_i"))
              (= (sslength ss) 1)
              (setq ename (ssname ss 0)
              )
           )
      )
    (setq ename  (car (entsel "\nSelect Text to stretch or shrink:" )))
  )
 
 
  (cond
    ((not (setq textent (if ename (entget ename))))
      (princ "\nNo object selected!")
    )
    ((/= (acet-dxf 0 textent) "TEXT")
      (princ "\nSelected object is not Text!")
    )
    ((acet-layer-locked (acet-dxf 8 textent))
      (princ "\nSelected object is on a locked layer!")
    )
    (t
      (setq txtsz (textbox textent))
      (setq newend (distance
                      (list
                        (caadr txtsz) ;upper right x coord
                        (cadar txtsz) ;lower left y coord
                      )
                      (car txtsz) ;; ll xyz
                   );distance
      );setq
      ;set snap along text entity
      (setvar "snapang"
        (angtof (angtos (acet-dxf 50 textent) 0 8) 0 )
      )
      (initget 0 "Start _Start")
      (setq
        tmp (getpoint (acet-dxf 10 textent) "\nSpecify end point or [Start point]: ")
      )
      (setvar "snapang" 0)
      (cond
        ((= (type tmp) 'STR) ;;new starting point to be selected
          (setq start (getpoint "\nSpecify new starting point: "))
          (if start
            (progn
              (acet-ucs-cmd (list "_E" (acet-dxf -1 textent)))
              (setvar "orthomode" 1)
              (setq newpt
                (if start
                  (getpoint (trans start 0 1) " ending point: ")
                  nil
                ) ;if
              ) ;setq
              (if newpt
                (setq newpt (trans newpt 1 0))
              )
              (setvar "orthomode" 0)
              (acet-ucs-cmd (list "_p"))
            ) ;progn
          ) if
        )
        ((not (null tmp))    ;;new ending point selected
          (setq start (acet-dxf 10 textent)
                newpt tmp)
        )
        (t  (setq start nil
                  newpt nil)
        )
      ) ;cond
      (if (and start newpt)
        (progn
          (setq val (assoc 41 textent) ;;current width factor
                val (if val (cdr val) 1.0)
                ltc_% (* (/ (distance start newpt) newend) val)
                textent (subst (cons 41 ltc_%)
                               (assoc 41 textent)
                               textent)
                textent (subst (cons 10 start)
                               (assoc 10 textent)
                               textent)
                textent (subst (cons 11 newpt)
                               (assoc 11 textent)
                               textent)
          ) ;setq
          (entmod textent)
          (entupd (acet-dxf -1 textent))
        )
      )  ;;end of points check
    )
  ) ;cond
  (acet-error-restore)
  (princ)
) ;end defun
 
 
 
 
(defun c:TFHELP (/)
 
(prompt " TEXTFIT will change the width factor of the selected text, \n")
(prompt " to fit within the user specified points.\n")
(prompt "\n")
(prompt " TEXTFIT will prompt:  Select Text to stretch/shrink:\n")
(prompt " The user is expected to select the text.\n")
(prompt "\n")
(prompt " TEXTFIT will then prompt:  Specify starting Point/<select new ending point>: \n")
(prompt " At which time the user can specify a new ending point \n")
(prompt "                          or\n")
(prompt " The user can provide the letter \"S\" for a new starting point\n")
(prompt " to which TEXTFIT will prompt:  Specify new starting point:  \n")
(prompt " and:  ending point: \n")
(textscr)
(princ)
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCiw1zBkl1ShasgbLQSu3vDh0CeJJleQUQk2ZqN
;;; BSIiwoeKNE80UZ4Qd83UGevp8Y/a7TxPyhPwAIDk566I+4WOuQXbJrUKoDpTAPtE
;;; WlWoYD4eSjLGlnkQc3KJiFpgzvkouKL1PdpspvaDyA6CS2y75Ju6faLMPDKTV0sH
;;; 5FihITm3B3DpjBDZ2+gPfayfnjCNsw98CT2ilqoBsoGEZ3wQ+MqEbwaKaki8zdQ5
;;; l8wKLdhulpylVVJR9mRqBo3b4h1wp49S/ms+JyZ/2zAhHNRUFVDj9sEytckg0I+n
;;; ZunPcavN+FgzMySHNvJl6RuJg0zMGNfuN/USc+D+365+cT2c2OZ3+ebpR7ekytsB
;;; SZ72J1Df3mNWf/pB7ZWgYnJhx5cfwOg8nMKd5dTQMgpCrJCIoqDQsTCkaqShDbd2
;;; awLnAkzTPkYLV3ivRovV9n/CNI3sUdKpZSGRHHh+Re3hH9cF4h72md0mQ1kO57Xn
;;; zSIkzL9rJGswDo28irZUtI3a+YJW59eiXGlqIU0CZg6fcFPZSK98UJrACNYhtQYI
;;; qsPEvFvBAS5TSdh2hBppiGo8vk83oALXpCvQgA9YgG6n5ChwV6BMEDh29QeM9OfQ
;;; n1y5CNFHSXprPXeQu/rGhKalA0pnmFAxIcyhQSvkLSN8Cb5/jnhhOQzOiU0HBNAo
;;; F8vpqw==
;;; -----END-SIGNATURE-----