;;;
;;;    ASPACE.LSP - C:ALIGNSPACE Written by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;  Align-SPACE
(defun c:alignspace ( / ss2 p1 p2 p3 p4 flag flag2 a)
 
(acet-error-init
 (list (list   "cmdecho" 0
             "regenmode" 1
             "ucsfollow" 0
       );list
       T
 );list
);acet-error-init
 
(cond
 ((not (equal 0 (getvar "tilemode")))
  (princ "\n  Command not allowed unless TILEMODE is set to 0  ")
 );cond #1
 ((and (setq flag (acet-viewport-next-pickable))
       (car flag)
  );and
  (progn
   (command "_.mspace")
   (if (not (equal (car flag) (getvar "cvport")))
       (progn
        (princ "\nPicking in perspective view not allowed...")
        (princ "\nSwitching to next available model space viewport")
        (setvar "cvport" (car flag))
       );progn then
   );if
   (while (not flag2)
    (if (setq p1 (getpoint "\nFIRST alignment point in MODEL space: "))
        (progn
         (setq p2
               (getpoint p1 "\nSECOND point in MODEL space or <Return> for none: ")
         );setq
         (setq p1 (trans p1 1 0))
         (if p2
             (setq p2 (trans p2 1 0))
         )
        );progn then
    );if
    (if (and p1
             (equal p1 p2)
        );and
        (princ "\n*Invalid* Points cannot be the same.")
        (setq flag2 T)
    );if
   );while
   (if p1
       (progn
        (command "_.pspace")
        (setq flag2 nil)
        (while (not flag2)
         (if p2
             (setq a "\nFIRST alignment point in PAPER space: ")
             (setq a "\nAlignment point in PAPER space: ")
         );if
         (if (and (setq p3 (getpoint a))
                  p2
             );and
             (setq p4 (getpoint p3 "\nSECOND alignment point in PAPER space: "))
         );if
         (if (and p3
                  (equal p3 p4)
             );and
             (princ "\n*Invalid* Points cannot be the same.")
             (setq flag2 T)
         );if
        );while
        (setq flag2 nil)
        (command "_.mspace")
        (if (and (setq ss2 (ssget "_x" '((0 . "VIEWPORT") (67 . 1))));setq
                 (= (sslength ss2) 2)
            );and
            (setq flag2 T)
        );if
        (while (not flag2)
         (getstring "\nActivate the desired viewport to align and press ENTER to continue.")
         (if (equal (getvar "cvport")
                    (car (setq flag (acet-viewport-next-pickable)))
             );equal
             (setq flag2 T)
             (progn
              (princ "\n*Invalid*")
              (princ (strcat "\nCannot use viewports that are turned"
                             " off or have perspective view on."
                     )
              );princ
             );progn
         );if
        );while
        (if (or (and p1
                     (not p2)
                     p3
                     (not p4)
                );and
                (and p1 p2 p3 p4);and
            );or
            (alignspace p1 p2 p3 p4)
        );if
       );progn then got 2 model space points
   );if
  );progn
 );cond #2
 ((cadr flag)
  (princ (cadr flag))
 );cond #3
);cond close
 
(acet-error-restore)
(princ)
);defun c:aspace
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes two points in model space (p1 p2) and zooms in the current viewport to align
;it visually woith the two matching point in paper space (p3 p4).
;if p2 and p4 are nil then no view scaling occurrs and only p1 is matched to p3.
;NOTE model space points must be provided in world coords.
;
(defun alignspace ( p1 p2 p3 p4 / vp a b c d na vplocked )
 
(if (= 1 (getvar "cvport"))
    (command "_.mspace")
);if
(setq       na (acet-currentviewport-ename)
      vplocked (acet-viewport-lock-set na nil) ;unlock the viewport if needed.
                                               ;sets vplocked to the ename of viewport if locked/nil if not
);setq
 
(command "_.pspace")
(setq p3 (trans p3 1 3))
(if p4
    (setq p4 (trans p4 1 3))
);if
 
(command "_.mspace")
 
(acet-ucs-cmd (list "_view"))
 
(setq p1 (trans p1 0 1));setq
(if p2
    (setq p2 (trans p2 0 1))
);if
 
(if (not p2)
    (progn
     (setq vp (acet-currentviewport-ename)
           vp (entget vp)
           vp (cdr (assoc 41 vp))
            a (/ vp (getvar "viewsize"))
     );setq
    );progn
    (setq p2 (list (car p2) (cadr p2) (caddr p1)) ;rk added 5:39 PM 9/1/97
          p4 (list (car p4) (cadr p4) (caddr p3))
           a (/ (distance p3 p4)
                (distance p1 p2)
             )
    );setq else
);if
 
(setq c (trans p3 3 2)
      c (trans c 2 1)
);setq
(if p4
    (setq d (trans p4 3 2)
          d (trans d 2 1)
    );setq
);if
(if (and p2
         p4
         (not (equal (angle p1 p2) (angle c d) 0.0001))
    );and
    (progn
     (acet-ucs-cmd (list "_z" (* (- (angle p1 p2) (angle c d))
                              (/ 180.0 pi)
                           )
                   );list
     );acet-ucs-cmd
     (command "_.plan" "_c")
     (acet-ucs-cmd (list "_p"))
    );progn then
);if
(if (and p2 p4)
    (command "_.zoom" (strcat (rtos a 2 6) "xp"))
);if
(setq b (trans p3 3 2)
      b (trans b 2 1)
);setq
(command "_.-pan" p1 b)
 
;restore the original ucs
(acet-ucs-cmd (list "_p"))
 
(if vplocked
    (acet-viewport-lock-set na T) ;re-lock the viewport
)
 
(if (and p2 p4
         (or (not (equal (caddr p1) (caddr p2) 0.0001))
             (not (equal (caddr c) (caddr d) 0.0001))
         );or
    );and
    (progn
     (princ "\nWarning: Selected Points are not parallel to view")
     (princ "\nCommand results may not be obvious")
    );progn
);if
(princ "\nPaper space = Model space")
(princ (strcat "\n"
               (substr "             " 1
                       (max (- 11 (strlen (rtos 1.0))) 0)
               );substr
               (rtos 1.0)
               " = "
               (rtos (/ 1.0 a))
       );strcat
);princ
(princ (strcat "\nCurrent zoom factor = " (rtos a) "xp"))
);defun alignspace


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCTQ9CXGSmCH52+EbQ12cyxg7SMr/qmJ65/SBpA
;;; Ql3TTbt30CxE5Yj2UXeH4+d8GbcNfdN0PdSsouIj8ZXk3ytZenr0UT5m/OiOyu5V
;;; QJlOrl44xGejh6fb2JOt8/kbhIroVvWXIgztTdqS8HBgNqe8mQzWV1fX51hmzD1V
;;; bYb40/JoQw1fTSwecpFNvV4YUnw1tJnord5vYP1VXkJHCR8HoTQmPRCb4M1Y0wjQ
;;; +v94sce4Nximj31bQD9lpeK7mHqNKtw1nRQxb8nDKjk5frTwVuOCrBRESmolorpC
;;; OQvEwyKBy4rSWpLfxGZAw/v6JoZY9w0nD8p2Qcmh4CV+1RVvDYbmJp5PZ0WVBRQD
;;; oJR23jWa0mwRS4vLQwgPhgSYDpfTbJ64fR1StA4TQAUVCmv1c53t5P6P228UKm4Q
;;; ahwvJkH1Xzstzzb4wKg5A2a4e2DAZUAwlm7ELqEm8Z/9wCLDpt92uXxFKjopCR3n
;;; XUIs9VAV/xLPdRoD8QpE8+F1Kb1o0stjKhdLcyylfWBF9j7WB8+z6+arcuMwZkBm
;;; +k/1h/dnp3kqt9EK0sMWuihsxOjv5GJP0Zp442EKRl7lVSzl64wYOMNgUhIl+rXE
;;; 0poKaoKGLD51yB/ggPmZHRirJaeTSpPGHfxzA7Q+finxpcMgkPDIUsh0f55rUi+a
;;; N6Swcw==
;;; -----END-SIGNATURE-----