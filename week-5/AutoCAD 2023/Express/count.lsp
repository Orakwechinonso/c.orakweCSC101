;;;
;;;    COUNT.LSP
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
 
(defun c:bcount ( / ss flt a n lst)
(acet-error-init
 (list nil T)
);acet-error-init
 
;build a filter of valid block names
(setq lst (acet-table-name-list (list "block" 1 4 16))) ;exclude anonymous and xref blocks
(setq n 0)
(repeat (length lst)
(setq  a (nth n lst)
       a (cons 2 a)
     flt (cons a flt)
);setq
(setq n (+ n 1));setq
);repeat
 
(setq flt (append '((0 . "INSERT")
                    (-4 . "<OR")
                   )
                   flt
                   '((-4 . "OR>"))
          );append
);setq
(acet-ss-clear-prev)
(princ "\nPress Enter to select all or...")
 
(if (setq ss (ssget))
    (setq ss (ssget "_p" flt))
    (setq ss (ssget "_x" flt))
);if
(if ss
    (bns_count ss)
    (princ "\nNo valid objects selected.")
);if
 
(acet-error-restore)
);defun c:count
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_count ( ss / bna lst na e1 n a mx )
 
 
;get a list of all unique block names
(setq mx 1)
(setq n 0)
(repeat (sslength ss)
(setq  na (ssname ss n)
       e1 (entget na)
      bna (cdr (assoc 2 e1))
       mx (max mx (strlen bna))
);setq
(if (not (assoc bna lst))
    (setq lst (cons (cons bna 1) lst))
    (setq   a (cdr (assoc bna lst))
            a (+ a 1)
          lst (subst (cons bna a) (assoc bna lst) lst)
    );setq
);if
(setq n (+ n 1));setq
);repeat
 
(if lst
    (progn
     (setq mx (+ mx 5));setq
     (princ (bns_count_format "Block" "Count" mx))
     (setq a "\n")
     (while (< (strlen a) (+ mx 7))
;;      (setq a (strcat a "-"))
      (setq a (acet-str-format "%1-" a))
     );while
     (princ a)
    );progn then print header
);if
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
 (princ (bns_count_format (car a) (itoa (cdr a)) mx))
(setq n (+ n 1));setq
);repeat
);defun bns_count
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_count_format ( a b mx / )
 
 (while (<= (strlen a) mx)
;;  (setq a (strcat a "."))
  (setq a (acet-str-format "%1." a))
 );while
;; (setq a (strcat "\n" a b))
 (setq a (acet-str-format "\n%1%2" a b))
);defun bns_count_format


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCyrWeZ9zByNqE9KBM41OjcVJJhtHbjaDLL/tER
;;; Sow8Y5eUKaqZEw3qpFJYXwsslBWE1/gO78ZUfsKmioEWH0qBu1+Eitp35nGyueJv
;;; XeDLAnpB6HmvlLgEjTvNcuoIY6GorCQlo6qYoZgtSQqxSeR7gyLRmHlZqV6Tekae
;;; Kxy+3tkP6lOlpcNJ6+hf7e5P0iLgt0cLboiHMqDHFd1z3x3eCfj69iQvfMOU8tB+
;;; 6JYc9afo3kiTQ2vGfA7bUuBZoG/34rQqeNRG46a1MG4MpqS4IlfoittHfCKPt8m0
;;; PXpaIUDjidPsaPZxPPmBJjALpQ3fgMBYgqBqiLGaYu6N2MhhoAUej9/6okSkNdpW
;;; GbI1TvD4gRQOwoCn4nQf2MpDcJw+Nfu24FBTr+8FWyaEQ+SnhacyVkIaO4L7Jv2A
;;; Tcv0rVBgc40WTjfvH8Ikir6bzafCKI5KXBw6UgVWTCxLv3nrGb9IKC94BayJnjL7
;;; /N9/zmTKOwJiqEyEim/Mk5R4aKI19yFPmBa42Fy9UX3DJq9L4nKzD1/WH+e7M4wA
;;; +hBSXGaJnc/Sy07MH2mGApDa9sstvG6GvfSD0w3uULkARzziSpJK2rju9t+8+Ju3
;;; vyRyNGHRkO5WD4XkS/8gYDgG2R1tqmJvT+8hlzlnj7UN1rC557/9gzQRow5QGPTO
;;; 2jPvCA==
;;; -----END-SIGNATURE-----