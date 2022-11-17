;;;
;;;    PLT2DWG.LSP - Written by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Reads a plt file (HPGL format only) and creates polylines in the current drawing
;from the plot file information.
; Each pen number is directly related to the color of the polyline that will be created.
;i.e. pen 1 will create red polylines.
;
(defun c:plt2dwg ( / a n fna fh flag p1 pd ptcnt)
(acet-error-init
 (list (list   "cmdecho" 0
             "regenmode" 1
              "limcheck" 0
                "clayer" nil
       )
       0  ;do not undo everything if cancel is pressed
 );list
);acet-error-init
 
(setq fna (acet-ui-getfile "Enter the plot file" (acet-filename-ext-remove (getvar "dwgname")) "PLT" "Acet:plt2dwg" 1664));setq
(cond
 ((not fna) (princ))
 ((not (setq flag (acet-plt2dwg-file-is-hpgl fna)))
  (acet-alert "\nThe selected plt file is not the proper HPGL format.")
 )
 (flag                   ;(= flag 1)
  (acet-plt2dwg fna)
  (command "_.zoom" "_e")
 )
);cond close
 
(acet-error-restore)
);defun c:plt2dwg
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-plt2dwg ( fna / fh n a b pd p1 ptcnt lst n lst2 na ss )
 
    (setq fh (acet-file-open fna "r")
          na (entlast) 
    );setq
    (setq n 0);setq
    (while (setq a (acet-plt2dwg-plt-read fh));setq
      (setq    a (acet-str-replace "PU" ";PU;" a)
               a (acet-str-replace "PD" ";PD;" a)
            lst2 (acet-str-to-list ";" a)
      );setq
 
     (foreach a lst2
      (if (equal n (* 75 (/ n 75)))
          (acet-spinner)
      );if
      (setq b (substr a 3)
            a (substr a 1 2)
      );setq
 
      (if (= b "")
          (setq b nil)
      );if
      (cond
       ((= a "")(princ));cond #1
       ((acet-str-equal a "PD")
        (if (not (wcmatch (getvar "cmdnames") "*PLINE*"))
            (progn
             (acet-cmd-exit)
             (command "_.pline")
            );progn then
        );if
        (setq    pd T
              ptcnt 0
        );setq
       );cond #2
       ((and b
             (acet-str-equal a "SP") ;;;set pen
        );and
        (if (and pd
                 (equal ptcnt 0)
                 p1
            )
            (command p1 p1 "")
         );if
         (acet-cmd-exit)
         (if (not (tblobjname "layer" b))
             (command "_.-layer" "_make" b "_color" b b "") 
             (command "_.-layer" "_thaw" b "_on" b "_set" b "")
         );if
         (princ (acet-str-format "\rImporting pen number: %1" b))
         (setq pd nil)
       );cond #3
       ((and b
             (acet-str-equal a "CI")
        );and
         (if pd
             (progn
              (if (equal ptcnt 0)
                  (command p1 b)
                  (command b)
              );if
             );progn
         );if
         (command "_.circle" p1 b)
       );cond #4
       ((and b
             (acet-str-equal a "PA")
        );and
         (if pd
             (progn
              (if (equal ptcnt 0)
                  (command p1 b)
                  (command b)
              );if
             );progn
         );if
         (setq p1 b)
       );cond #5
       ((and b
             (acet-str-equal a "PR")
        )
        (if (and pd 
                 (= ptcnt 0)
            )
            (command p1) ;;drop the first point
        );if 
        (setq lst (acet-str-to-list "," b))
        (setq n 0)
        (repeat (/ (length lst) 2)
         (setq  a (list (atoi (nth n lst))
                        (atoi (nth (+ n 1) lst))
                  )
               p1 (mapcar 'atoi (acet-str-to-list "," p1))
               p1 (strcat (itoa (+ (car p1) (car a)))
                          ","
                          (itoa (+ (cadr p1) (cadr a)))
                  )
         );setq
         (if pd 
             (progn
              (command p1)
              (setq ptcnt (+ ptcnt 1))
             );progn then
         );if 
         (setq n (+ n 2))
        );repeat
       );cond #6
       ((acet-str-equal a "PU")
        (if (and pd
                 (equal ptcnt 0)
                 p1
            )
            (command p1 p1 "")
        );if
        (acet-cmd-exit)
        (setq pd nil)
       );cond #7
      );cond close
 
     );foreach
 
    );while reading the file
    (close fh)
    (acet-cmd-exit)
 
    (if (setq ss (acet-ss-new na))
        (command "_.scale" ss "" "0,0" (/ 1.0 1020.0))
    );if
 
    (princ "\r                                                                   ")
    (princ)
);defun acet-plt2dwg
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a plt file name and returns 
;- 1 if the file is in ABSOLUTE HPGL format.
;- 2 if the file is in RELATIVE HPGL format.
;
(defun acet-plt2dwg-file-is-hpgl (fna / a b n fh flag aplt rplt )
 (setq aplt "\e.(;\e.I81;;17:\e.N;19:IN;SC;PU"
       rplt "\e.(;\e.I81;;17:\e.N;19:IN;SC;PU"
 );setq
 (if (and (setq fna (findfile fna))
          (setq fh (acet-file-open fna "r"))
     );and
     (progn
      (setq b "")
      (setq n 1)
      (while (and (<= n 29)
                  (setq a (read-char fh))
                  (or (acet-str-equal (chr a) (substr aplt n 1))
                      (acet-str-equal (chr a) (substr rplt n 1))
                  );or
             );and
       (setq b (strcat b (chr a)));setq
      (setq n (+ n 1))
      );while
      (cond
       ((acet-str-equal aplt b)
        (setq flag 1) ;absolute 
       )
       ((acet-str-equal rplt b)
        (setq flag 2) ;relative
       )
      ); 
      (close fh)
     );progn then
 );if
 flag
);defun acet-plt2dwg-file-is-hpgl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-plt2dwg-plt-read ( fh / a b c )
 
 (while (and (setq a (read-char fh));setq ;;read past any leading semi-colons.
             (setq a (chr a))
             (equal a (chr 59))
        );and
 );while
 (if a
     (setq b a)
     (setq b "")
 );if
 (while (and (setq a (read-char fh));setq
             (setq a (chr a))
             (not (equal a (chr 59)))
        );and
  
  (setq b (strcat b a));setq
 );while
 (if (equal b "")
     (setq b nil);setq
 );if
 b
);defun acet-plt2dwg-plt-read


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDNqJz3G2NLRmIO+A7Wrvgt/vvG3bK6BuYGHkX6
;;; jXok03bfoBFEZU3wU657vPi8k2gfH+vPOeW8QNeHfr0DUktL3HSMBLyvB/sRwKfG
;;; 8QNJ2JjiCzAGRjbah3D3Cy/IBvKAsfRsRT4i+LEINHgzssRsm/AIaDz5IKzt5bbO
;;; EgAjDAJDtwsuHHt+5SOrPBqWTa70iW+KKKripobiisCjevp/gJa1MaJqlgYg6a1G
;;; k3Ihhh1uTpkLXQyqa5sKhWlp0CHWyXzUC26nsQziKuMFJIAFTeLOC0DLyV0KTHuU
;;; K1M5GsXnt/kAQR6MbJsuFTKBRqOSi0BP7DlK5uteRaNNGNw8s0WAakmRR8oP9qUV
;;; mfwZAky4ayKfW8OpeR6DbYeUB06H3l5l2SR4oCtYTly8Klt3Qk78PO35X4w6s0tk
;;; wAz2dW/ZkT3QmorwtuIaYKZR5z6p+eu5A8YTKiy9jSXxn4PoVA+b1mI4PxmACGXJ
;;; CBIMOxFrmQ0Xobv63Wl888zhN+RtPWIaCCLA8owK70XLb7BPjkuFMnP7yXKnx68Q
;;; l/dNRw7X3tZIXG3SoyeZhWOSL7obsMbg+bSHHHVNIc9h2jP5MEfpubmjAVMlwAOF
;;; XUwjHywJ7IFE06wORn0X8KwVGtGdbr708bBA5Ur/xnoEFuJufor2NePoPg4i9Bzf
;;; LpsnFA==
;;; -----END-SIGNATURE-----