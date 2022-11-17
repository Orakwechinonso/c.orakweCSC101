;;
;;  etbug.lsp - report an express tool bug and email it to the xpress Tools team.
;;                    
;;
;;  Copyright © 1999 by Autodesk, Inc.
;;
;;  Your use of this software is governed by the terms and conditions
;;  of the License Agreement you accepted prior to installation of this
;;  software.  Please note that pursuant to the License Agreement for this
;;  software, "[c]opying of this computer program or its documentation
;;  except as permitted by this License is copyright infringement under
;;  the laws of your country.  If you copy this computer program without
;;  permission of Autodesk, you are violating the law."
;;
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;  DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;  UNINTERRUPTED OR ERROR FREE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:etbug ( / fna ans msg path )
  (if (equal "failed" (load "acadinfo.lsp" "failed"))
      (progn
       (alert "Could not load \"acadinfo.lsp\". Check the AutoCAD support path in OPTIONS.")
       (exit)
      );progn then
  );if
 
  ;; Get the default location of the express directory just in 
  ;; case express is not on the support path.
  (if (setq path (findfile "acad.exe"))
      (setq path (substr path 1 (- (strlen path) 8))    ;; the acad root dir
            path (strcat path "express\\")		;; add the express dir
      );setq then
      (setq path "")
  );if
 
  (princ "\nGathering AutoCAD information...\n")
  (acet-etbug-acadinfo (+ 1 2 4 8 16 32 64)) ;; do not include lisp dump (128)
 
  (cond 
   ((not (setq fna (findfile "acadinfo.txt")))
    (alert "Acadinfo.txt was not successfully created.")
   );cond #1
 
   ((and (not (member "acetutil.arx" (arx)))
         (equal "failed" (arxload "acetutil.arx" "failed"))
         (equal "failed" (arxload (strcat path "acetutil.arx") "failed"))
    );and 
    (alert (strcat " \nCould not load \"acetutil.arx\". "
                   "\nCheck the AutoCAD support path in OPTIONS."
                   "\nNOTE: You can manually e-mail the acadinfo.txt file just created."
           )
    );alert
   );cond #2
 
   ((and (not (member "acetmail.arx" (arx)))
         (equal "failed" (arxload "acetmail.arx" "failed"))
         (equal "failed" (arxload (strcat path "acetmail.arx") "failed"))
    );and 
    (alert (strcat "Unable to send email."
                   " \nCould not load \"acetmail.arx\". "
                   "\nCheck the AutoCAD support path in OPTIONS."
                   "\nNOTE: You can manually e-mail the acadinfo.txt file just created."
           )
    );alert
   );cond #3
 
   ((setq msg (acet-etbug-get-userinfo))
    (acet-send-bug msg)
   );cond #4
 
  );cond close
 
(princ)
);defun c:etbug
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-etbug-get-userinfo ( / fna fh str a )
 (setq fna (findfile "acadinfo.txt"))
 (cond 
  ((not fna)
   (princ "\nCould not find acadinfo.txt")
  );cond #1
  ((not (setq fh (open "acadinfo.txt" "r")))
   (princ "\nCould open acadinfo.txt for read.")
  );cond #2
  (T
   (repeat 20 (read-line fh));skip the header information
   (setq str "")
   (while (setq a (read-line fh))
    (setq str (strcat str "\r\n" a))
   );while
   (close fh)
   (setq a (acet-ui-txted 
               (strcat "Problem Description: \r\n\r\n\r\n" 
                       "Steps to reproduce: \r\n" 
                       "- \r\n- \r\n- \r\n- \r\n"
               );strcat
               "ETBUG: Enter a description of the problem and steps to reproduce"
           )
   );setq
   (if a
       (progn
        (setq str (strcat a str))
        (if (setq fh (open fna "w"))
            (progn
             (write-line str fh)
             (close fh)
            );progn then
            (princ "\nError: Could not open acadinfo.txt for write.")
        );if
       );progn then
       (progn
        (setq str nil)
        (princ "\nOperation canceled.")
       );progn else
   );if
  );cond #3
 );cond close
 str
);defun acet-etbug-get-userinfo
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-send-bug ( msg / host err )
 (princ "\n ")
 (princ "\n ")
 (princ "\r")
 (princ "                                                         ")
 (princ "\n")
 (setq host (acet-get-email-host))
 (if (/= host "")
     (progn
      (setq err (acet-smtp-send
                 host
                 (getvar "loginname")
                 "expresstools@autodesk.com"
                 "Beta Bug"
                 msg
                )
      );setq
      (if err
          (alert err)
          (princ "\nMessage successfully sent.")
      );if
     );progn then
     (princ "\nNo email host provided.")
 );if
 
);defun acet-send-bug
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-get-email-host ( / def a )
 (setq def (getenv "acet_email_host"))
 (if (or (not def)
         (equal def "")
     );or
     (setq def "autodesk.com")
 );if
 (if (/= def "")
     (setq a (getstring (strcat "\nEnter the name of your outgoing mail server <" def ">: ")))
     (setq a (getstring "\nEnter your email host name: "))
 );if
 (if (and (= a "")
          (/= def "")
     );and
     (setq a def)
 );if
 (setenv "acet_email_host" a)
 a
);defun acet-get-email-host
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;Takes a flags arg that is the sum of the following:
; 0 do nothing
; 1    (acet-acadinfo-do-header fh)
; 2    (acet-acadinfo-do-general fh)
; 4    (acet-acadinfo-do-express fh)
; 8    (acet-acadinfo-do-fileloads fh)
; 16   (acet-acadinfo-test-load ...)
; 32   typelib
; 64   sysvars
; 128  Lisp dump
;
;
(defun acet-etbug-acadinfo ( flags / fna fh op)
 
;initialize a bare bones error handler that will recover from a first try
;load_test failure and re-issue this command.
 
  (if (not Acet:Acadinfo-Olderr)
      (setq Acet:Acadinfo-Olderr *error*)
  );if
 
  (defun *error* ( msg / )
    (if Acet:Acadinfo-Error-On-Load-Test
      (progn
        (if fh
            (close fh)
        );if
        (setq *error* Acet:Acadinfo-Olderr
              Acet:Acadinfo-Olderr nil
        );setq
        (acet-etbug-acadinfo flags)
        (setq Acet:Acadinfo-Error-On-Load-Test nil)
      );progn
      (princ msg)
    );if
  );defun *error*
 
 
  (setq fna "acadinfo.txt");setq
 
  (if Acet:Acadinfo-Error-On-Load-Test
    (setq op "a")
    (progn
       (setq op "w")
    );progn then
  );if
 
  (if (setq fh (open fna op))
    (progn
      (close fh) (setq fh (open fna op)) ;close and re-open again in case of
                                         ;garbage echo from error recovery.
 
      (if (not Acet:Acadinfo-Error-On-Load-Test)
        (progn
 
          (if (= 1 (logand 1 flags))
              (progn
               (setq flags (- flags 1))
               (acet-acadinfo-do-header fh)
              );progn
          );if
          (if (= 2 (logand 2 flags))
              (progn
               (setq flags (- flags 2))
               (acet-acadinfo-do-general fh)
              );progn
          );if
          (if (= 4 (logand 4 flags))
              (progn
               (setq flags (- flags 4))
               (acet-acadinfo-do-express fh)
              );progn
          );if
          (if (= 8 (logand 8 flags))
              (progn
               (setq flags (- flags 8))
               (acet-acadinfo-do-fileloads fh)
              );progn
          );if
 
          (if (= 16 (logand 16 flags))
              (progn
               (setq flags (- flags 16))
               (princ "\Performing load tests...")
               (write-line "Tests for successful load of LISP initialization files." fh)
               (write-line (acet-acadinfo-test-load "acad2000.lsp") fh)
               (write-line (acet-acadinfo-test-load "acad2000doc.lsp") fh)
               (write-line (acet-acadinfo-test-load "acettest.fas") fh)
               (write-line (acet-acadinfo-test-load "acetutil.fas") fh)
               (write-line (acet-acadinfo-test-load "acetmain.mnl") fh)
              );progn
          );if
 
        );progn then
        (progn
          (write-line "" fh)
          (write-line "*****FAILURE during lisp file load tests.**** " fh)
          (write-line "One of the following files causes an error on load: " fh)
          (write-line "  acad2000.lsp"  fh)
          (write-line "  acad2000doc.lsp"  fh)
          (write-line "  acettest.fas" fh)
          (write-line "  acetutil.fas" fh)
          (write-line "  acetmain.mnl" fh)
        );progn else
      );if
 
 
      (if (= 32 (logand 32 flags))
          (progn
           (setq flags (- flags 32))
           (write-line " ------------------------- TYPELIB TEST -------------------------" fh)
           (write-line "" fh)
           (acet-acadinfo-check-typelib fh)
           (write-line "" fh)
          );progn
      );if
 
      (if (= 64 (logand 64 flags))
          (progn
           (setq flags (- flags 64))
           (write-line "" fh)
           (write-line (strcat "(arx) -> " (acet-acadinfo-item-to-string (arx))) fh)
           (write-line "" fh)
           (write-line " ------------------- SYSTEM VARIABLE SETTINGS -------------------" fh)
           (write-line "|;" fh)
           (close fh)
           (acet-acadinfo-vars-to-scr fna -1);append
          );progn
          (close fh)
      );if
      (if (= 128 (logand 128 flags))
          (progn
           (setq flags (- flags 128))
           (acet-acadinfo-lisp-dump fna)
          );progn
      );if
 
      (princ "\nDone.")
    );progn then
    (princ "\nCannot open file for write.")
  );if
 
  (setq *error* olderr)
 
  (princ)
);defun acet-etbug-acadinfo


(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-do-header fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-do-general fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-do-express fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-do-fileloads fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-test-load fna)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-check-typelib fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-item-to-string a)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-vars-to-scr fna flag)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-lisp-dump fna)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgC3sDU6N1LDgP7j6M03TEViYAuJE3mfGzG1v2f+
;;; 4M+aS3MSq3eV8Rd5gh62OW/t4PTT423UPaiNr9UoqHOWIZ43JGLd8Bm8cdH9dXoq
;;; YHi++3atTnWzutOKpZ2wnxYN04ZVNo7pCXaUrOJcbRUxc6TALiWRtX1HFGizilhF
;;; NnLZ0mlh5yhipgWxjvDosRZVhBFBysT6xRkWWz+nUf7tPijvJEwFBwUlwCOtVxyg
;;; 3+GM4FIrBgzhzLHYu+f/bt/8cKbQX7uUgXtATY10AF47stJ5BMdGUtzE/yWdW6bN
;;; WqxaLJr9dsX/DGRegzvQQlxRqHUL3IqAbfhr+aUaJUlTl8zgy40Zoa/F4xilX+zy
;;; nUdYu7YSnKmoulDUPZrfBKoDZi2+DkixunY9tde/5A1mIlOFhbtSFO+8wjH4i9ER
;;; KBAg/14EQkoz8C3necE9AAtNYBo2C0D0OfD9jZpdqeVaaEayt0YeDEn6OV9pCBWZ
;;; 1OvMXO5xdM6iiozwKPRCwSoZ3R97fT7bdE6fga6Kt6h/CVnMipvM+CIE2wyxSfpY
;;; +ruLsouzIHG+uW37keG/cQUJd03h8XhjFVwmZvHJkT+syRQ+5OsGnheCrqYMfctj
;;; tKcZFxswu7sn7AwZqWz/kwE2VJbLRe63JvqOnE4Ew+evnJyjl42tFETt3pgaxgVx
;;; ZOzZjw==
;;; -----END-SIGNATURE-----