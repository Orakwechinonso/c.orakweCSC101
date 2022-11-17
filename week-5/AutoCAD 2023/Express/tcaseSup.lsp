;;
;;  TcaseSup.lsp
;;               
;;
;;  Copyright © 1999-2006 by Autodesk, Inc.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Valid modes are "Sentence" "Lower" "Upper" "Title" "Toggle"
;
(defun acet-tcase-change-string ( a mode / n lst b c d str j )
 (setq mode (xstrcase mode))
 
 (cond
  ((= mode "UPPER") (setq a (xstrcase a)));cond #1
 
  ((= mode "LOWER") (setq a (strcase a T)));cond #2
 
  ((= mode "SENTENCE")
   (setq   a (strcase a T) ;force to lower
         lst (acet-str-to-list "." a)	;split it apart using "." as delimiter
           d ""
   );setq
   ;; re-build the main string forcing the first non-blank character in each element to upper case.
   (setq j 0)
   (repeat (length lst)
    (setq str (nth j lst))
    (setq n 1)
    (if (< (+ j 1) (length lst))
        (setq str (strcat str "."))
    );if
    (while (and (<= n (strlen str))
                (or (= " " (substr str n 1))
                    (= "\t" (substr str n 1))
                );or
           );and
     (setq n (+ n 1))
    );while
    (if (> n 1)
        (setq b (substr str 1 (- n 1)))
        (setq b "")
    );if
    (setq c (substr str (+ n 1))
          d (strcat d
                    b
                    (xstrcase (substr str n 1))
                    c
            );strcat
    );setq
    (setq j (+ j 1))
   );repeat
   (setq a d)
  );cond #3
 
  ((= mode "TITLE")
   (setq   a (strcase a T) ;force to lower
         lst (acet-str-to-list " " a)	;split it apart using " " as delimiter
           d ""
   );setq
   ;; re-build the main string forcing the first character in each element to upper case.
   (setq j 0)
   (repeat (length lst)
    (setq str (nth j lst))
    (if (< (+ j 1) (length lst))
        (setq str (strcat str " "))
    );if
    (setq d (strcat d
                    (xstrcase (substr str 1 1))
                    (substr str 2)
            );strcat
    );setq
    (setq j (+ j 1))
   );repeat
   (setq a d)
  );cond #4
 
  ((= mode "TOGGLE")
   (setq d "")
   (setq n 1)
   (while (<= n (strlen a))
    (setq str (substr a n 1))
    (if (acet-str-is-upper str)
        (setq str (strcase str T))
        (setq str (xstrcase str))
    );if
    (setq d (strcat d str))
    (setq n (+ n 1));setq
   );while
   (setq a d)
  );cond #4
 
 );cond close
 
 a
);defun acet-tcase-change-string
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a string and returns T if the first character is in the alphabet and is upper case.
;
(defun acet-str-is-upper ( a / n flag )
 (if (> (strlen a) 0)
     (progn
      (setq a (substr a 1 1)
            n (ascii a)
      );setq
      (if (and (> n 64)
               (< n 91)
          );and
          (setq flag T)
          (setq flag nil)
      );if
     );progn then
     (setq flag nil)
 );if
 flag
);defun acet-str-is-upper
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a raw string string and list of format pairs of the form:
;  ((controlChar startposition)
;   (controlChar startposition)
;   (controlChar startposition)
;    ...
;  )
;
;returns a string with the formating applied in the proper locations.
;
;
(defun acet-mtext-format-apply ( str flst / n a b frmt j )
 
 (setq n 0)
 (repeat (length flst)
  (setq    a (nth n flst)
           j (cadr a)			;; the start position
        frmt (car a)			;; the formating string
           a (substr str 1 (- j 1))
           b (substr str j)
  );setq
  (if (and (or (= frmt "\\P")
               (= frmt "\\~")
           );or
           (= " " (substr b 1 1))
      );and
      (setq b (substr b 2))
  );if
  (setq str (strcat a frmt b))
  (setq n (+ n 1))
 );repeat
 str
);defun acet-mtext-format-apply
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a string from mtext and returns 
;a list of the form:
;( "RawTextString"
;  ((controlChar startposition)
;   (controlChar startposition)
;   (controlChar startposition)
;    ...
;  )
;)
;
(defun acet-mtext-format-extract ( str / lst raw len pos frmt flst a n j lst2 )
 
 (setq lst (list "{"	"}"	"\\P"	"\\~"
                 "\\{"	"\\}"	"\\O"	"\\L"
                 "\\S"	"\\A1"	"\\A2"	"\\A3"
                 "\\f"	"\\C"	"\\H"	"\\T"
                 "\\Q"	"\\W" "\\p"
           );list
       raw ""
       len (strlen str)
       pos 0
 );setq
 
 (while (> (strlen str) 0)
  
  (setq lst2 (mapcar '(lambda (x) (acet-str-find x str)) lst)
        lst2 (mapcar '(lambda (x) (if x (list x) x)) lst2)
        lst2 (apply 'append lst2)
           j (apply 'min lst2)
  );setq 
  (if (/= j 0)
      (progn
        (setq  raw (strcat raw 
                           (substr str 1 (- j 1))
                   )
               str (substr str j)
                 a (acet-mtext-format-bite str) ;; (list format str offset)
              frmt (car a)
               str (cadr a)
                 n (+ pos j)
               pos (+ pos 
                      j 
                      (caddr a)
                      (- (strlen frmt) 1)
                   )
              frmt (list frmt n)
              flst (cons frmt flst)
        );setq
        (setq n (+ (length lst) 10));get out of inner loop
      );progn
      (setq raw (strcat raw str)
            str ""
      );setq then get out
  );if    
 
 );while
 
 (list raw (reverse flst))
);defun acet-mtext-format-extract
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a string that begins with formating and returns the format string and
;the remainder of the string provided in a list
;  ("format" str)
;
(defun acet-mtext-format-bite ( str / a f1 n )
 
 (setq a (substr str 1 2)
       n 0
 )
 
 (cond 
  ((or (= "{" (substr str 1 1))
       (= "}" (substr str 1 1))
   );or
   (setq  f1 (substr str 1 1)
         str (substr str 2)
   );setq
  );cond #1
 
  ((or (= "\\P" a)
       (= "\\~" a)
   )
   (setq  f1 (substr str 1 2)
         str (strcat " " (substr str 3))
           n -1
   )
  );cond #2
 
  ((or (= "\\{" a)
       (= "\\}" a)
       (= "\\O" a)
       (= "\\L" a)
       (= "\\S" a)
       ;(= "\\\\" a)
   )
   (setq  f1 (substr str 1 2)
         str (substr str 3)
   )
  );cond #3
 
  ((or (= "\\A1" (substr str 1 3))
       (= "\\A2" (substr str 1 3))
       (= "\\A3" (substr str 1 3))
   );or
   (setq  f1 (substr str 1 3)
         str (substr str 4)
   );setq
  );cond #4
 
  ((or (= "\\f" a)
       (= "\\C" a)
       (= "\\H" a)
       (= "\\T" a)
       (= "\\Q" a)
       (= "\\W" a)
       (= "\\p" a)
   )
   (setq   n (acet-str-find ";" str)
          f1 (substr str 1 n)
         str (substr str (+ n 1))
           n 0
   );setq
  );cond #6
 );cond close
 
 (list f1 str n)
);defun acet-mtext-format-bite


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBvGmGK5VyjmKuMtO+WyrWyIhPN+4E3VEE+/DRO
;;; Dwymjo5FUF08IY0ATAsy2SN+PRXl6LhWZoz0umRSzjPk3Q6jd9pnToIVJF9FyuVq
;;; mTVFJldM7pD5PG6GXMzeIIKPa8s9sRk3tubAWijGxDw5iNrfdP0PifkwjJqdG/3e
;;; 6guw3d+SUYo1bShBleDu4n974cObL1nr6m7gU/GPLiR3Pq78+AtxHA/a0Bpbv435
;;; D6M2DLqQnRohBBZDVqYamU0DgfIPP6DOOIwvPjPChNQT83eI4eJSujpg2zGoeOVK
;;; dCYlvbA6qoapKWTFauaaQdaytXns+uvhVbAox/4pUmVHoM3QrpCuE4bweKWOnImB
;;; RurXFcVtwHDYzNEe49jFAR0/KcgC1nNGM4A1whQz/F11u/3HPssPJMrT9WfAT/rY
;;; IvT9jQCn0BL+/AnHY0HQsDkEic1XyzR5Oy2c7iGxWHlvUIPsQhznsMI8/hBbpUiy
;;; oWUZFt3tHiaI01WAYhpiuC0G9m2pNc3zxmKKg9gvsXk7nmHFtMBdrZP2hEh5GXZT
;;; tHYOLGfh1Hv678K5PMMy1ErnQ+fI8G0fv2UKntlTFYNFsCaRGg2c2XcAnd7R/OA/
;;; 26xq6C3XsY5gCA6fYuwiA08K9rlUdCM4KNL9YV5gnQ+M/CsO1+S2id3xs/puWBt0
;;; sjh81Q==
;;; -----END-SIGNATURE-----