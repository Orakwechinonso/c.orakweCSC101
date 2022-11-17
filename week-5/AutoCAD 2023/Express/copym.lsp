;;
;;  Copym.lsp - Multiple copy command with measure, divide and array capabilities.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:copym ( / ss p1 cmd snaptp ucshold )
 (acet-error-init 
   (list (list  "cmdecho" 0 
               "snaptype" 0
               "snapmode" nil
               "gridmode" nil
               "snapunit" nil
               "gridunit" nil
         );list
         0
         '(progn
            (acet-sysvar-set (list "cmdecho" 0))
            (if ss 
                (acet-ss-redraw ss 4)
            )
            (if ucshold
                (acet-ucs-set ucshold)
            )
            (acet-sysvar-restore)
            (princ)
          );progn
   );list
 );acet-error-init
 (setq ucshold (acet-ucs-get nil))
 (if (setq ss (ssget)) 
     (progn
      (acet-ss-redraw ss 3)
      (setq p1 (getpoint "\nBase point: "))
      (acet-ss-redraw ss 4)
      (if p1
          (acet-copym ss p1)
      );if
     );progn then
 );if
 (acet-error-restore)
);defun c:copym
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-copym ( ss p1 / na p2 n d lst j p3 )
 (setq p2 T)
 (setq n 0)
 (while p2
  (setq na (entlast))
  (if (not lst)
      (setq lst (list (list ss p1)))
  );if
  (setvar "lastpoint" p1)
  (acet-ss-redraw ss 3)
  (initget 128 "Repeat Divide Measure Array Undo eXit")
  (setq p2 (acet-ss-drag-move 
            ss
            p1 
            "\nSecond point or \n[Repeat (last)/Divide/Measure/Array (dynamic)/Undo] <exit>: "
            nil
           );acet-ss-drag-move 
  );setq
  (acet-ss-redraw ss 4)
  (if (= p2 "eXit")
      (setq p2 nil)
  );if
  (cond
   ((= p2 "Undo")
    (if (= n 0)
        (princ "\nNothing to undo.")
        (progn
         (command "_.undo" "1")
         (setq   n (- n 1)
               lst (cdr lst)
                ss (car lst)
                p1 (cadr ss)
                ss (car ss)
         );setq
        );progn else
    );if
   );cond #1
   ((= p2 "Repeat")
    (if (= n 0)
        (princ "\nNothing to repeat.")
        (progn
         (setq p2 (cadr (car lst))
               p1 (cadr (cadr lst))
                d (list (- (car p2) (car p1))
                        (- (cadr p2) (cadr p1))
                        (- (caddr p2) (caddr p1))
                  );list
         );setq
         (command "_.copy" ss "" d "")
         (setq   n (+ n 1)
                ss (acet-ss-new na)
                p1 (list (+ (car p2) (car d))
                         (+ (cadr p2) (cadr d))
                         (+ (caddr p2) (caddr d))
                   );list
               lst (cons (list ss p1) lst)
         );setq
        );progn else
    );if
   );cond #2
   ((equal 'LIST (type p2))
    (command "_.copy" ss "" p1 p2)
    (setq   n (+ n 1)
           ss (acet-ss-new na)
           p1 p2
          lst (cons (list ss p1) lst)
    );setq
   );cond #3
   ((and (= "Divide" p2)
         (setq p3 (getpoint p1 "\nSelect division ending point: "))
         (progn
          (initget 6)
          (setq j (getint "\nNumber of copies: "))
         );progn
    );and
    (setq  ss (acet-copym-divide ss p1 p3 j)
           p1 p3
          lst (cons (list ss p1) lst)
            n (+ n 1)
    );setq
   );cond #4
   ((and (= "Measure" p2)
         (setq p3 (getpoint p1 "\nSelect measure ending point: "))
         (progn
          (initget 6)
          (setq d (getdist "\nDistance between copies: "))
         );progn
    );and
    (setq  ss (acet-copym-measure ss p1 p3 d) ;returns selset and base point
           p1 (cadr ss)
           ss (car ss)
          lst (cons (list ss p1) lst)
            n (+ n 1)
    );setq
   );cond #5
   ((= "Array" p2)
    (setq  ss (acet-copym-array ss p1)
           p1 (cadr ss)
           ss (car ss)
          lst (cons (list ss p1) lst)
            n (+ n 1)
    );setq
   );cond #6
   (p2
    (princ "\nInvalid input.")
   );cond #7
  );cond close
 );while
 
);defun acet-copym
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-copym-array ( ss p1 / a )
 (initget "Pick Measure Divide")
 (setq a (getkword "\nPick (dynamic)/Measure/Divide <Pick>: "))
 (cond
  ((or (not a)
       (= a "Pick")
   );or
   (setq a (acet-copym-array-dynamic ss p1))
  );cond #1
  ((= a "Measure")
   (setq a (acet-copym-array-measure ss p1))
  );cond #2
  ((= a "Divide")
   (setq a (acet-copym-array-divide ss p1))
  );cond #3
 );cond close
 a
);defun acet-copym-array
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-copym-array-dynamic ( ss p1 / snap grid snapu gridu p2 p3 p4 dx dy lst ss2 na a )
 
 (acet-undo-begin)
 (setq p2 (getangle p1 "\nSpecify angle <0>: "))
 (if p2
     (setq p2 (polar p1 p2 1.0)) ;convert angle to a point
     (setq p2 (polar p1 0.0 1.0));use default of 0 and convert to point
 );if
 (setq p3 (polar p1 (+ (angle p1 p2) (/ pi 2.0)) 1.0)
       p1 (trans p1 1 0)
       p2 (trans p2 1 0)
       p3 (trans p3 1 0)
 );setq
 (acet-ucs-cmd (list "_3p" (trans p1 0 1) (trans p2 0 1) (trans p3 0 1)))
 (setq p1 (trans p1 0 1)
       p2 (trans p2 0 1)  
       p3 (trans p3 0 1)  
 );setq
 
 (setq  p2 (acet-copym-getcorner p1 "\nPick a corner point to establish COLUMN and ROW distances: " T)
        dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1))
       lst (list p1)
        p4 T
 );setq 
 (acet-sysvar-set
  (list
    "snapunit" (list (abs dx) (abs dy))
    "gridunit" (list (abs dx) (abs dy))
    "snapmode" 1
    "gridmode" 1
  )
 );acet-sysvar-set
 
 (while p4
  (setvar "snapmode" 1)
  (setvar "gridmode" 1)
  ;(setq p4 (getpoint p1 "\nPick location for array element or <enter> when done: "))
  (setq p4 (acet-ss-drag-move 
             ss
             p1
             "\nPick location for array element or <enter> when done: "
             nil
            );acet-ss-drag-move 
  );setq
  (cond
   ((not p4) T);cond #1
   ((member p4 lst)
    (princ "\n*invalid* You already picked that point!")
   );cond #2
   (T
    (setq  na (entlast)
          lst (cons p4 lst)
    );setq
    (command "_.copy" ss "" p1 p4)
   );cond #3
  );cond close
 );while
 (if na 
     (setq  p1 (trans (getvar "lastpoint") 1 0)
           ss2 (acet-ss-new na)
     );setq
     (setq ss2 ss);setq else
 );if
 (acet-ucs-cmd (list"_prev"))
 (setq p1 (trans p1 0 1))
 
 (acet-sysvar-restore)
 (acet-undo-end)
 
 (list ss2 p1)
);defun acet-copym-array-dynamic
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-copym-getcorner ( p1 msg nozero / flag p2 na )
 (while (not flag)
  (setq na (entlast))
  (command "_.rectang" p1)
  (while (wcmatch (getvar "cmdnames") "*RECTANG*") 
   (princ msg)
   (command pause)
  );while
  (setq p2 (getvar "lastpoint"));setq
  (if (not (equal na (entlast)))
      (entdel (entlast))
  );if
  (cond
   ((not nozero)
    (setq flag T)
   );cond #1
   ((and (equal (car p1) (car p2) 0.00000001)
         (equal (cadr p1) (cadr p2) 0.00000001)
    );and
    (princ "\n*Points cannot be equal*")
   );cond #2
   ((= (car p1) (car p2))
    (princ "\n*X coords cannot be equal*")
   );cond #3
   ((= (cadr p1) (cadr p2))
    (princ "\n*Y coords cannot be equal*")
   );cond #4
   (T
    (setq flag T)
   );cond #5
  );cond close
 );while
 p2
);defun acet-copym-getcorner
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-copym-array-measure ( ss p1 / snap grid snapu gridu p2 p3 p4 dx dy 
                                     ss2 na a n j k m x y
                           )
 (acet-undo-begin)
 
 (setq p2 (getangle p1 "\nSpecify angle <0>: "))
 (if p2
     (setq p2 (polar p1 p2 1.0)) ;convert angle to a point
     (setq p2 (polar p1 0.0 1.0));use default of 0 and convert to point
 );if
 (setq p3 (polar p1 (+ (angle p1 p2) (/ pi 2.0)) 1.0)
       p1 (trans p1 1 0)
       p2 (trans p2 1 0)
       p3 (trans p3 1 0)
 );setq
 (acet-ucs-cmd (list "_3p" (trans p1 0 1) (trans p2 0 1) (trans p3 0 1)))
 
 (setq  p1 (trans p1 0 1)
        p2 (acet-copym-getcorner p1 "\nPick a corner point to establish ROW and COLUMN distances: " T)
        dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1))
        p4 T
 );setq 
 (acet-sysvar-set
  (list
    "snapunit" (list (abs dx) (abs dy))
    "gridunit" (list (abs dx) (abs dy))
    "snapmode" 1
    "gridmode" 1
  )
 );acet-sysvar-set
 
 (setq p2 (acet-copym-getcorner p1 "\nOther corner for array fill: " T))
 (if (> (car p2) (car p1))
     (setq dx (abs dx))
     (setq dx (* -1.0 (abs dx)))
 );if
 (if (> (cadr p2) (cadr p1))
     (setq dy (abs dy))
     (setq dy (* -1.0 (abs dy)))
 );if
 (setq k (/ (abs (- (car p2) (car p1)))
            (abs dx)
         )
       m (/ (abs (- (cadr p2) (cadr p1)))
            (abs dy)
         )
       k (+ 1 (atoi (rtos k 2 0)))
       m (+ 1 (atoi (rtos m 2 0)))
 );setq
 
 (setq n 0)
 (repeat m	;; rows
  (setq y (+ (cadr p1) (* dy n)))
 
  (setq j 0)
  (repeat k	;; columns
   (setq x (+ (car p1) (* dx j)))
   (setq na (entlast))
   (if (not (and (= n 0)
                 (= j 0)
            );and
       );not
       (command "_.copy" ss "" p1 (list x y (caddr p1)))
   );if
   (setq j (+ j 1));setq
  );repeat
 
 (setq n (+ n 1))
 );repeat
 (if na
     (setq  p1 (trans (getvar "lastpoint") 1 0)
           ss2 (acet-ss-new na)
     );setq
     (setq ss2 ss);setq else
 );if
 (acet-ucs-cmd (list "_prev"))
 (setq p1 (trans p1 0 1))
 
 (acet-sysvar-restore)
 (acet-undo-end)
 
 (list ss2 p1)
);defun acet-copym-array-measure
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-copym-array-divide ( ss p1 / p2 dx dy ss2 na a n j k m x y p3 )
 (acet-undo-begin)
 
 (setq p2 (getangle p1 "\nSpecify angle <0>: "))
 (if p2
     (setq p2 (polar p1 p2 1.0)) ;convert angle to a point
     (setq p2 (polar p1 0.0 1.0));use default of 0 and convert to point
 );if
 (setq p3 (polar p1 (+ (angle p1 p2) (/ pi 2.0)) 1.0)
       p1 (trans p1 1 0)
       p2 (trans p2 1 0)
       p3 (trans p3 1 0)
 );setq
 (acet-ucs-cmd (list "_3p" (trans p1 0 1) (trans p2 0 1) (trans p3 0 1)))
 
 (setq p1 (trans p1 0 1)
       p2 (acet-copym-getcorner p1 "\nOther corner for array fill: " nil)
 );setq
 
 (initget 6)
 (setq k (getint "\nEnter number of columns: "))
 (initget 6)
 (setq m (getint "\nEnter number of rows: "))
 (setq dx (/ (- (car p2) (car p1)) k)
       dy (/ (- (cadr p2) (cadr p1)) m)
 );setq
 
 (setq n 0)
 (repeat m	;; rows
  (setq y (+ (cadr p1) (* dy n)))
 
  (setq j 0)
  (repeat k	;; columns
   (setq x (+ (car p1) (* dx j)))
   (setq na (entlast))
   (if (not (and (= n 0)
                 (= j 0)
            );and
       );not
       (command "_.copy" ss "" p1 (list x y (caddr p1)))
   );if
   (setq j (+ j 1));setq
  );repeat
 
 (setq n (+ n 1))
 );repeat
 (if na
     (setq  p1 (trans (getvar "lastpoint") 1 0)
           ss2 (acet-ss-new na)
     );setq
     (setq ss2 ss);setq else
 );if
 (acet-ucs-cmd (list "_prev"))
 (setq p1 (trans p1 0 1))
 
 (acet-undo-end)
 
 (list ss2 p1)
);defun acet-copym-array-divide
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a selection set, two points and the distance between 
;consecutive copies.
;Returns a list containing a selection set the most 
;recent copy and a base point.
;
(defun acet-copym-measure ( ss p1 p3 d / j n na p2 )
 
 (acet-undo-begin)
 (setq j (fix (/ (distance p1 p3) d))
       n 1
 );setq
 (repeat j
 (setq p2 (polar p1 (angle p1 p3) (* d n))
       na (entlast)
 );setq
 (command "_.copy" ss "" p1 p2)
 (if (= n j)
     (setq ss (acet-ss-new na))
 );if
 (setq n (+ n 1))
 );repeat 
 (acet-undo-end)
 (list ss p2)
);defun acet-copym-measure
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a selection set, two points and the number of copies to 
;make of the selection between the two points.
;returns a selection set the most recent copy
;
(defun acet-copym-divide ( ss p1 p3 j / d n na p2 )
 
 (acet-undo-begin)
 (setq d (/ (distance p1 p3) j)
       n 1
 );setq
 (repeat j
 (setq p2 (polar p1 (angle p1 p3) (* d n))
       na (entlast)
 );setq
 (command "_.copy" ss "" p1 p2)
 (if (= n j)
     (setq ss (acet-ss-new na))
 );if
 (setq n (+ n 1))
 );repeat 
 (acet-undo-end)
 ss
);defun acet-copym-divide


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCJYXsXojVqUgMEP30swTWTMsZn8qWCs8dgNTs5
;;; 2qn2mfYnSmu4P3/zvjZJkNjMOLIw+1+yst0X3Ck5rlHWYW34RFX08DMxzie/Z7/4
;;; uOZpEfPa8rAAxHVm3/X/jjykdOMKhUePEZSnpMZSEuepoGDSsSQ1t3sTMQI2MNK0
;;; tL4EEhFUVH5Nv9nQE/VYvvULwXjN7bLIhICxJTfEi0oeXlKNKiJ3e1lUh+qbzbaK
;;; srpxlgm/uWEHXLJZ+AzuDI1o8siG6XiKUqVLxOjFNpwPJo8roOvALcWsvCY3UwhC
;;; eVEzsAWLI9W8uO/53SjJmUBzsG5v3u6ry2lgCMWlhCe+zyJ6/Ps9zMfJvrPYhI7D
;;; 7VHsqDRYn7hBwnHgYOGMdtpfBf+qY0CS1vXi8rgkmTZ5qetGLUk6PfYK1OcOYJME
;;; dggVoFf53ktkqt3UKjUK9+28memoG0Bbml5mCdz2ImA/TU/1wR6EG5GIpFCLOXBT
;;; fOb66Qm8KQIIANAbTplghaOboCju6NyDMWEm9C5iz/RXyzjYWdG5u5ZQeqHh8TZb
;;; l/ftQjOzludBpvW0SCEERTblU1h6PJyRuzWLCmTNmAZlFiOgiAjmOurMPNUp77lR
;;; oQy36uGE2aPAqhk69ayv6nbymjeh1NLmZZ7+iGvmTk1+X4HUnL+akNnlHR4h45Ca
;;; fanxOw==
;;; -----END-SIGNATURE-----