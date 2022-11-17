;;
;;  Bscale.lsp - Express Tools block scaling utilities
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
;;
;; Description:
;; 
;; Series of block/xref scaling utilities.
;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun c:bscale ( / flt ss absolute val )
 (acet-error-init '(("cmdecho" 0
                     "highlight" nil
                     "regenmode" 0
                    )
                    1
                   )
 );acet-error-init
 
 (setq flt '((0 . "INSERT")));setq
 (if (and (setq ss (ssget "_:L" flt))
          (setq ss (car (acet-ss-filter (list ss nil T))));setq filter out non-current space
     );and
     (progn
      (setq absolute (acet-bscale-ui-get-mode));setq
      (if (= absolute 0)
          (setq val (acet-bscale-ui-get-relative-factors))
          (setq val (acet-bscale-ui-get-absolute-factors))
      );if
      (acet-bscale ss absolute (car val) (cadr val) (caddr val))
     );progn then
 );if
 (acet-error-restore)
);defun c:bscale
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PSBSCALE - paper-space-block-scale
;
(defun c:PSBSCALE ( / flt ss mode scale na e1 n setmode vh xd vs val id )
 (acet-error-init '(("cmdecho" 0 
                     "highlight" nil
                     "regenmode" 0
                    )
                    1
                   )
 );acet-error-init
 (setq id "ACET-PSBSCALE")
 (if (not (tblobjname "appid" id))
     (regapp id)
 );if
 
 (setq flt '((0 . "INSERT")));setq
 (if (and (setq ss (ssget "_:L" flt))
          (setq ss (car (acet-ss-filter (list ss nil T))));setq filter out non-current space
     );and
     (progn
      (setq setmode (acet-PSBSCALE-ui-get-mode)) ;; set or update - 1 or 0 respectively
      (if (= setmode 1)
          (progn
           (setq scale (acet-PSBSCALE-ui-get-scale-factors))
           (setq n 0)
           (repeat (sslength ss)
            (setq na (ssname ss n))
            (acet-ps-scale-set-xdata na scale id)
            (setq n (+ n 1));setq
           );repeat
          );progn then set the paper space height value
      );if
 
      (cond
       ((= (getvar "tilemode") 1)
        (princ "\n** Update not allowed in Model Tab **")
       );cond #1
       ((not (setq na (acet-currentviewport-ename)))
        (princ "\nUnable to get current viewport.")
       );cond #2
       ((acet-viewport-is-perspective na)
        (princ "\n** Update cannot be performed in a perspective view **")
       );cond #3
       (T
        (setq  e1 (entget na '("ACAD"))
               vs (cdr (assoc 41 e1))		;; view size
               xd (cdr (assoc -3 e1))
               xd (cdr (assoc "ACAD" xd))
               xd (acet-list-m-assoc 1040 xd)
               vh (cdr (nth 1 xd))		;; view height
              val (/ vh vs)
        );setq vh/vs=ps/ms
        (acet-bscale ss 2 val val val) ;; pass a mode of 2 to indicate ps scaling.
       );cond #4
      );cond close
 
     );progn then
 );if
 (acet-error-restore)
);defun c:PSBSCALE
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Returns 1 for set and 0 for update
;
(defun acet-PSBSCALE-ui-get-mode ( / def ans id )
  (setq  id "ACET-PSBSCALE-SET"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1)
  );if
  (if (= def 1)
      (setq ans "Set")
      (setq ans "Update")
  );if
  (initget "Set Update")
  (setq ans (getkword 
             (acet-str-format "\nUpdate or set paper space scale for blocks [Set/Update] <%1>: " ans)
            )
  );setq
  (if ans
      (progn
       (if (= ans "Set")
           (setq def 1)
           (setq def 0)
       );if
       (acet-setvar (list id def 2)) ;set this one in the reg only
      );progn then
  );if
  def
);defun acet-PSBSCALE-ui-get-mode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of 3 scale factors- (x y z)
;
(defun acet-PSBSCALE-ui-get-scale-factors ( / def ans x y z id )
  (setq  id "ACET-PSBSCALE-SCALE-X"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1.0)
  );if
  (initget "Xyz" 6)
  (setq ans (getdist (acet-str-format "\nX scale factor relative to paper space or [XYZ] <%1>: "
                                      (rtos def 2 (getvar "luprec"))
                     )
            )
  );setq
  (if (not ans)
      (setq ans def)
  );if
  (if (= ans "Xyz")
      (progn
       (initget 6)
       (setq ans (getdist (acet-str-format "\nX scale factor relative to paper space <%1>: "
                                           (rtos def 2 (getvar "luprec"))
                          )
                 )
       );setq
       (if (not ans)
           (setq x def)
           (setq x ans)
       );if
       (initget 6)
       (setq ans (getdist "\nY scale factor relative to paper space <use X scale factor>: "))
       (if (not ans)
           (setq y x)
           (setq y ans)
       );if
       (initget 6)
       (setq ans (getdist "\nZ scale factor relative to paper space <use X scale factor>: "))
       (if (not ans)
           (setq z x)
           (setq z ans)
       );if
      );progn then XYZ
      (progn
       (setq x ans
             z ans
       );setq
       (initget 6)
       (setq ans (getdist "\nY scale factor relative to paper space <use X scale factor>: "))
       (if ans
           (setq y ans)
           (setq y x)
       );if
      );progn else
  );if
 
  (if (and x y z)
      (progn
       (acet-setvar (list id x 3))
       (setq ans (list x y z))
      );progn
      (setq ans nil)
  );if
  ans
);defun acet-PSBSCALE-ui-get-scale-factors
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scale relative or absolute.
;
(defun acet-bscale-ui-get-mode ( / def ans id )
  (setq  id "ACET-BSCALE-ABSOLUTE"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 0)
  );if
  (if (= def 0)
      (setq ans "Relative")
      (setq ans "Absolute")
  );if
  (initget "Absolute Relative")
  (setq ans (getkword
             (acet-str-format 
               "\nSpecify type of scaling [Absolute (final)/Relative (multiply)] <%1>: "
               ans
             )
            )
  );setq
  (if ans
      (progn
       (if (= ans "Absolute")
           (setq def 1)
           (setq def 0)
       );if
       (acet-setvar (list id def 3))
      );progn then
  );if
  def
);defun acet-bscale-ui-get-mode
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-bscale-ui-get-relative-factors ( / def ans id x y z )
  (setq  id "ACET-BSCALE-RELATIVE-FACTOR-X"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1.0)
  );if
  (initget "Xyz" 6)
  (setq ans (getdist (acet-str-format "\nX scale factor or [XYZ] <%1>: "
                                      (rtos def 2 (getvar "luprec"))
                     )
            )
  );setq
  (if (not ans)
      (setq ans def)
  );if
  (if (= ans "Xyz")
      (progn
       (initget 6)
       (setq ans (getdist (acet-str-format "\nX scale factor <%1>: "
                                           (rtos def 2 (getvar "luprec"))
                          )
                 )
       );setq
       (if (not ans)
           (setq x def)
           (setq x ans)
       );if
       (initget 6)
       (setq ans (getdist "\nY scale factor <use X scale factor>: "))
       (if (not ans)
           (setq y x)
           (setq y ans)
       );if
       (initget 6)
       (setq ans (getdist "\nZ scale factor <use X scale factor>: "))
       (if (not ans)
           (setq z x)
           (setq z ans)
       );if
      );progn then XYZ
      (progn
       (setq x ans
             z ans
       );setq
       (initget 6)
       (setq ans (getdist "\nY scale factor <use X scale factor>: "))
       (if ans
           (setq y ans)
           (setq y x)
       );if
      );progn else
  );if
 
  (if (and x y z)
      (progn
       (acet-setvar (list id x 3))
       (setq ans (list x y z))
      );progn
      (setq ans nil)
  );if
  ans
);defun acet-bscale-ui-get-relative-factors
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-bscale-ui-get-absolute-factors ( / def ans id x y z )
  (setq  id "ACET-BSCALE-ABSOLUTE-FACTOR-X"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1.0)
  );if
  (initget "Xyz" 6)
  (setq ans (getdist (acet-str-format "\nAbsolute X scale or [XYZ] <%1>: "
                                      (rtos def 2 (getvar "luprec"))
                     )
            )
  );setq
  (if (not ans)
      (setq ans def)
  );if
  (if (= ans "Xyz")
      (progn
       (initget 6)
       (setq ans (getdist (acet-str-format "\nAbsolute X scale <%1>: "
                                           (rtos def 2 (getvar "luprec"))
                          )
                 )
       );setq
       (if (not ans)
           (setq x def)
           (setq x ans)
       );if
       (initget 6)
       (setq ans (getdist "\nAbsolute Y scale <use X scale>: "))
       (if (not ans)
           (setq y x)
           (setq y ans)
       );if
       (initget 6)
       (setq ans (getdist "\nAbsolute Z scale <use X scale>: "))
       (if (not ans)
           (setq z x)
           (setq z ans)
       );if
      );progn then XYZ
      (progn
       (setq x ans
             z ans
       );setq
       (initget 6)
       (setq ans (getdist "\nAbsolute Y scale <use X scale>: "))
       (if ans
           (setq y ans)
           (setq y x)
       );if
      );progn else
  );if
 
  (if (and x y z)
      (progn
       (acet-setvar (list id x 3))
       (setq ans (list x y z))
      );progn
      (setq ans nil)
  );if
  ans
);defun acet-bscale-ui-get-absolute-factors
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Takes a selection set a scaling mode and values.
; Scaling modes are as follows:
; 0 relative
; 1 absolute
; 2 paperspace
;
; Returns number of objects successfully modified.
;
(defun acet-Bscale ( ss mode xs ys zs / id na n j tmp )
 (setq  id "ACET-PSBSCALE"
       tmp "acet-tmp-block"
 );setq
 (acet-sysvar-set 
  (list "regenmode" 0 
           "attreq" 0
          "ucsicon" 0
  )
 )
 (acet-ui-progress-init "Scaling block inserts" (sslength ss))
 (setq j 0)
 (setq n 0) 
 (repeat (sslength ss)
  (setq na (ssname ss n));setq
  (acet-ui-progress-safe n)
  (if (acet-Bscale-ent na mode xs ys zs id tmp)
      (setq j (+ j 1));setq
  );if
 (setq n (+ n 1));setq
 );repeat
 (acet-ui-progress-done)
 (acet-sysvar-restore)
 (if (tblobjname "block" tmp)
     (acet-table-purge "block" tmp T)
 );if
 j
);defun acet-Bscale
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-Bscale-ent ( na mode xs ys zs id tmp / e1 ps bna na2 ixs iys izs )
  (setq  e1 (entget na (list id))
        ixs (cdr (assoc 41 e1))
        iys (cdr (assoc 42 e1))
        izs (cdr (assoc 43 e1))
  );setq
  (cond
   ((= mode 0)				;; scale relative
    (setq xs (* xs ixs)
          ys (* ys iys)
          zs (* zs izs)
    );setq
   );cond #1
   ((= mode 2)				;; get ps height from xdata
    (if (setq ps (acet-ps-scale-get-xdata na id))
        (setq xs (* (car ps) xs)
              ys (* (cadr ps) ys)
              zs (* (caddr ps) zs)
        );setq then 			(val=ps/ms)
        (setq xs nil);setq else no xdata
    );if
   );cond #2
  );cond close
 
  (if (and xs ys zs)
      (progn
       (setq bna (cdr (assoc 2 e1)))
       (if (or (/= 1 (cdr (assoc 66 e1)))
               (= (substr bna 1 1) "*")
           );or
           (progn
            (setq e1 (subst (cons 41 xs) (assoc 41 e1) e1)
                  e1 (subst (cons 42 ys) (assoc 42 e1) e1)
                  e1 (subst (cons 43 zs) (assoc 43 e1) e1)
            );setq
            (setq e1 (entmod e1))
           );progn then either no attribs or it's annonymous
           (progn 
            (acet-ucs-cmd (list "_ob" na))
            (if (tblobjname "block" tmp)
                (command "_.-block" tmp "_y" "0,0" na "")
                (command "_.-block" tmp "0,0" na "")
            );if
            (if (not (entget na))
                (progn
                 (setq na2 (entlast))
                 (command "_.-insert" tmp "0,0" "_xyz" 
                          (/ xs ixs) 
                          (/ ys iys)
                          (/ zs izs)
                          "0"
                 )
                 (if (not (equal na2 (entlast)))
                     (acet-explode (entlast))
                 );if
                );progn then the block command worked
            );if
            (acet-ucs-cmd (list "_p"))
           );progn else it has attributes and it is NOT annonymous.
       );if
      );progn then
      (setq e1 nil);setq else
  );if
  e1
);defun acet-Bscale-ent


(acet-autoload2	'("tscale.lsp"	(acet-ps-scale-get-xdata na appid)))
(acet-autoload2	'("tscale.lsp"	(acet-ps-scale-set-xdata na scale appid)))
(acet-autoload2	'("tscale.lsp"	(acet-viewport-is-perspective na)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCoMwMi2RFzPqJ0w3JmmyBbqfMWjnceON7VMDs9
;;; AND+3z08SLNxvDO1dMYRt8rpX71BZvRWXOZeDyDtUQImsUM5HKYKIhJSwbP34TLs
;;; YQimpBXNfbBOMk3lJ/V7OM+P3uE/hvUtKyrbeVUAqnDpv4brCZcGuvwgO5KpLu+J
;;; fwCi+f86a3mvqr/AXAewseYR9MQqUudgOQdJHmIb+NTKkqRo9GWhSXStXNTXn6Tx
;;; UNu1ygxlrEG7R3Nncs3hnUp434IAcaKTcu6SAF7Od0f12nsdMRoi3mP/usHXZA67
;;; Ktv0ACM4Sfs/vpfC9SGoYxoOG1e8/5Oo5dwOfp+QtM+5fq69i1VYZlrVJZ+/0mWq
;;; xkEGWKDH+z/bKpsaJHrePFgEmANosCtZngNSQWG3DTM0hmymH7qt6PerHA8HQKJD
;;; BbBsKnZ13FiPCVA8GHpHtv2QjZmOw2tvst3P9ldxJdGjyW+4kRWfBzDg+NdvQ16V
;;; gqaEnRHv/T/+arggLSowQDOVtUgcm9pgAyJEVDqHEcGj/EZ1RSPrrEpGKbJJOX20
;;; RMQ2zm+d4BSGYW9s9RNb+YPwfxVziQ0FmwnmHoXj6W5EG72eMhjA1c+EjRK98bXx
;;; 4S5uNE89HIS6S84hOvCY8C725kFRUwlsePTn7UImjwSmtsKr2LX7naCmr0SzdCSU
;;; QzhjLQ==
;;; -----END-SIGNATURE-----