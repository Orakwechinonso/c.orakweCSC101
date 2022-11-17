;;;
;;;    ACETESS.LSP
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
 
; ACETESS.LSP - Selection set tools.
;
;          Exclude Options -
;               - Offers a set of alternate options for each of the standard
;               selection methods such as W, C, P, F, WP, and CP. These alternate
;               options are the opposites of their standard AutoCAD counterparts.
;               They are prefixed with "EX" meaning _exclusion_.
;                i.e.
;                 For example: "EXC" prompts for a crossing window like "C", but EXC means
;                              select everything in the drawing _EXCEPT_ what falls within
;                              the selected corner points.
;
;               The options are: EXW  - exclusion window
;                                EXC  - exclusion crossing window
;                                EXP  - exclusion previous
;                                EXF  - exclusion fence
;                                EXWP - exclusion window polygon
;                                EXCP - exclusion crossing polygon
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun c:exp ( / ss ss2 na n )
(acet-error-init (list nil nil))
(setq  ss (ssget "_p")
      ss2 (ssget "_x")
);setq
(if ss
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
    (progn
     (if ss2
         (progn
          (princ "\nNo previous selection set. Selecting everthing...")
          (if (not (equal (getvar "cmdnames") ""))
              (command ss2)
              (sssetfirst ss2 ss2)
          );if
         );progn then
     );if
    );progn then
);if
(acet-error-restore)
);defun c:exp
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exc ( / ss ss2 na n p1 p2)
 
(acet-error-init (list nil nil))
(if (and (setq ss2 (ssget "_x"));setq
         (setq p1 (getpoint "\nSpecify first corner: "));setq
         (not (initget 32))
         (setq p2 (getcorner p1 "Specify other corner: "));setq
         (or (setq ss (ssget "_c" p1 p2))
             (setq ss (ssadd))
         );or
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
);if
(acet-error-restore)
);defun c:exc
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exw ( / ss ss2 na n p1 p2)
 
(acet-error-init (list nil nil))
(if (and (setq ss2 (ssget "_x"));setq
         (setq p1 (getpoint "\nSpecify first corner: "));setq
         (setq p2 (getcorner p1 "Specify other corner: "));setq
         (or (setq ss (ssget "_w" p1 p2))
             (setq ss (ssadd))
         );or
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
);if
(acet-error-restore)
);defun c:exw
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exf ( / lst ss ss2 na p1 n)
 
(acet-error-init (list nil nil
                      '(progn
                        (acet-sysvar-set (list "cmdecho" 0))
                        (command "_.redraw")
                        (acet-sysvar-restore)
                       )
                )
)
(if (setq ss2 (ssget "_x"));setq
    (progn
     (setq p1 (getpoint "\nSpecify first fence point: "));setq
     (while p1
      (cond
       ((equal (type p1) 'LIST)
        (setq lst (append (list p1) lst));setq
        (if (> (length lst) 1)
            (grdraw (car lst)
                    (cadr lst)
                    7
                    1
            );grdraw
        );if
       );cond #1
       ((equal p1 "Undo")
        (if (> (length lst) 1)
            (grdraw (car lst)
                    (cadr lst)
                    0
            );grdraw
        );if
        (setq lst (cdr lst)
               p1 (car lst)
        );setq
       );cond #2
      );cond
      (if p1
          (progn
           (initget "Undo _Undo" (+ 32 128))
           (setq p1 (getpoint p1 "\nSpecify an option [Undo] <Endpoint of line>: "));setq
          );progn
      );if
     );while
 
     (bns_vlist_undraw lst)
 
     (if (and (setq lst (bns_points_in_view lst))
              (or (setq ss (ssget "_f" lst))
                  (setq ss (ssadd))
              );or
         );and
         (progn
          (setq n 0)
          (repeat (sslength ss)
           (setq na (ssname ss n))
           (if (ssmemb na ss2)
               (setq ss2 (ssdel na ss2));setq then
           );if
           (setq n (+ n 1));setq
          );repeat
          (if (not (equal (getvar "cmdnames") ""))
              (command ss2)
              (sssetfirst ss2 ss2)
          );if
         );progn then
     );if
    );progn then
);if
(acet-error-restore)
);defun c:exf
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:excp ( / ss ss2 lst n na)
 
(acet-error-init (list nil nil
                      '(progn
                        (acet-sysvar-set (list "cmdecho" 0))
                        (command "_.redraw")
                        (acet-sysvar-restore)
                       )
                )
)
(if (and (setq ss2 (ssget "_x"));setq
         (setq lst (acet-ui-polygon-select 1));setq
         (or (setq  ss (ssget "_cp" lst));setq
             (setq ss (ssadd))
         );or
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
);if
(acet-error-restore)
);defun c:excp
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exwp ( / ss ss2 lst n na)
 
(acet-error-init (list nil nil
                      '(progn
                        (acet-sysvar-set (list "cmdecho" 0))
                        (command "_.redraw")
                        (acet-sysvar-restore)
                       )
                )
)
 
(if (and (setq ss2 (ssget "_x"));setq
         (setq lst (acet-ui-polygon-select 0));setq
         (or (setq  ss (ssget "_wp" lst));setq
             (setq ss (ssadd))
         );or
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
);if
(acet-error-restore)
);defun c:ewp
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a list of points
;returns a list of points that are on screen.
;
(defun bns_points_in_view ( lst / a b n len lst2 lst3 x x2 y y2 )
 
(setq  len (length lst)
      lst3 (acet-geom-view-points)
       lst (acet-geom-m-trans lst 1 2)
      lst3 (acet-geom-m-trans (acet-geom-view-points) 1 2)
         x (car (car lst3))
        x2 (car (cadr lst3))
         y (cadr (car lst3))
        y2 (cadr (cadr lst3))
);setq
(if (> (length lst) 1)
    (setq lst2 (list (bns_truncate_2_view (car lst) (cadr lst) x y x2 y2)
               );list
    );setq
);if
(setq a (car lst));setq
(setq n 1);setq
(while (< n len)
(setq b (nth n lst)
      b (bns_truncate_2_view b a x y x2 y2)
);setq
(if (not (equal b (last lst2) 0.000001))
    (setq lst2 (append lst2 (list b)));setq
);if
(setq a b);setq
(setq n (+ n 1));setq
);while
(setq b (bns_truncate_2_view b a x y x2 y2));setq
(if (not (equal b (last lst2) 0.000001))
    (setq lst2 (append lst2 (list b)));setq then
);if
 
(setq lst2 (acet-geom-m-trans lst2 2 1));setq
 
lst2
);defun bns_points_in_view
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_vlist_undraw ( lst / j )
 (setq j 0)
 (while (< j (- (length lst) 1))
  (grdraw (nth j lst)
          (nth (+ j 1) lst)
          0
  );grdraw
  (setq j (+ j 1));setq
 );while
);defun bns_vlist_undraw


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCmwhL9NTMsFxN6F9Zg2YnETWscfBQalbpxWybC
;;; lUUUYp5swEv7+h1JfQ6DOhSaIOC3dg61dl7J/iTBekNMyT8e4Tq8ns3Po+Gk9XR6
;;; 53yr5698I0MeUR2N4K20SI+mVIGqvoRL7xBhrBGXMTFWOoYFglcFdj1XrstXk0Z7
;;; UuNoC3+sjC61qP7VFJNMV+mj1estZF17vORVT5P/2R4H4AIlDET5SL5yevh0oDcx
;;; X9OFjHsyXOt4aTVG3Ktqvyf7hk8+z4Z2L4sHD7rsINBLaq9M+lIEL8fGvoxMlZC5
;;; YCkGPfeb0MTq68IGQDNerxPJDA3bVx/4RVGoVkHnAs6rr75vzVW7ZVbCL2vETs30
;;; eOjVZx/ah5ku8ey79ZF4xF6wtMRefERX9QOY9qCUFO8Dnlt2uJz7iNKFKmisTNnx
;;; Wsp3jzOOu2Bh7jiA+WkqV72ZO2hYYFV4LlMK7Y4J1a+Ugw+4Xco6ca+/lOvWaanG
;;; Tl5FsthKQx/wnqGIR3naw5VAIa6fy2Of+rUN3w1sDAp0kZzXY8KnzpLaT0TPO10V
;;; d3DMOZerSLlaHpnEKxbtv/lQkdcokCWzrL7r4bX5C9l6BIUYibGSmJNGjExGOB6z
;;; +ecWEJremf/Q12IoMybr0wpjqkk7ZLHWFw5/BhwHoaJsl0u+q1nj7rwT67ivV2Nu
;;; Y5LRVg==
;;; -----END-SIGNATURE-----