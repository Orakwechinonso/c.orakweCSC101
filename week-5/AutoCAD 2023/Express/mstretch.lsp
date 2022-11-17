;;
;;;
;;;    MSTRETCH.LSP
;;;    Copyright © 2007 by Autodesk, Inc.
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
 
(defun c:mstretch ( / n ss ss2 a b lst lst2 lst3 lst4 flag p1 p2 p3 p4 zflag delname ssrmv templst checkss found)
    (acet-error-init
        (list
            (list   "cmdecho" 0
                "highlight" 0
                "dragmode" (getvar "dragmode")
                "osmode" 0
                "cecolor" "6"
                "celtype" "CONTINUOUS"
                "limcheck" 0
            )
            T
            '(progn
                (acet-temp-segment nil nil nil 1)
                (acet-sysvar-set (list "cmdecho" 0));turn cmdecho off
                (command "_.redraw");do redraw
                (acet-sysvar-restore);re-set cmdecho back
                (princ)
                ;(command "_.redraw")
             )
        )
    )
    (sssetfirst nil nil)
    (princ "\nDefine crossing windows or crossing polygons...")
    (setvar "highlight" 1)
    (setq ss (ssadd))
    (command "_.select")
    (while (not flag)
        (if (not lst)
            (progn
                (initget 128 "CP C _CP C")
                (princ "\nOptions: Crossing Polygon or Crossing first point")
                (setq a (getpoint "\nSpecify an option [CP/C] <Crossing first point>: "))
            );progn
            (progn
                (initget 128 "CP C Undo _CP C Undo")
                (princ "\nOptions: Crossing Polygon, Crossing first point or Undo")
                (setq a (getpoint "\nSpecify an option [CP/C/Undo] <Crossing first point>: "))
            );progn
        );if
        (cond
            ;cond #1
            ((or (and (= a "C")
                     (progn (initget 1) (setq a (getpoint "\nSpecify first corner: ")))
                     (progn (initget 33) (setq b (getcorner a "\nSpecify other corner: ")))
                     ;(setq lst2 (acet-geom-rect-points a b));setq
                 );and
                 (and a
                     (equal (type a) 'LIST)
                     (progn (initget 33) (setq b (getcorner a "\nSpecify other corner: ")))
                     ;(setq lst2 (acet-geom-rect-points a b));setq
                 );and
             );or

                (setq  lst (append lst (list (list a b)))
                       lst4 (append lst4 (list (ssget "_c" a b)))
                       p3 (trans '(0.0 0.0 0.0) 1 0)
                       p4 (trans (getvar "viewdir") 1 0 T);rk added T 4:12 PM 8/12/97
                );setq
     
                (acet-lwpline-make
                    (list
                        (list (cons 210 p4))
                        ;(acet-geom-m-trans (acet-geom-rect-points a b) 1 2)
                        (acet-geom-rect-points (trans a 1 p4); p4 was 2
                            (trans b 1 p4)
                        )
                    );list
                );acet-lwpline-make-make
     
                (command (entlast))
                (setq lst3 (append lst3 (list (entlast))))
            );cond #1
            ;cond #2
            ((= a "CP")
                (progn
                    (if (setq lst2 (acet-ui-polygon-select 1))
                        (progn
                            (setq lst2 (append lst2 (list (car lst2)))
                                  lst (append lst (list lst2))
                                  lst4 (append lst4 (list (ssget "_cp" (cdr lst2))))
                                  p3 (trans '(0.0 0.0 0.0) 1 0)
                                  p4 (trans (getvar "viewdir") 1 0 T)
                                  ;p4 (list (- (car p4) (car p3)) (- (cadr p4) (cadr p3)) (- (caddr p4) (caddr p3)))
                            );setq
                            (acet-lwpline-make
                                (list
                                    (list (cons 210 p4))
                                    (acet-geom-m-trans
                                        lst2
                                        1
                                        p4 ;rk 2 4:27 PM 8/12/97
                                    )
                                );list
                            );acet-lwpline-make-make
     
                            (command (entlast))
                            (setq lst3 (append lst3 (list (entlast))))
                        );progn
                    );if
                );progn
            ); cond #2
            ; cond #3
            ((and lst (= a "Undo"))                ;;;;;Undo the last window definition
                (command "_r" (last lst3) "_a")
                (if (acet-layer-locked (getvar "clayer"))
                    (progn
                        (command "")
                        (command "_.layer" "_unl" (getvar "clayer") "")
                        (entdel (last lst3))
                        (command "_.layer" "_lock" (getvar "clayer") "")
                        (command "_.select")
                        (if (> (length lst3) 1)
                            (eval (append '(command) (cdr (reverse lst3))))
                        );if
                    );progn then the current layer is locked
                    (entdel (last lst3))
                );if
                (setq lst3 (reverse (cdr (reverse lst3)))
                      lst4 (reverse (cdr (reverse lst4)))
                      lst (reverse (cdr (reverse lst)))
                );setq
            ); cond #3
            ; cond #4
            ((or (= a "") (not a))
                (setq flag T)
            ); cond #4
            ; default
            (T
                (princ "\nInvalid")
            ); default
        );cond all
    );while
    (command "");end select
    (setvar "highlight" 0)
 
    (if lst
        (progn
            (princ "\nDone defining windows for stretch...")
            (if (acet-layer-locked (getvar "clayer"))
                (progn
                    (command "_.layer" "_unl" (getvar "clayer") "")
                    (setq lst (reverse lst))
                    (setq n 0)
                    (repeat (length lst3)
                        (entdel (nth n lst3))
                        (setq n (+ n 1))
                    );repeat
                    (command "_.layer" "_lock" (getvar "clayer") "")
                );progn then the current layer is locked
            ;else
                (progn
                    (setq lst (reverse lst))
                    (setq n 0)
                    (repeat (length lst3)
                        (entdel (nth n lst3))
                        (setq n (+ n 1))
                    );repeat
                );progn else
            );if
            (setvar "highlight" 1)
            (command "_.select")
            (repeat (length lst4)
                (if (car lst4) (command (car lst4)))
                (setq lst4 (cdr lst4))
            );repeat
            (command "")
            (setq ss (ssget "_p"))
            (if ss
                (progn
                    (command "_.select" ss)
                    (if (assoc "OSMODE" (car acet:sysvar-list))
                        (setvar "osmode" (cadr (assoc "OSMODE" (car acet:sysvar-list))))
                    );if
                    (setq p1 nil)
                    (while (not p1)
                        (initget 128 "Remove _Remove")
                        (setq p1 (getpoint "\nSpecify an option [Remove objects] <Base point>: "))
                        (if (not p1) (setq p1 (car (acet-geom-view-points))))
                        (if (and p1 (not (equal p1 "Remove")) (not (equal (type p1) 'LIST)))
                            (progn
                                (setq p1 nil)
                                (princ "\nInvalid input.")
                            );progn then
                        );if
                    );while
                    (command "")
                    (if (= p1 "Remove")
                        (progn
                            (setvar "highlight" 0)
                            (acet-ss-clear-prev)
                            ;(command "_.select" (entnext) "")
                            ;(command "_.undo" "1")
                            (setvar "highlight" 1)
                            (command "_.select" ss "_r" "_auto")
                            (setvar "cmdecho" 1)
                            (while (wcmatch (getvar "cmdnames") "*SELECT*")
                                (command pause)
                            );while
                            (if (setq ss2 (ssget "_P"));setq
                                (progn
                                    (command "_.select" ss2)
                                    (setq p1 (getpoint "\nSpecify base point: "))
                                    (command "")
                                    (if (not p1) (setq p1 (car (acet-geom-view-points))))
                                );progn
                            );if
                        );progn
                    ;else
                        (setq ss2 ss)
                    );if
                    (if ss2
                        (progn
                            ;;get the extents of the crossing window definitions
                            (setq lst2 lst)
                            (repeat (length lst2)
                                (if (> (length (car lst2)) 2)
                                    (setq lst2 (append (cdr lst2) (car lst2)))
                                ;else
                                    (setq lst2 (cdr lst2))
                                );if
                            );repeat
                            (if (and (> (length (car lst)) 2)                 ;;;cp_off_screen?
                                     (acet-geom-zoom-for-select (car lst))
                                );and
                                (progn
                                    (setvar "cmdecho" 0)
                                    (command "_.select" ss2)
                                    (setq p2 (getpoint p1 "\nSpecify second base point: "))
                                    (command "")
                                );progn
                            ;else
                                (progn
                                    ;;get removed objects(bug fix 761695)
                                    (setq ssrmv ss)
                                    (setq n 0)
                                    (repeat (sslength ss2)
                                        (setq delname (ssname ss2 n))
                                        (if (= nil (ssdel delname ssrmv))
                                            (setq n (+ n 1))
                                        );if
                                    );repeat
                                    (setvar "cmdecho" 0)
                                    ;remove empty selection from lst until first non-empty selection is seen
                                    (setq templst nil) ;new list without empty boxes
                                    (setq checkss nil)
                                    (setq found 0)
                                    (setq n 0)
                                    (repeat (length lst)
                                        (setq a (nth n lst))
                                        (if (equal found 1)
                                            ;if first non-empty selection is found, just append the rest of the list
                                            (setq templst (append templst (list a)))
                                        ;else
                                            (progn
                                                (if (equal (length a) 2)
                                                    (setq checkss (ssget "_c" (car a) (cadr a)))
                                                ;else
                                                    (setq checkss (ssget "_cp" a))
                                                );if
                                                (if (/= checkss nil) ;if there's any object selected, append it
                                                    (progn
                                                        (setq templst (append templst (list a)))
                                                        (setq found 1) ;we've found the first non-empty selection
                                                    );end progn
                                                );end if
                                            );end progn
                                        );endif
                                        (setq n (+ n 1))
                                    );repeat
                                    (setq lst templst)
                                    (command "_.stretch")
                                    (cp_loop (car lst))
                                    (command "_r" ssrmv "_a" ss2 "" p1)
                                    (setvar "cmdecho" 1)
                                    (princ "\nSecond base point: ")
                                    (command pause)
                                    (setvar "cmdecho" 0)
                                    (setq p2 (getvar "lastpoint"))
                                    (setq lst (cdr lst))
                                );progn
                            );if
                            (if (setq zflag (acet-geom-zoom-for-select lst2))
                                (command "_.zoom" "_w" (car zflag) (cadr zflag))
                            );if
                            ;;get removed objects(bug fix 761695)
                            (setq ssrmv ss)
                            (setq n 0)
                            (repeat (sslength ss2)
                                (setq delname (ssname ss2 n))
                                (if (= nil (ssdel delname ssrmv) )
                                    (setq n (+ n 1))
                                );if
                            );repeat
                            (setvar "highlight" 0)
                            (setvar "dragmode"  0)
                            (setvar "osmode"    0)
                            (setq n 0);setq
                            (repeat (length lst)
                                (setq a (nth n lst))
                                (command "_.stretch")
                                (cp_loop a)
                                (command "_r" ssrmv "" p1 p2)
                                (setq n (+ n 1))
                            );repeat
                            (if zflag (command "_.zoom" "_p"))
                        );progn then ss2
                        (princ "\nNothing selected")
                    );if
                );progn then ss
                (princ "\nNothing selected")
            );if
        );progn then lst
    );if
    (acet-error-restore)
    (princ)
);defun c:mstretch
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cp_loop ( lst / n)
    (if (equal (length lst) 2)
        (command "_c" (car lst) (cadr lst))
    ;else
        (progn
            (command "_cp")
            (setq n 0)
            (repeat (length lst)
                (command (nth n lst))
                (setq n (+ n 1))
            );repeat
            (command "")
        );progn
    );if
);defun cp_loop

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDVJ05sQ5A5zTHnc8oWtc+byScio1WsWJqIj0ul
;;; PLokb2lltZ6b0mBjhhyNUc8Yq3cOZH+01E4DW0aE0Idu4+29I4tbX2AQB/gaBB29
;;; y0Ct7F2tRJfC5DCF1c3nn6YhdO5+c1Pdcxs5X1Y73H5zePo43aIlikafkilXEX/K
;;; 9osXnoivyep/wW8cyYSySkB3Y8gdkkx0TfnvpOy9X1PkP/dc7rPDnS9H6GaK7Ypk
;;; +M6OqThmuERAOCI27fxh9mLryF7kfCwGK6DOdi6nlss2Y/hi9VLBHYCTFIwaufo6
;;; UE8KoM9a0UsO1Q0Xy+JZS48EQO19JRvW80UzQiPSqifSichrtalKqFcsm7nlWmfa
;;; qkRCnZktdPCA6ZlMwEopVs5NQpbGjFAfCbjIjAWc2eUWKBMDBfLd+qxYHScVecTe
;;; iNguXFs8AfqkjPJlW60I3D7ZbnqodYT1cFeIIDfUqlCqnAYXg7UryHFAVPAB14GX
;;; xVatRlCTlzq5e3WgKacxw79MohSTeUG6O51zdRynXnIVcVzBpqy8lw5Xm5V+WXC9
;;; 4bOVLNETq5QYt+thIaGmjrckBvTDIn+jb41Hvu6enJtyYvAKppXczWhLlll0p6Uw
;;; 82cOoun6kVDP8W1V2xFRozL2MThtcxkZ1+yioHeGWWPqy0V8VfdizDyPQM0LcgY5
;;; KN0g1A==
;;; -----END-SIGNATURE-----