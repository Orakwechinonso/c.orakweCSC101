;;
;;  exoffset.lsp - Express Tools offset replacement command
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
 
;|
 
EXOFFSET
Express offset
 
This command is direct a replacement for offset.
The prompt sequence is the same as the standard offste command 
except that this command has several added options and enhancements.
Enhanced features include:
 
- Option to specify what layer new objects are placed on (Current or Source).
 
- Ability to select mutiple objects to be offset and dynamically step 
  through each one to specify an offset side.
 
- Undo option within the command.
 
- Multiple option allows you to offset a single selected object 
  multiple times without re-selecting it each time.
 
- Ability to set a new offset distance at any point within the command.
 
- Improved selection. Finally offset allows standard options like 
  implied crossing and window as well as fence and others.
 
|;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exOffset ( / flt na ss exOffset-err undolst )
 (acet-error-init
  (list (list "cmdecho" 0)
        0
        '(exOffset-err)
  )
 )
 
 ;; extra error stuff
 (defun-q exOffset-err ()
  (if (and na 
           (equal 'ENAME (type na))
      );and
      (setq na (ssadd na))
  );if
  (if (and na
           (equal 'PICKSET (type na))
      );and
      (acet-ss-redraw na 4)
  );if
  (if lk
      (progn
       (acet-sysvar-set (list "cmdecho" 0))
       (command "_.-layer" "_lock" lk "")
       (acet-sysvar-restore)
      );progn then
  );if
  (princ)
 );defun-q
 
 (setq flt '((-4 . "<OR") (0 . "LINE") (0 . "CIRCLE") (0 . "ARC") (0 . "POLYLINE")
                          (0 . "LWPOLYLINE") (0 . "ELLIPSE") (0 . "RAY")
                          (0 . "XLINE") (0 . "SPLINE")
             (-4 . "OR>")
            )
 );setq
 
 (if (and (= (getvar "pickfirst"))
          (setq na (cadr (ssgetfirst)))
     );and
     (progn
      ;(setq na (acet-ss-ssget-filter na flt))
 
      (setq ss (car (acet-ss-filter 
                        (list na '(( "LAYERUNLOCKED")) T)
                    )
               );car
      );setq
 
      (sssetfirst nil nil)
     );progn
     (sssetfirst nil nil)
 );if
 
 (acet-exOffset-print-status)
 (acet-exOffset-ui-get-dist)
 (while (or na
            (setq na (acet-ui-entsel
                      (list "\nSelect object(s) to offset or [Options/Undo]: "	;; prompt
                            128							;; bits
                            "Options Undo"					;; keywords
                            T							;; implied windowing ok
                            flt							;; filter
                            nil							;; lockedOK
                      );list
                     )
            );setq
        );or
   (cond
    ((equal (type na) 'PICKSET)
     (princ (acet-str-format "\n%1 object(s) found." (itoa (sslength na))))
     (setq undolst (acet-exOffset-ui-multi-select na undolst))
    );cond #2
    ((equal na "Options")
     (acet-exOffset-ui-get-settings)
    );cond #3
    ((equal na "Undo")
     (setq undolst (acet-exOffset-undo undolst))
    );cond #4
   );cond close
 
   (setq na nil)
 );while
 
 ;; get rid of any duplicates that may have been created
 (acet-exoffset-remove-dups undolst)
 
 (acet-error-restore)
);defun c:exOffset
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Takes an ename or a selection set and erases the obnjkects using entdel so that undo does not 
; get affected.
;
(defun acet-exoffset-erase ( ss / n na )
 (if (equal 'ENAME (type ss))
     (setq ss (ssadd ss (ssadd)))
 );if
 (setq n 0)
 (repeat (sslength ss)
  (setq na (ssname ss n))
  (if (entget na)
      (entdel na)
  );if
  (setq n (+ n 1));setq
 );repeat
);defun acet-exoffset-erase
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-exoffset-clean ( lst / n na lst2 lst3 lst4 )
  (setq n 0)
  (repeat (length lst)
   (setq lst2 (nth n lst)
         lst3 nil
   );setq
 
   (foreach na lst2
     (if (entget na)
         (setq lst3 (cons na lst3))
     );if
   );foreach
   (setq lst4 (cons (reverse lst3) lst4))
 
   (setq n (+ n 1));setq
  );repeat
  (setq lst (reverse lst4))
 
);defun acet-exoffset-clean
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Takes a list of lists. Each sub-list is a list of enames.
;
(defun acet-exoffset-remove-dups ( lst / ss lst2 lst3 lst4 n na ss2 )
 
 (if (and lst
          (setq lst2 (acet-exoffset-clean lst))
          (setq lst2 (apply 'append lst2))
          (setq ss (acet-list-to-ss lst2));setq
          (setq ss (acet-ss-remove-dups ss 0.00000001 nil))
          (setq ss2 (cadr ss))
     );and
     (progn
      (setq ss (car ss))
      (if ss2
          (progn
           (if (ssmemb (car (car lst)) ss2)
               (setq ss2 (ssdel (car (car lst)) ss2)
                      ss (ssadd (car (car lst)) ss)
               );setq then
           );if
           (if (> (sslength ss2) 0)
               (progn
                (acet-exoffset-erase ss2)
                (princ (acet-str-format "\nRemoved %1 duplicate object(s)." (sslength ss2)))
               );progn then
           );if
          );progn then
      );if
;      (setq n 0)
;      (repeat (length lst)
;       (setq lst2 (nth n lst)
;             lst3 nil
;       );setq
; 
;       (foreach na lst2
;         (if (ssmemb na ss)
;             (setq lst3 (cons na lst3))
;         );if
;       );foreach
;       (setq lst4 (cons (reverse lst3) lst4))
;
;       (setq n (+ n 1));setq
;      );repeat
;      (setq lst (reverse lst4))
     );progn then
 );if
 lst
);defun acet-exoffset-remove-dups
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-exOffset-ui-multi-select ( ss undolst / na n j )
 (setq j (sslength ss))
; (if (> j 1)
;     (acet-ui-progress-init "Offset multiple objects..." (sslength ss))
; );if
 
 (setq n 0)
 (repeat (sslength ss)
  (if (> j 1) 
      (progn
       (acet-ui-progress-init "Offset multiple objects..." (sslength ss))
       (acet-ui-progress-safe n)
      );progn then
  );if
  (setq na (ssname ss n))
  (setq undolst (acet-exOffset-ui-ent na undolst))
  (setq n (+ n 1));setq
 );repeat
 
 (if (> j 1) (acet-ui-progress-done));if
 
 undolst
);defun acet-exOffset-ui-multi-select
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-exOffset-ui-ent ( na undolst / e1 d p1 mult flag msg shift control )
 
 (acet-sys-shift-down)   ;call once to initialize
 (acet-sys-control-down)
 
 (setq d (acet-exOffset-get-dist))
 (if (= d 0.0)
     (setq d "_through")
 );if
 (while (and 
             (not flag)

             (setq e1 (entget na))
 
             (progn
              (if (acet-layer-locked (cdr (assoc 8 e1)))
                  (progn
                   (princ "\nObject is on a locked layer.")
                   (setq e1 nil)
                  )
              );if
              e1
             );progn
 
;;;;             (not flag)

             (progn
              (redraw na 3)
              (if (and (= 'STR (type d))
                       (acet-str-equal d "_through")
                  );and
                  (setq msg "\nSpecify through point or ")
                  (setq msg "\nSide to offset or ")
              );if
              (if (not mult)
                  (progn
                   (princ "\nShift+Pick for multiple; Ctrl+Pick to erase source object.")
                   (initget "Multiple Options Undo")
                   (setq msg (strcat msg "[Multiple/Options/Undo]: "))
                  );progn then
                  (progn
                   (princ "\nPress <enter> to exit multiple mode; Ctrl+Pick to erase source object.")
                   (initget "Options Undo")
                   (setq msg (strcat msg "[Options/Undo]: "))
                  );progn else
              );if
              (setq p1 (getpoint msg))
              (if (equal (type p1) 'LIST)
                  (setq   shift (acet-sys-shift-down)
                        control (acet-sys-control-down)
                  );setq
              );if
              (redraw na 4)
              p1
             );progn
        );and
 
       (cond
        ((equal (type p1) 'LIST)
         (setq undolst (acet-exOffset-ent na d p1 undolst control))
         (if undolst
             (setq na (car (car undolst)));setq then
         );if
         (if (not shift)
             (setq flag T)
         );if
        );cond #1
 
        ((equal p1 "Multiple")
         (setq mult T)
        );cond #2
 
        ((equal p1 "Options")
         (acet-exOffset-ui-get-settings)
         (setq d (acet-exOffset-get-dist))
         (if (= d 0.0)
             (setq d "_through")
         );if
        );cond #3
 
        ((equal p1 "Undo")
         (setq undolst (acet-exOffset-undo undolst))
         (if undolst
             (setq na (car (car undolst)));setq then
         );if
        );cond #4
 
       );cond close
 
       (if mult
           (setq flag nil)
       );if
 
  );while
 
  undolst
);defun acet-exOffset-ui-ent
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-exOffset-undo ( undolst / ss lk )
  (if (not undolst)
      (princ "\nCommand has been completely undone.")
      (progn
       (setq lk (acet-layer-unlock-all))
       (foreach x (car undolst)
         (entdel x)
       )
       (setq undolst (cdr undolst));setq
       (if lk
           (command "_.layer" "_lock" lk "")
       );if
      );progn then
  );if
 undolst
);defun acet-exOffset-undo
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-exOffset-ent ( na d p1 undolst control / layermode lna ss )
 (setq       lna (entlast)
       layermode (acet-exOffset-get-layermode)
 );setq
 (command "_.offset" d na p1 "")
 (if (not (equal lna (entlast)))
     (progn
      (setq ss (acet-ss-new lna)
            ss (acet-ss-to-list ss)
      );setq
      (if control
          (progn
           (setq ss (append ss (list na)))
           (entdel na)
          );progn then
      );if
      (setq undolst (cons ss undolst));setq
      (if (and (equal layermode "Current")
               (setq ss (acet-list-to-ss ss))
          );and
          (command "_.chprop" ss "" "_la" (getvar "clayer") "")
      );if
      (setq undolst (acet-exoffset-remove-dups undolst))
     );progn then offset succeeded
 );if
 undolst
);defun acet-exOffset-ent
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Offer a main prompt from which the default is the distance entry but other options are included such
; as layer gaptype ....etc.
;
(defun acet-exOffset-ui-get-settings ( / def str ans d )
 (acet-exOffset-print-status)
 (setq def (acet-exOffset-get-dist))
 (if (equal def 0.0)
     (setq str "Through");setq
     (setq str (rtos def))
 );if
 (while (progn
         (initget 6 "Distance Layer Gaptype")
         (setq ans (getkword "\nSpecify an option to set [Distance/Layer/Gaptype]: "));setq
        );progn 
  (cond
   ((= ans "Distance")
    (acet-exOffset-ui-get-dist)
   );cond #1
   ((equal ans "Layer")
    (acet-exOffset-ui-get-layermode)
   );cond #2
   ((equal ans "Gaptype")
    (acet-exOffset-ui-get-gaptype)
   );cond #3
  );cond close
 );while
);defun acet-exOffset-ui-get-settings
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-exOffset-ui-get-dist ( / def str ans d )
 (setq def (acet-exOffset-get-dist))
 (if (equal def 0.0)
     (setq str "Through");setq
     (setq str (rtos def))
 );if
 (while (not d)
  (initget 6 "Through")
  (setq ans (getdist (acet-str-format "\nSpecify offset distance or [Through] <%1>: " str)));setq
  (cond
   ((not ans)
    (setq d def)
   );cond #1
   ((= ans "Through")
    (setq d 0.0)
    (acet-setvar (list "ACET-EXOFFSET-DIST" d 3))
   );cond #2
   ((equal 'REAL (type ans))
    (setq d ans)
    (acet-setvar (list "ACET-EXOFFSET-DIST" d 3))
   );cond #3
  );cond close
 );while
 d
);defun acet-exOffset-ui-get-dist
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-exOffset-get-dist ( / def )
 (setq def (acet-getvar (list "ACET-EXOFFSET-DIST" 3)))
 (if (not def)
     (setq def 0.0)
 );if
 def 
);defun acet-exOffset-get-dist
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-exOffset-ui-get-layermode ( / def ans )
 (setq def (acet-exOffset-get-layermode))
 (initget "Source Current")
 (setq ans (getkword (acet-str-format "\nSpecify layer for new objects [Source/Current] <%1>: " def)))
 (if ans
     (acet-setvar (list "ACET-EXOFFSET-LAYERMODE" ans 3)) ;store it in dwg and current profile
     (setq ans def)
 );if
 ans
);defun acet-exOffset-ui-get-layermode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-exOffset-get-layermode ( / def )
 (setq def (acet-getvar '("ACET-EXOFFSET-LAYERMODE")))
 (if (not def)
     (setq def "Source")
 );if
 def
);defun acet-exOffset-get-layermode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-exOffset-ui-get-gaptype ( / def ans )
 (setq def (getvar "offsetgaptype"))
 (cond
  ((= def 0) (setq ans "Normal"))
  ((= def 1) (setq ans "Fillet"))
  ((= def 2) (setq ans "Chamfer"))
 );cond close
 (initget "\nSelect object(s) to offset or [Options/Undo]: ")
 (setq ans (getkword (acet-str-format "\nSelect object(s) to offset or [Options/Undo]: " ans)))
 (if ans
     (progn
      (cond
       ((= ans "Normal")  (setq ans 0))
       ((= ans "Fillet")  (setq ans 1))
       ((= ans "Chamfer") (setq ans 2))
      );cond close
      (setvar "offsetgaptype" ans)
     );progn then
     (setq ans def)
 );if
 ans
);defun acet-exOffset-ui-get-gaptype
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-exOffset-print-status ( / dist lay gap )
 (setq dist (acet-exOffset-get-dist)
        lay (xstrcase (acet-exOffset-get-layermode))
        gap (getvar "offsetgaptype")
 );setq
 (if (= dist 0)
     (setq dist "Through")
     (setq dist (rtos dist))
 );if
 (cond
  ((= gap 0) (setq gap "Normal"))
  ((= gap 1) (setq gap "Fillet"))
  ((= gap 2) (setq gap "Chamfer"))
 );cond close
     
 (princ (acet-str-format "\nSettings: Distance = %1,  Layer = %2, Gaptype = %3"
                         dist lay gap
        )
 );princ
);defun acet-exOffset-print-status


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDBr2Gx6859KRkzvQycsE6CsKIjbNDHKYfZhPl+
;;; tpXc8D1MgP4Cjvqq7Wxz0xVSFEmqSTHAXgXfLVd6uLxT71q483uZZuePsZ7QPubj
;;; SwjT+crEpI1M6RtnCRITry7DAgG3zIYAGgOBaK4RJPZxHX6ync62mqZCZPDBF7+O
;;; jkNKRczoyc8TjEvgHCMbNh+nxycl88w2RsrnC9GNa2XDG9SkHOXC5jwGk6Faddsc
;;; HWC4PIAiKNauOeSag54PGHf63hyDtyA1jbIG0TAaaozEHfiPp/UuJYM1uZNuWe9w
;;; oxj+emVayfFg0zX7k/ug2UziN3zRkg1LptBsodJCp0dtq7PHF+rLdmn6lPQdjDYl
;;; h/d24zJhBdan67dElCBDtRLRsDmbLK+9NZJP2ySyfp9e3b9pJ8Kp5gc2IZIdrxd7
;;; wr97R084Yma51aJcsk2mP57UR/js17Z6u8KtvjBDyvODvtzNO5V/L3NY/X7BRbl/
;;; W8RnkpEmS6R+1LiTuX6gfCjjp7IfRJnSmqkPT7LGJQzRHMtVCjAEqwEyaaSZxraL
;;; S4qIn+7RIA9qzX+t58Ha8zu69+qJyc8yVhOMFl+wq6M5AT55X9xcrxJV2PoK49UE
;;; xTmpWZxUrWs2kYByrycCQXTnUiZFzRGTk4Ltd7Mej6MIhgd7KW5ikfPFpm5Jd7a2
;;; v7c5sg==
;;; -----END-SIGNATURE-----