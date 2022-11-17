;;
;;;
;;;    BLOCKTOXREF.LSP - Randy Kintzley
;;;
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
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; block replace - replaces one block insert with another.
(defun c:blocktoxref ()
 (acet-error-init 
  (list (list "cmdecho" 0 "highlight" 0 )
        1
  )
 )
 (acet-blocktoxref-ui)
 (acet-error-restore)
);defun c:blocktoxref
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; command line version
(defun c:-blocktoxref ()
 (acet-error-init 
  (list (list "cmdecho" 0 "highlight" 0 "cmddia" 0 "filedia" 0 )
        1
  )
 )
 (acet-blocktoxref-ui)
 (acet-error-restore)
);defun c:-blocktoxref
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BlockToXref
;
(defun acet-blocktoxref-ui ( / bna fna n prg )
 (if (not acet:blocktoxref-block)
     (setq acet:blocktoxref-block "")
 );if
 (if (= "" (getvar "refeditname"))
     (setq bna (acet-ui-table-name-get 
                (list "Select a block to be replaced with an xref" 
                  acet:blocktoxref-block
                  "block"
                  2		 ;; dis-allow xrefs
                  nil		 ;; ssget style filter
                  "acet1509.hlp" ;; help file
                  "blocktoxref"  ;; topic
                );list
               )
     );setq then not in refedit mode
     (acet-alert "Command not allowed while REFEDIT is active.")
 );if
 
 ;;default value for next time.
 (if bna
     (setq acet:blocktoxref-block bna)
 )
 
 ;; set up a default xref name
 (if (and bna 
          (not acet:blocktoxref-xref)
     );and
     (setq acet:blocktoxref-xref (strcat bna ".dwg"))
 );if
 (if (and bna
          (setq fna (ACET-FILE-WRITEDIALOG 
                      "Select an xref file" 
                      acet:blocktoxref-xref
                      "dwg"
                      "Acet:BlocktoXref"
                      1664
                    )
          );setq
     );and
     (progn
      (setq acet:blocktoxref-xref fna)
      (if (progn 
           (initget "Yes No")
           (/= "No" (getkword "\nPurge unreferenced items when finished? <Y>: "))
          )
          (setq prg T)
      );if
      (setq n (acet-block-to-xref bna fna prg))
      (princ (acet-str-format "\n%1 block inserts replaced with xref: %2" n fna))
      (if (> n 0)
          (princ "\nResults may not be apparent until next regen.")
      );if
     );progn then
 );if
);defun acet-blocktoxref-ui
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; block replace - replaces one block insert with another.
(defun c:blockreplace ()
 (acet-error-init
  (list (list "cmdecho" 0 "highlight" 0 )
        1
  )
 )
 (acet-blockreplace-ui)
 (acet-error-restore)
);defun c:blockreplace
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; command line version
(defun c:-blockreplace ()
 (acet-error-init 
  (list (list "cmdecho" 0 "highlight" 0 "cmddia" 0 )
        1
  )
 )
 (acet-blockreplace-ui)
 (acet-error-restore)
);defun c:-blockreplace
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-blockreplace-ui ( / bna bna2 n lk prg lst flag bnames  )
 (if (not acet:blockreplace-blocksearch)
     (setq acet:blockreplace-blocksearch "")
 );if
 (if (= "" (getvar "refeditname"))
     (setq bna (acet-ui-table-name-get
                (list "Select the block to be replaced" 
                  acet:blockreplace-block
                  "block"
                  2		 ;; dis-allow xrefs
                  nil		 ;; ssget style filter
                  "acet1509.hlp" ;; help file
                  "blockreplace"  ;; topic
                );list
               )
     );setq
     (acet-alert "Command not allowed while REFEDIT is active.")
 );if
 (if bna
     (setq acet:blockreplace-blocksearch bna)
 );if
 (if (not acet:blockreplace-blockreplace)
     (setq acet:blockreplace-blockreplace "")
 )
 (while (and (not flag)
             bna
             (setq bna2 (acet-ui-table-name-get
                         (list (acet-str-format "Select a block to replace %1" bna)
                             acet:blockreplace-blockreplace
                             "block"
                             2			;; dis-allow xrefs
                             nil		;; ssget style filter
                             "acet1509.hlp"	;; help file
                             "blockreplace" 	;; topic
                         );list
                        )
            );setq
        );and
   (cond
    ((acet-str-equal bna bna2)
     (acet-alert "Invalid. Block names cannot be equal.")
    )
    ;;Cannot replace bna with a block that references bna
    ((and (setq lst (acet-block-referenced-tables bna2 nil))
          (setq bnames (cdr (assoc 2 lst)))
          (member (xstrcase bna) (mapcar 'xstrcase bnames))
     );and
     (acet-alert (acet-str-format "Invalid. Circular reference. %1 references %2." bna2 bna))
    )
    (T
     (setq flag T)
    )
   );cond close
 );while
 
 (if (and bna bna2);and
     (progn
      (setq acet:blockreplace-blockreplace bna2)
      (if (progn 
           (initget "Yes No")
           (/= "No" (getkword "\nPurge unreferenced items when finished? <Y>: "))
          )
          (setq prg T)
      );if
      (setq lk (acet-layer-unlock-all))
      (if prg
          (setq lst (acet-block-referenced-tables bna nil))
      );if
      (setq n (acet-block-replace bna bna2))
      (if lk
          (command "_.-layer" "_lock" lk "")
      );if
      (if prg
          (acet-block-referenced-tables-purge lst)
      );if
      (princ (acet-str-format "\n%1 blocks replaced." n))
      (if (> n 0)
          (princ "\nResults may not be apparent until next regen.")
      );if
     );progn then
 );if
);defun acet-blockreplace-ui


(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-referenced-tables bna tblst)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-referenced-tables-ent e1 tblst)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-referenced-tables-purge tblst)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-replace bna bna2)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-to-xref bna fna prg)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-blocktoxref-apply-xref-layer-props lalst origlays laymap)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-blocktoxref-copy-layer-props la la2)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-blocktoxref-local-layer-name xla loclst)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-blocktoxref-local-to-xref-layer-map xrefname)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAth+aKbcE5gq1ZYtL2YuDXSzG3GbLhDcTXG/Bv
;;; DBWnCqKUq2ERxM6DNVYNK8fVNdDtQWXe20FDTrWDDaQolTmsW7Vh44QSx2gitLbB
;;; Oh4Sz+AzX7+qBYR+qKjbjuE6d8N/GlGSGTnZzJK9kyxYzxFkrOPwQYtjv9NCQJjj
;;; o0r/zvxn4iEzk8Cz+M+6LWv2CD6xBHDe5glqh08plUz7rjwp/iU1ykud4WZob/gR
;;; wP4j3AY1cvTFARjKNZ3/usls5ZwBsLlEN95fwKJMsRYe3ToD8/taai2+gbrTTo1n
;;; T9eJzljtDRT5Ue0ujQaAjrpW3xeLJZB9sl/X3eW6g19JWKe40YFzWdgihEFxpBL6
;;; 8POmEULTkS7s0Lj06+Ql8JFO/4q0CFiCLBYdi02CILBGahrW3QhjZN/JN9N6H32l
;;; QgZN4IX6b2YXePCgfexHYRPk//j7exrq/WVBOJszN4v6l7RyVZdKTKuLnZLAZN2P
;;; aFvp0fblpYXpAeg+qor51G57dRURzb+UZPHzaZ7/vhilUijnk2lMYifCXNkO/+z6
;;; 8+gEvtDpk83QTL6jJzgwU4UZ6LrsbhFB8IHJJvj8ZMQJFbRRCgDgONWNOZav1xCm
;;; 51VIhJ/n5dNot884iFWzJtzY+XvYYHF4E5oVRJwAnCo7sCsAb/s4PLIdetO9prVa
;;; NVgNcg==
;;; -----END-SIGNATURE-----