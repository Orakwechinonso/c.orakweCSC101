;;
;;  revert.lsp - REVERT command
;;
;;  Copyright � 1999 by Autodesk, Inc.
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
;;  Use, duplication, or disclosure by the U.S. Government is subject to
;;  restrictions set forth in FAR 52.227-19 (Commercial Computer
;;  Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;  (Rights in Technical Data and Computer Software), as applicable.
;;  
;;-------------------------------------------------------------------------
;;
;;  DESCRIPTION
;;    Implements the REVERT command.
;;
;;-------------------------------------------------------------------------
 
 
;;
;;  revert to previous saved version of this drawing
;;
(defun C:REVERT (/ dwg reply sdiOpen)
  ;;  special open for SDI mode
  (defun sdiOpen (name force)
    (vl-load-com)
    (vla-sendcommand
        (vla-get-activedocument
            (vlax-get-acad-object))
                (acet-str-format "(command \"_.OPEN\"%1)\n%2\n"
                                 (if force " \"_Y\"" "")
                                 name ) )
  )
 
  ;;  quiet
  (acet-error-init '(("CMDECHO" 0)))
  ;;  check for the necessary support code
  (if (acet-reg-get (strcat "HKEY_LOCAL_MACHINE\\"
                            (acet-reg-machine-prodkey)
                            "\\Applications\\acadvba\\commands" )
                    "VBASTMT" )
    ;;  worth trying ??
    (if (= 1 (getvar "DWGTITLED"))
      (progn
        ;;  pick file name
        (setq dwg (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
        ;;  drawing modified ??
        (if (and (/= 0 (getvar "DBMOD"))
                 (= Acet:IDCANCEL
                    (acet-ui-message
                        (acet-str-format "Abandon changes to %1?" dwg)
                                         "AutoCAD - REVERT"
                                         (+ Acet:OKCANCEL
                                            Acet:ICONWARNING
                                            Acet:DEFBUTTON2 ) ) ) )
          (exit)
        )
        ;; SDI ??
        (if (/= 0 (getvar "SDI"))
          (sdiOpen dwg (/= 0 (getvar "DBMOD")))
          (command "_vbastmt" (acet-str-format
              "ThisDrawing.Close(FALSE):AcadApplication.Documents.Open \"%1\""
              dwg ) )
        )
      )
      (acet-ui-message "Drawing has never been saved."
                       "AutoCAD - REVERT"
                       Acet:ICONSTOP )
    )
    (acet-ui-message "VBA support not installed."
                     "AutoCAD - REVERT"
                     Acet:ICONSTOP )
  )
 
  (acet-error-restore)
  (princ)
)


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDMd3PgBX8klJQekC2cEdlvbzPRIPHOE69easUG
;;; fjqIeFGaSnfyn7lc/ZEhd8CCMkzJnsUUm4xsjIw1us587vC9Hbs5YtsnKo0NteXE
;;; zrQ95ePUakCQPQQZahX8Jy5hr0bDbFhsaCzlXg6K9svx/rXmX911VI1YOIcOBukb
;;; tMe/fAcgz3Fp5iTie2F/uOorMQX0zkMBqErHDFMSIN8jNtHzeQecL7m7SJvum4YV
;;; Cz+GvR4nz7ScFUqRnzgxgUJtHBqbOvrxTAzbs8fF2arx+ejXFoRp8svBjvCq/PMW
;;; 1i/KQKYemTAzF8fH8ChOB63+hbpyFjfTGVt6wbwVUjuwXVXrig6Sara6cZxjE+kC
;;; E8e/QtqSzqKLSTlSDOaut91Ll3BgjQ/WKUt+d/xOKOJxx2LF5Sn/Ju5WMasTOmqQ
;;; sEx+/omgQf1oBGcwh/0N6HbvD7jgTz0SHrnOJlKlLEj5dFrolHgAvao6E2p8XP7o
;;; zJ0G3bw4EE+QTyABQE6rvsjHxFCY5Piv7PGomrUsX2qU2qgNSXz9qmgv8P5Sosa4
;;; M2sjNjrDEFBKrm2OUSIQ+HIwH7ZhsPel3BQh1OCFZmEBb7qu1pDZgUel/kPnx27j
;;; gO3hYe5MDXtLJIIfw4cZof/t33xc/lM8H40TRyLY5dj2S++DEa/4lgemV/5unCPI
;;; qedt8A==
;;; -----END-SIGNATURE-----