;;
;;  saveall.lsp - SAVEALL command
;;
;;  Copyright © 2000 by Autodesk, Inc.
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
;;    Implements the SAVEALL command.
;;
;;-------------------------------------------------------------------------
 
 
;;
;;  save all open drawings
;;
(defun C:SAVEALL (/ dwg saveDwg)
  ;;  save drawing
  (defun saveDwg (dwg / titled writeable name)
    (vl-load-com)
    (setq titled (= 1 (vlax-variant-value (vla-getvariable dwg "DWGTITLED")))
          writeable (= 1 (vlax-variant-value (vla-getvariable dwg "WRITESTAT")))
          name (if titled
                 (vlax-get dwg "fullname")
                 (vlax-variant-value (vla-getvariable dwg "DWGNAME")) )
    )
    (cond
      ;;  REFEDIT active ??
      ((/= "" (vlax-variant-value (vla-getvariable dwg "REFEDITNAME")))
        (acet-ui-message
          (acet-str-format "%1\nCannot Save while REFEDIT active." name)
          "AutoCAD - SAVEALL"
          Acet:ICONSTOP )
      )
      ;;  simple save if titled and writeable
      ((and titled writeable)
        (vla-save dwg)
      )
      ;;  otherwise ask for name first
      (T
        (if (setq name (ACET-FILE-WRITEDIALOG "%1\nCannot Save while REFEDIT active." name "dwg" "Acet:SaveAll" 1665))
          (vla-saveas dwg (vlax-make-variant name)) )
      )
    )
  )

  ;;  quietly
  (vl-load-com)
  (acet-error-init '(("CMDECHO" 0)))

  ;;  for each drawing
  (vlax-for dwg (vla-get-documents (vlax-get-acad-object))
    ;;  save if modified
    (if (/= 0 (vlax-variant-value (vla-getvariable dwg "DBMOD")))
      (saveDwg dwg) )
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAMSDhnoFU6lMqNlhJNBqfP28EM9BVi0C7dluO3
;;; CdicL3GifZTlBl65ZQPfw8AkPDIoKMnkAiQ1uJlajc+7U/lyss1Roud2KMFfc0a/
;;; sry4cSDbJzXbea4JAgzJwlq1xZcPb+QHEr4kkOy4AJMSc87QoE+eZqp//bWxb/d+
;;; NnpWzOYNEq7JkWCIQn38MFQsegehu2McYS6GLNv4XTQZJiuUrXQxVAR51KAvTxtX
;;; yM1Mh3fV3Q6Xh13S5Rz26fWWEkZP5w5xjpp4Aw7sGHixxnAhmWDM4fi68GJTSnVe
;;; LVm1B3LnoKscICb3lywsr0bGN19j2UGSeE3e6BVt1ojBsNeEgLPM+lU1FqgrJ3Kb
;;; eoq77oAUTDAk/J/Df9G+83DqD+DC3pw4LiSBQKFuVjnekxlNvR/OQO8HwHKq/sIp
;;; /rX6GfpZAjNC2InN/fqFo6hVMS3jwMGJuoBUT3jXgpMny7rdLuJx5OkJXCersh4r
;;; cCTkA95m31+s5/R3K3JgDOTAMb3xKENoLXPziqb2m/Dcy8G+JjclNLo8L51GI3KK
;;; OHd3SzO23KUUWvLncS0BPvvig774U1U3lECyo16qe2JcPY3ql0XIeh2D4zC8Xtb7
;;; a0IkzOUTLV4P1BNe4KgJGxQ7awV0H3gBGM/bRH/+7s1F2UvZfE1+PN0cva+6vGEk
;;; sZGZHg==
;;; -----END-SIGNATURE-----