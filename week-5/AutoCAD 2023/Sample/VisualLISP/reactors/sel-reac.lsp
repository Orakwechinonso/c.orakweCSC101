;;;                                                                    ;
;;;  SEL-REAC.LSP                                                      ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998, 1999    ;
;;;  by Autodesk, Inc. All Rights Reserved.                            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;                                                                    ;
;;;  Incorporation of any part of this software into other software,   ;
;;;  except when such incorporation is exclusively for your own use    ;
;;;  or for use by others in your organization in the performance of   ;
;;;  their normal duties, is prohibited without the prior written      ;
;;;  consent of Autodesk, Inc.                                         ;
;;;                                                                    ;
;;;  Copying, modification and distribution of this software or any    ;
;;;  part thereof in any form except as expressly provided herein is   ;
;;;  prohibited without the prior written consent of Autodesk, Inc.    ;
;;;                                                                    ;
;;;  AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      ;
;;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           ;
;;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       ;
;;;  INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          ;
;;;  WILL BE UNINTERRUPTED OR ERROR FREE.                              ;
;;;                                                                    ;
;;;  Restricted Rights for US Government Users.  This software         ;
;;;  and Documentation are provided with RESTRICTED RIGHTS for US      ;
;;;  US Government users.  Use, duplication, or disclosure by the      ;
;;;  Government is subject to restrictions as set forth in FAR         ;
;;;  12.212 (Commercial Computer Software-Restricted Rights) and       ;
;;;  DFAR 227.7202 (Rights in Technical Data and Computer Software),   ;
;;;  as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       ;
;;;  Parkway, San Rafael, California 94903.                            ;
;;;                                                                    ;

;;;--------------------------------------------------------------------;
;;; General Note:  THIS FILE IS A MEMBER OF THE REAC-TST PROJECT       ;
;;;--------------------------------------------------------------------;
;;; This file contains various Example how to select all reactors      ;
;;; binded to the vla object                                           ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;       Function:  SELECT-OBJECT-REACTORS                            ;
;;;                                                                    ;
;;;    Description:  This function passes a vla object to              ;
;;;                  select-object-reactors-id. And serves as a        ;
;;;                  constructor.                                      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    select-object-reactors-id                       ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      vla_object = a valid vla object.                              ;
;;;                                                                    ;
;;; Returned Value:  A list of vla objects.                            ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(select-object-reactors                                ;
;;;                       vla_object                                   ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
(defun select-object-reactors (vla_object)
  (select-object-reactors-id
    (vla-get-ObjectID vla_object)
    (VLR-reactors :VLR-Object-Reactor)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  SELECT-OBJECT-REACTORS                            ;
;;;                                                                    ;
;;;    Description:  This function passes a vla object to              ;
;;;                  select-object-reactors-id. And serves as a        ;
;;;                  constructor.                                      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    select-object-reactors-id                       ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         objId = a valid vla object id.                             ;
;;;     react-lst = a list of vlr reactors.                            ;
;;;                                                                    ;
;;; Returned Value:  A list of vla objects.                            ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(select-object-reactors-id                             ;
;;;                       objId                                        ;
;;;                       react-lst                                    ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
(defun select-object-reactors-id (objId react-lst)
  ;; it is important to use object Id for checking object eqe.
  ;; because the same object may have many different vla-object
  ;; pointed to it but all of them have the same objectId.
  (cond
    ((null react-lst) nil)
    ((member objId
	     (mapcar
	       (function vla-get-ObjectID)
	       (VLR-Owners (car react-lst))
	     )
     )
     (cons (car react-lst)
	   (select-object-reactors-id objId (cdr react-lst))
     )
    )
    (t (select-object-reactors-id objId (cdr react-lst)))
  )
)
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCiOFk+1etKMs4ufgKrVbBKdop9kDnuVboz5RdJ
;;; 1elxJOlBojBf3OevGTc2dgmCzutrVBXx6ecnF6JR1QE1O+baXHWh0+P25o3aa1A6
;;; cttcZefOuvq4TM4vROzu/MLog4SKQkJRX567cR1fe29jAO5qPgJ+dKFMg+ZE8MWf
;;; +zWG4e1HSyvHtffrl2v3NTit9m5jDNlb0shrpgWwQRLA8zRyDODK7RqebQWGD5Y4
;;; X7PaOcNSB7Ct7uhDilZIm2XPv+8I6N6pmDSXVDRx33h9g9qL75+SL2hSubvN9KSZ
;;; iiU/USb9qYHLEZQzl1YVQnLJUfFg6zd7QfNVKKkdeA4gh737mT4rCQQFCosDZM62
;;; KNFOW521EbXfU7DqEOVsnJeWCKsAnh+hnwSRjFD0l2IlDqawqB36jT5sFCag+9EV
;;; OIaUq0RKwkDgoD7VnOaIL7XStoXFIXJYCmn8FjTxHd6fJ6e7o3KSI+1mqpaozXhR
;;; L/uUvVROmZt3UjR7bk4T8FSwR9L/2VgAmncgHr+wBhyel4f2TcCcVUmA/GgfZQVD
;;; XUy4nUpJ8kLF2jwzn6DMOFWCI/syFV0+xvZ065bY5GPbrpU0y7R2qE4iwPeqku6s
;;; Eq8psPgTULu2hGIbsae463/YCryFcCCnl2rHYXvuy1awjl2HFfxmqhoXppVmSjMA
;;; XqbfNg==
;;; -----END-SIGNATURE-----