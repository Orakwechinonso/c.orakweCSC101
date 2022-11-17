;;
;;  explan.lsp - Express Tools plan replacement command
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
;;  Similar to PLAN except that EXPLAN automatically zooms to the center 
;; of the extents of selected objects after performing a plan view to
;; the specified ucs. i.e. In other words; it keeps the zoom distance 
;; consistant.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun c:explan ( / ss c s lst d )
 (acet-error-init 
   (list '( "cmdecho" 0
            "ucsicon" nil
          ) 
         1
         '(setq ACET:UCS-LIST nil)
   );list
 )
 (setq s (getvar "viewsize"))
 (princ "\nSelect objects to zoom to or press <enter> to select everything on screen...")
 (if (and (setq ss (ssget))
          (setq c (acet-geom-ss-extents ss nil))
     );and
     (setq c (acet-geom-midpoint (car c) (cadr c)));setq then
     (progn
      (setq lst (acet-explan-view-extents)
            lst (acet-geom-m-trans lst 0 1)
            lst (acet-geom-cube-points lst)
            lst (acet-geom-list-extents lst)
              c (acet-geom-midpoint (car lst) (cadr lst))
              d (distance (car lst) (cadr lst))
      );setq
      (if (> d s)
          (setq s (/ (+ d s) 2.0));then use the average of the two
      );if
      ;(command "_.line" c pause "")
     );progn else
 );if
 (princ "\nEnter an option [Current ucs/Ucs/World] <Current>: ")
 (command "_.plan")
 (setvar "cmdecho" 1)
 (while (wcmatch (getvar "cmdnames") "*PLAN*")
  (command pause)
 );while
 (setvar "cmdecho" 0)
 (if c
     (command "_.zoom" "_c" c s)
 );if
 (acet-error-restore)
);defun c:explan
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Selects everything  on screen and uses the extents of that combined with the screen 
;corcer points to determine the center point (wcs) to use for the zoom after the plan 
;command.
;Returns list of points in wcs
;
(defun acet-explan-view-extents ( / ss p1 p2 p3 p4 lst px )
 (princ "\nSelecting objects on screen...")
 (acet-sysvar-set (list "ucsicon" 0))
 (acet-ucs-cmd (list "_view"))
 (setq  p1 (acet-geom-view-points)
        p2 (cadr p1)
        p1 (car p1)
        px (acet-geom-pixel-unit)
        p1 (list (+ (car p1) px)
                 (+ (cadr p1) px)
                 (caddr p1)
           )
        p2 (list (- (car p2) px)
                 (- (cadr p2) px)
                 (caddr p2)
           )
 );setq
 (if (or (setq ss (ssget "_w" p1 p2))
         (setq ss (ssget "_c" p1 p2))
     );or
     (setq p3 (acet-geom-ss-extents ss nil) ;no shrinkwrap
           p4 (cadr p3)
           p3 (car p3)
           p1 (list (car p1)		;; combine the view extents xy with extents of objects in z
                    (cadr p1)
                    (min (caddr p3) (caddr p4))
              )
           p2 (list (car p2)
                    (cadr p2)
                    (max (caddr p3) (caddr p4))
              )
     );setq then
 );if
 (setq lst (acet-geom-cube-points (list p1 p2)) ;generate the remaining points based on lower left and upper right
       lst (acet-geom-m-trans lst 1 0)
 );setq
 (acet-ucs-cmd (list "_previous"))
 (acet-sysvar-restore)
 (princ "Done")
 lst
);defun acet-explan-view-extents
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of 8 points 4 top and 4 bottom.
;
(defun acet-geom-cube-points ( lst / p1 p2 z1 z2 )
 (setq lst (acet-geom-list-extents lst)
        p1 (car lst)
        p2 (cadr lst)
        z1 (min (caddr p1) (caddr p2))
        z2 (max (caddr p1) (caddr p2))
 );setq
 (list (list (car p1) (cadr p1) z1)
       (list (car p2) (cadr p1) z1)
       (list (car p2) (cadr p2) z1)
       (list (car p1) (cadr p2) z1)
       (list (car p1) (cadr p1) z2)
       (list (car p2) (cadr p1) z2)
       (list (car p2) (cadr p2) z2)
       (list (car p1) (cadr p2) z2)
 );list
);defun acet-geom-cube-points


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDX+NhQD38yo8RsE2fVfmsLJZHBJZCuTOq7bWWR
;;; gzUuqwBwr/hIRrX0nqj9EhmWBq2g5NjEIs9ZvwWe4K1eRdnx1qrvKe5Y4iFFm5Ey
;;; 3WyPED7y3ULwBdTIdLiHRXH9v7bntn4aXlq85AhlfJwitDcpuCVvFcJQEbGBQhbG
;;; 6FiwdCOouJ6X3Kwl6N1qpxc47TaJNEhJSCrfJt2vpbPm9SlnzNpA05/zpxu7eKFG
;;; oc8CNBU9W0AyYAJ85VQypOMB6uebcNn/ezgLK0rj1keTILuxJ24oRn5ymnDssAG3
;;; wLbK3ajahIO+hhbvuDnOjCUaRH3HzUltnFBuUd3VUgPnreLLFDwVYjtHrUvH06I4
;;; RMx8CWO9JacQUxhJ6S73cx1GY4OlRBEzXsFXekjZzc3IYKQofCv7KbCTs6OXjqAu
;;; oOKTwjC0bnf3+XcipoauCm6yo1vZjXhBee6bVvjwewClzW2ptZWGlc60LRD7tOKA
;;; e8mOeBC7GkqlYS42G2VIT88xZ284DazoIMRvQdRCzIbbc01tzBL+dVVQ74cYvfFz
;;; QqQb8Jo+ryZstkJY/G/SmcdjYgoPfxiDGlyhtDHDUzf3TJ7D58DPxUr7xVGHSJkG
;;; NsAUVsKffbs/cDRHXuxyu2+tXc9z12Y8e9K86Ec+jH1QxN/p/DMPyTsCVt0W8tfh
;;; ugE2cA==
;;; -----END-SIGNATURE-----