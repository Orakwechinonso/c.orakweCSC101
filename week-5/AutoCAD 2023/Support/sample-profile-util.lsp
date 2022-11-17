;;;
;;;  Copyright 2022 Autodesk, Inc.  All rights reserved.
;;;
;;;  Use of this software is subject to the terms of the Autodesk license 
;;;  agreement provided at the time of installation or download, or which 
;;;  otherwise accompanies this software in either electronic or hard copy form.
;;;
;;;
;;; DESCRIPTION: 
;;;  Sample profile manipulation utilities. All functions return T on success and nil 
;;  on failure. See comments above each function for additional details.
;;;
;;; EXAMPLES:
;;;   
;;; - Set active profile: 
;;;     (sample-profile-set-active "MyProfile")
;;;
;;; - Import a profile:
;;;     (sample-profile-import "c:\\myExportedProfile.arg" "MyFavoriteProfile" T)
;;;
;;; - Delete a profile:
;;;     (sample-profile-delete "unwanted")
;;;
;;;
;;; - Import a profile, even if it already exists, and set it active.
;;;
;;;    (sample-profile-import "c:\\CompanyProfile.arg" "MyProfile" T)
;;;    (sample-profile-set-active "MyProfile")
;;;
;;;
;;; - Import a profile, if not already present, and set it active
;;;
;;;    (if (not (sample-profile-exists "myProfile"))
;;;        (progn
;;;         (sample-profile-import "c:\\CompanyProfile.arg" "MyProfile" T)
;;;         (sample-profile-set-active "MyProfile")
;;;        )
;;;    )
;;;
;;;
;;; - Import a profile and set it active when AutoCAD is first started.
;;;  Place the following code in acaddoc.lsp with the desired ".arg" filename 
;;;  and profile name...
;;;
;;;    (defun s::startup ()
;;;      (if (not (vl-bb-ref ':sample-imported-profile)) ;; have we imported the profile yet?
;;;          (progn
;;;  
;;;            ;; Set a variable on the bulletin-board to indicate that we've been here before.
;;;            (vl-bb-set ':sample-imported-profile T) 
;;;          
;;;            ;; Import the profile and set it active
;;;            (sample-profile-import "c:\\CompanyProfile.arg" "MyProfile" T)
;;;            (sample-profile-set-active "MyProfile")
;;;   
;;;          );progn then
;;;      );if
;;;    );defun s::startup
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This helper function gets the profiles object.
;;
(defun sample-get-profiles-object ( / app pref profs )
 (vl-load-com)
 (and
  (setq   app (vlax-get-acad-object))
  (setq  pref (vla-get-preferences app))
  (setq profs (vla-get-profiles pref))
 )
 profs
);defun sample-get-profiles-object

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determine if a profile exists. Returns T if the specified profile name exists, and nil if not.
;;
(defun sample-profile-exists ( name / profs )
 (and name
      (setq names (sample-profile-names))
      (member (strcase name) (mapcar 'strcase names))
 )
);defun sample-profile-exists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the active profile. 
;; NOTES: 
;;  - If the specified profile name is already active then the function returns T and makes no additional 
;;    changes.
;;
;;  - The specified profile must exist. (You can import a profile using the  'sample-profile-import' 
;;    function.) If the specified profile does not exist, the function returns nil.
;;
(defun sample-profile-set-Active ( name / profs )
 (and
  name
  (setq profs (sample-get-profiles-object))
  (or (equal (strcase name) (strcase (getvar "cprofile")))
      (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-activeProfile (list profs name))))
  )
 );and
);defun sample-profile-set-Active

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete the specified profile. Fails if the specified profile is current.
;; 
(defun sample-profile-delete ( name / profs )
 (and
  name
  (setq profs (sample-get-profiles-object))
  (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-deleteprofile (list profs name))))
 )
);defun sample-profile-delete
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy profile.
;;
(defun acad-pref-profile-copy ( source target / profs )
 (and
  source
  target
  (setq profs (sample-get-profiles-object))
  (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-CopyProfile (list profs source target))))
 )
);defun sample-profile-copy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a list of profile names
;;
(defun sample-profile-names ( / profs result )
 (and
  (setq profs (sample-get-profiles-object))
  (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetAllProfileNames (list profs 'result))))
  result
  (setq result (vlax-safearray->list result))
 )
 result
);defun sample-profile-names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename
;;
(defun sample-profile-rename ( oldName newName / profs )
 (and
  oldName
  newName
  (setq profs (sample-get-profiles-object))
  (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-RenameProfile (list profs oldName newName))))
 )
);defun sample-profile-rename

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a unique profile name. This function returns a unique profile name that is guaranteed 
;; to not be present in the current list of profiles.
;;
(defun sample-get-unique-profile-name ( / names n name )
 (setq names (sample-profile-names)
       names (mapcar 'strcase names)
        name "TempProfileName"
           n 1
 )
 (while (member (strcase (setq name (strcat name (itoa n)))) names)
  (setq n (+ n 1))
 )
 name
);defun sample-get-unique-profile-name

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import
;; This function imports the specified .arg file and creates a new profile with the provided profile name.
;; If the specified profile already exists, it will be overwritten.
;; If the 'bUsePathInfo' parameter is non-nil then path information will be imported from the specified 
;; file. Otherwise, path information will be ignored.
;;
;; NOTES: 
;;  This function does not set the active profile. If you import a new profile 
;;  it will not become active unless it matches the name of the existing active profile. 
;;
;;  You can set the active profile by calling: 
;;    (sample-profile-set-active "ProfileName")
;;
(defun sample-profile-import ( filename profileName bUsePathInfo / sample-oldError profs isCProfile tempProfile result )

 ;; Set up an error handler so, if something goes wrong, we can put things back the way we found them
 (setq sample-oldError *error*)
 (defun *error* ( msg / )
  (if (and profileName
           tempProfile
           (equal tempProfile (getvar "cprofile"))
      )
      (progn
       ;; Something went wrong so put things back the way they were.
       (sample-profile-rename tempProfile profileName)
       (sample-profile-set-active profileName)
       (sample-profile-delete tempProfile)
      );progn then
  );if
  (setq *error* sample-oldError)
  (if msg
      (*error* msg)
      (princ)
  )
 );defun *error*

 (if (and bUsePathInfo
          (not (equal :vlax-false bUsePathInfo))
     )
     (setq bUsePathInfo :vlax-true)
     (setq bUsePathInfo :vlax-false)
 )
 (if (and filename
          (setq filename (findfile filename))
          profileName
          (setq profs (sample-get-profiles-object))
     );and
     (progn
      ;; We can't import directly to the current profile, so if the provided profile name matches 
      ;; the current profile, we'll need to:
      ;;  - rename the current profile to a unique name
      ;;  - import
      ;;  - set the new one current
      ;;  - delete the old one with the temp name
      (setq isCProfile (equal (strcase (getvar "cprofile")) (strcase profileName)))
      (if isCProfile
          (progn
           (setq tempProfile (sample-get-unique-profile-name))
           (sample-profile-rename (getvar "cprofile") tempProfile)
          );progn then
      );if

      ;; Import          
      (setq result (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-ImportProfile (list profs profileName filename bUsePathInfo)))))

      (if isCProfile
          (progn
           ;;  Handle current profile case...
           ;;  If the import was successful, then set the new profile active and delete the original
           ;;  else if something went wrong, then put the old profile back
           (if (and result
                    (setq result (sample-profile-set-Active profileName)) ;; set the newly imported profile active
               );and
               (sample-profile-delete tempProfile)            ;; then delete the old profile
               (sample-profile-rename tempProfile profileName);; else rename the original profile back to its old name
           );if
          );progn then
      );if
     );progn then
 );if

 (*error* nil) ;; quietly restore the original error handler
 result
);defun sample-profile-import

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgADVdSXnWXgfF8oGRrP+IWgPdxmP1sXMi6qAFfr
;;; 3bH17xRNcZjG1dabFllT1Ek/eCI8R8EBYd8y6Nz45+m4elc6uMVo5/yxNIZ52/kQ
;;; 1Tww2fTAIlTIXz01HEf+Gh1IC/Az6NVew51v7O4OejzjOY6ZntwC+e2yoYQGXITF
;;; W5biuR/w/8NQHAfiOLy5/ISYn99CkOTYrypRTy2yhLTnFUFOF/5MV0tjkVjmzrQm
;;; DzjtrrKrZco+cbLgN0PAb9mqk0USAdhJNpOfXtyXlo9RoWifv21BUbDpcp/dur/q
;;; w2TRAysPvI+YGwnz/L8RQFXdg1HlCC5dndTiiTo9sPd7aZAy5QMZp086Plo1CI6i
;;; vPGEJ85c+H842XpZS+pBXHIvKKGs8FAZXZcwSxNud7VT/RNYhFEoycdPr+OilbWV
;;; HFl6o2dkFzZAvqxb9UjzlJbSWhc8WVGPe+rNKY028JcJLR2X+HGjToB03Y2WNQiB
;;; +tpNfbECV3tbQozlxVKKw8HX/SvzhuXaE+g6OM9qqJ5qlSVnvdDt0eBYUKikjkCM
;;; fdIq3w9JSeXLcHg4wmgSrBPeSujoU8CiPJwO/TVsFuiaOUt2yxpHH6NaKs5z0Kd7
;;; ZDiLKBZSB5nhMy6rh7BWLRd+qU5heqWxU8S+sgZ5E4U9GJlABwZgwjaZ/jm0Oloi
;;; NWlTng==
;;; -----END-SIGNATURE-----