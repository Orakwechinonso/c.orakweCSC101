; Next available MSG number is    13 
; MODULE_ID ATTREDEF_LSP_
;;;
;;;    attredef.lsp
;;;
;;;  Copyright 2022 Autodesk, Inc.  All rights reserved.
;;;
;;;  Use of this software is subject to the terms of the Autodesk license 
;;;  agreement provided at the time of installation or download, or which 
;;;  otherwise accompanies this software in either electronic or hard copy form.
;;;
;;;
;;;
;;; DESCRIPTION
;;;
;;;    ATTREDEF command - Redefines a block and updates associated attributes
;;;
;;;    New attributes assigned to existing block references use their default
;;;    values.  Old attributes in the new block definition retain their old
;;;    values in each associated block reference.    Any old attributes that
;;;    are not included in the new block definition are deleted from each 
;;;    associated block reference.
;;;
;;;    This command has been updated to support Multiline Text Attributes.
;;;    Note, when a single line attribute definition is redefined as a 
;;;    multiline text attribute definition, the associated references will be
;;;    updated to be multiline text attributes with a single line value and
;;;    set with a boundary width of 0.  When a multiline text attribute
;;;    definition is redefined as a single line attribute definition, the
;;;    associated references will be converted to single line attributes
;;;    if the length of the attribute value is less than 256  characters.
;;;
;;;    Warning! ATTREDEF removes any format or property changes made with the ATTEDIT or
;;;    EATTEDIT commands. It also deletes any extended data associated with the block, 
;;;    and might affect dynamic blocks and blocks created by third-party applications.
;;;

;;; --------------------------------------------------------------------------;
;;;
;;; Oldatts sets "old_al" (OLD_Attribute_List) to the list of old Attributes
;;; for the Block Reference, identified by b1.  The list does not include 
;;; constant Attributes.
;;;
(defun oldatts (/ e_name e_list cont)
  (setq oa_ctr 0 
        cont   T
        e_name b1
  )
  (while cont
    (if (setq e_name (entnext e_name))
      (progn
        (setq e_list (entget e_name))
        ;; If necessary, update the text string (group code 1) in e_list
        ;; to ensure the multiline attribute text is used
        (if (and (= (cdr (assoc 0 e_list)) "ATTRIB")
                 (_matts_util (cdr (assoc -1 e_list))))
            (setq e_list (subst 
                             (car (_matts_util (cdr (assoc -1 e_list)) 2))
                             (assoc 1 e_list)
                             e_list)
            )
        )
        (if (and (= (cdr (assoc 0 e_list)) "ATTRIB")
                 ;; NOT a constant attribute -- (cdr (assoc 70 e_list)) != 2)
                 (/= (logand (cdr (assoc 70 e_list)) 2) 2))
          (progn
             (if old_al
                 (setq old_al (cons e_list old_al))
                 (setq old_al (list e_list))
              )
             (setq oa_ctr (1+ oa_ctr))           ; count the number of old atts
          )
          ;; else, exit
          (setq cont nil)
        )
      )
      (setq cont nil)
    )
  )
)
;;;
;;; Newatts sets "new_al" to the list of new Attributes in the new Block.
;;; The list does not include constant Attributes.
;;;
(defun newatts (ssetn ssl / i e_name e_list)
  (setq i 0 na_ctr 0)
  (while (< i ssl)
    (if (setq e_name (ssname ssetn i))
      (progn
        (setq e_list (entget e_name))
        (if (and (= (cdr (assoc 0 e_list)) "ATTDEF")
                 ;; NOT a constant attribute -- (cdr (assoc 70 e_list)) != 2)
                 (/= (logand (cdr (assoc 70 e_list)) 2) 2))
          (progn
              (if new_al
                  (setq new_al (cons e_list new_al))
                  (setq new_al (list e_list))
               )
              (setq na_ctr (1+ na_ctr))     ; count the number of new atts
          )
        )
      )
    )
    (setq i (1+ i))
  )
  na_ctr
)
;;;
;;; This utility function updates the given list with ename2
;;; if the attribute specified by ename1 is a multiple line text
;;; attribute or attribute definition.  Otherwise, it updates
;;; the list with a nil entry.
;;; 
(defun updateMattsList ( ename1 ename2 mattsList / mattsEname )
    (if (_matts_util ename1)
        (setq mattsEname ename2)
        (setq mattsEname nil)
    )
    (if mattsList
        (setq mattsList (cons mattsEname mattsList))
        (setq mattsList (list mattsEname))
    )
    mattsList
)
;;; This utility function prompts the user for permission 
;;; to truncate upon conversion of a multiple line text attribute 
;;; to a single line text attribute.
;;; Note, if permission is not granted, the ATTREDEF command is
;;; cancelled and undone.
;;;
(defun truncateUponConversionToSingleLine (old_al_jth nMaxChars /
           truncated_text old_al_jth_revised)
    ;; Prompt for truncation permission using a dialog
    (if (_matts_util (entlast) 6) 
        (progn
            ;; Truncate the text
            (setq truncated_text
                  (cons 1 (substr (cdr (assoc 1 old_al_jth)) 1 nMaxChars)))
             ;; Update the sublist
             (setq old_al_jth_revised 
                   (subst truncated_text (assoc 1 old_al_jth) old_al_jth))
             ;; Update the old_al list with the revised sublist
             (setq old_al (subst old_al_jth_revised old_al_jth old_al))
        )
        (progn
            ;; Cancel the command
            (prompt "Function cancelled")
            ;; Force one undo after the command is "cancelled"
            (setq _attredef_undo T)
            (exit)
        )
    )
)
;;;
;;; Compare the list of "old" to the list of "new" Attributes and make
;;; the two lists "same" and "preset". "Same" contains the old values of
;;; all the Attributes in "old" with equal tag values to some Attribute
;;; in "new" and the default values of all the other Attributes. "Preset"
;;; contains the preset Attributes in old with equal tag values to some
;;; Attribute in new.
;;;
;;; Note, the presetMatts and sameMatts list are created to track the
;;; ids (enames) of multiple line attributes / attribute definitions
;;; in the "preset" and "same" lists.   When an attribute or
;;; attribute definition is "single line", a "nil" entry will be added
;;; to the presetMatts or sameMatts lists.
;;;
(defun compare (/ i j)
  (setq i 0
        j 0
        pa_ctr 0
        same nil
        va_ctr 0
        preset nil)
  ;; "i" is a counter that increments until the number of new attributes
  ;; is reached.
  (while (< i na_ctr)
    (cond 
      ;; If there are old attributes AND the tag strings of the old and new 
      ;; attributes are the same...
      ((and old_al
            (= (cdr (assoc 2 (nth j old_al))) (cdr (assoc 2 (nth i new_al)))))
        ;; If converting an old attribute from multiple line mode to
        ;; single line mode, prompt user for any needed truncation or 
        ;; cancellation
        (if (and (_matts_util (cdr (assoc -1 (nth j old_al))))
                 (not (_matts_util (cdr (assoc -1 (nth i new_al)))))
                 (> (strlen (cdr (assoc 1 (nth j old_al)))) 255)
            )
            (truncateUponConversionToSingleLine (nth j old_al) 255)
        )
        ;; IS a preset attribute -- (cdr (assoc 70 e_list)) == 8)
        (if (= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
          ;; If the attribute is a preset attribute then add it to the list
          ;; of preset attributes and increment the counter "pa_ctr".
          ;; IS a preset attribute -- (cdr (assoc 70 e_list)) == 8)
          (progn
            (if preset
              (setq preset (cons (nth j old_al) preset))
              (setq preset (list (nth j old_al)))
            )
            (setq pa_ctr (1+ pa_ctr))     ; count preset atts
            ;; Add the ename of the old preset attrib to the presetMatts
            ;; list when the new attrib is MATTS; otherwise add nil.
            (setq presetMatts 
                  (updateMattsList
                     (cdr (assoc -1 (nth i new_al)))
                     (cdr (assoc -1 (nth j old_al)))
                     presetMatts
                  )
            )
          )
          (progn
            ;; Else, add it to the list of same attributes "same".
            (if same
                (setq same (cons (cdr (assoc 1 (nth j old_al))) same))
                (setq same (list (cdr (assoc 1 (nth j old_al)))))
            )
            ;; Add the ename of the old same attrib to the sameMatts
            ;; list when the new attrib is MATTS; otherwise add nil.
            (setq sameMatts 
                  (updateMattsList
                     (cdr (assoc -1 (nth i new_al)))
                     (cdr (assoc -1 (nth j old_al)))
                     sameMatts
                  )
            )
          )
        )

        ;; If the attribute must be verified, increment counter "va_ctr".
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (and (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
                 ;; IS a verified attribute -- (cdr (assoc 70 e_list)) == 4)
                 (= (logand (cdr (assoc 70 (nth i new_al))) 4) 4))
          (setq va_ctr (+ 1 va_ctr))
        )
        (setq i (1+ i))
        (setq j 0)
      )
      ;; If the number of old attributes equals the old attribute counter "j"
      ((= j oa_ctr)
        ;; If this attribute is not a preset attribute, but is not in the 
        ;; old list, then add it to the list "same".
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
          (progn
             (if same
                 (setq same (cons (cdr (assoc 1 (nth i new_al))) same))
                 (setq same (list (cdr (assoc 1 (nth i new_al)))))
              )
              ;; Add the ename of the new attrib definition to the sameMatts
              ;; list when the new attrib is MATTS; otherwise add nil.
              (setq sameMatts 
                  (updateMattsList
                     (cdr (assoc -1 (nth i new_al)))
                     (cdr (assoc -1 (nth i new_al)))
                     sameMatts
                  )
              )
           )
        )
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (and (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
                 ;; IS a verified attribute -- (cdr (assoc 70 e_list)) == 4)
                 (= (logand (cdr (assoc 70 (nth i new_al))) 4) 4))
          (setq va_ctr (+ 1 va_ctr))
        )
        (setq i (1+ i))
        (setq j 0)
      )
      ;; Increment the old attribute counter "j"...
      (t
        (setq j (1+ j))
      )
    )
  )
)
;;;
;;; Find the entity for each of the "preset" Attributes in the newly
;;; inserted Block.
;;;
(defun findpt ( )
  (setq test T)
  (setq en (entnext e1))
  (setq e_list (entget en))
  (while test
    (if (and (= (cdr (assoc 0 e_list)) "ATTRIB") 
             (= (cdr (assoc 2 e_list)) tag)
        )
      (setq test nil)
      (progn
        (setq ex en)
        (setq en (entnext ex))
        (if e_list
          (setq e_list (entget en))
        )
      )
    )
  )
)


;; This function determines a block, specified by the name, bn,
;; is not scalable in each dimension (i.e. upon insertion, 
;; it is restricted to uniform scaling only).
(defun isBlockNotScalable ( bn / blkBeginName blkBegin btrName btr notscalable)
  ;; Note, the uniform scaling restriction flag is indicated by 
  ;; the 281 group code on the block table record for block bn
  (setq blkBeginName (tblobjname "BLOCK" bn))
  (if blkBeginName
      (setq blkBegin (entget blkBeginName))
      (setq blkBegin nil)
  )
  (if blkBegin
      (setq btrName (cdr (assoc 330 blkBegin)))
      (setq btrName nil)
  )
  (if btrName
      (setq btr (entget btrName))
      (setq btr nil)
  )
  (if btr
      (setq notscalable (= 1 (cdr (assoc 281 btr)) ) )
      (setq notscalable nil)
  )
)

;;;
;;; Insert a new Block on top of each old Block and set its new Attributes
;;; to their values in the list "same". Then replace each of the "preset"
;;; Attributes with its old value.
;;;
(defun redef (/ xsf ysf zsf ls i e1 v blkref refSpace curTilemode curVport
                notscalable bNextIsMatts mattsIdList sameMattsItem 
                presetItem presetMattsItem bIsPresetMatts 
             )
  (setq blkref (entget b1))
  (setq xsf (cdr (assoc 41 blkref))) ; find x scale factor
  (setq ysf (cdr (assoc 42 blkref))) ; find y scale factor
  (setq zsf (cdr (assoc 43 blkref))) ; find z scale factor
  (setq refSpace (cdr (assoc 67 blkref)))
  (setq ls (length same))
  (setq i 0)
  ;; switch spaces to that of the block reference, if necessary
  (setq curVport (getvar "CVPORT")
        curTilemode (getvar "TILEMODE"))
  ;; switch to tilemode on, if necessary
  (if (and (= refSpace 0) (= curTilemode 0))(setvar "TILEMODE" 1))
  ;; switch to tilemode off, if necessary
  (if (and (= refSpace 1) (= curTilemode 1))(setvar "TILEMODE" 0))
  ;; switch to paper space, if necessary
  (if (and (= refSpace 1) (/= curVport 1))(command "_.PSPACE"))
  (command "_.UCS" "_E" b1)  ; define the block's UCS

  ;; is block set for uniform scaling only?
  (setq notscalable (isBlockNotScalable bn))

  (if (= T notscalable)
    (command "_.-INSERT" bn "0.0,0.0,0.0" xsf "0.0")
    (command "_.-INSERT" bn "0.0,0.0,0.0" 
      "_XYZ" xsf ysf zsf "0.0")
   )

  (setq mattsIdList nil)
  
  ;;; Note, the -INSERT command is now prompting for attribute values,
  ;;; which will be set (stuffed via the command function) below, using
  ;;; values from the "same" list.
  (while (< i ls)                     ; set attributes to their values
    (setq sameMattsItem (nth i sameMatts))
    (if sameMattsItem
       (progn
         (setq bNextIsMatts T)
         ;; save the ename in a list for later use!
         (if mattsIdList
             (setq mattsIdList (append mattsIdList (list sameMattsItem)))
             (setq mattsIdList (list (entlast) sameMattsItem))
         )
       )
       (setq bNextIsMatts nil) ; Else - this attribute is not multiline
    )
    (command (nth i same))
    ;; Terminate the multiline line text attribute prompt, when necessary.
    (if bNextIsMatts 
        (command "")
    )
    (setq i (1+ i))
  )

  ;;; In the case of any VERIFY attributes, finish the -INSERT command
  ;;; by accepting the values.
  (while (< 0 va_ctr)
    (command "")                      ; at prompts, verify attributes
    (setq va_ctr (1- va_ctr))
  )
  
  ;;; Postprocess the preset attributes
  (setq i 0)
  (setq e1 (entlast))
  (while (< 0 pa_ctr)                    ; edit each of the "preset" attributes
     (setq presetItem (nth i preset))
     (setq tag (cdr (assoc 2 presetItem)))
     (setq v (cdr (assoc 1 presetItem)))
      ;; If the preset attribute is a multiline attrib, save its id
      ;; for postprocessing after this loop.
     (setq presetMattsItem (nth i presetMatts))
     (if presetMattsItem
         (progn
             (setq bIsPresetMatts T)
             (if mattsIdList
                 (setq mattsIdList (append mattsIdList (list presetMattsItem)))
                 (setq mattsIdList (list (entlast) presetMattsItem))
             )
         )
        (setq bIsPresetMatts nil)
     )
    
     ;;; For single line attributes only, find the old preset attribute that
     ;;;  matches and entmod the new preset attribute to have the same value.
     (if (not bIsPresetMatts)
         (progn 
              ; find the entity to modify (e_list is set)
             (findpt)
             (setq e_list (subst (cons 1 v) (assoc 1 e_list) e_list))
             ; modify the entity's value
             (entmod e_list)
         )
     )
     (setq i (1+ i))
     (setq pa_ctr (1- pa_ctr))
  )
  
  ;; Note, e1 is the id of the new block reference created by -INSERT.
  ;; Postprocess any multiline text attributes in the block reference,
  ;; replacing their default values with any of the original values.
  (if mattsIdList
      (_matts_util e1 5 mattsIdList)
  )

  ;; Cleanup
  (command "_.UCS" "_P")           ; restore the previous UCS
  ;; restore the current tilemode and space
  (if (/= curTilemode (getvar "TILEMODE"))
      (setvar "TILEMODE" curTilemode)
  )
  (if (and (= curTilemode 0)
           (/= curVport (getvar "CVPORT")))
    (command "_.MSPACE")
  )
)
;;;
;;; System variable save
;;;
(defun modes (a)
  (setq mlst '())
  (repeat (length a)
    (setq mlst (append mlst (list (list (car a) (getvar (car a))))))
    (setq a (cdr a)))
)
;;;
;;; System variable restore
;;;
(defun moder ()
  (repeat (length mlst)
    (setvar (caar mlst) (cadar mlst))
    (setq mlst (cdr mlst))
  )
)
;;;
;;; Internal error handler
;;;
(defun attrerr (s)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  )
  (moder)                             ; restore saved modes
  ; Terminate the undo grouping
  (command "_.UNDO" "_END")
  ; If desired, undo one command to cancel the changes of this command
  (if _attredef_undo
    (progn
      (command "_.UNDO" "1")
      (setq _attredef_undo nil)
    )
  )
  ; Restore CMDECHO without undo recording
  (ai_setCmdEcho _attdef_oldCmdEcho)
  
  (setq *error* olderr)               ; restore old *error* handler
  (princ)
)
;;;
;;; Main program
;;;
(defun C:ATTREDEF (/ k n olderr bn sseto ssetn pt ssl new_al
                     old_al same preset b1 oa_ctr va_ctr na_ctr
                     sameMatts presetMatts
                  ) 
  (setq k 0
      n 0
      test T
      olderr *error*
      *error* attrerr
  )

  (if (= (getvar "blockeditor") 1) 
    (progn
        (defun *error* (s)
            (if olderr (setq *error* old_error))
            (princ)
        )
        (prompt "\n** ATTREDEF command not allowed in block editor. **\n")
        (exit)
    )
  )
  
  ;;;
  ;;; Prompt for (uppercase) block-name pattern
  ;;;
  (defun getucsymbolstring (p / resp)
    (setq resp ; raw response
      (getstring
        (if (or (eq (getvar "EXTNAMES") 0)
                (eq (logand (getvar "CMDACTIVE") 4) 4))
          nil 1)
        p))
    (if (wcmatch resp "\"*\"")
      (setq resp (substr resp 2 (- (strlen resp) 2))))
    (xstrcase (ai_strtrim resp))
  )

  (modes '("ATTDIA" "ATTREQ" "GRIDMODE"
     "UCSFOLLOW"))

  (setq _attdef_oldCmdEcho (getvar "CMDECHO"))
  ; set CMECHO without undo recording
  (ai_setCmdEcho 0)

  (command "_.UNDO" "_GROUP")
  (setvar "attdia" 0)                 ; turn attdia off
  (setvar "attreq" 1)                 ; turn attreq on
  (setvar "gridmode" 0)               ; turn gridmode off
  (setvar "ucsfollow" 0)              ; turn ucsfollow off  

  (while 
    (progn
      (setq bn (getucsymbolstring
          "\nEnter name of the block you wish to redefine: "))
      (if (tblsearch "block" bn)
        (progn
          (setq sseto (ssget "_x" (list (cons 0 "INSERT") (cons 2 bn))))
          (setq test nil)
        )
        (progn
          (princ "\nBlock \"")
          (princ bn)
          (princ "\" is not defined. Please try again.\n")
        )
       )
    )
  )
  (if sseto
    (progn
      ;; Filter out references on locked layers
      (while (< k (sslength sseto))
        ;; get reference's layer name and get the layer's flags
        ;; to check if it's on a locked layer.
        (if (= 4 (logand 4 (cdr (assoc '70 (tblsearch "LAYER" 
                           (cdr (assoc  '8 (entget (ssname sseto k)))))))))
           (setq sseto (ssdel (ssname sseto k) sseto))
           (setq k (1+ k)) ; else: step to next item in the set
        )
      )
      (setq k 0) ; reset selection set index to start position
      (while 
        (progn
          (princ "\nSelect objects for new Block... ")
          (if (null (setq ssetn (ssget "_:l")))
            (princ "\nNo new Block selected. Please try again.")
            (setq test nil)
          )
        )
      )
      ;; find the list of new attributes
      (setq na_ctr (newatts ssetn (sslength ssetn)) )
      (if (> na_ctr 0)
        (progn
          (initget 1)
          (setq pt (getpoint "\nSpecify insertion base point of new Block: "))
          (setq ssl (sslength sseto))
          ;; redefine the block
          (command "_.-BLOCK" bn "_Y" pt ssetn "") 
          (while (< k ssl)
            (setq b1 (ssname sseto k))    ; For each old block...
            (setq old_al nil)
            (oldatts)                     ; find the list of old attributes,
            (compare)                     ; compare the old list with the new,
            (redef)                       ; and redefine its attributes.
            (entdel b1)                   ; delete the old block.
            (setq k (1+ k))
          )
          ; Regen is no longer necessary, as the -BLOCK command now
          ; regens all affected blocks.         
          ; (command "_.REGENALL")
        )
        (princ "\nNew block has no attributes. ")
      )
    )
    (princ (strcat "\nNo insertions of block " bn " found to redefine. "))
  )
  (moder)                             ; restore saved modes
  (command "_.UNDO" "_END")
  ; Restore CMDECHO without undo recording
  (ai_setCmdEcho _attdef_oldCmdEcho)
  (setq *error* olderr)               ; restore old *error* handler
  (princ)
)


(defun ai_abort (app msg)
   (defun *error* (s)
      (if old_error (setq *error* old_error))
      (princ)
   )
   (if msg
     (alert (strcat " Application error: "
                    app " \n\n  " msg "  \n"))
   )
   (exit)
)

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

(cond
   ((and ai_dcl (listp ai_dcl)))          ; it's already loaded.
   ((not (findtrustedfile "ai_utils.lsp"))                     ; find it
      (ai_abort "ATTREDEF"
                (strcat "Can't locate file AI_UTILS.LSP."
                        "\n Check support directory.")))

   ((eq "failed" (load (findtrustedfile "ai_utils.lsp") "failed"))   ; load it
    (ai_abort "ATTREDEF"
              " Can't load file AI_UTILS.LSP"))
)

(defun c:at () (c:attredef))
(princ 
"\nC:ATtredef loaded.  Start command with AT or ATTREDEF.")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgB3wkOa1bXSdZ/Fti18on4m1eFw3qOO35YDkH7d
;;; fsyaU6zS3LYCric8eJygUUivWhrdzBYE3oBt5l9YOlriIKQtMvyhG3ohiWMNarO9
;;; 3QCJn4EQ740DAtbpJY4cVT0ocWPBr8jrmGPy40M6Xd8rOSlaqEvCOrcuCZQ7L1A8
;;; 31n12Mp7oxIaaEgFSbLQhElT6zbopJdx/+RidIaFdwp+9og+x7OYaN97q7R2EsPH
;;; M2COor7TsOR0mmfehbkQ9YVxrQbzDpNt1PC0qe+e2oqzsbO249f4gJtW7LHq4iHe
;;; ONIG328f6D9XOtlnXsi3vcz0/Rcl/z0u9RdUW8WliIm16BDXb47H7OODyuJFdafx
;;; kdSPsSX3EF87EExDwDuUS9w8lsgy5e9t0Vj5DyJBv4eEaAAsU55xFresYOhmD7Ec
;;; cme/v9qz3Fkw0mseorTpHjigOdYKOXe9C/s/K+vu3j2KGTLhAB9woGiEh0lg0pIl
;;; 44kfsjFT4IRD/XG6KX6c60BeR3M51isGEGqkiktv4BncphiH62fQUy/AJpR5CSM9
;;; EVlo+nn1nQnAxDTGD5w+IPJviODKEp762YyhkzgMXOaBgfybgziD5M/wLkMDQciE
;;; IFsOEKdDFpvDhhsl9TiJj7iLmZU4N/rtvrEb4UxmWE4ape1rkHYbkg6HNa80Ecfd
;;; a+F9Pg==
;;; -----END-SIGNATURE-----