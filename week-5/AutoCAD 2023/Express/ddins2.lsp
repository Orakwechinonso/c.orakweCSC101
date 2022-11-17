;;;
;;;    DDINS2.LSP
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
 
;;;   DESCRIPTION
;;;
;;;   An AutoLISP implementation of the AutoCAD INSERT command with a dialogue
;;;   interface.  Answers the oft requested feature of being able to select
;;;   at Insert time either an internal or external drawing.
;;;
;;;   The user is presented with a dialogue allowing the selection from nested
;;;   dialogues of either an internal or external block.  Edit fields can be
;;;   used to enter or preset the insertion point, scale, and rotation angle,
;;;   or alternatively, these can be set dynamically as in the INSERT command.
;;;
;;;   UPDATE:  Removed code that presumed a .dwg extension for drawings.  A
;;;   file extension is defined to be the letters following the final "." in
;;;   the drawing name.  This extension and period are removed to give the
;;;   block name.
;;;
;;;----------------------------------------------------------------------------
;;;   Prefixes in command and keyword strings:
;;;      "."  specifies the built-in AutoCAD command in case it has been
;;;           redefined.
;;;      "_"  denotes an AutoCAD command or keyword in the native language
;;;           version, English.
;;;
;;;----------------------------------------------------------------------------
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================
;;;
 
  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (acet-str-format " Application error: %1 \n\n  %2  \n" app msg))
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
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.
 
     (  (not (findtrustedfile "ai_utils.lsp"))                     ; find it
        (ai_abort "DDINSERT" "Can't locate file AI_UTILS.LSP.\n Check support directory."))
 
     (  (eq "failed" (load (findtrustedfile "ai_utils.lsp") "failed"))      ; load it
        (ai_abort "DDINSERT" "Can't load file AI_UTILS.LSP"))
  )
 
  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "DDINSERT" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.
 
;;; ==================== end load-time operations ===========================
 
 
;;;----------------------------------------------------------------------------
;;;  The main dialogue.
;;;----------------------------------------------------------------------------
(defun ddins2 (/
                     $value           do_insert      old_cmd            x_pt
                     blk_exists       do_selection   parse_path         x_scale
                     blk_name         error_msg      pat                y_pt
                     blk_name1        explode        path_name          y_scale
                     bl_match         ex_name        path_name_exist1   z_pt
                     bl_name          error_scale    pat_match          z_scale
                     check_current    globals        redefine
                     check_fd         good_value     reset
                     check_i          ins_name       rotation
                     check_input      int_blocks     range
                     check_name       ins_var
                     check_name_ok    just_name      table_item
                     cmd              list1
                                      list_blocks    table_name
                     dcl_id           n              use_val
                     ddinsert_main    name           use_val_tog
                                      n_name         value
                                                     what_next
                  )
 
  ;;
  ;; Routine that inserts the selected block.
  ;;
  (defun do_insert()
    (cond
      ((= 1 on_screen)
       (command "_.insert")
       (setvar "cmdecho" 1)
       (command ins_name)
       (while (wcmatch (getvar "cmdnames") "*INSERT*") (command pause))
       (setvar "cmdecho" 0)
      )
      ((and (= 0 on_screen) (= 0 explode))
        (command "_.insert" ins_name (list x_pt y_pt z_pt)
                 "_xyz" x_scale y_scale z_scale rotation)
      )
      ((and (= 0 on_screen) (= 1 explode))
        (command "_.insert" ins_name (list x_pt y_pt z_pt)
                 x_scale rotation)
      )
      (T (princ "programming error in do_insert"))
    )
  )
  ;;
  ;;  Call routine to display nested dialogue.  Set edit box to returned value
  ;;  if not nil.
  ;;
  (defun int_blocks()
    (list_blocks)
    (if blk_name1
      (progn
        (set_tile "current_name" (setq blk_name (xstrcase blk_name1)))
        (set_tile "path_name" "")
      )
    )
  )
  ;;
  ;; Short hand error tile resetting.
  ;;
  (defun reset()
    (set_tile "error" "")
  )
  ;;
  ;; Check does the block exist, either internally or externally.
  ;;
  (defun check_current()
    (setq blk_name (xstrcase (ai_strtrim (get_tile "current_name"))))
    (setq path_name (ai_strtrim (get_tile "path_name")))
    (cond
      ;; Length less 32.
      ((not (snvalid blk_name))
        (set_tile "error" "Block name is invalid.")
      )
      ;; if the block exists in the drawing (necessary for logand).
      ((and (setq xcheck (tblsearch "block" blk_name))
            (not (zerop (logand 52 (cdr (assoc 70 xcheck)))))
       )
        (set_tile "error" "Error - Cannot Insert an Xref.  Use Xref Attach.")
      )
      ;; If the block is undefined give message.
      ((and (= "" path_name)
            (/= "" blk_name)
            (not (member blk_name table_list))
       )
        (set_tile "error" "Block name is invalid.")
      )
      ((and (= "" path_name)
            (= "" blk_name)
       )
        (set_tile "error" "Block name is invalid.")
      )
      ((not (or (member blk_name table_list) ; does name exist as an internal
                 (findfile path_name) ; or external block ??
            )
       )
        (set_tile "error" "Block name is invalid.")
      )
      (t)
    )
  )
  ;;
  ;; Check all input via a cond.  If any error exists, display the relevant
  ;; message. If no errors bring down the dialogue.
  ;;
  (defun check_name_ok(/ ex_found)
    (setq blk_name (xstrcase (ai_strtrim (get_tile "current_name"))))
    (setq path_name (ai_strtrim (get_tile "path_name")))
    ;; Check to see if the path name is valid once here.
    (if (findfile path_name) (setq ex_found 1))
 
    ;; It's acceptable for the user to type in a drawing name without
    ;; entering a Block name in the Block field.  If there is a valid pathname
    ;; and an empty block name field, use parse path to get the block name
    ;; from the drawing name.
    (if (and (= 1 ex_found) (= "" blk_name)) (parse_path))
 
    (setq redefine 1)
    ;; Check once here to see if the block name already exists in the drawing.
    (setq xcheck (tblsearch "block" blk_name))
 
    (cond
      ((and (= "" path_name)             ; Internal
            (member blk_name table_list)
            (= 0 explode)                ; explode off
       )
 
        (setq ins_name blk_name)
      )
      ((and (= "" path_name)             ; Internal
            (member blk_name table_list)
            (= 1 explode)                ; explode on
       )
        (setq ins_name (strcat "*" blk_name))
      )
      ((and (= 1 ex_found)               ; External
            (= explode 1)                ; Explode on
       )
 
        (setq ins_name (strcat "*" path_name))
      )
      ((and (= 1 ex_found)                       ; External
            (check_name blk_name)                ; blk name valid
            (not xcheck)                         ; unique
        )
        (setq ins_name (strcat blk_name "=" path_name))
      )
      ((and (= 1 ex_found)                                ; External
            xcheck
            (zerop (logand 53 (cdr (assoc 70 xcheck))))   ; Not an Xref
            (blk_exists)                                  ; redefine yes
       )
        (setq ins_name (strcat blk_name "=" path_name))
      )
      (T
 
        (cond
          ((not redefine)
          )
          ((and xcheck
                (not (zerop (logand 53 (cdr (assoc 70 xcheck))))) ; is an Xref
           )
            (set_tile "error" "Error - Cannot Insert an Xref.  Use Xref Attach."
            )
            ;; if the path_name in not "" set focus there on error.
            (if (read path_name)
              (mode_tile "path_name" 2)
              (mode_tile "current_name" 2)
            )
          )
          ((= "" path_name)
            (set_tile "error" "Block name is invalid.")
            (mode_tile "current_name" 2)
          )
          ((and (/= "" path_name)
          (not (findfile path_name))
          (not (findfile (strcat path_name ".dwg"))))
            (set_tile "error" "Invalid File name.")
            (mode_tile "path_name" 2)
          )
          ((and (/= "" path_name) (findfile path_name))
            (set_tile "error" "Block name is invalid.")
            (mode_tile "current_name" 2)
          )
          (T (princ "Block Name check error."))
        )
        nil
      )
    )
  )
  ;;
  ;;  On OK all input is checked before the dialogue is dismissed.
  ;;
  (defun check_input()
    (if (= 1 explode)
      (progn
        (setq range 6)                 ; non zero & non negative
        (setq error_scale "X scale must be positive & nonzero.")
      )
      (progn
        (setq range 2)                 ; non zero
        (setq error_scale "X scale must be nonzero.")
      )
    )
    (cond
      ((not (check_name_ok)))
      ((and (= 0 on_screen) (bad_xyzxr)))          ; check insert point coords.
      ((and (= 0 on_screen)        ; check scale if explode is off
            (= 0 explode)
            (bad_yz)
      ))
      (t (done_dialog 1))           ; if all is well, bring down the dialogue.
    )
  )
  ;;
  ;; Check some input and set focus on error.  Return nil is nothing bad.
  ;;
  (defun bad_xyzxr()
    (cond
      ((not (setq x_pt (ai_num (get_tile "x_pt") "Invalid X coordinate." 0)))
        (mode_tile "x_pt" 2)
      )
      ((not (setq y_pt (ai_num (get_tile "y_pt") "Invalid Y coordinate." 0)))
        (mode_tile "y_pt" 2)
      )
      ((not (setq z_pt (ai_num (get_tile "z_pt") "Invalid Z coordinate." 0)))
        (mode_tile "z_pt" 2)
      )
      ((not (setq x_scale (ai_num
                            (get_tile "x_scale") error_scale range)))
        (mode_tile "x_scale" 2)
      )
      ((not (setq rotation (ai_angle (get_tile "rotation")
                                    "Invalid Rotation angle."
                           )
            )
        )
        (mode_tile "rotation" 2)
      )
      (t nil)
    )
  )
  ;;
  ;; Check the Y scale and Z scale.  Return nil if bad.
  ;;
  (defun bad_yz()
    (cond
      ((not (setq y_scale (ai_num (get_tile "y_scale")
                                  "Y scale must be nonzero." 2)))
        (mode_tile "y_scale" 2)
      )
      ((not (setq z_scale (ai_num (get_tile "z_scale")
                                  "Z scale must be nonzero." 2)))
        (mode_tile "z_scale" 2)
      )
    )
  )
  ;;
  ;; If called with 0, display getfiled for a drawing name. If called with 1
  ;; get the string form the edit box.
  ;;
  (defun check_fd (bit)
    (cond
      ((and (= 0 bit)
            ;; Update here for filenames without dwg extensions...
            (setq ex_name (ACET-FILE-WRITEDIALOG "Select Drawing File" last_ddinsert_dir "dwg" "Acet:DDins2" 1664))
       )
        (setq path_name ex_name)
        (setq last_ddinsert_dir (car (fnsplitl path_name)))
        (check_fd1)
      )
      ((= 1 bit)
        (setq path_name (ai_strtrim (get_tile "path_name")))
        (setq last_ddinsert_dir "")
        (if (not (= path_name ""))
            (setq last_ddinsert_dir (car (fnsplitl path_name)))
        )
        (check_fd1)
      )
    )
  )
 
  (defun check_fd1( / tname)
    (cond
     ( (findfile path_name)           ; check to see if it exists
        (set_tile "path_name" path_name)
        (parse_path)
        (set_tile "current_name" blk_name)
        (check_current)
     )
     ( (findfile (setq tname (strcat path_name ".dwg")))  ; check to see if it exists
        (set_tile "path_name" tname)
        (parse_path)
        (set_tile "current_name" blk_name)
        (check_current)
     )
 
     ((and (= "" path_name)              ; OK to have a null pathname if the
           (member blk_name table_list)  ; Block name is valid
      )
     )
     (t (set_tile "error" "Invalid File name."))
    )
  )
  ;;
  ;;  Find dwg name from path name.
  ;;
  (defun parse_path( / a b c)
    (setq a 1)
    (while ( <= a (strlen path_name))
      (if (is_lead_byte(ascii (substr path_name a 1)))
        (progn
          (setq a (1+ a))
        )
        (progn
          (if (member (substr path_name a 1) '("/" "\\" ":"))
            (setq b a)
          )
          (if (member (substr path_name a 1) '("."))
            (setq c a)
          )
        )
      )
      (setq a (1+ a))
    )
    ;; Remove path
    (if b
      (setq blk_name (xstrcase (substr path_name (1+ b))))
      (setq blk_name (xstrcase path_name))
    )
    ;; Remove extension (the last period and the letters following it).
    (if (and c blk_name path_name)
      (progn
        ;; calculate c with respect to the blk_name rather than path_name.
        (setq c (- c (- (strlen path_name) (strlen blk_name))))
        (setq blk_name (xstrcase (substr blk_name 1 (- c 1))))
      )
    )
  )
  ;;
  ;; Enable/Disable for Insertion Point.
  ;;
  (defun on_screen_tog()
    (cond
      ((= 1 on_screen)
        (mode_tile "x_pt" 1)
        (mode_tile "y_pt" 1)
        (mode_tile "z_pt" 1)
        (mode_tile "x_scale" 1)
        (mode_tile "y_scale" 1)
        (mode_tile "z_scale" 1)
        (mode_tile "rotation" 1)
      )
      ((and (= 0 on_screen)(= 0 explode))
        (mode_tile "x_pt" 0)
        (mode_tile "y_pt" 0)
        (mode_tile "z_pt" 0)
        (mode_tile "x_scale" 0)
        (mode_tile "y_scale" 0)
        (mode_tile "z_scale" 0)
        (mode_tile "rotation" 0)
      )
      ((and (= 0 on_screen)(= 1 explode))
        (mode_tile "x_pt" 0)
        (mode_tile "y_pt" 0)
        (mode_tile "z_pt" 0)
        (mode_tile "x_scale" 0)
        (mode_tile "y_scale" 1)
        (mode_tile "z_scale" 1)
        (mode_tile "rotation" 0)
      )
    )
  )
  ;;
  ;; Displays a nested dialogue containing an edit box for wildcards and
  ;; a list box of the defined blocks in the drawing.
  ;;
  (defun list_blocks()
    (setq bl_match '())
    (if (not (new_dialog "list_blocks" dcl_id)) (exit))
    (if (not pat) (setq pat "*"))
    (set_tile "pattern" pat)
    (pat_match pat)
 
    (action_tile "bl_match"   "(bl_name)")
    (action_tile "pattern"    "(pat_match (setq pat (xstrcase $value)))")
    (action_tile "selection"  "(do_selection)")
    (action_tile "accept"     "(if (check_i)(done_dialog 1))")
    (action_tile "cancel"     "(setq blk_name1 nil)(done_dialog 0)")
 
    (start_dialog)
  )
  ;;
  ;; If a name is typed, check to see if  block with that name exists in the
  ;; drawing.
  ;;
  (defun do_selection()
    (set_tile "bl_match" "")
    (setq blk_name1 (xstrcase (get_tile "selection")))
    (check_i)
  )
  ;;
  ;; Display the selected block name in the edit box.
  ;;
  (defun bl_name()
    (set_tile "error" "")
    (set_tile "selection" (setq blk_name1 (nth (atoi $value) bl_match)))
  )
  ;;
  ;; Confirms that a block with the entered name exists in the drawing.
  ;;
  (defun check_i()
    (if (member blk_name1 table_list)
      (progn
        (set_tile "error" "")
        T
      )
      (progn
        (set_tile "error" "Block name does not exist.")
;;;     (mode_tile "selection" 2)  Allow 'em to re-select from listbox!
        nil
      )
    )
  )
  ;;
  ;; This function displays the block list based on the pattern.
  ;;
  (defun pat_match (pat)
    (setq bl_match '())
    (foreach n table_list
      (if (wcmatch n pat)
        (setq bl_match (cons n bl_match))
      )
    )
    (if (>= (getvar "maxsort") (length bl_match)) ; Alphabetise if greater
      (if bl_match (setq bl_match (acad_strlsort bl_match))) ; than maxsort.
    )
    (start_list "bl_match")
    (mapcar 'add_list bl_match)
    (end_list)
  )
  ;;
  ;;  This function checks the validity of the Block name.  If legitimate, the
  ;;  Block name is returned, nil otherwise.
  ;;
  (defun check_name(name)
    (if (not (or (not name)
                 (= "" name)
                 (not (snvalid name))
             )
        )
      name
    )
  )
  ;;
  ;; Post a message, when focus is changed from new name, stating that a block
  ;; already exists with this name which will be redefined.
  ;;
  (defun path_name_exist1()
    (if (member n_name table_list)
      (set_tile "error" "A block with this name will be redefined.")
    )
  )
  ;;
  ;;  An Alert dialogue, called on OK to get confirmation of redefining block.
  ;;  Return T if redefine and nil if Cancel.
  ;;
  (defun blk_exists()
    (if (not (new_dialog "blk_exists" dcl_id)) (exit))
    (action_tile "redefine" "(done_dialog 2)")
    (action_tile "cancel" "(done_dialog 0)")
    (if (= (start_dialog) 2)  T (setq redefine nil))
  )
 
  ;;
  ;; Update the Y and Z scale when X scale is changed.
  ;;
  (defun up_xscale(/ x_temp)
    (reset)
    (if (= 1 explode)
      (progn
        (setq range 6)                 ; non zero & non negative
        (setq error_scale "X scale must be positive & nonzero.")
      )
      (progn
        (setq range 2)                 ; non zero
        (setq error_scale "X scale must be nonzero.")
      )
    )
    (if (setq x_temp (ai_num (get_tile "x_scale") error_scale range))
      (progn
        (set_tile "y_scale" (rtos x_temp))
        (set_tile "z_scale" (rtos (abs x_temp)))
      )
    )
  )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;set the insert type value
;(defun set_type ( key / )
;
;(set_tile key "1")
; (cond
;  ((equal key "htype1")
;   (mode_tile "int_blocks" 0)
;   (mode_tile "current_name" 0)
;   (setq #ihatch_type "Block")
;  )
;  ((equal key "htype2")
;   (mode_tile "int_blocks" 1)
;   (mode_tile "current_name" 1)
;   (setq #ihatch_type "Xref")
;  )
;  ((equal key "htype3")
;   (mode_tile "int_blocks" 1)
;   (mode_tile "current_name" 1)
;   (setq #ihatch_type "Xref")
;  )
;);cond close
;
;);defun set_type
 
  ;;
  ;; Put up the dialogue.
  ;;
  (defun ddinsert_main()
 
    (if (not (new_dialog "ddins2" dcl_id)) (exit))
 
    ;; Find the defined blocks in the drawing.
    (setq table_list (ai_table "block" 14)) ; no anonymous, Xrefs or
                                            ; Xref dependents.
    (setq table_list (mapcar 'xstrcase table_list))
 
    ;; Set up some defaults.
    (setq x_pt 0.0)                     (set_tile "x_pt" (rtos x_pt))
    (setq y_pt 0.0)                     (set_tile "y_pt" (rtos y_pt))
    (setq z_pt (getvar "elevation"))    (set_tile "z_pt" (rtos z_pt))
    (setq x_scale 1.0)                  (set_tile "x_scale" (rtos x_scale))
    (setq y_scale 1.0)                  (set_tile "y_scale" (rtos y_scale))
    (setq z_scale 1.0)                  (set_tile "z_scale" (rtos z_scale))
    (setq rotation 0.0)                 (set_tile "rotation" (angtos rotation))
 
    ;; initialize our directory memory
    (if (= last_ddinsert_dir nil)
        (setq last_ddinsert_dir "")
    )
 
    ;; If a default exists for the on screen toggle, use it.  Else set the
    ;; toggle to 1.
    (if (setq on_screen (cadr (assoc "ddinsert" ai_defaults)))
        (set_tile "on_screen" (itoa on_screen))
        (set_tile "on_screen" (itoa (setq on_screen 1)))
    )
 
    (on_screen_tog)
 
    ;(set_tile "explode" "0")
    (setq explode 0)
    ;; If the last insert was of a *block (explode on), then insname
    ;; will have a * in front of the block name.  If the blk_name
    ;; exists within the drawing, then put the name in the block field
    ;; and leave the File field empty, else put the path in the file field
    ;; and the block name in the block field.  In both cases, leave explode off.
    (setq ins_var (getvar "insname"))
    (if (= "*" (substr ins_var 1 1))
      (setq path_name (substr ins_var 2))
      (setq path_name ins_var)
    )
    (parse_path)
    (set_tile "current_name" blk_name)
    (if (member blk_name table_list)
      (set_tile "path_name" (setq path_name ""))
      (set_tile "path_name" path_name)
    )
 
 
    (action_tile "int_blocks" "(reset)(int_blocks)")
    (action_tile "ext_blocks" "(reset)(check_fd 0)")
    (action_tile "current_name" "(reset)(check_current)")
    (action_tile "path_name" "(reset)(check_fd 1)")
    (action_tile "on_screen" "(reset)(setq on_screen (atoi $value))(on_screen_tog)")
    (setq cmd_coor (strcat "(reset)(ai_num $value \""
                           "Invalid X coordinate."
             "\" 0)"))
    (action_tile "x_pt" cmd_coor)
    (setq cmd_coor (strcat "(reset)(ai_num $value \""
                          "Invalid Y coordinate."
             "\" 0)"))
    (action_tile "y_pt" cmd_coor)
    (setq cmd_coor (strcat "(reset)(ai_num $value \""
                           "Invalid Z coordinate."
             "\" 0)"))
    (action_tile "z_pt" cmd_coor)
    (action_tile "x_scale" "(up_xscale)")
    (setq cmd_scale (strcat "(reset)(ai_num $value \""
         "Y scale must be nonzero."
         "\" 2)"))
    (action_tile "y_scale" cmd_scale)
 
    (setq cmd_scale (strcat "(reset)(ai_num $value\""
                     "Z scale must be nonzero."
                     "\" 2)"))
    (action_tile "z_scale" cmd_scale)
 
    (setq cmd_scale (strcat "(reset)(ai_angle $value \""
                     "Invalid Rotation angle."
                     "\")"))
    (action_tile "rotation" cmd_scale)
 
    ;(action_tile "explode" "(setq explode (atoi $value))(on_screen_tog)")
 
 
    (action_tile "accept" "(check_input)")
    (action_tile "cancel" "(done_dialog 0)")
    (action_tile "help" "(acet-help \"superhatch\")")
 
    (setq what_next (start_dialog))
 
    (if (= 1 what_next)
        (progn
         (do_insert)
         (if (assoc "ddinsert" ai_defaults)
             (setq ai_defaults (subst (list "ddinsert" on_screen)
                                      (assoc "ddinsert" ai_defaults)
                                      ai_defaults
                               )
             );setq
             (setq ai_defaults (cons (list "ddinsert" on_screen) ai_defaults));setq
         );if
        );progn
    );if
  );defun ddinsert_main
 
  ;; Set up error function.
  ;(setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
  ;      old_error  *error*            ; save current error function
  ;      *error* ai_error              ; new error function
  ;)
 
  ;(setvar "cmdecho" 0)
 
  (cond
     (  (not (ai_notrans)))                       ; transparent not OK
     (  (not (ai_acadapp)))                       ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "ddins2"))))  ; is .DCL file loaded?
 
     (t (ddinsert_main))                          ; proceed!
  )
 
  ;(setq *error* old_error)
  ;(setvar "cmdecho" old_cmd)
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgB6vllofrgGZ9B+4Zgn5QJPNbtWZDtoGQ+4OpVC
;;; hFa9Ggz2D5boiOR3Dpx/9tgaTEOaPEpx0Vw+q/k8FyvpeS+LLrHagicCaUdyGSC+
;;; BhcXS6GDgXMZcETdo5h6Fd8t3Oxtb8EundVCf5P4+znYeoXoS9gFJNxU7+EPrEvq
;;; vBlA5lkIH5lc3gqLd10RScwVAM8m0PObIZ8XLAuVhNamLpFlPXr+mEJRg0QEnMkf
;;; GszPQ5HXcSVnucKqrSHPGSR2VaXxPCQaKwTDbW3gOQVi0eoyjAOTppi101t4Ln7g
;;; KJtG08J1fcLXC8TVMPpkRhvckTij9gupBxufhyN7Q8yB7+f6GvBinEFEwHFrUBUF
;;; hRQRSw1oyb5qNgv/DTNqeoetjOHeFhVhgqspqUJC+j5yz95PElwvXZ9yzHkjhJXe
;;; jIHy8PdOUhC2J42WZBNkNS6sh8gKV4dC2puPYdHjzkhBFIU7/SrM6rAGYgNw1wXt
;;; yqBxH5fiH+OSUCWAWdaPe4GpJdXpLlvS1WvUeczc1G+SBtekDQhKurqT8dpkb9kA
;;; 7MojCP23e9PwDx4+eOUV7nkbDh1G+hwFtUZ0SYZenOchn5FWsiksdfBL+o+MdAet
;;; 2DSKcmamFpQpChyZKgddqovOsXk5D7QEg3cwHxUm50jaT87I4YSgeZOeLJlX1Vy9
;;; gCYSaA==
;;; -----END-SIGNATURE-----