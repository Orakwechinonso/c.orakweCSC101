;;
;;  tblname.lsp - table name functions
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
;;  Use, duplication, or disclosure by the U.S. Government is subject to
;;  restrictions set forth in FAR 52.227-19 (Commercial Computer
;;  Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;  (Rights in Technical Data and Computer Software), as applicable.
;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;acet-ui-table-name-get - 
;Prompts for a name of a symbol table record with a dialog or at the command line.
; i.e layer name block name ...etc.
;Takes a list containing of a variable number of arguments...
;
; title   - title for dialog/command prompt
;
; default - default value
;
; tblname - name of symbol table (i.e. layer block ...etc.)
;
; flags   - Sum of the following.
;           0 - must select existing name
;           1 - allow entry of a new name
;           2 - dis-allow externally referenced records (apply only to symbol table names)
;           4 - Allow only items that match the ssget style filter provided in flt
;               Note: If this flag is set then flag 1 will be ignored and users will
;                     not be allowed to enter a new or non existant name.
;           8 - dis-allow object selection option (Pick button or "=" at command line).
;          16 - allow "bylayer" and "byblock" entries (where applicable).
;               Note: This flag applies only when used with one of the following tables:
;                     "ltype" "acad_plotstylename"
;          32 - Multiple select flag.
;               Notes: Should selection be allowed? 
;                      Should the edit box be enabled?
;                      Default value will be ignored when this flag is set.
;
; flt - a list of sub-lists that contain ssget-style filters.
;       Each sub-list is in the form: (filterlist "message") ...
;       where 'filterlist' is an ssget filter and 'message' is what gets
;       displayed if the user selects an item that does not match the coorisponding
;       filter.
;
; helpfile  - optional help file
;
; helptopic - string specifies help topic to display when help button on dialog is pressed.
;
(defun acet-ui-table-name-get ( alst / a )
 
 (acet-arxload-or-bust "acetutil.arx")
 (acet-arxload-or-bust "acapp.arx")
 ;(acet-autoload '("acetflt.lsp" "(bns_tbl_match tblname flt)"))
 ;(acet-autoload '("acetflt.lsp" "(bns_filter_match elist flt)"))
 
 
 (if (or (= 0 (getvar "cmddia"))
         (= 4 (logand 4 (getvar "cmdactive")))
     );or
     (setq a (acet-ui-table-name-get-cmd alst))
     (setq a (acet-ui-table-name-get-dlg alst))
 );if
 a
);defun acet-ui-table-name-get
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-ui-table-name-get-dlg ( alst / title def tblname flags flt helpfile helptopic
                                           selected-names list-pick check-name check-names
                                           iv n lst a b names msg lst2 ans flag helpstr
                                  )
 
 ;---------------------Local function-------------------
 (defun selected-names ( lst / nlst n names )
   (setq nlst (get_tile "name_list")
         nlst (acet-str-to-list " " nlst)
         nlst (mapcar 'atoi nlst)
   );setq
   (foreach n nlst
    (setq names (cons (nth n lst) names));setq
   );foreach
   (setq names (reverse names))
 );defun selected-names
 
 
 ;---------------------Local function-------------------
 (defun list-pick ( val reason lst flags / names )
  (set_tile "error" "")
  (setq names (selected-names lst))
  (if (/= 32 (logand 32 flags))
      (progn						;; single select
       (set_tile "name" (car names))
       (if (equal reason 4)		;; double click
           (done_dialog 1) 		;;  then dismiss the dialog
       );if
      );progn then single select
  );if
  names
 );defun list-pick
 
 ;----------------------Local function------------------
 (defun check-names ( tblname flags flt lst lst2 / a names )
  (set_tile "error" "")
  (if (/= 32 (logand 32 flags))
      (progn
       (if (setq a (check-name tblname flags flt lst))
           (setq names (list a))
       );if
      );progn then single select
      (setq names (selected-names lst2));else multiple select so just return selected names
  );if
  (if names
      (done_dialog 1)
  );if
  names
 );defun check-names
 
 ;----------------------Local function------------------
 (defun check-name ( tblname flags flt lst / name a b ans )
  (setq name (get_tile "name")
        name (acet-str-lr-trim " " name)  ;strip leading and trailing spaces
        name (acet-str-lr-trim "\"" name)
        name (acet-str-lr-trim " " name)
  );setq
  (setq       a (acet-ui-table-name-get-is-valid name tblname flags flt lst)
              b (cadr a)
              a (car a)
        tblname (strcase (acet-ui-table-name-format tblname) T)
  );setq
  (cond 
   ((not a)
    (set_tile "error" b)
    (setq name nil)
   );cond #1
   ((and (not (assoc (xstrcase name) lst))
         (setq ans (acet-ui-message 
                      (acet-str-format "The %1 '%2' does not exist. \nDo you want to create it?"
                                       tblname
                                       name
                      )
                      (acet-str-format "Create %1?" tblname)
                      acet:yesnocancel
                   )
         )
         (/= acet:idyes ans) ;they said no
    );and
    (setq name nil)
   );cond #2
  );cond close
 
  name
 );defun check-name
 
 
 ;; Extract the individual arguments from the single argument list provided.
 (setq lst '( title def tblname flags flt helpfile helptopic ))
 (setq n 0)
 (repeat (min (length lst) (length alst))
  (set (nth n lst) (nth n alst))
 (setq n (+ n 1));setq
 );repeat
 (cond
  ((and (not helpfile)
        (not helptopic)
   );and
   (setq helpstr "(acet-help)")
  );cond #1
  ((and helpfile
        helptopic
   );and
   (setq helpstr (acet-str-format "(acet-help \"%1\")" helptopic))
  );cond #2
  (helptopic
   (setq helpstr (acet-str-format "(acet-help \"%1\")" helptopic))
  );cond #3
 );cond close
 
 ;; check the flags value and resolve any conflicts
 (setq flags (acet-ui-table-name-check-flags tblname flags flt)
         flt (cadr flags)
       flags (car flags)
 );setq
 
 ;; get a list of existing valid entries
 (setq  lst (acet-ui-table-name-get-item-list tblname flags flt)
       lst2 (mapcar '(lambda (x) 
                      (cdr (assoc 2 (cadr x)))
                     ) 
                     lst
            );mapcar a list of names expressed in their in original case
 );setq
 (if lst2
     (setq lst2 (acad_strlsort lst2));setq
     (princ "\nNo valid items found.")
 );if
 
 ;; Do some error checking
 ;;  if the default is invalid then set it to the first one in the valid list.
 (if (or (= 32 (logand 32 flags)) ;multiple select
         (not def)
         (not (car (acet-ui-table-name-get-is-valid def tblname flags flt lst)))
     );or
     (setq def (car lst2));setq then
 );if
 
 
(while (or (not flag)
           (and (= flag 2)
                (not names)
           );and
       );or
 
 ;;; now it's time to get busy with the dialog
 (if (> (setq iv (load_dialog (getfileET "tblname.dcl")));setq
        0
     );test
     (progn
      (if (or (and (/= 32 (logand 32 flags))	;; single select
                   (new_dialog "tblname" iv)
              );and
              (and (= 32 (logand 32 flags))	;; multiple select
                   (new_dialog "tblname_m" iv)
              );and
          );or
          (progn
            (if helptopic
                (set_tile "dlgTitle" (xstrcase helptopic))
            );if
            (set_tile "msg" title)
            (acet-dcl-list-make "name_list" lst2)
            (if def
                (set_tile "name" def)
                (set_tile "name" "")
            );if
            (if def 
                (set_tile "name_list"
                          (itoa (vl-position (cdr (assoc 2
                                                         (cadr (assoc (xstrcase def) lst))
                                             )    );cdr assoc
                                             lst2
                                )
                          );itoa
                );set_tile
            );if
 
            (if (= 8 (logand 8 flags))
                (mode_tile "pickit" 1) ;; then disable the pickit button
            );if
 
            (if (/= 32 (logand 32 flags))
                (mode_tile "name" 2) ;; single select so set focus to the editbox
            );if
 
            (action_tile "name_list" "(setq names (list-pick $value $reason lst2 flags))")
            (action_tile "name"
                         (strcat
                          "(if (equal $reason 1)"
                               "(setq names (check-names tblname flags flt lst lst2))"
                          ")"
                         );strcat
            );action_tile
            (action_tile "pickit" "(done_dialog 2)")
           
            (action_tile "accept" "(setq names (check-names tblname flags flt lst lst2))")
            (action_tile "cancel" "(done_dialog 0)")
 
            (action_tile "help" helpstr)
 
            (setq flag (start_dialog));setq
 
          );progn then initialize the tiles and activate the dialog box
          (progn
           (alert "Unable to display dialog box")
           (setq flag 0)
          );progn
      );if new dialog
      (unload_dialog iv);unload it when done
     );progn then
     (progn
      (alert "Unable to load dialog box")
      (setq flag 0)
     );progn else get out of the loop
 );if load
 
 (if (= flag 2)
     (setq names (acet-ui-table-name-get-pickit alst))
 );if
 
);while
(if (= flag 0)
    (setq names nil);setq then
);if
(if (/= 32 (logand 32 flags))
    (setq names (car names));then single select so return a single string.
);if
names
);defun acet-ui-table-name-get-dlg
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-ui-table-name-get-cmd ( alst / title def tblname flags flt 
                                           n lst a names name msg ans opt tblname2 spc
                                  )
 
 ;;; Extract the individual arguments from the single argument list provided.
 (setq lst '( title def tblname flags flt ))
 (setq n 0)
 (repeat (min (length lst) (length alst))
  (set (nth n lst) (nth n alst))
 (setq n (+ n 1));setq
 );repeat
 
 (setq flags (acet-ui-table-name-check-flags tblname flags flt)
         flt (cadr flags)
       flags (car flags)
 );setq
 
 ;;;get a list of existing valid entries
 (setq lst (acet-ui-table-name-get-item-list tblname flags flt))
 
 ;;; Do some error checking
 ;;; if the multi-select or if default is invalid then set it to nil.
 (if (or (= 32 (logand 32 flags))
         (and def
              (not (car (acet-ui-table-name-get-is-valid def tblname flags flt lst)))
         );and
     );or
     (setq def nil) ;then override the default and set it to nil.
 );if
 
 ;;;format the prompt
 (if (= 8 (logand 8 flags))
     (setq opt " or [?]") 			;; dis-allow selection
     (setq opt " or [?/= (select object)]") 
 );if
 (if def
     (setq msg (strcat "\n" title opt " <" def ">: "))
     (setq msg (strcat "\n" title opt ": "))
 );if
 (setq tblname2 (acet-ui-table-name-format tblname)
       tblname2 (strcase tblname2 T)
 );setq
 
 (while (not names)
 
  (if (= 32 (logand 32 flags))
      (progn					;; then multiple select
       (if (= (getvar "extnames") 1)
           (setq spc T)
           (setq spc nil)
       );if
       (setq names (acet-ui-m-get-names 
                        (list spc							;; allow spaces flag
                              msg 							;; prompt
                              (mapcar 'car lst) 					;; list of valid names
                              (acet-str-format "No matching %1(s) found." tblname2)	;; fail msg
                        );list
                  )
       );setq
       (if (not names)
           (setq names (list "")) ;jump out of the while loop
       );if
      );progn then multiple select enabled
      (progn					;; else single select
       (if (= (getvar "extnames") 1)
           (setq name (acet-ui-get-long-name msg))
           (setq name (acet-str-lr-trim "\"" (getstring msg)))
       );if
       (setq names (list name))
      );progn else
  );if
  (foreach name names
   (if (and (/= 32 (logand 32 flags))
            (= name "")
            def
       );and
       (setq names (subst def name names)
              name def
       );setq then used default
   );if
   (cond
    ((or (not name) (equal name ""));or
      nil
    );cond #1 then don't do anything
 
    ((or (= name "=")
         (= name "`")
     );or
     (if (/= 8 (logand 8 flags))
         (setq name (acet-ui-table-name-get-pickit alst));setq then
         (progn
          (princ "\nObject selection not allowed.")
          (setq  name nil
                names nil
          );setq
         );progn else
     );if
     (if name
         (setq names (append names name))
     );if
     (setq names (vl-remove "=" names))
    );cond #2 ask them to select an object to obtain the property
   
    ((= name "~")
     (setq name (acet-ui-table-name-get-dlg alst))
     (if name
         (setq names (append names (list name)));setq then
     );if
     (setq names (vl-remove "~" names))
    );cond #3 display the dialog
  
    ((equal name "?")
     (setq name nil)
     (acet-ui-table-name-get-cmd-list
            (acet-str-format "\nEnter %1 name(s) to list <*>: " tblname2)
            lst
     ); list the valid items
     (setq names (vl-remove "?" names))
    );cond #3 
    
    ((and (setq a (acet-ui-table-name-get-is-valid name tblname flags flt lst))
          (car a)
     );and
        (if (and (not (assoc (xstrcase name) lst))
                 (progn
                   (initget "Yes No _Yes _No")
                   (setq ans (getkword 
                                (acet-str-format 
                                  "\nThe %1 '%2' does not exist. Do you want to create it? <Y>:"
                                  tblname2
                                  name
                                )
                             );getkword
                   );setq
                   (or (= ans "No")
                       (= ans "_No")
                   );or
                 );progn
             );and
             (setq  name nil
                   names nil
             );then name does not exist and the user doesn't want to create it
        );if
    );cond #4
 
    ((cadr a)
     (princ (strcat "\n" (cadr a))) ;the print what the problem was
     (setq  name nil
           names nil
     );setq
    );cond #5
   );cond close
 
  );foreach name
 
 );while not names
 
 (setq names (acet-list-remove-duplicates names nil))
 (if (equal names (list ""))
     (setq names nil)
 );if
 (if (/= 32 (logand 32 flags))
     (progn
      (setq names (car names));then single select so return a single string.
      (if (not names)
          (setq names def)
      );if
     );progn then
 );if
 
 names
);defun acet-ui-table-name-get-cmd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This function will prompt with "select objects:" and will extract the needed table 
;information from the selected object. Returns nil if nothing is selected.
;
(defun acet-ui-table-name-get-pickit ( alst / tlst 
                                              title def tblname flags flt lst
                                              ss n na e1 name lst2 x flt2 gcode flag 
                                              tblname2 namelst
                                     )
 
 ;;; Extract the individual arguments from the single argument list provided.
 (setq tlst '( title def tblname flags flt lst ))
 (setq n 0)
 (repeat (min (length tlst) (length alst))
  (set (nth n tlst) (nth n alst))
 (setq n (+ n 1));setq
 );repeat
 
 (setq flags (acet-ui-table-name-check-flags tblname flags flt)
         flt (cadr flags)
       flags (car flags)
 );setq
 
 (if (not lst)
     ;;;get a list of existing valid entries
     (setq lst (acet-ui-table-name-get-item-list tblname flags flt));setq
 );if
 
 (cond
  ((acet-str-equal "block" tblname)
   (setq   gcode 2
         flt2 '((0 . "INSERT")) 
   )
  );cond #1
  ((acet-str-equal "dimstyle" tblname)
   (setq   gcode 3
         flt2 '((-4 . "<OR") (0 . "DIM*") (0 . "LEADER") (-4 . "OR>")) 
   )
  );cond #2
  ((acet-str-equal "style" tblname)
   (setq   gcode 7
         flt2 '((-4 . "<OR") (0 . "*TEXT") (0 . "ATTDEF") (-4 . "OR>")) 
   )
  );cond #3
  ((acet-str-equal "layer" tblname)
   (setq   gcode 8
         flt2 '((8 . "*"))
   );setq
  );cond #4
  ((acet-str-equal "ltype" tblname)
   (setq   gcode 6
         flt2 '((6 . "*"))
   );setq
  );cond #5
 
  ((acet-str-equal tblname "ACAD_GROUP")
   (setq   gcode 330
         flt2 '((0 . "*"))
   );setq
  );cond #6
  ((acet-str-equal tblname "ACAD_PLOTSTYLENAME")
   (setq   gcode 390
         flt2 '((0 . "*"))
   );setq
  );cond #7
  ((acet-str-equal tblname "ACAD_MLINESTYLE")
   (setq   gcode 2
         flt2 '((0 . "MLINE"))
   );setq
  );cond #8
  ((acet-str-equal tblname "ACAD_PLOTSETTINGS")
   (setq   gcode 330
         flt2 '((0 . "*"))
         flag T
   );setq
   (princ "\nCannot select an object to get PageSetup information.")
  );cond #9
  ((acet-str-equal tblname "ACAD_LAYOUT")
   (setq   gcode 410
         flt2 '((0 . "*"))
   );setq
  );cond #10
  (T
   (setq flag T)
   (print tblname)
   (princ "\nUnknown table type in acet-ui-table-name-get.")
  );cond #11
 );cond close
 
 (setq tblname2 (strcase (acet-ui-table-name-format tblname) T))
 (if (= 32 (logand 32 flags))
     (princ (acet-str-format "\nSelect object(s) with the desired %1 name..." tblname2)) ;; multiple select
     (princ (acet-str-format "\nSelect an object with the desired %1 name..." tblname2)) ;; single select
 );if
 (while (and (not flag)
             ;(setq na (acet-ui-single-select flt2 T))
             (setq ss (acet-ui-entsel 
                       (list "\nSelect objects: "	;; prompt
                             nil			;; initget flags
                             nil			;; initget key words
                             T				;; Allow implied windowing
                             flt2			;; ssget filter
                             T				;; locked layer selection is ok
                       );list
                      );acet-ui-entsel
             );setq
        );and
  (setq n 0)
  (repeat (sslength ss)
   (setq na (ssname ss n));setq
   (if (and na
            (setq e1 (entget na))
       );and
       (progn
        (setq name (acet-ui-table-name-object-data tblname e1 gcode))
        (cond
         ((not name)
          (princ (acet-str-format "\nSpecified object does not contain %1 data." tblname2))
         );cond #1
         ((and (/= 16 (logand 16 flags))
               (or (acet-str-equal "BYLAYER" name)
                   (acet-str-equal "BYLAYER" name)
               );or
          );and
          (princ (acet-str-format "\n%1 not allowed." (xstrcase name)))
          (setq name nil)
         );cond #2
         (T
          (setq flag (acet-ui-table-name-get-is-valid name tblname flags flt lst))
          (if (not (car flag))
              (progn
               (princ name)
               (princ "\n")
               (princ (cadr flag))
               (setq flag nil
                     name nil
               )
              );progn then print what's wrong
          );if
         );cond #3
        );cond close
       );progn then
       (setq name nil);setq else
   );if
   (if (= 32 (logand 32 flags))
       (setq flag nil) ;then multiple select so wait until user presses return to exit.
   );if
   (if (and name
            (not (member name namelst))
       );and
       (setq namelst (cons name namelst))
   );if
   (setq n (+ n 1))
  );repeat
 
  (if namelst
      (progn
       (if (= 32 (logand 32 flags))
           (progn
            (princ (acet-str-format "\nSelected %1(s): "
                                    tblname2
                   )
            )
            (foreach x (reverse namelst)
              (princ x) 
              (if (not (equal x (car namelst)))
                  (princ ", ")
              );if
            );foreach
            (princ (chr 8)) (princ (chr 8))
           );progn then multiple select
           (princ (acet-str-format "\nSelected %1 is: %2." tblname2 (car namelst))) ;else single select
       );if
      );progn then
  );if
 
 );while
 (setq namelst (reverse namelst))
 
 namelst
);defun acet-ui-table-name-get-pickit
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Prints to the command line, all valid entries based on a provided list of items and 
;a wild card specifiaction.
;the list provided must be of the form returned by acet-ui-table-name-get-item-list
;
(defun acet-ui-table-name-get-cmd-list ( msg lst / wld flag x )
 (setq wld (getstring T msg))
 (if (equal (acet-str-lr-trim " " wld) "")
     (setq wld "*")
 );if
 (setq lst (mapcar 'car lst)
       lst (acad_strlsort lst)
 );setq
 (foreach x lst
   (if (wcmatch x wld)
       (progn
        (princ (strcat "\n" x))
        (setq flag T)
       );progn
   )
 );foreach
 (if (not flag)
     (princ "\nNo matching names found.")
 );if
);defun acet-ui-table-name-get-cmd-list-valid
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes:
; name - record name
; tblname - table name
; flags - sum of 1,2,4
; flt   - list of filters and corrisponding messages
; lst   - list of valid items  ((name elist) (name elist) (name elist)...)
; and flags and a filter and returns a list of the form:
;
;Returns a list of the form: (validflag "message")
;where validflag is a T/nil flag. 
;If not valid then the message is a description of why it is not valid.
;
(defun acet-ui-table-name-get-is-valid ( name tblname flags flt lst / x msg na e1 ok dict extnames )
 
 
 (setq       x name 
          name (xstrcase name)
       tblname (strcase tblname T)
            na (acet-ui-table-name-get-ename tblname name)
 );setq
 (if (acet-is-symbol-table tblname)
     (setq  extnames (getvar "extnames"));setq then it's a symbol table
     (progn
      (setq extnames 1
             tblname (acet-ui-table-name-format tblname)
             tblname (strcase tblname T)
      );setq
     );progn else assume it's a dictionary entry
 );if
 
 
 (cond
  ((and (not (snvalid name))			;;;invalid symbol table name
   )
     (cond
      ((and (= 1 extnames)
            (> (strlen name) 255)
       );and  
       (setq msg "Invalid %1 name. \nMaximum length allowed is 255 characters.")
      )
      ((and (= 0 (getvar "extnames"))
            (> (strlen name) 255)
       );and  
       (setq msg "Invalid %1 name. \nMaximum length allowed is 31 characters.")
      )
      (T 
       (acet-sysvar-set '("extnames" 1))
       (if (snvalid name)
           (setq msg " Invalid %1 name. (\"EXTNAMES\"=0)") ;then invalid because extnames is off
           (setq msg " Invalid characters in %1 name.\n The following characters are invalid:\n <>\\/\":?*|,=`")
       );if
       (acet-sysvar-restore)
      )
     );cond close
  );cond #1
 
  ((and (not na) 			;; does not exist and new items are NOT allowed.
        (or (/= 1 (logand 1 flags))
            (= 4 (logand 4 flags))
        );or
   );and
   (setq msg "The %1 '%2' was not found.")
  );cond #2
 
  ((and 
        (= 2 (logand 2 flags))        	;; dis-allow xref dependant.
        na
        (or (= 16 (logand 16 (cdr (assoc 70 (entget na)))))	;; xref dependent or
            (and (acet-str-equal "block" tblname)               ;; this actually _is_ an xref
                 (= 4 (logand 4 (cdr (assoc 70 (entget na)))))
            )
        );or
   );and
   (setq msg "Externally dependent %1s are not allowed.")
  );cond #3
 
  ((and na 				;;;does not match filter
        (= 4 (logand 4 flags))
        (not (assoc name lst))
   );and
   ;;find the first filter that does not match use the corrisponding msg
   (setq e1 (entget na))
   (while (bns_filter_match e1 (car (car flt)))
    (setq flt (cdr flt))
   );while
   (setq msg (cadr (car flt)))
  );cond #4
 
  ((or na             			;;;already exist or is at least valid
       (snvalid name)
   );or
   (setq ok T)
  );cond #5
 );cond close
 
 (if msg
     (setq msg (acet-str-format msg (strcase tblname T) x)) ;place the table name (tblname) and the record name (x)
 );if
 
 (list ok msg)
);defun acet-ui-table-name-get-is-valid
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of sub-lists of the form ("name" entlist)
;where "name" is in caps.
;If the allow bylayer/byblockk bit is set (16) then 
; ("BYLAYER" nil) and ("BYBLOCK" nil) will be included in the list.
; 
(defun acet-ui-table-name-get-item-list ( tblname flags flt / tmp lst lst2 x name symtbl )
 
 (foreach x flt
  (setq lst (append (car x) lst))
 );foreach
 (setq flt lst
       tmp flt
 );setq
 (if (= 2 (logand 2 flags)) ; dis-allow xref dependant entries?
     (progn
      (if (acet-str-equal tblname "block")
          (setq flt (append flt
                       '((-4 . "<AND")
                           (-4 . "<NOT") (-4 . "&") (70 . 16) (-4 . "NOT>")	;not xref dependent
                           (-4 . "<NOT") (-4 . "&") (70 . 4) (-4 . "NOT>")	;not an xref
                         (-4 . "AND>")
                        )
                    );append
          );setq then check for xrefs as well as xref dependent
          (setq flt (append flt
                           '((-4 . "<NOT") (-4 . "&") (70 . 16) (-4 . "NOT>"))
                    );append
          );setq then not block 
      );if
     );progn then add the needed filter
 );if
 (cond
  ((not flt)
   (setq flt '((0 . "*")))
  )
  (tmp
   (setq flt (append '((-4 . "<AND")) flt '((-4 . "AND>"))));setq
  )
 );cond close
 
 (if (setq symtbl (acet-is-symbol-table tblname))
     (setq lst (bns_tbl_match tblname flt))
     (setq lst (acet-dict-filter-match tblname flt))
 );if
 
 (if (= 16 (logand 16 flags))
     (setq lst2 (list '("BYLAYER" ((2 . "ByLayer" )))
                      '("BYBLOCK" ((2 . "ByBlock" ))) 
                );list
     );setq then
 );if
 (foreach x lst
  (setq name (cdr (assoc 2 x)))
  (if (or (not symtbl)
          (snvalid name)
      );or
      (setq lst2 (cons (list (xstrcase name) ;name in upper case
                             x                           ;elist
                       );list
                       lst2
                 );cons
      );setq then
  );if
 );foreach
 
 lst2
);defun acet-ui-table-name-get-item-list
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;change the acad_XXXXXX dictionary name to a meaningfull string for the user.
;
(defun acet-ui-table-name-format ( tblname / )
 
 (cond
  ((acet-str-equal tblname "ACAD_GROUP")
   (setq tblname "Group")
  )
  ((acet-str-equal tblname "ACAD_PLOTSTYLENAME")
   (setq tblname "PlotStyle")
  )
  ((acet-str-equal tblname "ACAD_MLINESTYLE")
   (setq tblname "MlineStyle")
  )
  ((acet-str-equal tblname "ACAD_PLOTSETTINGS")
   (setq tblname "PageSetup")
  )
  ((acet-str-equal tblname "ACAD_LAYOUT")
   (setq tblname "Layout")
  )
  ((acet-str-equal tblname "LTYPE")
   (setq tblname "LineType")
  )
 );cond close
 
 tblname
);defun acet-ui-table-name-format
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-ui-table-name-get-ename ( tblname name / na )
 (if (acet-is-symbol-table tblname)
     (setq na (tblobjname tblname name))
     (setq na (acet-dict-ename tblname name))
 );if
 na
);defun acet-ui-table-name-get-ename
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;returns T is the string provided is a symbol table name
(defun acet-is-symbol-table ( str / lst )
 (setq lst '("APPID" "BLOCK" "LAYER" "LTYPE" "STYLE" "DIMSTYLE" "VIEW" "UCS")) 
 (if (member (xstrcase str) lst)
     T
     nil
 );if
);defun acet-is-symbol-table
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a symbol table name and a filter (ssget style)
;Returns a list of elists from table records that
;match the specified filter.
;
(defun acet-dict-filter-match ( tblname flt / n lst na e1 lst2 )
 
 (acet-autoload '("acetflt.lsp" "(bns_filter_match tblname flt)"))
 
     
 (setq lst (acet-dict-name-list tblname))
 
 (setq n 0)
 (repeat (length lst)
  (setq na (acet-dict-ename tblname (nth n lst))
        e1 (entget na)
  );setq
  (if (bns_filter_match e1 flt)
      (setq   e1 (cons (cons 2 (nth n lst)) e1)
            lst2 (append lst2 (list e1))
      );setq
  );if
  (setq n (+ n 1));setq
 );repeat
 
 lst2
);defun acet-dict-filter-match
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;returns the name of the table referenced by the selected object.
 
(defun acet-ui-table-name-object-data ( tblname e1 gcode / lst lst2 na name )
 
 
 (cond
  ((acet-str-equal tblname "ACAD_GROUP")
   (setq  lst (acet-dict-name-list tblname)
         lst2 (mapcar '(lambda (x) (acet-dict-ename tblname x)) lst)
           e1 (acet-list-m-assoc 330 e1)
           e1 (mapcar 'cdr e1)
   );setq
   (while (setq na (car e1))
     (setq e1 (cdr e1))
     (if (member na lst2)
         (setq name (nth (vl-position na lst2) lst)
                 e1 nil
         );setq then
     );if
   );while
  );cond #1
  ((acet-str-equal tblname "ACAD_PLOTSTYLENAME")
   (if (setq na (cdr (assoc 390 e1)))
       (progn
        (setq  lst (acet-dict-name-list tblname)
              lst2 (mapcar 'acet-dict-ename lst)
        );setq
        (setq name (nth (vl-position na lst2) lst));setq
       );progn then
       (setq name "BYLAYER")
   );if
  );cond #2
  (T
   (if (setq gcode (cdr (assoc gcode e1)))
       (setq name gcode)
       (setq name "BYLAYER")
   );if
  );cond #3
 );cond close
 
 
 name
);defun acet-ui-table-name-object-data
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this functionchecks the flags and filter val;ues.
;Takes the tblname the flags and the filter and returns a list containing 
;the new flags value and filter.
;
(defun acet-ui-table-name-check-flags ( tblname flags flt / )
 
 (if (/= 4 (logand 4 flags))
     (setq flt nil);then ignore the filter because the proper flag is not set
 );if
 (if (and (acet-str-equal tblname "ACAD_PLOTSETTINGS")
          (/= 8 (logand 8 flags))
     );and
     (setq flags (+ flags 8));setq then automatically add the no-pick flag cuz u can't pick a pagesetup
 );if
 (if (and (= 16 (logand 16 flags))
          (not (or (acet-str-equal tblname "ACAD_PLOTSTYLENAME")
                   (acet-str-equal tblname "ltype")
               );or
          );not
     );and
     (setq flags (- flags 16));setq then remove the flag because bylayer/byblock does not apply
 );if
 
 (list flags flt)
);defun acet-ui-table-name-check-flags


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgC/TlBwx3Mednb9aGsgvgeBz0Ld1TQYUh4SRNMQ
;;; lf4IZMK9fOT+bbQvM062uH/5pI8rgWffYauSsP/gngu78jIhIU/W2dFhcw7xiu/u
;;; 3YIXOo+2QgyY5CnHlB4rxb5Y1CgM24dYVPDLRG//7NaV6UHcgWV6SIP1rWVP03Zr
;;; H00OdDip8uMp5PXu/AzGYTq4RbCUraqHO2jdidEU2tWyAPbZsjTifTqg/d7K4c/S
;;; e6Rz8G38IlbOtts/S8RazFD2MUiZMSf7/+GXBdFuw5twLQxzEgMxj93auT+a68UQ
;;; cJlPFTQdLL37DdRU3F6Yx1fC2hhcWK4rnEnjVN6ehnlRoILkBIL5urr3rr62jydK
;;; sL4O/iYH2yPPzGWGWthWcc2SPS5JgRz2M6oCM7FnYxHvGIb5PTVN/kUwL+PyblQa
;;; rnJe9cH7vIKNysivnhZcPVm/uXWQIWGX6d+NadJO/wSn3ecpadDR+OD6Z0UeNXUU
;;; EKme3d4tggi6IOTMrjX8AtEIvVcnQKKyNHgcbLQVqDUZk9aq5XmDWY5lqSlyvg84
;;; IzjxpNT+D8B7mHlT0Bu2zjc+2Qo6NKf5A2jYHxMtVZJ3fyX8O9T76nIj0kjeg0+u
;;; rSPcLwQAXvERDKgQ5Ab+t22NnxtVQXbqaUFxmPm5f7Z3XRrFpovwshXBrROspxZz
;;; jdw7Sg==
;;; -----END-SIGNATURE-----