;;;
;;;    LMAN.LSP
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
 
;;(defun c:lman ( / )
;; 
;;(if (or (equal (getvar "cmddia") 0)
;;        (equal 4 (logand 4 (getvar "cmdactive")))
;;    );or
;;    (c:-lman);the dialogs are off or a script is running.
;;    (bns_dd_lman)
;;);if
;;(princ)
;;);defun c:lman
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_dd_lman ( /  msg str iv lst lst2 flag );
 
(acet-error-init (list
                   (list   "cmdecho" 0
                         "regenmode" 1
                            "expert" 0
                           "ucsicon" 0
                           "filedia" 1
                   )
                   nil     ;flag. True means use undo for error clean up.
                  );list
);acet-error-init
 
(bns_lman_ltypefx)
 
(bns_reg_it)
 
(setq  lst (bns_get_layerstates));setq
(if (and (equal (cadr #last_restore) (bns_get_la_status));equal
         (member (car #last_restore) lst)
    );and
    (setq lst2 (list (acet-str-format "Current layer state: %1" (car #last_restore))
                     (car #last_restore)
                     (itoa (vl-position (car #last_restore) lst))
               );list
    );setq then
    (setq          lst2 (list "Current layer state: *UNNAMED*" "" "")
          #last_restore nil
    );setq
);if
 
(if (not #dlg_pnt)
    (setq #dlg_pnt '(-1 -1))
);if
(while (or (not flag)
           (equal flag 2)
       );or
 
(if (> (setq iv (load_dialog (getfileET "lman.dcl"))) 0)
    (progn
     (setq #iv iv);setq
     (if (new_dialog "lman" iv "" #dlg_pnt);NEW_DIALOG DISPLAYS THE DIALOG
         (progn
          (if lst
              (progn
               (acet-dcl-list-make "list_states" lst);make the pop-up list
               (set_tile "list_states" (caddr lst2))
              );progn then
              (setq lst2 (list (car lst2) (cadr lst2) ""));setq else
          );if
          (set_tile "msg" (car lst2))
          (action_tile "list_states"
                       "(setq lst2 (bns_list_pick lst))"
          );action_tile
          (setq msg "Save current layer status as:"
                str (strcat "(setq lst "
                            "(bns_dlg_sl lst iv "
                            "(if (not (equal (get_tile \"list_states\") \"\")) "
                                 "(bns_new_state lst "
                                   "(nth (atoi (get_tile \"list_states\")) lst) "
                                   "msg "
                                 ")"
                                 "(bns_new_state lst (bns_get_nextname lst) msg)) "
                             ")"
                            "lst2 (cadr lst) "
                            "lst (car lst))"
                    );strcat
          );setq
          (action_tile "saveas" str);action_tile
          (action_tile "edit"
                       "(setq lst (bns_dlg_edit lst iv) lst2 (cadr lst) lst (car lst))"
          );action_tile
          (action_tile "delete"
                       "(setq lst (bns_dlg_dl lst iv) lst2 (cadr lst) lst (car lst))"
          );action_tile
          (action_tile "rename"
                       "(setq lst (bns_dlg_rename lst iv) lst2 (cadr lst) lst (car lst))"
          );action_tile
          (action_tile "import"
                       "(bns_dlg_import iv)"
          );action_tile
          (action_tile "export"
                       "(bns_dlg_export iv)"
          );action_tile
          (action_tile "options"
                       "(bns_get_lmanmode_dd)"
          );action_tile
          (action_tile "restore"
                       "(setq lst (bns_dlg_rl lst iv) lst2 (cadr lst) lst (car lst))"
          );action_tile
          (action_tile "close"
                       "(setq #dlg_pnt (done_dialog 0))"
          );action_tile
          (action_tile "help" "(acet-help \"LMAN\")")
          (mode_tile "restore" 2)
 
          (setq flag (start_dialog));setq
 
         );progn then initialize the tiles and activate the dialog box
         (progn
          (alert "Unable to display dialog box")
          (setq flag 1)
         );progn
     );if new dialog
 
     (setq #iv nil);setq
 
     (if (equal flag 2)
         (progn
          (setq lst (down_dlg_operation lst))
          (if (and (equal (length lst) 2)
                   (or (equal (type (list 1)) (type (car lst)))
                       (not (car lst))
                   );or
                   (equal (type (list 1)) (type (cadr lst)))
              );and
              (setq lst2 (cadr lst)
                     lst (car lst)
              );setq
          );if
         );progn
         (unload_dialog iv);unload it when done
     );if
    );progn then
    (progn
     (alert "Unable to load dialog box")
     (setq flag 1)
    );progn else
);if load
 
);while
 
(cond
 ((equal flag 1)
  (progn
       (bns_rl (nth (atoi (nth 2 lst2))
    lst
     );nth
       );bns_rl
  );progn then
 );cond #1
);cond close
 
(acet-error-restore)
);defun bns_dd_lman
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_dlg_import ( iv / )
  (setq down_dlg_operation (list '(lst / lst2 )
         '(setq lst2 (bns_c_import_lay))
                                 '(list (bns_get_layerstates) lst2);'lst
                           );list
  );setq
  (setq #dlg_pnt (done_dialog 2));setq
  (unload_dialog iv)
 );defun bns_dlg_import
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_dlg_export ( iv / )
  (setq down_dlg_operation (list '(lst / lst2)
                                 '(setq lst2 (bns_c_export_lay))
                                 '(list lst lst2)
                           );list
  );setq
  (setq #dlg_pnt (done_dialog 2))
  (unload_dialog iv)
 );defun bns_dlg_export
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_dlg_sl ( lst iv a / msg b lst2)
 
  (if (and (not #iv)
           (or (not (equal (cadr #last_restore) (bns_get_la_status)
                    );equal
               );not
               #incomplete
           );or
      );and
      (progn
       (setq msg "Save changes as:"
               b a
               a (bns_new_state lst a msg)
       );setq
       (if a
           (setq #incomplete nil)
       );if
      );progn then
      (progn
       (if (not #iv)
           (setq b a
                 a nil
           );setq then
       );if
      );progn else
  );if
  (if a
      (progn
       (if (not (member a lst))
           (setq  lst (append lst (list a))
                  lst (acad_strlsort lst)
                 lst2 (list (acet-str-format "Created: %1" a)
                            a
                            (itoa (vl-position a lst))
                      );list
           );setq then
           (setq lst2 (list (acet-str-format "Redefined: %1" a)
                            a
                            (itoa (vl-position a lst))
                      );list
           );setq else
       );if
       (if (not #iv)
           (progn
            (bns_sl a)
           );progn then
           (progn
            (setq down_dlg_operation (list '(lst / )
                                            (append '(bns_sl) (list a))
                                           'lst
                                     );list
            );setq
            (setq #dlg_pnt (done_dialog 2))
            (unload_dialog iv)
           );progn
       );if
      );progn then
      (progn
       (if (and lst
                #iv
           );and
           (progn
            (setq lst2 (list (acet-str-format "Current layer state: %1"
                                     (nth (atoi (get_tile "list_states")) lst)
                             )
                             (nth (atoi (get_tile "list_states")) lst)
                             (get_tile "list_states")
                       );list
            );setq
           );progn then
           (progn
 
            (if (and (member b lst)
                     (equal (cadr #last_restore) (bns_get_la_status))
                );and
                (setq lst2 (list (acet-str-format "Current layer state: %1" b)
                                 b
                                 (itoa (vl-position b lst))
                           )
                );setq then
                (setq lst2 (list "Current layer state: *UNNAMED*"
                                 nil
                                 ""
                           )
                );setq else
            );if
           );progn else
       );if
       (setq
        down_dlg_operation (list '(lst / )
                                 '(princ)
                                 'lst
                           );list
       );setq
      );progn else
  );if
  (setq lst (list lst lst2))
  lst
 );defun bns_dlg_sl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_dlg_rl ( lst iv /  lst2 a b)
  (if lst
      (progn
       (setq    a (get_tile "list_states")
                b (nth (atoi a) lst)
             lst2 (list (acet-str-format "Restored: %1" b)
                        b
                        a
                  );list
        down_dlg_operation (list '(lst / )
                                 (append '(bns_rl) (list b))
                                'lst
                           );list
       );setq
       (setq #dlg_pnt (done_dialog 2))
       (unload_dialog iv)
      );progn then
      (setq lst2 (list "" nil ""));setq
  );if
  (list lst lst2)
 );defun bns_dlg_rl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_get_nextname (lst / mx n a )
  (setq  n 0
        mx 0
  );setq
  (repeat (length lst)
   (setq a (nth n lst))
   (if (wcmatch a "LAYER_STATE*")
       (setq mx (max mx
                     (atoi (substr a 12))
                );max
       );setq
   );if
   (setq n (+ n 1));setq
  );repeat
  (setq a (strcat "LAYER_STATE" (itoa (+ 1 mx))));setq
  a
 );defun bns_get_nextname
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_dlg_edit ( lst iv / lstate lst2)
  (if (and lst
           (not (equal "" (get_tile "list_states")))
      );and
      (progn
       (setq lstate (nth (atoi (get_tile "list_states")) lst)
             lstate (xstrcase (acet-str-space-trim lstate))
               lst2 (list (acet-str-format "Current layer state: %1" lstate)
                          (get_tile "save_name")
                          (get_tile "list_states")
                    );list
       );setq
       (setq down_dlg_operation (list
                                      '(lst)
                                      (append '(progn)
                                       (list
                                        (append '(bns_rl) (list lstate))
                                        (append '(initdia))
                                        (append '(command) (list "_.layer"))
                                       );list
                                      );append
                                      (append '(bns_dlg_sl) (list 'lst iv lstate))
                                );list
       );setq
      );progn then
      (progn
       (if (member (car #last_restore) lst)
           (setq   lst2 (list (acet-str-format "Current layer state: %1" (car #last_restore))
                              (car #last_restore)
                              (itoa (vl-position (car #last_restore) lst))
                        );list
                 lstate (car #last_restore)
           );setq then
           (setq   lst2 (list "Current layer state: *UNNAMED*" nil "")
                 lstate (bns_get_nextname lst)
           );setq else
       );if
 
       (setq down_dlg_operation (list
                                      '(lst)
                                       (append '(progn)
                                               (list
                                                (append '(initdia))
                                                (append '(command) (list "_.layer"))
                                               );list
                                       );append
                                       (append '(bns_dlg_sl) (list 'lst iv lstate))
                                );list
       );setq
      );progn else
  );if
  (setq #dlg_pnt (done_dialog 2))
  (unload_dialog iv)
 
  (setq lst (list lst lst2))
 );defun bns_dlg_edit
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_dlg_dl ( lst iv / lstate lst2 n)
 
  (if (and lst
           (not (equal ""
                       (setq n (get_tile "list_states"))
                );equal
           );not
           (setq      n (atoi n)
                 lstate (nth n lst)
                 lstate (xstrcase (acet-str-space-trim lstate))
                   lst2 (list (get_tile "msg")
                              lstate
                              (get_tile "list_states")
                        );list
           );setq
           (equal 1 (bns_warning lstate "Are you sure you want to delete"))
      );and
      (progn
       (setq lst2 (list (acet-str-format "Deleted: %1" lstate)));setq
       (setq lst (acet-list-remove-nth n lst));setq
       (if (member (car #last_restore) lst)
           (setq lst2 (list (car lst2)
                            (car #last_restore)
                            (itoa (vl-position (car #last_restore) lst))
                      );list
           );setq then
           (setq lst2 (list (car lst2) nil ""));setq
       );if
       (setq down_dlg_operation (list '(lst)
                                      (append '(bns_dl) (list lstate))
                                      'lst
                                );list
       );setq
       (setq #dlg_pnt (done_dialog 2))
       (unload_dialog iv)
 
      );progn then
      (progn
       (if (member (car #last_restore) lst)
           (setq lst2 (list (acet-str-format "Current layer state: %1" (car #last_restore))
                            (car #last_restore)
                            (itoa (vl-position (car #last_restore) lst))
                      );list
           );setq then
           (setq lst2 (list "" nil ""));setq
       );if
      );progn else
  );if
 
  (setq lst (list lst lst2))
 );defun bns_dlg_dl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_list_pick ( lst / a)
  (setq a (list
              (get_tile "msg")
              (get_tile "save_name")
              (get_tile "list_states")
          );list
  );setq
  (if (equal $REASON 4)
      (setq #dlg_pnt (done_dialog 1))
  );if
 
  a
 );defun bns_list_pick
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_dl ( lstate / lst n)
  (bns_reg_it)
 
  (setq lst (acet-table-name-list "layer"));setq
  (setq n 0);setq
  (repeat (length lst)
   (bns_d_layerstate (nth n lst) lstate)
   ;(spinner)
   (setq n (+ n 1));setq
  );repeat
  (if (acet-str-equal lstate (car #last_restore))
      (setq #last_restore nil)
  );if
 (princ)
 );defun bns_dl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;delete layer state
 (defun bns_d_layerstate ( la lstate / lst lst2 lst3 e1 xd a b)
  (if lstate 
      (setq lstate (xstrcase lstate))
  )
  (setq     e1 (entget (tblobjname "layer" la) '("RAK"))
            xd (cdr (assoc -3 e1))
           lst (cdr (assoc "RAK" xd))
             a (cons 1000 (strcat "{" lstate))
             b (cons 1000 (strcat lstate "}"))
  );setq
  (if (member a lst)
      (progn
       (setq lst2 (reverse (cdr (member a (reverse lst))))
             lst3 (cdr (member b lst))
       );setq
       (setq
             lst (append lst2 lst3)
             lst (append (list "RAK") lst)
              xd (subst lst (assoc "RAK" xd) xd)
              xd (append (list -3) xd)
              e1 (subst xd (assoc -3 e1) e1)
       );setq
       (entmod e1)
      );progn then remove it
  );if
  (princ)
 );defun bns_d_layerstate
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_sl ( lstate / lst lst2 n ss na a vp clayer)
 
  (bns_reg_it)
  (if lstate
      (setq lstate (xstrcase lstate))
  )
  (if (and (equal (getvar "tilemode") 0)
           (setq na (acet-currentviewport-ename))
      );and
      (progn
       (setq lst2 (acet-viewport-frozen-layer-list na)
             lst2 (mapcar 'xstrcase lst2)
       );setq
      );progn then need to add the data to the viewports
  );if
  (setq clayer (xstrcase (getvar "clayer"))
           lst (acet-table-name-list "layer")
           lst (mapcar 'xstrcase lst)
  );setq
  (setq n 0);setq
  (repeat (length lst)
   (setq a (nth n lst));setq
   (if (member a lst2)
       (setq vp a)
       (setq vp nil)
   );if
   (bns_add_layerstate
                   (entget (tblobjname "layer" a) '("RAK"))
                   lstate
                   clayer
                   vp
                   nil
   );bns_add_layerstate
   (setq n (+ n 1));setq
  );repeat
  (setq #last_restore (list lstate (bns_get_la_status))
          #incomplete nil
  );setq
 );defun bns_sl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes
; e1- entity list of layer
; lstate name
; current layer name
; vp - name the layer if it is frozen in the current viewport
; lst5 - full data list if called on import operation.
;
 (defun bns_add_layerstate ( e1 lstate clayer vp lst5 / a b c d xd lst lst2 lst3 lst4)
  (if lstate 
      (setq lstate (xstrcase lstate))
  )
  (setq    a (cons 1000 (strcat "{" lstate))
           d (cons 1000 (strcat lstate "}"))
           c (cdr (assoc 2 e1))                     ;The layer name
          xd (cdr (assoc -3 e1))
         lst (cdr (assoc "RAK" xd))
  );setq
  (if (not lst5)
      (progn
       (setq lst2 (list
                    a                                               ;The layer_state name
                    (cons 1070 (cdr (assoc 70 e1)))                 ;The F/T L/UNlock xref bit
                    (cons 1070 (cdr (assoc 62 e1)))                 ;The color and on/off
                    (cons 1000 (bns_local_ltype (cdr (assoc 6 e1))));The linetype
                  );list
       );setq
       (if (acet-str-equal clayer c)
           (setq lst2 (append lst2 (list '(1070 . 1))
                      );append
           );setq then it's the current layer
       );if
       (if vp
           (setq lst2 (append lst2 (list (cons 1000 vp))));setq then save the vp freeze info
       );if
       (setq lst2 (append lst2
                          (list
                           (cons 1071 (cdr (assoc 370 e1)))                ;The line weight
                           (cons 1071 (cdr (assoc 290 e1)))                ;The Plot visibility
                           (cons 1005 (cdr (assoc 5                        ;The PlotStyle handle
                                                  (entget (cdr (assoc 390 e1)))
                                      )    );assoc
                           );cons
                          );list
                  );append
       );setq
       (setq lst2 (append lst2 (list d)));setq
      );progn then
      (setq lst2 lst5);setq else info from file import
  );if
 
  (if (member a lst)
      (progn
       (setq lst3 (reverse lst));setq
       (while (setq b (member a lst3))
        (setq    b (cdr b)
              lst3 b
        );setq
       );while
       (setq lst3 (reverse lst3));setq
       (setq lst4 lst);setq
       (while (setq b (member d lst4))
        (setq    b (cdr b)
              lst4 b
        );setq
       );while
      );progn then
      (setq lst3 lst);setq
  );if
  (setq lst2 (append (list "RAK") lst3 lst2 lst4));setq
 
  (if lst
      (setq lst2 (subst lst2 (assoc "RAK" xd) xd));setq then some xdata was there and my xdata was too
      (progn
       (if xd
           (setq lst2 (append xd (list lst2)));setq then xdata was there but not mine
           (setq lst2 (list lst2));setq else
       );if
      );progn else
  );if
  (setq lst2 (append (list -3) lst2));setq the new xdata
  (if xd
      (setq e1 (subst lst2 (assoc -3 e1) e1));setq
      (setq e1 (append e1 (list lst2)));setq else
  );if
 
 (entmod e1)
 );defun bns_add_layerstate
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_list_layerstates ( / lst a b n )
  (setq a (getstring "\nEnter layer states to list <*>:"));setq
  (if (equal a "") (setq a "*"));if
  (setq a (xstrcase a));setq
  (if (setq lst (bns_get_layerstates));setq
      (progn
       (setq n 0);setq
       (repeat (length lst)
        (setq b (nth n lst));setq
        (if (wcmatch b a)
            (princ (strcat "\n" b))
        );if
        (setq n (+ n 1));setq
       );repeat
      );progn then
      (princ "\nNo saved layer states found")
  );if
  (princ "\n")
 );defun bns_list_layerstates
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_get_layerstates ( / e1 xd lst lst2 lst3 a b n j)
  (setq lst (acet-table-name-list "layer"))
  (setq b (length lst));setq
  (setq n 0);setq
  (repeat b
  (setq   e1 (entget (tblobjname "layer" (nth n lst)) '("RAK"))
          xd (cdr (assoc -3 e1))
        lst2 (cdr (assoc "RAK" xd))
        ;lst2 (m_assoc 1000 lst2)
  );setq
  (setq j 0);setq
  (repeat (length lst2)
   (setq a (nth j lst2));setq
   (if (or (not (equal (car a) 1000))
           (not (equal "{" (substr (cdr a) 1 1)))
       );or
       (setq a nil)
       (progn
        (setq a (xstrcase (substr (cdr a) 2)))
        (if (not (member a lst3))
            (setq lst3 (append lst3 (list a)));setq then
        );if
       );progn else
   );if
   (setq j (+ j 1));setq
  );repeat
 
  (setq n (+ n 1));setq
  );repeat
 
  (if lst3
      (setq lst3 (acad_strlsort lst3));setq then
  );if
  lst3
 );defun bns_get_layerstates
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;a full list of raw xdata from the layer table
 (defun bns_get_layerstates_all ( / e1 xd lst lst2 b n )
  (setq lst (acet-table-name-list "layer"))
  (setq b (length lst));setq
  (setq n 0);setq
  (repeat b
   (setq   e1 (entget (tblobjname "layer" (nth n lst)) '("RAK"))
           xd (cdr (assoc -3 e1))
         lst2 (append lst2 (list
                                (list (cdr (assoc 2 e1))
                                      (cdr (assoc "RAK" xd))
                                );list
                           );list
              );append
   );setq
   (setq n (+ n 1));setq
  );repeat
  lst2
 );defun bns_get_layerstates_all
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_c_export_lay ( / fna lst2)
  (bns_reg_it)
  (setq fna (ACET-FILE-WRITEDIALOG "Export file name"
                      (strcat (vl-filename-base (getvar "dwgname"))
                              ".lay"
                      )
                      "lay"
                      "Acet:LayExport"
                      1665
            );ACET-FILE-WRITEDIALOG
  );setq
  (if fna
      (progn
       (bns_export fna)
       (setq lst2 (list (acet-str-format "Exported layer state(s) to %1" fna)
                        "" ""
                  );list
       );setq
      );progn
      (setq lst2 (list "" "" ""));setq
  );if
  lst2
 );defun bns_c_export_lay
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_export ( fna / fh lst n a )
  (setq lst (bns_get_layerstates_all)
         fh (open fna "w")
  );setq
  (setq n 0);setq
  (repeat (length lst)
  (setq a (nth n lst));setq
  (bns_write_it a fh)
  (setq n (+ n 1));setq
  );repeat
  (close fh)
 );defun bns_export
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_c_import_lay ( / fna lst2)
  (bns_reg_it)
  (setq fna (acet-ui-getfile "Import file name"
                      (strcat (getvar "dwgprefix")
                              (acet-filename-path-remove (acet-filename-ext-remove (getvar "dwgname")))
                              ".lay"
                      )
                      "lay"
                      "Acet:LayImport"
                      1664
            );acet-ui-getfile
  );setq
  (if fna
      (progn
       (bns_lman_import fna)
       (setq lst2 (list (acet-str-format "Imported layer state(s) from %1" fna)
                        "" ""
                  );list
       );setq
      );progn
      (setq lst2 (list "" "" ""));setq
  );if
  lst2
 );defun bns_c_import_lay
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_lman_import ( fna / lstate fh lst5 a la e1 flag x na )
  (bns_reg_it)
 
  (setq fh (open fna "r"));setq
  (read-line fh)
  (while (setq a (read-line fh));setq
   (setq a (read a));setq
   (if (equal 'STR (type a))
       (progn
         (setq la a);setq
         (if (not (setq na (tblobjname "layer" a)))
             (progn
              (cond
               ((setq x (wcmatch a "*|*"))
                (princ (acet-str-format "\nCannot import xref'd layer: \"%1\"." a))
               )
               ((not (snvalid a))
                (princ (acet-str-format "\nInvalid layer name: %1" a))
               )
               (T
                (command "_.layer" "_new" a "")
                (setq na (tblobjname "layer" a))
               )
              );cond close
             );progn then
         );if
         (if na
             (setq e1 (entget na '("RAK")));setq             
             (setq e1 nil)
         );if
       );progn then it's a layer name
       (progn
        (setq flag nil
              lst5 nil
        );setq
        (while (not flag)
         (setq lst5 (append lst5 (list a)));setq
         (if (and
                  (equal 'LIST (type a))
                  (equal 1000 (car a))
                  (equal "}" (substr (cdr a) (strlen (cdr a)) 1))
             );and
             (progn
              (setq flag T)
              (if e1
                  (progn
                   (setq lstate (substr (cdr a) 1 (- (strlen (cdr a)) 1)));setq
                   (bns_add_layerstate e1 lstate "0" nil lst5)
                   (setq e1 (entget na '("RAK")))
                  );progn valid layer
              );if
             );progn then
             (progn
              (if (not (setq a (read-line fh)))
                  (setq flag T);setq
                  (setq a (read a));setq
              );if
             );progn
         );if
        );while
       );progn else it's layer state data
   );if
  );while
  (close fh)
 );defun bns_lman_import
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_write_it ( lst fh / n a b)
  (setq   a (car lst)
        lst (cadr lst)
  );setq
  (print a fh)
  (setq n 0);setq
  (repeat (length lst)
   (setq b (nth n lst));setq
   (print b fh)
   (setq n (+ n 1));setq
  );repeat
 );defun bns_write_it
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_get_la_status ( / lst lst2 n a na)
  (setq lst (acet-table-name-list "layer"))
  (setq n 0)
  (repeat (length lst)
   (setq a (entget (tblobjname "layer" (nth n lst))))
   (setq lst2 (cons a lst2))
  (setq n (+ n 1));setq
  );repeat
  (setq  lst (reverse lst2)
        lst2 nil
  );setq
  (list (getvar "clayer")
        lst
        (if (and (equal (getvar "tilemode") 0)
                 (setq na (acet-currentviewport-ename))
            );and
            (list a (acet-viewport-frozen-layer-list na))
            nil
        );if
  );list
 );defun bns_get_la_status
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_rl ( lstate / na ss lst lst2 lst3 lst4 a b n clayer bitflag missinglt)
 
  (bns_reg_it)
  (if lstate
      (setq lstate (xstrcase lstate))
  )
  (if (equal (getvar "tilemode") 0)
      (setq na (acet-currentviewport-ename)) ;then need to add the data to the viewports
  );if
  (if (not (equal #last_restore (list lstate (bns_get_la_status))))
      (progn
       (setq #incomplete nil
                  clayer (getvar "clayer")
                     lst (acet-table-name-list "layer")
                 bitflag (bns_get_cur_lmanmode)
       );setq
       (setq n 0);setq
       (repeat (length lst)
        (setq lst2 (bns_r_layerstate (nth n lst) ;layer name
                                     lstate      ;layer state name
                                     lst2        ;list that's passed & returned for layer
                                     na          ;current viewport ename
                                     lst4        ;second list that's returned for VPlayer
                                     bitflag     ;sum of 1 =on/off,
                                                 ;       2 = f/t,
                                                 ;       4 = vp frz current vport
                                                 ;       8 = not used (f in new vports)
                                                 ;      16 = lock/unlock
                                                 ;      32 = color
                                                 ;      64 = linetype
                                                 ;     128 = line weight
                                                 ;     256 = plot
                                                 ;     512 = plotstyle
                   );bns_r_layer_state
              lst4 (cadr lst2)
              lst2 (car lst2)
        );setq
        (setq n (+ n 1));setq
       );repeat
 
       (if (and na lst4)
           (progn
            (command "_.vplayer" "_T" "*" "")
            (setq n 0);setq
            (repeat (length lst4)
             (command (nth n lst4))
            (setq n (+ n 1));setq
            );repeat
            (command "")
           );progn then do the vport thang
       );if
 
 
	;do a pre-process of the list and get a list of non-resolved linetypes
       (setq n 0);setq
       (repeat (length lst2)
        (setq lst3 (nth n lst2)
                 a (car lst3)
              lst3 (cdr lst3)
        );setq
        (while lst3
         (setq b (car lst3));setq
         (cond
          ((or (equal "_LW" b) (equal "_P" b) (equal "_PS" b) (equal "_C" b));or
           (setq lst3 (cdr lst3));setq
          );cond #1
          ((equal "_LT" b)
           (if (and (not (tblobjname "ltype" (cadr lst3)))
                    (not (member (cadr lst3) missinglt))
                    (not (command "_.-linetype" "_load" (cadr lst3) "acad.lin" ""))
                    (not (tblobjname "ltype" (cadr lst3)))
               );and
               (progn
                (setq missinglt (cons (cadr lst3) missinglt))
                (princ (acet-str-format "\nCannot find linetype: %1. Use linetype command to load it."
                                        (cadr lst3)
                       )
                );princ
               );progn then
           );if
           (setq lst3 (cdr lst3));setq
          );cond #2
         );cond close
         (setq lst3 (cdr lst3));setq
        );while
       (setq n (+ 1 n));setq
       );repeat
 
	;issue the layer command and apply the properties
       (command "_.layer")
       (setq n 0);setq
       (repeat (length lst2)
        (setq lst3 (nth n lst2)
                 a (car lst3)
              lst3 (cdr lst3)
        );setq
 
        (while lst3
         (setq b (car lst3));setq
         (cond
          ((or (equal "_LW" b)
               (equal "_P" b)
               (equal "_PS" b)
           );or
           (command b (cadr lst3) a)
           (setq lst3 (cdr lst3));setq
          );cond #1
          ((equal "_LT" b)
           (if (not (member (cadr lst3) missinglt))
               (command b (cadr lst3) a)
           );if
           (setq lst3 (cdr lst3));setq
          );cond #2
          ((equal "_C" b)
           (if (and (acet-str-equal a (getvar "clayer"))
                    (equal "-" (substr (cadr lst3) 1 1))
               );and
               (command b (cadr lst3) a "_Y")
               (command b (cadr lst3) a)
           );if
           (setq lst3 (cdr lst3));setq
          );cond #3
          ((and (or (equal b "_F")
                    (equal b "_OFF")
                );or
                (acet-str-equal a (getvar "clayer"))
           );and
           (if (equal b "_OFF")
               (command b a "_Y")
               (princ "\nCan't freeze the current layer.")
           );if
          );cond #4
          (T
           (command b a)
          );cond #5
         );cond close
         (setq lst3 (cdr lst3));setq
        );while
 
       (setq n (+ 1 n));setq
       );repeat
       (command "")
       (if (not #incomplete)
           (setq #last_restore (list lstate (bns_get_la_status)));setq
       );if
      );progn then need to perform a restore
  );if
 
  (princ)
 );defun bns_rl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;restore a layer state on the given layer
;takes
;layer name
;layer state name
;list passed to layer command
;entname of current viewport
;lst4 list passed to VPlayer
;bitflag sum of bits control options to restore.
;
 (defun bns_r_layerstate (la lstate lst2 na lst4 bitflag /
                          lstate2 e1 xd lst lst3 a b c d vp flag
                          aa bb cc e2
                         )
  (if lstate 
      (setq lstate (xstrcase lstate))
  );if
  (setq     e1 (entget (tblobjname "layer" la)
                       '("RAK")
               );entget
            xd (cdr (assoc -3 e1))
           lst (cdr (assoc "RAK" xd))
       lstate2 (strcat lstate "}")
        lstate (strcat "{" lstate)
  );setq
  (if (setq lst (member (cons 1000 lstate) lst));setq
      (progn
       (setq lst (reverse (member (cons 1000 lstate2) (reverse lst)))
             lst (cdr lst)
               a (cdr (nth 0 lst));the 70 code
              aa (cdr (assoc 70 e1))
               b (cdr (nth 1 lst));the color
              bb (cdr (assoc 62 e1))
               c (cdr (nth 2 lst));the linetype
              cc (cdr (assoc 6 e1))
             lst (cdr (cdr (cdr lst)))
               d (assoc 1070 lst)  ;the clayer ????
              vp (assoc 1000 lst)  ;vp frozen?????
       );setq
       (if (or (< b -255)
               (= b 0)
               (> b 255)
           );or
           (setq b 1)
       );if
       (if (or (< bb -255)
               (= bb 0)
               (> bb 255)
           );or
           (setq bb 1)
       );if
 
       (if (and na                           ;current viewport entity name
                (not (equal vp (cons 1000 lstate2))) ;vp is not the end of the xdata i.e. (1000 . "LSTATE}")
                (equal 4 (logand 4 bitflag)) ;vplayer thaw/freeze option is on
           );and
           (setq lst4 (append lst4 (list "_F" la "")));setq
           (progn
            (if (and (setq vp (cdr vp));setq
                     (equal (type vp) 'STR)
                     (equal 4 (logand 4 bitflag))
                );and
                (setq lst4 (append lst4 (list "_T" la "")));setq
            );if
           );progn then do the vport stuff
       );if
       (if (and (equal d '(1070 . 1))   ;then it was the current layer when saved
                (not (equal 16          ;if it's not xrefed then make it so this time too
                            (logand 16 (cdr (assoc 70 e1)))
                     );equal
                );not
           );and
           (progn
            (setq lst3 (list la "_T" "_ON" "_Set");lst3 is a list with layername followed
                  flag 99                         ;by the operations to perform on layername
            );setq
            (if (< bb 0)
                (setq lst3 (append lst3 (list "_OFF")));setq
            );if
           );progn then current layer set stuff
           (setq lst3 (list la)
                 flag nil
           );setq else
       );if
       (if (and (equal 32 (logand 32 bitflag)) ;color option is on
                (not (equal (abs b) (abs bb))) ;color change is needed.
           );and
           (progn
            (setq lst3 (append lst3
                               (list "_C" (itoa b)) ;rk was (abs b) instead of b
                       );append
            );setq then a color change is needed.
           );progn
       );if
       (if (and (equal 1 (logand 1 bitflag))    ;restore on/off option is active
                (< (/ (float b) (float bb)) 0)  ;it's on/off needs to be changed
           );and
           (progn
            (if (< b 0)
                (setq lst3 (append lst3 (list "_OFF")));setq then turn it off
                (progn
                 (if (equal 1 (logand 1 aa))
                     (setq lst3 (append lst3
                                        (list "_T" "_ON" "_F"))
                     );setq then it's frozen so, turn it on quietly.
                     (setq lst3 (append lst3 (list "_ON")));setq else
                 );if
                );progn else turn it on
            );if
           );progn then it's on/off status needs to be changed
       );if
       (if (and (equal 2 (logand 2 bitflag))  ;thaw/freeze option is active
                (not flag)                    ;not current (already thawed if it's going to be current)
                (not (equal (logand 1 a) (logand 1 aa)))  ;freeze thaw change needed
           );and
           (progn
            (if (equal 1 (logand 1 a))
                (setq lst3 (append lst3 (list "_F")));setq then freeze it
                (setq lst3 (append lst3 (list "_T")));setq thaw it
            );if
           );progn then needs to be frzn or thawed and it's not already that way.
       );if
       (if (and (equal 16 (logand 16 bitflag))            ;lock/unlock option is on
                (not (equal (logand 4 a) (logand 4 aa)))  ;locked unlocked status change needed
           );and
           (progn
            (if (equal 4 (logand 4 a))
                (setq lst3 (append lst3 (list "_LO")));setq then
                (setq lst3 (append lst3 (list "_UN")));setq else
            );if
           );progn then needs to be locked/unlocked and it's not already that way.
       );if
       (if (and (equal 64 (logand 64 bitflag))                ;linetype option is on
                (not (equal c (bns_local_ltype cc)))          ;ltype change needed.
           );and
           (setq lst3 (append lst3 (list "_LT" c)));setq
       );if
       (setq lst (member (assoc 1071 lst) lst)
               a (cdr (car lst))
             lst (cdr lst)
       );setq
       (if (and a
                (equal 128 (logand 128 bitflag))                ;lineweight option is on
                (/= a
                    (cdr (assoc 370 e1))
                ) ;change needed
           );and
           (progn
            (if (>= a 0)
                (progn
                 (if (= (getvar "LWUNITS") 0)
                     (setq b (cvunit (/ a 100.0) "mm" "in"))        ;convert from mm to inches
                     (setq b (/ a 100.0))                           ;leave as mm
                 );if
                 (setq b (rtos b 2 8))
                );progn then
                (setq b "_def")
            );if
            (setq lst3 (append lst3
                               (list "_LW" b);list
                       );append
            );setq
           );progn then
       );if
 
       (setq lst (member (assoc 1071 lst) lst)
               a (cdr (car lst))
             lst (cdr lst)
       );setq
       (if (and a
                (equal 256 (logand 256 bitflag))                ;plot restore option is on
                (/= a
                    (cdr (assoc 290 e1))
                ) ;change needed
           );and
           (progn
            (if (= a 1)
                (setq a "_P")
                (setq a "_no")
            );if
            (setq lst3 (append lst3
                               (list "_P" a);list
                       );append
            );setq
           );progn then
       );if
       (setq lst (member (assoc 1005 lst) lst)
               a (cdr (car lst))
             lst (cdr lst)
       );setq
       (if (and a
                (setq a (handent a))
                (equal 512 (logand 512 bitflag))                ;plotstyle restore option is on
                (not (equal a                                   ;change needed
                            (cdr (assoc 390 e1))
                     )
                );not
                (setq e2 (dictsearch (namedobjdict) "ACAD_PLOTSTYLENAME")
                      e2 (reverse e2)
                       a (cons 350 a)
                      e2 (cdr (member a e2))
                       a (cdr (car e2))
                );setq
           );and
           (progn
            (setq lst3 (append lst3
                               (list "_PS" a);list
                       );append
            );setq
           );progn then
       );if
 
       (if (> (length lst3) 1)
           (progn
            (if flag
                (setq lst2 (append (list lst3) lst2)) ;the set current layer first
                (setq lst2 (append lst2 (list lst3))) ;else append to end of list
            );if
           );progn then
       );if
      );progn then the layer state is defined
      (progn
       (princ (acet-str-format
"\nWarning. Layer \"%1\" is not defined in \"%2\". Save the layer state to update it."
                      la (substr lstate 2)
              )
       )
       (setq #incomplete T)
      );progn
  );if
  (list lst2 lst4)
 );defun bns_r_layerstate
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_dlg_rename ( lst iv / lstate lst2 n newlstate)
  (if (and lst
           (not (equal ""
                       (setq n (get_tile "list_states"))
                );equal
           );not
           (setq      n (atoi n)
                 lstate (nth n lst)
                 lstate (xstrcase (acet-str-space-trim lstate))
                   lst2 (list (get_tile "msg")
                                  lstate
                              (get_tile "list_states")
                        );list
           );setq
           (setq newlstate (bns_lman_rename_dd lstate))
      );and
      (progn
       (setq lst (subst newlstate lstate lst));setq
       (if (acet-str-equal (car #last_restore) lstate)
           (setq #last_restore (append (list newlstate) (cdr #last_restore)));setq
       );if
       (if (member (car #last_restore) lst)
           (setq lst2 (list (acet-str-format "Current layer state: %1" (car #last_restore))
                            (car #last_restore)
                            (itoa (vl-position newlstate lst))
                            ;(itoa (vl-position (car #last_restore) lst))
                      );list
           );setq then
           (setq lst2 (list "" nil ""));setq
       );if
 
       (setq down_dlg_operation (list '(lst)
                                      (append '(bns_lman_rename)
                                               (list lstate newlstate)
                                      )
                                      'lst
                                );list
       );setq
       (setq #dlg_pnt (done_dialog 2))
       (unload_dialog iv)
 
      );progn then
      (progn
       ;(if (equal ""
       ;           (setq n (get_tile "list_states"))
       ;    );equal
       ;    (setq lst2 (list "Nothing selected." nil ""));setq
       ;);if
       ;(print "nothing")
       ;(if (member (car #last_restore) lst)
       ;    (setq lst2 (list (strcat "Current layer state: " (car #last_restore));strcat
       ;                     (car #last_restore)
       ;                     (itoa (vl-position (car #last_restore) lst))
       ;               );list
       ;    );setq then
       ;    (setq lst2 (list "" nil ""));setq
       ;);if
      );progn else
  );if
  (setq lst (list lst lst2))
 );defun bns_dlg_rename
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_lman_rename_dd ( lstate / newlstate states ed_check iv flag)
 
 (defun ed_check ( val reason states / )
  (setq val (acet-str-space-trim (xstrcase val)));setq
  (set_tile "ed2" val)
  (cond
   ((equal val "")
    (set_tile "error" "New name cannot be empty.")
   );cond #1
   ;((equal val (get_tile "ed1"))
   ; (set_tile "error" "That's the same as the old name!")
   ;);cond #2
   ((and (member val states)
         (not (equal val (get_tile "ed1")))
    );and
    (set_tile "error" "That name is already in use.")
   );cond #3
   (T
    (set_tile "error" "")
    (if (equal reason 1)
        (done_dialog 1)
    );if
   );cond #4
  );cond close
  val
 );defun ed_check
 
 (if (> (setq iv (load_dialog (getfileET "lman.dcl"))) 0)
     (progn
 
      (if (new_dialog "lman_rename" iv);NEW_DIALOG DISPLAYS THE DIALOG
          (progn
 
           (setq states (bns_get_layerstates));setq list of layer state names.
 
           (set_tile "txt1" "Old name: ")
           (set_tile "txt2" "New name: ")
           (set_tile "ed1" lstate)
           (mode_tile "ed1" 1)
           (set_tile "ed2" lstate)
           (mode_tile "ed2" 2)
 
           (action_tile "ed2"
                        "(setq newlstate (ed_check $value $reason states))"
           );action_tile
           (action_tile "accept"
                        (strcat
                         "(setq newlstate "
                           "(ed_check (get_tile \"ed2\") 1 states)"
                         ")"
                        );strcat
           );action_tile
           (action_tile "cancel"
                        "(done_dialog 0)"
           );action_tile
           (setq flag (start_dialog));setq
           (if (equal flag 1)
               (progn
                ;(bns_lman_rename lstate newlstate)
                (if (not (acet-str-equal newlstate lstate))
                    (setq flag newlstate)
                    (setq flag nil)
                );if
               );progn then
               (setq flag nil)
           );if
          );progn then initialize the tiles and activate the dialog box
          (progn
           (alert "Unable to display dialog box")
           (setq flag nil)
          );progn
      );if new dialog
     );progn then
     (progn
      (alert "Unable to load dialog box")
      (setq flag nil)
     );progn else
 );if load
 
(if flag
    (setq flag (xstrcase flag))
);if
 
flag
);defun bns_lman_rename_dd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_c_rename_layerstate ( / flag a b lst)
 
(setq lst (bns_get_layerstates));setq
(while (not flag)                                    ;get the old name
 (setq a (xstrcase (getstring T "\nEnter old name [?]: "))
       a (acet-str-space-trim a)
 );setq
 (cond
  ((equal a "?")     (bns_list_layerstates))
  ((member a lst) (setq flag T))
  (T                    (princ "\nLayer state not found."))
 );cond close
);while
 
(setq flag nil)
(while (not flag)                                   ;get the new name
 (setq b (xstrcase (getstring T "\nEnter new name [?]: "))
       b (acet-str-space-trim b)
 );setq
 (cond
  ((equal b "?")     (bns_list_layerstates))
  ((not (member b lst)) (setq flag T))
  (T                    (princ "\nThat name is already in use."))
 );cond close
);while
(bns_lman_rename a b)
 
);defun bns_c_rename_layerstate
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_lman_rename ( lstate lstate2 / n lst flag)
 
(setq lst (acet-table-name-list "layer"));setq
(setq n 0);setq
(repeat (length lst)
 (setq flag (or (bns_rename_layerstate (nth n lst) ;layer name
                                       lstate      ;old layer state name
                                       lstate2     ;new layer state name
                );bns_rename_layerstate
                flag
            );or
 );setq
(setq n (+ n 1));setq
);repeat
 
flag
);defun bns_lman_rename
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_rename_layerstate (la oldlstate newlstate / e1 xd lst
                              lstate lstate2 lstate3 lstate4 flag
                             )
  (if oldlstate
      (setq oldlstate (xstrcase oldlstate))
  )
  (if newlstate
      (setq newlstate (xstrcase newlstate))
  )
  (setq    e1 (entget (tblobjname "layer" la)
                      '("RAK")
              );entget
           xd (cdr (assoc -3 e1))
          lst (cdr (assoc "RAK" xd))
       lstate (strcat "{" oldlstate)
      lstate2 (strcat oldlstate "}")
      lstate3 (strcat "{" newlstate)
      lstate4 (strcat newlstate "}")
 );setq
 (if (and (member (cons 1000 lstate) lst)
          (member (cons 1000 lstate2) lst)
     );and
     (progn
      (setq lst (subst (cons 1000 lstate3) (cons 1000 lstate) lst)
            lst (subst (cons 1000 lstate4) (cons 1000 lstate2) lst)
            lst (append (list "RAK") lst)
             xd (subst lst (assoc "RAK" xd) xd)
             xd (append (list -3) xd)
             e1 (subst xd (assoc -3 e1) e1)
      );setq
      (setq flag (entmod e1))
     );progn then the layer state is defined
     (progn
      ;doesn't exist
      (setq flag nil);setq
     );progn
 );if
 flag
);defun bns_rename_layerstate
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_new_state ( lst new_name msg /  lstate iv str flag);
  (if (> (setq iv (load_dialog (getfileET "lman.dcl"))) 0)
      (progn
       (if (new_dialog "new_lman" iv);NEW_DIALOG DISPLAYS THE DIALOG
           (progn
            (set_tile "new_msg" msg)
            (set_tile "new_name" new_name)
            (setq str (strcat
                        "(if (equal $reason 1) "
                            "(progn (setq lstate (get_tile \"new_name\")) "
                                   "(done_dialog 1) "
                             ")"
                         ")"
                      );strcat
            );setq
            (action_tile "new_name" str)
            (action_tile "accept"
                         "(setq lstate (get_tile \"new_name\")) (done_dialog 1)"
            );action_tile
            (action_tile "cancel"
                         "(done_dialog 0)"
            );action_tile
             (mode_tile "new_name" 2)
            (setq flag (start_dialog));setq
            ;START_DIALOG MAKES THE BUTTONS ACTIVE
           );progn then initialize the tiles and activate the dialog box
           (progn
            (alert "Unable to display dialog box")
            (setq flag 1)
           );progn
       );if new dialog
      );progn then
      (progn
       (alert "Unable to load dialog box")
       (setq flag 1)
      );progn else
  );if load
  (if lstate
      (setq lstate (acet-str-space-trim (xstrcase lstate)))
  );if
  (if (and (not (acet-str-equal lstate new_name))
           (member lstate lst)
      );and
      (progn
       (if (equal 0 (bns_warning lstate ""))
           (setq lstate (bns_new_state lst new_name msg));setq
       );if
     );progn
  );if
  (if lstate
      (setq lstate (xstrcase lstate))
  )
  lstate
 );defun bns_new_state
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_warning ( name msg2 /  iv flag);
  (if (> (setq iv (load_dialog (getfileET "lman.dcl"))) 0)
      (progn
       (if (new_dialog "warning" iv);NEW_DIALOG DISPLAYS THE DIALOG
           (progn
            (if (not (equal msg2 ""))
                (progn
                 (set_tile "warn_msg" (acet-str-format "%1 %2?" msg2 name))
                 (set_tile "warn_msg2" "")
                );progn
                (progn
                 (set_tile "warn_msg" (acet-str-format "\"%1\" already exist." name))
                 (set_tile "warn_msg2" "Do you want to overwrite it?")
                );progn
            );if
            (action_tile "accept"
                         "(done_dialog 1)"
            );action_tile
            (action_tile "cancel"
                         "(done_dialog 0)"
            );action_tile
            (mode_tile "accept" 2)
            (setq flag (start_dialog));setq
            ;START_DIALOG MAKES THE BUTTONS ACTIVE
           );progn then initialize the tiles and activate the dialog box
           (progn
            (alert "Unable to display dialog box")
            (setq flag 1)
           );progn
       );if new dialog
      );progn then
      (progn
       (alert "Unable to load dialog box")
       (setq flag 1)
      );progn else
  );if load
  flag
 );defun bns_warning
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_reg_it ( )
  (if (not (tblsearch "appid" "RAK"))
      (if (=  (regapp "RAK") nil)
         (princ "\nCan't register XDATA for RAK. ")
      );if
  );if
);defun bns_reg_it
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_c_delete_layerstate ( / lstate lst2 a)
  (bns_reg_it)
  (while (not lstate)
   (setq lstate (xstrcase (getstring T "\nEnter a name the for layer state to delete [?]:"))
   );setq
   (if (not (equal lstate ""))
       (progn
        (if (equal (acet-str-space-trim lstate) "?")
            (progn
             (bns_list_layerstates)
             (setq lstate nil);setq
            );progn then
            (progn
             (if (not lst2)
                 (setq lst2 (bns_get_layerstates))
             );if
             (if (not (member lstate lst2))
                 (progn
                  (princ (acet-str-format "\nCan't find saved layer state: %1" lstate))
                  (setq lstate nil);setq
                 );progn
                 (progn
                  (initget "Yes No _Yes No")
                  (setq a (getkword "Are you sure? [Yes/No] <Yes>: "))
                  (if (equal a "No")
                      (setq lstate "")
                  );if
                 );progn else
             );if
            );progn else
        );if
       );progn then
   );if
  );while
  (if (and (not (equal a "No"))
           (not (equal lstate ""))
      );and
      (progn
       (bns_dl lstate)
       (princ (acet-str-format "\nLayer state: %1 deleted." lstate))
      );progn then
  );if
  (princ)
 );defun bns_c_delete_layerstate
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun bns_c_restore_layerstate ( / lstate lst2)
  (bns_reg_it)
  (while (not lstate)
   (setq lstate (xstrcase
                 (getstring T "\nEnter a name the for layer state to restore [?]:")
                )
   );setq
   (if (not (equal "" lstate))
       (progn
        (if (equal (acet-str-space-trim lstate) "?")
            (progn
             (bns_list_layerstates)
             (setq lstate nil);setq
            );progn then
            (progn
             (setq lstate (acet-str-space-trim lstate));setq
             (if (not lst2)
                 (setq lst2 (bns_get_layerstates))
             );if
             (if (not (member lstate lst2))
                 (progn
                  (princ (acet-str-format "\nCan't find saved layer state: %1" lstate))
                  (setq lstate nil);setq
                 );progn
             );if
            );progn else
        );if
       );progn
   );if
  );while
  (if (not (equal lstate ""))
      (progn
       (bns_rl lstate)
       (princ (acet-str-format "\nLayer state: %1 restored."  lstate ))
      );progn then
  );if
  (princ)
 );defun bns_c_restore_layerstate
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
 ;
 (defun bns_c_save_layerstate ( / lstate cstate lst a)
  (bns_reg_it)
  (while (not lstate)
   (setq lstate (xstrcase (getstring T "\nEnter name for saving current layer status [?]: ")));setq
 
   (cond
    ((equal (acet-str-space-trim lstate) "?")
      (bns_list_layerstates)
      (setq lstate nil);setq
    );cond #1
    ((not (equal lstate ""))
      (setq lstate (acet-str-space-trim lstate))
      (if (equal "" lstate) ;(not (snvalid lstate))
          (progn
           (princ "\nInvalid name.")
           (setq lstate nil)
          );progn
          (progn
           (setq lst (bns_get_layerstates));setq
           (if (and (equal (cadr #last_restore) (bns_get_la_status));equal
                    (member (car #last_restore) lst)
               );and
               (setq cstate (car #last_restore))
               (setq cstate "*UNNAMED*")
           );if
           (if (not (or (not (member lstate lst))
                        (acet-str-equal cstate lstate)
                    );or
               );not
               (progn
                (initget "Yes No _Yes No")
                (setq a (getkword "Layer state already exists. Overwrite? [Yes/No] <Yes>: "))
                (if (equal a "No")
                    (setq lstate "")
                );if
               );progn then
           );if
          );progn then
      );if
    );cond #2
   );cond close
  );while
  (if (and (not (equal a "No"))
           (not (equal "" lstate))
      );and
      (progn
       (bns_sl lstate)
       (princ (acet-str-format "\nLayer state: %1 saved."  lstate ))
      );progn then
  );if
  (princ)
 );defun bns_c_save_layerstate
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_lman_ltypefx ( / lst n j a b na e1 )
 
(setq lst (acet-table-name-list "ltype"))
(setq n 0)
(repeat (length lst)
(setq  a (nth n lst)
      na (tblobjname "ltype" a)
      e1 (entget na)
);setq
(if (and (equal 16 (logand 16 (cdr (assoc 70 e1))))
         (wcmatch a "*|*")
    );and
    (progn
 
     (while (and (not (equal (substr a 1 1) "|"))
                 (> (strlen a) 1)
            );and
       (setq a (substr a 2))
     );while
     (setq a (substr a 2));setq
     (if (and (not (tblobjname "ltype" a))
              (not (equal "" a))
         );and
         (progn
           (setq b (cdr (assoc 70 e1))
                 b (- b 16)
           );setq
           (if (equal 32 (logand 32 b))
               (setq b (- b 32))
           );if
           (setq e1 (subst (cons 2 a) (assoc 2 e1) e1)
                 e1 (subst (cons 70 b) (assoc 70 e1) e1)
           );setq
 
           (if (assoc 340 e1)
               (progn
                 (setq b (substr (nth n lst)
                                 1
                                 (- (strlen (nth n lst)) (strlen a) 1)
                         );substr
                       j 0
                 );setq
                 (while (tblobjname "ltype" (strcat b "$" (itoa j) "$" a))
                      (setq j (+ j 1))
                 );while
                 (command "_.xbind" "_lt" (nth n lst))
                 (command "_.rename" "_lt"
                          (strcat b "$" (itoa j) "$" a)
                          a
                 );command
               );progn
               (entmake e1)
           );if
 
         );progn
     );if
    );progn
);if
(setq n (+ n 1))
);repeat
 
);defun bns_lman_ltypefx
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_local_ltype ( a / na e1 )
 
(if (and (setq na (tblobjname "ltype" a))
         (setq e1 (entget na))
         (equal 16 (logand 16 (cdr (assoc 70 e1))))
         (wcmatch a "*|*")
    );and
    (progn
     (while (and (not (equal (substr a 1 1) "|"))
                 (> (strlen a) 1)
            );and
       (setq a (substr a 2))
     );while
     (setq a (substr a 2))
    );progn
);if
a
);defun bns_local_ltype
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defun c:-lman ( / flag lst)
;; 
;; (acet-error-init (list
;;                    (list   "cmdecho" 0
;;                          "regenmode" 1
;;                             "expert" 0
;;                            "ucsicon" 0
;;                    )
;;                    nil     ;flag. True means use undo for error clean up.
;;                   );list
;; );acet-error-init
;; 
;; (bns_lman_ltypefx)
;; 
;; (bns_reg_it)
;; 
;; (setq lst (bns_get_layerstates));setq
;; (if (and (equal (cadr #last_restore) (bns_get_la_status));equal
;;          (member (car #last_restore) lst)
;;     );and
;;     (princ (acet-str-format "\nCurrent layer state: %1" (car #last_restore)))
;;     (progn
;;      (princ "\nCurrent layer state: *UNNAMED*")
;;      (setq #last_restore nil);setq
;;     );progn
;; );if
;; 
;; (setq flag T)
;; (while flag
;;  (initget "? Import Export Save Restore Delete reName _? Import Export Save Restore Delete reName")
;;  (setq flag (getkword "\nEnter an option for layer states [?/Import/Export/Save/Restore/Delete/reName]: "))
;;  (cond
;;   ((equal flag "?")
;;    (bns_list_layerstates)
;;   )
;;   ((acet-str-equal "Import" flag)
;;    (princ (strcat "\n" (car (bns_c_import_lay))))
;;   )
;;   ((acet-str-equal "Export" flag)
;;    (princ (strcat "\n" (car (bns_c_export_lay))))
;;   )
;;   ((acet-str-equal "Save" flag)
;;    (bns_c_save_layerstate)
;;   )
;;   ((acet-str-equal "Restore" flag)
;;    (bns_c_restore_layerstate)
;;   )
;;   ((acet-str-equal "Delete" flag)
;;    (bns_c_delete_layerstate)
;;   )
;;   ((acet-str-equal "reName" flag)
;;    (bns_c_rename_layerstate)
;;   )
;;   (T (setq flag nil))
;;  );cond close
;; );while
;; (acet-error-restore)
;;);defun c:-lman
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defun c:lmanmode ( / ) ;dialog unless script or cmddia=0
;; (acet-error-init nil)
;; (bns_get_lmanmode nil)
;; (setq #last_restore nil)
;; (acet-error-restore)
;;);defun c:lmanmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defun c:-lmanmode ( / ) ;command line
;; (acet-error-init nil)
;; (bns_get_lmanmode T)
;; (setq #last_restore nil)
;; (acet-error-restore)
;;);defun c:-lmanmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_get_lmanmode ( flag / mode)
 
(setq mode (bns_get_cur_lmanmode))
(if (or flag
        (equal 0 (getvar "cmddia"))
        (equal 4 (logand 4 (getvar "cmdactive")))
    );or
    (bns_get_lmanmode_cmd)
    (bns_get_lmanmode_dd)
);if
 
);defun bns_get_lmanmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bns_get_lmanmode_cmd
;prompts for lmanmode at the command line.
;The ACET-LMAN-MODE variable controls the types of operations that
;lman performs on layerstate restore.
;
;Lmanmode is a sum of the following:
;1  - on/off
;2  - thaw/freeze
;4  - Vpthaw/vpfreeze
;(8)  - ;not used
;16 - lock/unlock
;32 - color
;64 - linetype
;128 - line weight
;256 - plot
;512 - plotstyle
;
(defun bns_get_lmanmode_cmd ( / curmode mode a flag)
 
(setq curmode (bns_get_cur_lmanmode)
         mode curmode
);setq
(bns_princ_lmanmode mode)
(while (not flag)
 (initget 6) ; no zero and no negative
 (if (setq a (getint "\nEnter LMANMODE bit coded setting: "))
     (setq a (logand 1015 a))
 );if
 (cond
  ((not a)
   (setq    a curmode
        flag T
   );setq
  );cond #1
  ((<= a 1015)
   (setq a (acet-calc-bitlist a));setq
   (if (not a)
       (setq a 0)
   );if
   (if (member 8 a)
       (progn
        (setq a (- (apply '+ a) 8));setq then
        (princ "\nIgnoring 8 bit value.")
       );progn
       (setq a (apply '+ a));setq else
   );if
   (if (> a 0)
       (setq flag T);setq
   );if
  );cond #2
 );cond close
 (if (not flag)
     (princ "\nInvalid value.")
 );if
);while
(if (and a
         (equal (type a) 'INT)
    );and
    (progn
     (bns_princ_lmanmode a)
     (acet-setvar (list "ACET-LMAN-MODE" a 3))
    );progn then
);if
mode
);defun bns_get_lmanmode_cmd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bns_get_lmanmode_dd
;prompts for lmanmode using a dcl dialog with check boxes.
;sets the variable ACET-LMAN-MODE
;
;Lmanmode is a sum of the following:
; 1  - on/off
; 2  - thaw/freeze
; 4  - Vpthaw/vpfreeze
; (8)  - ;not used
; 16 - lock/unlock
; 32 - color
; 64 - linetype
; 128 - line weight
; 256 - plot
; 512 - plotstyle
;
(defun bns_get_lmanmode_dd ( / iv flag set_bit mode)
 
 (setq mode (bns_get_cur_lmanmode))
 (if (> (setq iv (load_dialog (getfileET "lman.dcl")));setq
        0
     );test
     (progn
      (if (new_dialog "lmanmode" iv)
          (progn
           (if (equal 1 (logand 1 mode))
               (set_tile "onoff" "1")
               (set_tile "onoff" "0")
           );if
           (if (equal 2 (logand 2 mode))
               (set_tile "thawfreeze" "1")
               (set_tile "thawfreeze" "0")
           );if
           (if (equal 4 (logand 4 mode))
               (set_tile "vpthawfreeze" "1")
               (set_tile "vpthawfreeze" "0")
           );if
           (if (equal 16 (logand 16 mode))
               (set_tile "lock" "1")
               (set_tile "lock" "0")
           );if
           (if (equal 32 (logand 32 mode))
               (set_tile "color" "1")
               (set_tile "color" "0")
           );if
           (if (equal 64 (logand 64 mode))
               (set_tile "linetype" "1")
               (set_tile "linetype" "0")
           );if
           (if (equal 128 (logand 128 mode))
               (set_tile "lineweight" "1")
               (set_tile "lineweight" "0")
           );if
           (if (equal 256 (logand 256 mode))
               (set_tile "plot" "1")
               (set_tile "plot" "0")
           );if
           (if (equal 512 (logand 512 mode))
               (set_tile "plotstyle" "1")
               (set_tile "plotstyle" "0")
           );if
 
           (defun set_bit ( a mode val / )
            (if (and (equal "0" val)
                     (equal a (logand a mode))
                );and
                (setq mode (- mode a));subtract the bit
                (progn
                 (if (equal "1" val)
                     (setq mode (logior a mode));setq then add the bit
                 );if
                );progn else
            );if
            (if (<= mode 0) ;disable the OK button
                (progn
                 (setq mode 0)
                 (set_tile "error" "Must select at least one option.")
                 (mode_tile "accept" 1)
                );progn then
                (progn
                 (set_tile "error" "")
                 (mode_tile "accept" 0)
                );progn else
            );if
            mode
           );defun set_bit
 
           (action_tile "onoff" "(setq mode (set_bit 1 mode $value))")
           (action_tile "thawfreeze"  "(setq mode (set_bit 2 mode $value))")
           (action_tile "vpthawfreeze" "(setq mode (set_bit 4 mode $value))")
           (action_tile "lock"  "(setq mode (set_bit 16 mode $value))")
           (action_tile "color"  "(setq mode (set_bit 32 mode $value))")
           (action_tile "linetype"  "(setq mode (set_bit 64 mode $value))")
           (action_tile "lineweight"  "(setq mode (set_bit 128 mode $value))")
           (action_tile "plot"  "(setq mode (set_bit 256 mode $value))")
           (action_tile "plotstyle"  "(setq mode (set_bit 512 mode $value))")
 
           (action_tile "accept" "(done_dialog 1)")
           (action_tile "cancel" "(done_dialog 0)")
           (action_tile "help" "(acet-help \"LMAN\")")
 
           (setq flag (start_dialog));setq ;START_DIALOG MAKES THE BUTTONS ACTIVE
           (if (and (equal flag 1)
                    (> mode 0)
               );and
               (progn
                (acet-setvar (list "ACET-LMAN-MODE" mode 3))
                (setq #last_restore nil)
               );progn then
               (setq mode (bns_get_cur_lmanmode));setq else
           );if
          );progn then initialize the tiles and activate the dialog box
          (alert "Unable to display dialog box")
      );if new dialog
      (unload_dialog iv);unload it when done
     );progn then
     (alert "Unable to load dialog box");else
 );if load
 
;(bns_princ_lmanmode mode)
mode
);defun bns_get_lmanmode_dd
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Gets the current lmanmode setting from the variable ACET-LMAN-MODE
;Returns an a bit sum integer. See header for bns_get_lmanmode for more details.
;
(defun bns_get_cur_lmanmode ( / mode )
 (if (not (setq mode (acet-getvar '("ACET-LMAN-MODE"))))
     (progn
      (setq mode (+ 1 2 4 16 32 64 128 256 512))
      (acet-setvar (list "ACET-LMAN-MODE"
                         (+ 1 2 4 16 32 64 128 256 512)
                         3
                   );list
      );acet-setvar
     );progn then
 );if
 mode
);defun bns_get_cur_lmanmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Lmanmode is a sum of the following:
; 1  - on/off
; 2  - thaw/freeze
; 4  - Vpthaw/vpfreeze
; (8)  - ;not used
; 16 - lock/unlock
; 32 - color
; 64 - linetype
; 128 - line weight
; 256 - plot
; 512 - plotstyle
;
(defun bns_princ_lmanmode ( mode / lst n a b )
 
(if (not mode)
    (setq mode (bns_get_cur_lmanmode))
);if
(setq lst (list '(1 "On/off")
                '(2 "thaw/Freeze")
                '(4 "Vpthaw/vpfreeze")
                ;;;;'(8 "not used")
                '(16 "Lock/unlock")
                '(32 "Color")
                '(64 "Linetype")
                '(128 "Lineweight")
                '(256 "Plot")
                '(512 "PlotStyle")
          );list
);setq
(setq b "")
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
(if (equal (car a) (logand (car a) mode))
    (setq b (strcat b ", " (cadr a)));setq
);if
(setq n (+ n 1));setq
);repeat
(if (equal (substr b 2) "")
    (princ "\nCurrent LMANMODE: None");then
    (princ (acet-str-format "\nCurrent LMANMODE: %1" (substr b 2)));else
);if
(substr b 2)
);defun bns_princ_lmanmode


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDT7izjD9GrBVD9L3FcsIAyJ76fao0KfHDlmzS0
;;; n4AVRbYIVNszgc0jLYxIQ1QoY+4mtAHhoWepJd/ihuJNQD+5THdVkw0oT4RuRRO8
;;; mA6V2XK3XAir3FC8fWgnoIEr/lzzmZNixZqGYT7j1ALbrGGOPnXmryE0y+fLZv0E
;;; iEszxYB1IvMF5z5UmjEToA7CA7FmNNet3T1NuJrV9AKXgD3/u2CJjc3BULE8J6YL
;;; 0CEywI5LCpg2Vtnqn2anXezwSTcDyNkNt+3bkap3fO+sMLS5wf7LMeaRGcf1Payg
;;; MIoS1BLz0kBdD0io3xBMcrlh5h7Yws4HQq8QbFIHmyGSrQlb7H+JdKkq1bom5ujL
;;; hdBIy3mX+hJYJk/elKJtKE5pH5oH9n8d1BIVh5li+k+YuWwX9UZT+TeQNZ03V5Gk
;;; AN39aUhS6eCWjB2bhPvrDI+XOroAnjuoakgL+8GW51YP12q6oaBpYy9IP0C/TQta
;;; 0qnl+ZlR+ZlC6QAMVxIZ0HsaFGyY7qlP+lRjDwOEDMNi4sJTME5ke5YcW+izWM7C
;;; DOjzvABmZCzV2WhJckxcO0bIyRj2cYyluwqARS/AeXt3fz1IsGbgfCa4pVTanFOM
;;; 0T+oqUvNIilsHPh1O6l3KGuSMNOrhH/S+RlM+wLxMcpzk7soA1NG7V3lAeQIeLte
;;; CwdWWg==
;;; -----END-SIGNATURE-----