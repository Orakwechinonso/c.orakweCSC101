;;;
;;;    CDORDER.LSP
;;;    Created by Randy Kintzley 6/24/98
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Defaults to dialog but will issue the command at the command line if
;a script is running.
;
(defun c:cdorder ()
 (acet-error-init
  (list (list  "cmdecho" 0
             "highlight" (getvar "highlight")
             ;"regenmode" 0
                "attreq" 0
                "expert" 0
              "limcheck" 0
                "osmode" 0
             "pickstyle" 0
         )
         1
  );list
 );acet-error-init
 (acet-autoload '("acetflt.lsp" "(bns_blk_match blk flt lst flag)"))
 (acet-autoload '("acetflt.lsp" "(bns_tbl_match tbl flt)"))
 
 (if (equal 4 (logand 4 (getvar "cmdactive")))
     (acet-cdorder-cmd)
     (acet-cdorder-dlg)
 );if
 
 (acet-error-restore)
);defun c:cdorder
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Command line version of the CDORDER command
;
(defun c:-cdorder ()
 (acet-error-init
  (list (list  "cmdecho" 0
             "highlight" (getvar "highlight")
             "regenmode" 0
                "attreq" 0
                "expert" 0
              "limcheck" 0
                "osmode" 0
             "pickstyle" 0
         )
         1
  );list
 );acet-error-init
 (acet-autoload '("acetflt.lsp" "(bns_blk_match blk flt lst flag)"))
 (acet-autoload '("acetflt.lsp" "(bns_tbl_match tbl flt)"))
 
 (acet-cdorder-cmd)
 
 (acet-error-restore)
);defun c:-cdorder
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-cdorder-cmd ( / ss ans a lst blk-ans mode def)
 
(if (and (setq ss (ssget '((-4 . "<NOT") (0 . "VIEWPORT") (-4 . "NOT>"))));setq
         (setq mode (acet-get-cdorder-mode))
         (or (equal mode 0)
             (and (equal mode 1)
                  (acet-cdorder-warning 0)
             );and
         );or
    );and
    (progn
     (acet-cdorder-init-defaults)
     (if (setq lst (acet-cdorder-get-colors))
         (progn
          (setvar "highlight" 0)
          (if (equal mode 0)
              (progn
               (initget "Front Back")
               (setq ans (getkword (strcat "\nEnter draworder location for specified objects [Front/Back] <"
                                           #acet-cdorder-location
                                           ">: "
                                   );strcat
                         );getkword
               );setq
               (if (not ans)
                   (setq ans #acet-cdorder-location)
                   (setq #acet-cdorder-location ans)
               );if
              );progn then
          );if
          (if (not ans) (setq ans "Front"));if
 
          (if (equal #acet-cdorder-blocks 1)
              (setq def "Yes")
              (setq def "No")
          );if
          (initget "Yes No")
          (setq blk-ans (getkword (strcat "\nRedefine order within selected blocks? <"
                                          (substr def 1 1)
                                          ">: "
                                  );strcat
                        );getkword
          );setq
          (if (not blk-ans)
              (setq blk-ans def)
          );if
          (if (equal blk-ans "Yes")
              (setq #acet-cdorder-blocks 1);setq then
              (setq #acet-cdorder-blocks 0);setq else
          );if
 
          (acet-cdorder ss ans lst blk-ans mode)
         );progn then
     );if
    );progn then
);if
);defun acet-cdorder-cmd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-cdorder-dlg ( / ss lst n a iv
                            c method ans blk-ans flag
                            show-color move-up move-down setup-tiles
                        )
 
 ;;; defun a few local functions to handle the dialog callbacks.
 
 (defun show-color ( c / w h )
  (setq w (dimx_tile "color_image")
        h (dimy_tile "color_image")
  );setq
  (start_image "color_image")
  (fill_image 0 0 w h (atoi c))
  (end_image)
 );defun show-color
 
 (defun move-up ( val lst / a b n j )
  (setq n (atoi val)
        j (- n 1)
  );setq
  (if (< j 0)
      (setq j 0)
  );if
  (setq   a (nth n lst)
          b (nth j lst)
        lst (subst 999 a lst)
        lst (subst a b lst)
        lst (subst b 999 lst)
  );setq
  (acet-dcl-list-make "color_list" lst)
  (set_tile "color_list" (itoa j))
  lst
 );defun move-up
 
 (defun move-down ( val lst / a b n j )
  (setq n (atoi val)
        j (+ n 1)
  );setq
  (if (>= j (length lst))
      (setq j (- j 1))
  );if
  (setq   a (nth n lst)
          b (nth j lst)
        lst (subst 999 a lst)
        lst (subst a b lst)
        lst (subst b 999 lst)
  );setq
  (acet-dcl-list-make "color_list" lst)
  (set_tile "color_list" (itoa j))
  lst
 );defun move-down
 
 (defun setup-tiles ()
   (if (equal "1" (get_tile "handles"))
       (progn
        (set_tile "front" "1")
        (mode_tile "location" 1)
        (setq method 1)
       );progn
       (progn
        (mode_tile "location" 0)
        (setq method 0)
       );progn
   );if
   (if (equal "1" (get_tile "front"))
       (setq ans "Front")
       (setq ans "Back")
   );if
   (if (equal "1" (get_tile "blocks"))
       (setq blk-ans "Yes")
       (setq blk-ans "No")
   );if
 );defun setup-tiles
 
 ;;; get down to the job at hand
 
(cond
 ((not (setq ss (ssget '((-4 . "<NOT") (0 . "VIEWPORT") (-4 . "NOT>")))))
  (princ)
 );cond #1
 ((not (setq lst (acet-cdorder-ss-colors ss)))
  (princ "\nProblem determining colors.")
 );cond #2
 ((or (not (setq iv (load_dialog (getfileET "cdorder.dcl"))))
      (<= iv 0)
  );or
  (acet-alert "Unable to load dialog box cdorder.dcl")
 );cond #3
 ((not (new_dialog "cdorder" iv))
  (acet-alert "Unable to display dialog box cdorder")
  (unload_dialog iv)
 );cond#4
 (T
  (acet-cdorder-init-defaults)
  (acet-dcl-list-make "color_list" lst)
 
  (action_tile "color_list"
                (strcat "(show-color "
                            "(nth (atoi "
                                   "(get_tile \"color_list\") "
                                 ") "
                                 "lst "
                            ")"
                        ")"
                );strcat
  );action_tile
  (action_tile "up"   "(setq lst (move-up   (get_tile \"color_list\") lst))")
  (action_tile "down" "(setq lst (move-down (get_tile \"color_list\") lst))")
 
  (action_tile "draworder" "(setup-tiles)")
  (action_tile "handles" "(setup-tiles)")
  (action_tile "front" "(setup-tiles)")
  (action_tile "back" "(setup-tiles)")
 
  (action_tile "blocks" "(setup-tiles)")
 
  (action_tile "accept"
               (strcat "(setup-tiles) "
                       "(if (or (equal method 0) "
                               "(and (equal method 1) "
                               "     (acet-cdorder-warning 1) "
                               ") "
                           ") "
                           "(done_dialog 1) "
                       ")"
               );strcat
  );action_tile
  (action_tile "cancel" "(done_dialog 0)")
  (action_tile "help" "(acet-help \"CDORDER\")")
 
  (show-color (car lst))
  (set_tile "color_list" "0")
 
  (if (equal #acet-cdorder-location "Front")
      (set_tile "front" "1")
      (set_tile "back" "1")
  );if
  (set_tile "blocks" (itoa #acet-cdorder-blocks))
  (if (equal #acet-cdorder-mode 0)
      (set_tile "draworder" "1")
      (set_tile "handles" "1")
  );if
  (setup-tiles)
  (mode_tile "accept" 2)
  (setq flag (start_dialog));setq
 
  (if (and lst
           (equal flag 1)
      );and
      (progn
       ;create a new default list of colors.
       (setq c "")
       (setq n 0)
       (repeat (length lst)
       (setq c (strcat c "," (nth n lst)))
       (setq n (+ n 1));setq
       );repeat
       (if lst
           (setq c (substr c 2))
       );if
       (if (not (equal c ""))
           (acet-setvar (list "ACET-CDORDER-COLORS" c 1))
       );if
       (setq #acet-cdorder-location ans);setq
       (if (equal blk-ans "Yes")
           (setq #acet-cdorder-blocks 1);setq
           (setq #acet-cdorder-blocks 0);setq
       );if
       (setq #acet-cdorder-mode method);setq
 
       (setq lst (reverse lst))
       (acet-cdorder ss ans lst blk-ans method)
      );progn then
  );if
  (unload_dialog iv)
 );cond #5
);cond close
 
);defun acet-cdorder-dlg
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;flag=0=command line
;flag=1=dialog
;returns T if Yes and nil if No.
;
(defun acet-cdorder-warning ( flag )
 
(if (equal 0 flag)
    (progn
     (textscr)
     (princ "\n*WARNING*")
     (princ "\nThe \"Handle\" re-ordering option will result in ")
     (princ "\nnew object handles being assigned to selected objects.")
     (princ "\nThis option is only recomended for extreme cases where ")
     (princ "\nthe \"Draworder\" option is not a sufficient option.")
     (princ "\n(i.e. within some externally referenced (xref) drawings)")
     (princ "\nRe-assignment of object handles may cause problems such as: ")
     (princ "\n - Some 3rd party applications may no longer function properly.")
     (princ "\n - Selection sets saved via AUTOSSP may be lost.")
     (princ "\n - AutoCAD Group name data may be lost.")
     (princ "\n - AutoCAD hatch associativity may be lost.")
     (initget "Yes No")
     (setq flag (equal "Yes"
                        (getkword "\n\nDo you wish to continue anyway? <N>: ")
                )
     );setq
     (graphscr)
    );progn then
    (progn
     (acet-autoload '("yes_no.lsp" "(bns_get_yes_no a b)"))
     (if (equal (bns_get_yes_no
                  (list " WARNING "
                    (strcat
                     "\nThe \"Handle\" re-ordering option will result in new object handles"
                     "\nbeing assigned to selected objects. This option is only recomended "
                     "\nfor extreme cases where the \"Draworder\" option is not a sufficient "
                     "\noption. (i.e. within some externally referenced (xref) drawings)"
                     "\nRe-assignment of object handles may cause problems such as: "
                     "\n - Some 3rd party applications may no longer function properly."
                     "\n - Selection sets saved via AUTOSSP may be lost."
                     "\n - AutoCAD Group name data may be lost."
                     "\n - AutoCAD hatch associativity may be lost."
                     "\n\n\t\tDo you wish to continue anyway?"
                    );strcat
                  );list
                  '(60 15)
                );bns_get_yes_no
                0
         );equal
         (setq flag nil)
         (setq flag T)
     );if
    );progn else prompt with a dialog
);if
flag
);defun acet-cdorder-warning
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ask for color numbers at the command line.
(defun acet-cdorder-get-colors ( / a b c n lst lst2)
 
(setq a (acet-getvar '("ACET-CDORDER-COLORS")))
(if (not a)
    (setq a "")
);if
(if (not (equal a ""))
    (setq b (getstring (strcat "\nEnter color numbers separated by commas <"
                               a
                               ">: "
                       )
            )
    );setq then
    (setq b (getstring "\nEnter color numbers separated by commas: "))
);if
(if (equal b "")
    (setq b a)
);if
 
(if (and b
         (not (equal b ""))
         (setq lst (acet-str-to-list "," b))
    );and
    (progn
     ;loop through the colors and throw out invalid stuff.
     (setq c "")
     (setq n 0)
     (repeat (length lst)
     (setq a (nth n lst))
     (if (and (not (wcmatch a "*`.*"))
              (numberp (read a))
              (> (read a) 0)
              (< (read a) 255)
         );and
         (setq lst2 (cons a lst2)
                  c (strcat c "," a)
         );setq then
         (princ (strcat "\nIgnoring invalid input: " a))
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not lst2)
         (princ "\nNo valid input.")
         (progn
          (setq c (substr c 2))
          (if (not (equal c ""))
              (acet-setvar (list "ACET-CDORDER-COLORS" c 1))
          );if
         );progn else
     );if
    );progn then
);if
 (reverse lst2)
);defun acet-cdorder-get-colors
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;get default values
;
(defun acet-cdorder-init-defaults ( / )
 (if (and (not #acet-cdorder-mode)
          (not (setq #acet-cdorder-mode (acet-getvar '("ACET-CDORDER-MODE" 1))))
     );and
     (progn
      (setq #acet-cdorder-mode 0)
      (acet-setvar '("ACET-CDORDER-MODE" 0 1))
     );progn then
 );if
 
 (if (and (not #acet-cdorder-blocks)
          (not (setq #acet-cdorder-blocks (acet-getvar '("ACET-CDORDER-BLOCKS" 3))))
     );and
     (progn
      (setq #acet-cdorder-blocks 1)
      (acet-setvar '("ACET-CDORDER-BLOCKS" 1 3))
     );progn then
 );if
 
 (if (and (not #acet-cdorder-location)
          (not (setq #acet-cdorder-location (acet-getvar '("ACET-CDORDER-LOCATION" 3))))
     );and
     (progn
      (setq #acet-cdorder-location "Front")
      (acet-setvar '("ACET-CDORDER-LOCATION" "Front" 3))
     );progn then
 );if
 
);defun acet-cdorder-init-defaults
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Prompt for value for re-ordering mode.
;0=draworder
;1=data base re-order
;
(defun acet-get-cdorder-mode ( / a b )
 (acet-cdorder-init-defaults)
 (if (equal #acet-cdorder-mode 0)
     (setq a "Draworder")
     (setq a "Handles")
 );if
 (initget "Draworder Handles")
 (setq b (getkword (strcat "\nEnter ordering method [Draworder/Handles] <" a ">: ")))
 (if (not b)
     (setq b a)
 );if
 (if (equal b "Draworder")
     (setq #acet-cdorder-mode 0)
     (setq #acet-cdorder-mode 1)
 );if
 (acet-setvar (list "ACET-CDORDER-MODE" #acet-cdorder-mode 1))
 #acet-cdorder-mode
);defun acet-get-cdorder-mode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-cdorder ( ss ans lst blk-ans mode / pss mss na e1 bna blk-lst gcode
                                                cspace sp n a lst2 la
                   )
 
(if (and #acet-cdorder-location
         #acet-cdorder-blocks
         #acet-cdorder-mode
    );and
    (progn
     (acet-setvar (list "ACET-CDORDER-LOCATION" #acet-cdorder-location 3))
     (acet-setvar (list "ACET-CDORDER-BLOCKS" #acet-cdorder-blocks 3))
     (acet-setvar (list "ACET-CDORDER-MODE" #acet-cdorder-mode 1))
    );progn then
);if
(if (and (equal mode 0)
         (equal ans "Back")
    );and
    (setq lst (reverse lst))
);if
(setq la (acet-layer-unlock-all))
 
;split ss into two selection sets. One for model and the other paper.
;also create a list of unique block names if blk-ans=Yes.
(setq pss (ssadd)
      mss (ssadd)
        n 0
);setq
(repeat (sslength ss)
 (setq na (ssname ss n)
       e1 (entget na)
 );setq
 (if (equal 1 (cdr (assoc 67 e1)))
     (setq pss (ssadd na pss))
     (setq mss (ssadd na mss))
 );if
 (if (and (equal blk-ans "Yes")
          (equal "INSERT" (cdr (assoc 0 e1)))
          (setq bna (cdr (assoc 2 e1)))
          (not (member bna blk-lst))
          (setq na (tblobjname "block" bna))
          (setq e1 (entget na))
          (setq gcode (cdr (assoc 70 e1)))
          (not (equal 1 (logand 1 gcode)))
          (not (equal 4 (logand 4 gcode)))
          (not (equal 16 (logand 16 gcode)))
     );and
     (setq blk-lst (cons bna blk-lst));setq then
 );if
 (setq n (+ n 1));setq
);repeat
(if (equal (sslength mss) 0)
    (setq mss nil)
);if
(if (equal (sslength pss) 0)
    (setq pss nil)
);if
 
;find nested blocks and add them to blk-lst
(setq n 0)
(repeat (length blk-lst)
 (setq bna (nth n blk-lst))
 bns_blk_match
;; (setq lst2 (acet-block-match bna
 (setq lst2 (bns_blk_match bna
                           '((0 . "INSERT"))
                            (append (reverse (cdr (member bna (reverse blk-lst))))
                                    (cdr (member bna blk-lst))
                            );append
                            T
            );bns_blk_match
       lst2 (car lst2)
 );setq
 (while lst2
  (setq bna (cadr (car lst2)))
  (if (and (not (member bna blk-lst))
           (setq na (tblobjname "block" bna))
           (setq e1 (entget na))
           (setq gcode (cdr (assoc 70 e1)))
           (not (equal 1 (logand 1 gcode)))
           (not (equal 4 (logand 4 gcode)))
           (not (equal 16 (logand 16 gcode)))
      );and
      (setq blk-lst (append blk-lst (list bna)))
  );if
  (setq lst2 (cdr lst2))
 );while
(setq n (+ n 1));setq
);repeat
 
(if blk-lst
    (progn
     (princ "\nAdjusting draworder in block definitions...")
     (acet-cdorder-blocks ans lst blk-lst)
     (princ " Done.")
    );progn then
);if
 
 
(cond
 ((equal (getvar "tilemode") 1)
  (if mss
      (progn
       (princ "\nAdjusting draworder in model space...")
       (acet-cdorder-top-level ans lst mss)
       (princ " Done.")
      );progn
  );if
  (if pss
      (progn
       (setvar "tilemode" 0)
       (command "_.pspace")
       (princ "\nAdjusting draworder in paper space...")
       (acet-cdorder-top-level ans lst pss)
       (princ " Done.")
       (setvar "tilemode" 1)
      );progn
  );if
 );cond #1 tilemode=1
 ((equal (getvar "tilemode") 0)
  (if (and (setq sp (acet-viewport-next-pickable))
           (car sp)
      );and
      (progn
       (setq cspace (acet-cspace))
       (if mss
           (progn
            (princ "\nAdjusting draworder in model space...")
            (if (not (equal 0 cspace))
                (progn
                 (command "_.mspace")
                 (setvar "cvport" (car sp))
                );progn then go to model space
            );if
            (acet-cdorder-top-level ans lst mss)
            (princ " Done.")
           );progn the model space
       );if
       (if pss
           (progn
            (if (not (equal 1 (acet-cspace)))
                (command "_.pspace")
            );if
            (princ "\nAdjusting draworder in paper space...")
            (acet-cdorder-top-level ans lst pss)
            (princ " Done.")
           );progn then paper space
       );if
       (if (not (equal cspace (acet-cspace)))
           (progn
            (if (equal cspace 0)
                (progn
                 (command "_.mspace")
                 (setvar "cvport" (car sp))
                );progn then return to model space
                (command "_.pspace")
            );if
           );progn then go back to original space
       );if
      );progn then there is an available model space viewport so use it
      (progn
       (if mss
           (progn
            (setvar "tilemode" 1)
            (princ "\nAdjusting draworder in model space...")
            (acet-cdorder-top-level ans lst mss)
            (princ " Done.")
            (setvar "tilemode" 0)
           );progn the switch to model and do the deed.
       );if
       (if pss
           (progn
            (if (not (equal 1 (acet-cspace)))
                (command "_.pspace")
            );if
            (princ "\nAdjusting draworder in paper space...")
            (acet-cdorder-top-level ans lst pss)
            (princ " Done.")
            (setvar "tilemode" 1)
           );progn then
       );if
      );progn else no model space viewports were available so use tilemode instead
  );if
 );cond #2 tilemode=0
);cond close
 
(if la
    (command "_.-layer" "_lock" la "") ;re-lock all layers
);if
 
(if ss
    (command "_.select" ss "") ;restore the original previous selection set.
);if
 
(if (or (equal mode 0)
        (equal blk-ans "Yes")
    );or
    (command "_.regenall")
);if
 
);defun acet-cdorder
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-cdorder-ss-colors ( ss / na e1 a lst lst2 lst3 n b )
 
(princ "\nDetermining unique colors...")
 
;loop through the selection set and build 3 lists
; unique colors
; unique layers
; unique block names (for internal recursion use)
;
(setq n 0)
(repeat (sslength ss)
 (setq   na (ssname ss n)
         e1 (entget na)
          a (acet-cdorder-ent-color na e1 lst lst2 lst3)
        lst (car a)   ;the unique colors
       lst2 (cadr a)  ;the unique layer names
       lst3 (caddr a) ;the unique block names
 );setq
 
 (if (equal n (* 20 (/ n 20)))
     (acet-spinner)
 );if
 (setq n (+ n 1));setq
);repeat
 
(setq n 0)
(repeat (length lst2)
 (if (and (setq a (nth n lst2))
          (setq na (tblobjname "layer" a))
          (setq e1 (entget na))
          (setq a (cdr (assoc 62 e1)))
          (setq a (abs a))
          (not (member a lst))
     );and
     (setq lst (cons a lst))
 );if
 (setq n (+ n 1));setq
);repeat
 
;convert the integers to strings.
(setq lst2 nil)
(setq n 0)
(repeat (length lst)
 (setq    a (nth n lst)
          a (itoa a)
       lst2 (cons a lst2)
 );setq
 (setq n (+ n 1));setq
);repeat
 
;now put things in order as close as possible to last usage of cdorder.
(if (setq b (acet-getvar '("ACET-CDORDER-COLORS")))
    (setq lst (setq lst (acet-str-to-list "," b)));setq then
    (setq lst nil)
);if
;create a list of last used that are also part of this usage
(setq lst3 nil)
(setq n 0)
(repeat (length lst)
(setq a (nth n lst))
(if (member a lst2)
    (setq lst3 (append lst3 (list a)))
);if
(setq n (+ n 1));setq
);repeat
 
;add any colors used this time that were NOT used last time to the end.
(setq n 0)
(repeat (length lst2)
(setq a (nth n lst2))
(if (not (member a lst3))
    (setq lst3 (append lst3 (list a)))
);if
(setq n (+ n 1));setq
);repeat
 
(princ "Done.")
 
lst3
);defun acet-cdorder-ss-colors
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-cdorder-ent-color ( na e1 lst lst2 lst3 / a gcode bna )
 
(if (setq a (cdr (assoc 62 e1)))
    (progn
     (setq a (abs a))
     (if (not (member a lst))
         (setq lst (cons a lst))
     );if
    );progn then
    (progn
     (setq a (cdr (assoc 8 e1)))
     (if (not (member a lst2))
         (setq lst2 (cons a lst2))
     );if
    );progn else
);if
(if (and (equal "INSERT" (cdr (assoc 0 e1)))
         (setq gcode (cdr (assoc 70 e1)))
         (not (equal 1 (logand 1 gcode)))
         (not (equal 4 (logand 4 gcode)))
         (not (equal 16 (logand 16 gcode)))
         (setq bna (cdr (assoc 2 e1)))
         (not (member bna lst3))
         (setq na (tblobjname "block" bna))
         (setq e1 (entget na))
    );and
    (progn
     (setq lst3 (cons bna lst3))
     (acet-spinner)
     (while (setq na (entnext na))
      (setq   e1 (entget na)
               a (acet-cdorder-ent-color na e1 lst lst2 lst3)
             lst (car a)
            lst2 (cadr a)
            lst3 (caddr a)
      );setq
     );while
    );progn then
);if
 
(list lst lst2 lst3)
);defun acet-cdorder-ent-color
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-cdorder-blocks ( ans lst blk-lst / n a lst2 ss )
 
(setq lst2 blk-lst)
(setq n 0)
(repeat (length lst2)
(setq a (nth n lst2));setq
(command "_.insert" a "0,0" 1 1 0)
(command "_.explode" (entlast))
(while (wcmatch (getvar "cmdnames") "*EXPLODE*") (command ""))
(if (setq ss (ssget "_p"))
    (progn
     (setq ss (acet-cdorder-top-level ans lst ss))
     (command "_.block" a "_y" "0,0" ss "")
    );progn then
);if
(setq n (+ n 1));setq
);repeat
 
);defun acet-cdorder-blocks
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-cdorder-top-level ( ans lst ss2 / n a lst2 ss ) ;lst3
 
 (acet-cdorder-init-defaults)
 (setq n 0)
 (repeat (length lst)
 (setq a (nth n lst)
       a (atoi a)
 );setq
 (acet-spinner)
 (if (and (> a 0)
          (< a 255)
          (not (member a lst2))
          (setq ss (acet-get-color-selection-set a ss2))
     );and
     (progn
      (setq lst2 (cons a lst2)
            ;lst3 (cons ss lst3)
      );setq
      (setq ss2 (acet-cdorder-objects
                             ss                  ;selection set of the objects to change
                             ss2                 ;all other objects
                             ans                 ;Front or Back
                             #acet-cdorder-mode  ;draworder or data base order
                );acet-cdorder-objects
      );setq
     );progn then
 );if
 (setq n (+ n 1));setq
 );repeat
 
; (setq lst3 (reverse lst3))
; (if lst3
;     (progn
;
;      (setq n 0)
;      (repeat (length lst3)
;       (setq ss2 (acet-cdorder-objects
;                             (nth n lst3)        ;selection set of the objects to change
;                             ss2                 ;all other objects
;                             ans                 ;Front or Back
;                             #acet-cdorder-mode  ;draworder or data base order
;                 );acet-cdorder-objects
;       );setq
;      (setq n (+ n 1));setq
;      );repeat
;     );progn then
; );if
 
 ss2
);defun acet-cdorder-top-level
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-cdorder-objects ( ss ss2 ans mode / na )
 
 (if (and ss
          ss2
          ans
          mode
     );and
     (progn
      (acet-spinner)
      (cond
       ((equal mode 0)
        (command "_.draworder" ss "" (strcat "_" ans))
       );cond #1
       ((equal ans "Front")
        (setq na (entlast))
        (command "_.copy" ss "" "0,0" "0,0")
        (command "_.erase" "_p" "")
        (setq  ss (acet-ss-new na)
              ss2 (acet-ss-union (list ss2 ss))
        );setq
       );cond #3
      );cond close
     );progn then
 );if
 
ss2
);defun acet-cdorder-objects
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-get-color-selection-set ( c ss2 / a n lst lst2 ss )
 
(setq   c (abs c)
      lst (bns_tbl_match "layer"
                            (list '(-4 . "<OR")
                                    (cons 62 c)
                                    (cons 62 (* c -1))
                                  '(-4 . "OR>")
                            );list
          );bns_tbl_match
);setq
(setq n 0)
(repeat (length lst)
 (setq    a (nth n lst)
          a (cons 8 (cdr (assoc 2 a)))
       lst2 (cons a lst2)
 );setq
 (setq n (+ n 1));setq
);repeat
(if lst2
    (setq lst2 (append
                (list (cons 67 (acet-cspace))      ;in the current space
                      '(-4 . "<OR")
                        (cons 62 c)                ;it's the color we want
                        '(-4 . "<AND")
                          '(62 . 256) ;bylayer     ;or it's bylayer and on one of the layers
                          '(-4 . "<OR")            ;we want
                );list
                           lst2
                (list
                          '(-4 . "OR>")
                        '(-4 . "AND>")
                      '(-4 . "OR>")
                );list
              );append
    );setq
    (setq lst2 (list (cons 67 (acet-cspace))  ;in the current space
                     (cons 62 c)              ;proper color
               );
    );setq
);if
 
(command "_.select" ss2 "")
(setq ss (ssget "_p" lst2))
 
ss
);defun acet-get-color-selection-set
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CSPACE
;returns the current space. 1 for paper and 0 for model.
;This number matches the 67 space group code for entities.
;
(defun acet-cspace ( / cspace )
 
(if (and (equal (getvar "tilemode") 0)
         (equal (getvar "cvport") 1)
    );and
    (setq cspace 1);then paper space is where we are.
    (setq cspace 0);else model space.
);if
cspace
);defun acet-cspace


(acet-autoload2	'("Yes_no.lsp"	(bns_get_yes_no lst size)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCNdXk+1r1dxGzpWCZuDngcM14+abl7rHh4HuVx
;;; rceSVojwLftQCtsgfk7uL0hp6hedEDH0KPY4LnRyM9t5L3D3GnSjBq8GtJdZYOMy
;;; O96mCZBh4z8iyT1azD09N1rtl/eT2s8BmNol2nqowGG2O6tFq1YXp/7uy422Xflc
;;; OiUb0pS32Lv4/VHYQJkavGvjPWVSzXGhyF4555mvVo4CBmSO/JAGazhY9Vbjs7lV
;;; 2qL4qADwJHf6jKHVL093LQ9YWef5XL4dtMnq71peDcTK6ibv/lED4i/+2BjsuOby
;;; kNTOdcRW70optjc0ebhwaywaiBSWDWnAEIvGetI2s2Ng1qFAMF0uGaVseMHF+m1W
;;; 6STar/Mz/CAgUUv3gxdj0ZgNydYyhVA27JuzWc8bzLaFh9+LPFNuVVQ7VC+fMMMA
;;; 4iSpn5UNGP5+iugUFarNMQQSyvbfbXaORACaxToIZz3gheH0rax6b9azsBDpMNt3
;;; owCe/WFZMLZ6xOXuTT/5IXMR62mTdnG030I3IpuQ3rEMJ1XOQxxJdRg3aTh86QZ7
;;; /JutT6CIfsSLlBJwRP/szjOCXRhCcihqh0xjPib6hBCqdO3KZo2b4RaiWtHei456
;;; Qvkwo0yZgSTEQchlAHskMDR/F1mU1CoweRqGhKlMtQWHhQP6WKI9H55oXRluS9/8
;;; q+nGkA==
;;; -----END-SIGNATURE-----