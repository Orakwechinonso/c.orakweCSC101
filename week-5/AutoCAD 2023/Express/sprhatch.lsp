;;;
;;;    SPRHATCH.LSP
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
 
 
;TFRAMES - Toggles Image and Wipeout frames on and off.
;
(defun c:tframes ( / e1 e2 status)
(acet-error-init
 (list (list "cmdecho" 0);list
       T
 );list
);acet-error-init
(setq e1 (dictsearch (namedobjdict) "ACAD_IMAGE_VARS"));setq
(setq e2 (dictsearch (namedobjdict) "ACAD_WIPEOUT_VARS"));setq
(if (or e1 e2)
    (progn
     (if (and e1
              (equal (cdr (assoc 70 e1)) 1)
         );and
         (setq status "_OFF")
         (progn
          (if e1
              (setq status "_ON")
              (progn
               (if (and e2
                        (equal (cdr (assoc 70 e2)) 1)
                   );and
                   (setq status "_OFF")
                   (progn
                    (if e2
                        (setq status "_ON")
                    );if
                   );progn
               );if
              );progn
          );if
         );progn
     );if
     (if status
         (progn
          (if (= status "_ON")
            (command "_.imageframe" "1")
            (command "_.imageframe" "0")
          );if status
          (command "_.wipeout" "_f" status)
          (princ (acet-str-format "\nIMAGE/WIPEOUT frames are toggled %1."  (substr status 2)))
         );progn then
     );if
    );progn then
    (princ "\nNo images or wipeouts in the current drawing.")
);if
(acet-error-restore)
);defun c:tframes
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun c:Superhatch ( / flag lla htype ss lst2
                    )
 
(acet-error-init
 (list (list   "cmdecho" 0
             "highlight" 0
                "osmode" 0
             "orthomode" 0
             "plinetype" 1
               "ucsicon" 0
               "cecolor" nil
               "celtype" nil
             "regenmode" 1
                "luprec" nil
             "ucsfollow" 0
              "limcheck" 0
              "plinewid" 0
             "pickstyle" 0
       );list
       T
 );list
);acet-error-init
(sssetfirst nil nil)
 
(bns_sprhatch_init_overlap)
 
(acet-arxload-or-bust "acetutil.arx")
(acet-autoload (list "clipit.lsp" "(c_clipit a b)"))
(acet-autoload (list "clipit.lsp" "(wipeout_clipit a b)"))
(acet-autoload (list "ddins2.lsp" "(ddins2)"))
(acet-autoload (list "ai_utils.lsp" "(ai_rtos a)"))
(acet-autoload (list "ai_utils.lsp" "(ai_num a b c)"))
 
 
(reg_it2);ensure that sprhatch is registered as an xdata app
 
 
(setq flag (acet-viewport-next-pickable))
(if (and (not (equal 1 (getvar "cvport")))
         (not (equal (car flag) (getvar "cvport")))
    );and
    (progn
     (princ "\n  That command may not be invoked in a perspective view  ")
    );progn then jump out of this command cuz perspective view is on.
    (progn
     (setq  lla (acet-layer-unlock-all))
     (setq htype (htype_info))
     ;make absolutely sure that these vars are set.
     (setvar    "cmdecho" 0)
     (setvar  "highlight" 0)
     (setvar     "osmode" 0)
     (setvar  "orthomode" 0)
     (setvar  "plinetype" 1)
     (setvar    "ucsicon" 0)
     (setvar  "regenmode" 1)
    );progn else all is good
);if
 
(if (and htype
         (setq ss (get_boundary_plines))
         (car (setq lst2 (do_boundary_stuff #clipitres ss)))
    );and
    (progn
 
     (sprhatch #sprhatch_type (car htype) (cadr htype) (caddr htype) lst2)
     (cond
      ((equal #sprhatch_type "Image")
       (princ "\nUse TFRAMES to toggle object frames on and off.")
      );cond #1 image
      ((equal #sprhatch_type "Wipeout")
       (princ "\nUse TFRAMES to toggle object frames on and off.")
      );cond #2 Wipeout
     );cond close
    );progn then go for it
    (progn
     (if htype
         (progn
          (if (car htype)
              (entdel (car htype))
          );if
          (if (not ss)
              (progn
               (acet-ss-clear-prev)
              );progn then
          );if
         );progn then
     );if
    );progn else the boundary stuff failed to create polyline boundaries.
);if
(if lla
    (progn
     (command "_.-layer" "_lock" lla "")
     (while (wcmatch (getvar "cmdnames") "*LAYER*")  (command ""));while
    );progn then re-lock the layers that were originally locked
);if
 
(acet-error-restore)
);defun c:superhatch
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reg_it2 ( )
 (if (not (tblsearch "appid" "BNS_SPRHATCH"))
     (if (=  (regapp "BNS_SPRHATCH") nil)
         (princ "\nCan't register XDATA for BNS_SPRHATCH.")
     );if
 );if
);defun reg_it2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun htype_info ( / htype na flag flag2)
 
(while (not flag2)
(if (setq htype (dd_htype_info))
    (progn
     (setq    na (cadr htype)
           htype (car htype)
     );setq
     (cond
      ((or (equal htype "Block")
           (equal htype "Xref")
       );or
       (setq flag (get_insert_info na htype));setq entname, rectang and xtlist
      );cond #1
      ((equal htype "Image")
       (setq flag (get_image_info na))
       (if flag
           (setq flag2 T)
       );if
      );cond #2
      ((equal htype "Wipeout")
       (setq  flag (list nil nil)
             flag2 T
       );setq
      );cond #3
     );cond
     (if flag
         (setq flag2 T);then the user did not create an image, block, or xref.
     );if
    );progn then
    (setq flag2 T)
);if
);while
(if (car flag)
    (progn
     (acet-ss-visible (ssadd (car flag) (ssadd)) 1);then make it stealth
     (princ)
    );progn
);if
flag
);defun htype_info
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dd_htype_info (  / str na luprec iv flag na2 a)
 
(setq luprec (getvar "luprec"))
(setvar "luprec" 8)
(setq flag -1)
(setq iv T)
(while (and iv
            (equal flag -1)
       );and
 (if (> (setq iv (load_dialog (findtrustedfile "sprhatch.dcl")));setq
        0
     );test
     (progn
      (if (new_dialog "sprhatch" iv)
          (progn
           ;get some defaults ready and initialize some tiles.
           (if (not #sprhatch_type) (setq #sprhatch_type "Image"));if
 
           (mode_tile (strcase #sprhatch_type T) 2)
           (if #clipitres
               (setq a (ai_rtos #clipitres))
               (setq a (ai_rtos (acet-geom-pixel-unit)));setq
           );if
           (set_tile "clipitres" a)
 
           (action_tile "clipitres" "(check_clipitres nil)")
 
           (setq str "(if (check_clipitres T) (done_dialog 1))")
           (action_tile "image"
                        (strcat "(setq #sprhatch_type \"Image\")" str);strcat
           );action_tile
           (action_tile "block"
                        (strcat "(setq #sprhatch_type \"Block\")" str)
           );action_tile
           (action_tile "xref"
                        (strcat "(setq #sprhatch_type \"Xref\")" str)
           );action_tile
           (action_tile "wipeout"
                        (strcat "(setq #sprhatch_type \"Wipeout\")" str)
           );action_tile
 
           (action_tile "select" "(if (check_clipitres T) (done_dialog -1))")
 
           (action_tile "accept" str)
           (action_tile "cancel" "(done_dialog 0)")
           (action_tile "help" "(acet-help \"SUPERHATCH\")")
 
           (setq flag (start_dialog));setq ;START_DIALOG MAKES THE BUTTONS ACTIVE
 
          );progn then initialize the tiles and activate the dialog box
          (alert "Unable to display dialog box")
      );if new dialog
      (unload_dialog iv);unload it when done
     );progn then
     (alert "Unable to load dialog box");else
 );if load
 (if (equal flag -1)
     (setq flag (sel_existing)
             na (cadr flag)
           flag (car flag)
     );setq
 );if
);while
(setvar "luprec" luprec)
 
(if (equal flag 1)
    (setq flag (list #sprhatch_type na));setq
    (setq flag nil)
);if
 
flag
);defun dd_htype_info
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check_clipitres ( flag / a range errmsg)
 (setq range 6)                 ; non zero & non negative
 (setq errmsg "Value must be positive & nonzero.")
 (setq a (ai_num (get_tile "clipitres") errmsg range))
 (if (and flag
          (not a)
     );and
     (mode_tile "clipitres" 2)
     (progn
      (if (and flag a)
          (setq #clipitres a)
     );if
     );progn
 );if
 a
);defun check_clipitres
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sel_existing ( / flag flt e1 bna na na2 e2)
  (setq flt (list '(-4 . "<OR")
                   '(-4 . "<AND")
                     '(0 . "INSERT")
                     '(-4 . "<NOT")
                       '(2 . "` ")
                     '(-4 . "NOT>")
                   '(-4 . "AND>")
                 '(0 . "IMAGE")
                 '(0 . "WIPEOUT")
                 '(-4 . "OR>")
           );list
 );setq
  (if (setq na (acet-ui-single-select flt nil))
     (progn
      (setq e1 (entget na '("*"))
            e1 (acet-list-assoc-remove 330 e1)
            e1 (acet-list-assoc-remove -1 e1)
            e1 (acet-list-assoc-remove 5 e1)
      );setq
      (if (not (equal "INSERT" (cdr (assoc 0 e1))))
          (entmake e1)         ;then entmake a copy of the selected object
          (progn
           (entmake e1)         ;entmake a copy of the selected insert
           (if (equal 1 (cdr (assoc 66 e1)))
               (progn
                (setq na (entnext na)
                      e1 (entget na)
                );setq
                (while (and na
                            (not (wcmatch (cdr (assoc 0 e1)) "*END*"))
                       );and
                 (entmake e1)
                 (setq na (entnext na)
                       e1 (entget na)
                 );setq
                );while
                (entmake e1)
               );progn then
           );if
          );progn else it's an insert
      );if
      (setq na (entlast)
            e1 (entget na)
      );setq
      (cond
       ((equal "INSERT" (cdr (assoc 0 e1)))
        (setq bna (cdr (assoc 2 e1))
              na2 (tblobjname "block" bna)
               e2 (entget na2)
        );setq
        (if (equal 4 (logand 4 (cdr (assoc 70 e2))))
            (setq #sprhatch_type "Xref")
            (setq #sprhatch_type "Block")
        );if
 
        (command "_.xclip" na "" "_d")
        (while (wcmatch (getvar "cmdnames") "*XCLIP*") (command nil))
       );cond #1
       ((equal "IMAGE" (cdr (assoc 0 e1)))
        (setq #sprhatch_type "Image")
        (command "_.imageclip" na "_d")
        (while (wcmatch (getvar "cmdnames") "*IMAGECLIP*") (command nil))
       );cond #2
       ((equal "WIPEOUT" (cdr (assoc 0 e1)))
        (setq #sprhatch_type "Wipeout")
       );cond #3
      );cond close
      ;(setq flag (list 1 na na3))
      (setq flag (list 1 na nil))
     );progn then
     (setq flag (list -1))
 );if
 flag
);defun sel_existing
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_insert_info ( na2 htype / na na3 fna bna blk lst2 xtlst
                                     uflag zflag ans vpna vplocked
                       )
 
(setq na3 na2
       na (entlast)
);setq
(setvar_rt)
(setq ans "No")
(while (equal ans "No")
 (setq ans nil);setq
 (if (equal htype "Block")
     (progn
      (if (not na2)
          (progn
           (ddins2)
            (while (wcmatch (getvar "cmdnames") "*INSERT*")
             (command pause)
            );while
            (if (not (equal na (entlast)))
                (setq na2 (entlast));setq
                (setq na2 nil);setq
           );if
          );progn
      );if
     );progn
     (progn
      (if (not na2)
          (progn
                (command "_.xattach" "~")
                (setvar "cmdecho" 1)
                (if (wcmatch (getvar "cmdnames") "*XATTACH*")
                    (princ "\nSpecify insertion point or [Scale/X/Y/Z/Rotate/PScale/PX/PY/PZ/PRotate]: ")
                );if
                (while (wcmatch (getvar "cmdnames") "*XATTACH*") (command pause));while
                (setvar "cmdecho" 0)
                (if (not (equal na (entlast)))
                    (progn
                     (setq na2 (entlast));setq
                     ;(setq #sprhatch_xref fna)
                    );progn then
                    (setq na2 nil)
                );if
          );progn
      );if
     );progn else
 );if
 
 (if na2
     (progn
      (setq uflag T)
      (acet-ucs-cmd (list "_ob" na2))
      (setq xtlst (getblkextents na2));setq
      (if (not xtlst)
          (progn
           (princ "\nCould not obtain object extents. Possibly due to RAY or XLINE object.")
           (setq ans "No")
           (entdel na2)
           (setq na2 nil)
          );progn then reject this insert.
      );if
     );progn then
 );if
 (if (and (not na3)
          na2
     );and
     (progn
      (if (setq vpna (acet-currentviewport-ename))
          (setq vplocked (acet-viewport-lock-set vpna nil))
      );if
      (if (setq zflag (acet-geom-zoom-for-select xtlst))
          (command "_.zoom" "_w" (car zflag) (cadr zflag))
      );if
      (if uflag
          (progn
           (acet-ucs-cmd (list "_p"))
           (setq uflag nil)
          );progn then
      );if
      (initget "Yes No _Yes No")
      (setq ans (getkword
                 (acet-str-format "\nIs the placement of this %1 acceptable? {Yes/No] <Yes>: "  (xstrcase htype))
                );getkword
      );setq
      (if (equal ans "No")
          (progn
           (entdel na2)
           (setq na2 nil)
          );progn then
      );if
      (if zflag (command "_.zoom" "_p"))
      (if vplocked 
          (acet-viewport-lock-set vpna T)
      );if
     );progn then
 );if
 (if uflag
     (progn
      (acet-ucs-cmd (list "_p"))
      (setq uflag nil)
     );progn then
 );if
);while
(setvar "osmode" 0)
(if na2
    (setq  lst2 (get_insert_rectang na2 htype xtlst)
          xtlst (cadr lst2)
           lst2 (car lst2)
    );setq then got an insert
);if
(if lst2
    (list na2 lst2 xtlst)
    nil
);if
);defun get_insert_info
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;function resets cecolor and osmode back to original settings
(defun setvar_rt ()
 (if (assoc "CECOLOR" (car acet:sysvar-list))
     (setvar "cecolor" (cadr (assoc "CECOLOR" (car acet:sysvar-list))))
 );if
 (if (assoc "CELTYPE" (car acet:sysvar-list))
     (setvar "CELTYPE" (cadr (assoc "CELTYPE" (car acet:sysvar-list))))
 );if
 (if (assoc "OSMODE" (car acet:sysvar-list))
     (setvar "osmode" (cadr (assoc "OSMODE" (car acet:sysvar-list))))
 );if
);defun setvar_rt
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_insert_rectang ( na htype xtlst / e1 deflst na2 na3 na4 p1 p2
                                       xtlst2 lst2 n a lst zflag vpna vplocked
                          )
 
 (redraw na 3)
 (setvar "osmode" 0)
 (setvar "celtype" "continuous")
 (setq e1 (entget na))
 (acet-ucs-cmd (list "_ob" na))
 (if (setq deflst (getxdata_defpnts na))
     (progn
      (setq deflst (append deflst (list (car deflst)))
            deflst (scale_pnts_xyz deflst
                                   '(0.0 0.0 0.0)
                                   (cdr (assoc 41 e1))
                                   (cdr (assoc 42 e1))
                                   (cdr (assoc 43 e1))
                   );scale_pnts_xyz
      );setq
     );progn then use a default rectang from a previous time.
     (setq deflst xtlst);setq else use extents of the insert.
 );if
 (if (setq vpna (acet-currentviewport-ename))
     (setq vplocked (acet-viewport-lock-set vpna nil))
 );if
 (if (setq zflag (acet-geom-zoom-for-select deflst))
     (command "_.zoom" "_w" (car zflag) (cadr zflag))
 );if
 (command "_.rectang" "_f" 0.0 "_w" (* 1.0 (acet-geom-pixel-unit)) "_th" 0.0 nil)
 
 (princ
  (acet-str-format "\nSelect a window around the %1 to define column and row tile distances."  (strcase htype T))
 );princ
 (setq p1 T)
 (while p1
  (setq na2 nil
        na3 nil
  );setq
 
  (setvar "osmode" 0) (setvar "cecolor" "6") (setvar "celtype" "continuous")
  (command "_.rectang" (car deflst) (caddr deflst));Draw a magenta rectang for visual def.
  (setq na4 (entlast))
 
  (setvar_rt)
  (initget "Extents _Extents")
  (setq p1 (getpoint
                     (acet-str-format "\nSpecify %1 [Extents] First corner <magenta rectang>: " (strcase htype T))
           );getpoint
  );setq
  (setvar "osmode" 0) (setvar "cecolor" "6") (setvar "celtype" "continuous")
 
  (if (and p1
           (equal (type p1) 'LIST)
      );and
      (progn
       (setq na3 (entlast))
       (command "_.rectang" p1)
       (setvar_rt)
       (while (wcmatch (getvar "cmdnames") "*RECTANG*")
        (princ "Other corner: ")
        (command pause)
       );while
       (setvar "osmode" 0) (setvar "cecolor" "6") (setvar "celtype" "continuous")
       (setq na2 (entlast))
      );progn then
      (progn
       (if (equal p1 "Extents")
           (setq deflst xtlst);setq
       );if
      );progn
  );if
 
  (if (and (not (equal na3 na2))
           p1
      );and
      (progn
       (entdel na2)
       (setq   p2 (getvar "lastpoint")
               p1 (acet-geom-list-extents (list p1 p2))
               p2 (cadr p1)
               p1 (car p1)
             lst2 (list (list (car p1) (cadr p1) (getvar "elevation"))
                        (list (car p2) (cadr p1) (getvar "elevation"))
                        (list (car p2) (cadr p2) (getvar "elevation"))
                        (list (car p1) (cadr p2) (getvar "elevation"))
                        (list (car p1) (cadr p1) (getvar "elevation"))
                  );list
             lst2 (plst_round lst2 0.00001)  ;to avoid possible osnap round off error
           xtlst2 (plst_round xtlst 0.00001) ;if user snaps to magenta rectang
       );setq
 
       (if (and (>= (car  (car xtlst2)) (car  (car lst2)))
                (>= (cadr (car xtlst2)) (cadr (car lst2)))
                (<= (car  (car xtlst2)) (car  (caddr lst2)))
                (<= (cadr (car xtlst2)) (cadr (caddr lst2)))
                (>= (car  (caddr xtlst2)) (car  (car lst2)))
                (>= (cadr (caddr xtlst2)) (cadr (car lst2)))
                (<= (car  (caddr xtlst2)) (car  (caddr lst2)))
                (<= (cadr (caddr xtlst2)) (cadr (caddr lst2)))
           );and
           (progn
            (setq deflst lst2);setq else
           );progn then
           (progn
            (setq lst2 nil)
            (princ
              (acet-str-format "\n*Invalid* Window must fully contain the %1."  (strcase htype T))
            );princ
           );progn else
       );if
      );progn then got a rectang
      (setq lst2 deflst);setq else use default
  );if
  (if (and lst2
           (or (equal (car (car lst2)) (car (caddr lst2)) 0.0001)
               (equal (cadr (car lst2)) (cadr (caddr lst2)) 0.0001)
           );or
      );and
      (progn
       (princ "\n*Invalid* Selected window has no area with respect to the current ucs.")
       (setq p1 T)
      );progn then
  );if
  (if (and p1
           lst2
      );and
      (setq deflst lst2)
  );if
  (entdel na4)
 );while selected rectang is invalid or has not been accepted.
 (setvar_rt) (setvar "osmode" 0)
 
 (set_insert_defs lst2 na)
 
 (setq  lst2 (acet-geom-m-trans lst2 1 0)
       xtlst (acet-geom-m-trans xtlst 1 0)
 );setq
 (acet-ucs-cmd (list "_p"))
 (setq  lst2 (acet-geom-m-trans lst2 0 1)
       ;lst2 (plst_round lst2 0.000001)   ;@Rk removed 6:28 PM 9/24/97
       xtlst (acet-geom-m-trans xtlst 0 1)
       ;xtlst (plst_round xtlst 0.000001) ;@Rk removed 6:28 PM 9/24/97
 );setq
 (setq n 0)
 (repeat (length xtlst)
 (setq   a (nth n xtlst)
         a (list (car a)
                 (cadr a)
                 ;(getvar "elevation")   ;@Rk removed 6:28 PM 9/24/97
           );list
       lst (append lst (list a))
 );setq
 (setq n (+ n 1))
 );repeat
 (setq xtlst lst
         lst nil
 );setq
 (setq n 0)
 (repeat (length lst2)
 (setq   a (nth n lst2)
         a (list (car a) (cadr a) (getvar "elevation"))
       lst (append lst (list a))
 );setq
 (setq n (+ n 1))
 );repeat
 (setq lst2 lst)
 
(redraw na 4)
(if zflag
    (command "_.zoom" "_p")
);if
(if vplocked
    (acet-viewport-lock-set vpna T) ;re-lock the viewport
);if
 
(command "_.rectang" "_f" 0.0 "_w" 0.0 "_th" 0.0 nil)
 
(list lst2 xtlst)
);defun get_insert_rectang
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set_insert_defs ( lst na / e1 a b c)
(setq  e1 (entget na)
        a (/ 1.0 (cdr (assoc 41 e1)))
        b (/ 1.0 (cdr (assoc 42 e1)))
        c (/ 1.0 (cdr (assoc 43 e1)))
      lst (scale_pnts_xyz lst
                          '(0.0 0.0 0.0)
                          a b c
          );scale_pnts_xyz
);setq
(setxdata_defpnts na
                  lst
);setxdata_defpnts
 
);defun set_insert_defs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getxdata_defpnts ( na / e1 x lst )
 
(setq e1 (entget na '("BNS_SPRHATCH")));setq
(if (setq x (cadr (assoc -3 e1)));setq
    (progn
     (setq x (cdr x))
     (while x
      (setq lst (append lst (list (cdr (car x))))
              x (cdr x)
      );setq
     );while
    );progn then
);if
 
lst
);defun getxdata_defpnts
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setxdata_defpnts ( na lst / e1 x )
 
(reg_it2)
(setq e1 (entget na)
       x (list -3
               (list "BNS_SPRHATCH"
                     ;(cons 1001 "SPRHATCH_DEFPNTS")
                     (cons 1010 (car lst))
                     (cons 1010 (cadr lst))
                     (cons 1010 (caddr lst))
                     (cons 1010 (cadddr lst))
               );list
         );list
);setq
(if (assoc -3 e1)
    (setq e1 (subst x (assoc -3 e1) e1));setq then
    (setq e1 (append e1 (list x)));setq else
);if
(entmod e1)
 
);defun setxdata_defpnts
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scale_pnts_xyz ( lst bspnt a b c / lst2 n p1)
 
(setq n 0)
(repeat (length lst)
(setq   p1 (nth n lst)
        p1 (list (* a (car p1))
                 (* b (cadr p1))
                 (* c (caddr p1))
           );list
      lst2 (append lst2 (list p1))
);setq
(setq n (+ n 1));setq
);repeat
 
lst2
);defun scale_and_rotate_pnts
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plst_round ( lst rnd / n a b lst2)
 
(setq n 0)
(repeat (length lst)
(setq a (nth n lst)
      b nil
);setq
(while a
(setq b (append b (list (acet-calc-round (car a) rnd)))
      a (cdr a)
);setq
);while
(setq lst2 (append lst2 (list b)))
(setq n (+ n 1));setq
);repeat
lst2
);defun plst_round
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;getblkextents takes an entity name of a block insert.
;returns a list of two points. (lower left and upper right corners)
(defun getblkextents ( na / shift bspnt a b d e1 e2 na2 na3 lst p1 p2)
 
 (defun shift ( e1 bspnt a d / a2 p1 p2)
  (if (setq a2 (cdr (assoc 50 e1)))
      (setq a2 (- a2 a));setq
  );if
  (if (setq p1 (cdr (assoc 10 e1)))
      (setq p1 (acet-geom-point-rotate p1 bspnt (* -1.0 a))
            p1 (list (- (car p1) (car d))
                     (- (cadr p1) (cadr d))
                     (- (caddr p1) (caddr d))
               );list
      );setq
  );if
  (if (setq p2 (cdr (assoc 11 e1)))
      (setq p2 (acet-geom-point-rotate p2 bspnt (* -1.0 a))
            p2 (list (- (car p2) (car d))
                     (- (cadr p2) (cadr d))
                     (- (caddr p2) (caddr d))
               );list
      );setq
  );if
  (setq e1 (subst (cons 50 a2) (assoc 50 e1) e1)
        e1 (subst (cons 10 p1) (assoc 10 e1) e1)
        e1 (subst (cons 11 p2) (assoc 11 e1) e1)
        e1 (subst '(210 0.0 0.0 1.0) (assoc 210 e1) e1)
  );setq
  e1
 );defun shift
 
;;(entmake nil)
(entmake)
;;(entmake nil)
(entmake)
(setq na3 (entlast)
       e1 (entget na)
        b (cdr (assoc 2 e1))
        a (cdr (assoc 50 e1))
        d (cdr (assoc 10 e1))
    bspnt d
       e1 (shift e1 bspnt a d)
);setq
(if (assoc 60 e1)
    (setq e1 (subst (cons 60 1) (assoc 60 e1) e1));setq
    (setq e1 (append e1 (list (cons 60 1))));setq else
);if
 
(entmake e1)
 
(if (equal 1 (cdr (assoc 66 e1)))
    (progn
     (setq na2 (entnext na)
            e2 (entget na2)
            e2 (shift e2 bspnt a d)
     );setq
     (while (and na2
                 (not (wcmatch (cdr (assoc 0 e2)) "*END*"))
            );and
      (entmake e2)
      (setq na2 (entnext na2)
             e2 (entget na2)
             e2 (shift e2 bspnt a d)
      );setq
     );while
     (entmake (entget na2))
    );progn then the insert has attributes
);if
 
(if (not (equal na3 (entlast)))
    (progn
     (setq lst (acet-ent-geomextents (entlast)));setq
     (entdel (entlast))
 
     (if (and lst
              (setq b (entget (tblobjname "block" b)))
              (assoc 3 b)
              (not (equal (cdr (assoc 3 b)) ""))
              ;(equal 4 (logand 4 (cdr (assoc 70 b))))
         );and
         (progn
          (setq   b (cdr (assoc 10 b))
                  b (list (- (car b)) (- (cadr b)) (- (caddr b)))
          );setq
          (setq lst (list (acet-geom-vector-add (car lst) b)
                          (acet-geom-vector-add (cadr lst) b)
                    );list
          );setq
         );progn then adjust for insbase
     );if
 
     (if lst
         (progn
          (setq  p1 (car lst)
                 p2 (cadr lst)
                lst (list p1
                          (list (car p2) (cadr p1) (caddr p1))
                          (list (car p2) (cadr p2) (caddr p1))
                          (list (car p1) (cadr p2) (caddr p1))
                          p1
                    );list
          );setq
          (acet-ucs-cmd (list "_ob" na))
          (setq lst (acet-geom-m-trans lst 1 0));setq
          (acet-ucs-cmd (list "_p"))
          (setq lst (acet-geom-m-trans lst 0 1));setq
         );progn then
     );if
 
    );progn
);if
 
lst
);defun getblkextents
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_image_info ( na / zflag ans lst2 e1 a b p1 p2 p3 p4 vpna vplocked)
(if na
    (setq lst2 (acet-geom-image-bounds na))
    (progn
     (setvar_rt)
     (setq ans "No")
     (while (equal ans "No")
      (setq ans nil);setq
      (setq na (entlast))
      (command "_.imageattach" "~")
      (setvar "cmdecho" 1)
      (if (wcmatch (getvar "cmdnames") "*IMAGE*")
          (princ "\nInsertion point <0,0>: ")
      );if
      (while (wcmatch (getvar "cmdnames") "*IMAGE*")
       (command pause)
      );while
      (setvar "osmode" 0)
      (setvar "cmdecho" 0)
      (if (not (equal na (entlast)))
          (progn
           (if (setq vpna (acet-currentviewport-ename))
               (setq vplocked (acet-viewport-lock-set vpna nil))
           );if
           (setq na (entlast));setq
           (setq lst2 (acet-geom-image-bounds na))
           (if (setq zflag (acet-geom-zoom-for-select lst2))
               (command "_.zoom" "_w" (car zflag) (cadr zflag))
           );if
 
           (initget "Yes No _Yes No")
           (setq ans (getkword "\nIs the placement of this IMAGE acceptable? [Yes/No] <Yes>: "))
           (if (equal ans "No")
               (progn
                (entdel na)
                (setq lst2 nil)
               );progn
           );if
           (if zflag (command "_.zoom" "_p"))
           (if vplocked
               (acet-viewport-lock-set vpna T)
           );if
          );progn then got an image
      );if
     );while
    );progn else
);if
(if lst2
    (progn
     (if (not (equal 0.0 #bns_image_overlap))
         (setq  e1 (entget na)  ;we are going to adjust by 2 percent of an image pixel.
                 a (trans (cdr (assoc 11 e1)) 0 1 T)
                 b (trans (cdr (assoc 12 e1)) 0 1 T)
                ;a (acet-geom-vector-scale a 0.01)
                ;b (acet-geom-vector-scale b 0.01)
                 a (acet-geom-unit-vector '(0.0 0.0 0.0) a)
                 b (acet-geom-unit-vector '(0.0 0.0 0.0) b)
                 a (acet-geom-vector-scale a #bns_image_overlap)
                 b (acet-geom-vector-scale b #bns_image_overlap)
                p1 (car lst2)
                p2 (cadr lst2)
                p3 (caddr lst2)
                p4 (cadddr lst2)
                p1 (acet-geom-vector-add p1 a)
                p1 (acet-geom-vector-add p1 b)
                p2 (acet-geom-vector-add p2 (acet-geom-vector-scale a -1.0))
                p2 (acet-geom-vector-add p2 b)
                p3 (acet-geom-vector-add p3 (acet-geom-vector-scale a -1.0))
                p3 (acet-geom-vector-add p3 (acet-geom-vector-scale b -1.0))
                p4 (acet-geom-vector-add p4 a)
                p4 (acet-geom-vector-add p4 (acet-geom-vector-scale b -1.0))
              lst2 (list p1 p2 p3 p4 p1)
         );setq then
     );if
     (list na lst2)
    );progn then
    nil
);if
);defun get_image_info
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_sprhatch_init_overlap ()
 (if (not #bns_image_overlap)
     (progn
      (setq #bns_image_overlap (acet-getvar '("BNS_SPRHATCH_IMAGEOVERLAP"))
      );setq get it from dwg or reg
      (if (not #bns_image_overlap)
          (progn
           (setq #bns_image_overlap 0.0)
           (acet-setvar '("BNS_SPRHATCH_IMAGEOVERLAP" 0.0 3));dwg and the registry
          );progn then
      );if
     );progn then
 );if
);defun bns_sprhatch_init_overlap
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:imageoverlap ( / a)
 
(acet-error-init (list nil nil))
 
(bns_sprhatch_init_overlap)
 
(setq a (getdist
          (acet-str-format "\nEnter overlap distance for image tiling in superhatch <%1>: " (ai_rtos #bns_image_overlap))
        );getdist
);setq
(if a
    (progn
     (setq #bns_image_overlap (abs a))
     (acet-setvar '("BNS_SPRHATCH_IMAGEOVERLAP" #bns_image_overlap 3));dwg and the registry
    );progn then
);if
 
(acet-error-restore)
);defun c:imageoverlap
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_adjust_for_pixel_roundoff ( e1 lst / na )
 
(if (not #bns_image_overlap)
    (setq #bns_image_overlap 0.0)
);if
(if (not (equal 0.0 #bns_image_overlap))
    (progn
     (acet-lwpline-make (list (list (cons 60 1))
                        lst
                  )
     );acet-lwpline-make-make
     (setq na (entlast))
     (command "_.offset"
              #bns_image_overlap
              na
              (polar (car (acet-geom-list-extents lst)) pi 1.0)
              ;(polar (car lst)
              ;       0
              ;       (* 2.0 (distance (getvar "extmin") (getvar "extmax")))
              ;)
              ""
     );command
     (while (not (equal "" (getvar "cmdnames"))) (command nil));while
     (entdel na)
     (setq na (entlast));setq
     (acet-ss-visible (ssadd na (ssadd)) 1)
     (setq lst (acet-geom-vertex-list na))
     (entdel na)
    );progn then
);if
lst
);defun bns_adjust_for_pixel_roundoff
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sprhatch ( htype na2 rlst xtlst lst2 / lst p1 p2 p3 xt1 xt2 x1 zflag ss2
                                              lst3 n lst4 lst5 ss3 j na3 e2 vpna vplocked
                );local variables
 
(setq  lst (car lst2)   ;list of boundary loops
        p1 (cadr lst2)  ;max min points
        p2 (cadr p1)
        p1 (car p1)
        p3 (list (car p2) (cadr p1))
       xt1 p1
       xt2 p2
      lst2 rlst
        x1 (car lst2) ;p1
);setq
(if na2
    (setq e2 (entget na2));setq
);if
(if (setq vpna (acet-currentviewport-ename))
    (setq vplocked (acet-viewport-lock-set vpna nil))
);if
(if (setq zflag (acet-geom-zoom-for-select (list xt1 xt2)));setq
    (command "_.zoom" (car zflag) (cadr zflag))
);if
(if (not (equal "Wipeout" htype))
    (progn
     (setq  ss2 (ssadd na2 (ssadd))
           lst3 (do_tile_stuff lst lst2 xtlst (* (distance p1 p2) 2.0))
     );setq
     (setq n 0);setq
     (repeat (length lst)
      (setq lst2 (nth n lst)
            lst4 (nth n lst3)
            lst5 (cadr lst4)
            lst4 (car lst4)
             ss3 (ssadd)
      );setq
      (if (equal htype "Image")
          (setq lst2 (bns_adjust_for_pixel_roundoff e2 lst2))
      );if
 
      (setq j 0)            ;repeat through the ones that will need to be clipped
      (repeat (length lst4)
       (command "_.copy" na2 "" x1 (nth j lst4));command
       (setq na3 (entlast))
       (cond
        ((equal htype "Image")
         (if (c_clipit na3 lst2)
             (progn
              (setq ss2 (ssadd na3 ss2))
              (while (setq na3 (entnext na3))
               (setq ss2 (ssadd na3 ss2))
              );while
             );progn then
             (entdel na3)
         );if
         (acet-spinner)
        );cond #1
        ((or (equal htype "Xref") (equal htype "Block"))
         (setq ss3 (ssadd na3 ss3) ;the xclip selection set
               ss2 (ssadd na3 ss2) ;the ss_visible selection set for use at the end
         );setq
        );cond #2
       );cond close
       (setq j (+ j 1));setq
      );repeat
 
      (setq j 0)
      (repeat (length lst5)
       (command "_.copy" na2 "" x1 (nth j lst5));command
       (setq ss2 (ssadd (entlast) ss2))
       (if (and (equal htype "Image")
                (equal j (* 2 (/ j 2)))
           );and
           (acet-spinner)
       );if
       (setq j (+ j 1));setq
      );repeat
 
      (if (and lst4
               (or (equal htype "Block")
                   (equal htype "Xref")
               );or
          );and
          (progn
           (c_clipit ss3 lst2);then xref or block
          );progn
          (acet-spinner)
      );if
      (setq n (+ n 1));setq
     );repeat
    );progn then image block or xref
    (progn
     (setq ss2 (ssadd))
     (setq n 0);setq
     (repeat (length lst)
      (setq lst2 (nth n lst));setq
      (wipeout_clipit nil lst2)
      (setq ss2 (ssadd (entlast) ss2));setq
     ;(acet-spinner)
     (setq n (+ n 1));setq
     );repeat
    );progn else
);if
 
(if zflag (command "_.zoom" "_p"));if
(if vplocked 
    (acet-viewport-lock-set vpna T) ;re-lock the viewport
);if
(princ "\nPreparing hatch objects for display...")
(if (and na2 ss2)
    (progn
     (setq ss2 (ssdel na2 ss2))
     (entdel na2)
     (acet-ss-visible ss2 0)
    );progn
);if
(if (and ss2
         (> (sslength ss2) 0)
    );and
    (progn
     ;(setq lst (acet-table-name-list "block"))
     ;(setq n 0)
     ;(setq j 0)
     ;(repeat (length lst)
     ;(setq a (nth n lst))
     ;(if (and (> (strlen a) 8)
     ;         (equal (substr a 1 8) "SPRHATCH")
     ;         (numberp (read (substr a 9)))
     ;         (> (read (substr a 9)) j)
     ;    );and
     ;    (setq j (read (substr a 9)));setq
     ;);if
     ;(setq n (+ n 1));setq
     ;);repeat
     ;(setq j (+ j 1)
     ;      a (strcat "SPRHATCH" (itoa j))
     ;);setq
     ;(command "_.block" a "0,0,0" ss2 "")
     ;(command "_.insert" a "0,0,0" "1" "1" "0")
     (acet-group-make-anon (list ss2) "Superhatch")
    );progn
);if
(princ "\nDone.")
 
);defun sprhatch
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;- do_tile_stuff
;   - do_tile_stuff2
;
;
(defun do_tile_stuff ( lst lst3 xtlst dst / offset dx dy rot a b n lst2)
 
(setq   dx (distance (car lst3) (cadr lst3))
        dy (distance (cadr lst3) (caddr lst3))
);setq
(if xtlst
    (setq offset (list (- (car (car xtlst)) (car (car lst3)))
                       (- (cadr (car xtlst)) (cadr (car lst3)))
                       0.0
                 );list
          offset (acet-geom-point-rotate offset
                             '(0.0 0.0 0.0)
                             (* -1.0 (angle (car lst3) (cadr lst3)))
                 );acet-geom-point-rotate
    );setq
    (setq offset '(0.0 0.0 0.0))
);if
(if (< (+ (abs dx) (abs dy)) (* 0.04 dst))
    (progn
     (princ "\nThe hatch object is very small in relation to the boundary data.")
     (princ "\nThe operation may take a while to complete.")
     (initget "Yes No _Yes No")
     (setq b (getkword "\nAre you sure you want to do this? [Yes/No] <Yes>:"))
     (if (equal b "No")
         (progn
          (princ "\nAborting hatch...")
          (exit)
         );progn then
     );if
    );progn
);if
(setq  rot (angle (car lst3) (cadr lst3))
      ;lst3 nil
);setq
(setq n 0)
(repeat (length lst)
(setq a (nth n lst))
(setq lst2 (append lst2
                   (list (do_tile_stuff2 a dx dy rot dst (car lst3) xtlst offset)
                   );list
           );append
);setq
(acet-spinner)
(setq n (+ n 1));setq
);repeat
 
lst2
);defun do_tile_stuff
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do_tile_stuff2 ( lst dx dy rot dst bspnt xtlst offset /
                        p1 p2 p3 p4 x y a b c d n j lst2 lst3 lst4 lst5
                        indx flag dx2 dy2
                      )
 
(if (not (equal (car lst) (last lst) 0.0001))
    (setq lst (append lst (list (car lst))));setq
);if                       ;'(0.0 0.0 0.0)
(setq lst (rotate_pnts lst bspnt (* -1.0 rot))
       p1 (acet-geom-list-extents lst)
       p2 (cadr p1)
       p1 (car p1)
       p3 p1
       p4 p2
       p1 (list (- (car p1) (car bspnt))
                (- (cadr p1) (cadr bspnt))
                (getvar "elevation")
          );list
       p2 (list (- (car p2) (car bspnt))
                (- (cadr p2) (cadr bspnt))
                (getvar "elevation")
          );list
        a (+ (acet-calc-round (car p1) dx) (car bspnt))
        b (+ (acet-calc-round (cadr p1) dy) (cadr bspnt))
        c (+ (acet-calc-round (car p2) dx) (car bspnt))
        d (+ (acet-calc-round (cadr p2) dy) (cadr bspnt))
);setq
(if xtlst
    (setq dx2 (distance (car xtlst) (cadr xtlst))
          dy2 (distance (cadr xtlst) (caddr xtlst))
    );setq
    (setq dx2 dx
          dy2 dy
    );setq
);if
(if (> a (car p3)) (setq a (- a dx)));if
(if (> b (cadr p3)) (setq b (- b dy)));if
(if (< c (car p4)) (setq c (+ c dx)));if
(if (< d (cadr p4)) (setq d (+ d dy)));if
(setq p1 (list a b 0.0)
      p2 (list c d 0.0)
);setq
 
(setq n 0)
(repeat (fix (acet-calc-round (/ (- d b) dy) 1))
(setq y (+ b (* n dy)));setq
(setq lst2 nil)
 (setq j 0)
 (repeat (fix (acet-calc-round (/ (- c a) dx) 1))
 (setq x (+ a (* j dx)));setq
 (setq lst2 (append lst2 (list (list x y 0.0))));setq
 (setq j (+ j 1))
 );repeat the columns of this row
(setq lst3 (append lst3 (list lst2)));setq
(setq n (+ n 1))
);repeat the rows
 
(setq n 0)
(repeat (length lst3)
(setq lst2 (nth n lst3))
 
 (setq j 0)
 (repeat (length lst2)
 (setq    a (nth j lst2)
          a (acet-geom-vector-add a offset)
          b (polar a 0 dx2)            ;rk use the dx of the xtlst here
          c (polar b (/ pi 2.0) dy2)   ;rk use the dy of the xtlst here
          d (polar a (/ pi 2.0) dy2)
       indx (list n j)
       flag (member indx lst4)
 );setq
 
 (if (and (not flag)
          ;(= n 0)
     );and
     (progn
      (if (setq x (poly_inters a b lst))
          (setq lst4 (append lst4 (list indx))
                flag T
          );setq then ON the boundary
      );if
     );progn then
 );if
 (if (and (not flag)
          ;(= j 0)
          (poly_inters a d lst)
     );and
     (setq lst4 (append lst4 (list indx))
           flag T
     );setq then ON the boundary
 );if
 (if (setq x (poly_inters b c lst))
     (progn
      (if (not flag)
          (setq lst4 (append lst4 (list indx))
                flag T
          );setq then ON the boundary
      );if
      ;(if (and (< (+ j 1) (length lst2))
      ;         (not (member (list n (+ j 1)) lst4))
      ;    );and
      ;    (setq lst4 (append lst4 (list (list n (+ j 1)))));setq then ON the boundary
      ;);if
     );progn
 );if
 (if (poly_inters c d lst)
     (progn
      (if (not flag)
          (setq lst4 (append lst4 (list indx))
                flag T
          );setq then ON the boundary
      );if
      ;(if (and (< (+ n 1) (length lst3))
      ;         (not (member (list (+ n 1) j) lst4))
      ;    );and
      ;    (setq lst4 (append lst4 (list (list (+ n 1) j))));setq then ON the boundary
      ;);if
     );progn
 );if
 (if (and (not flag)
          (or (acet-geom-point-inside a lst dst)  ;check to see if any of the points are inside.
              ;(acet-geom-point-inside b lst dst)
              ;(acet-geom-point-inside c lst dst)  ;double check in case 'a' is directly ON the bounds
              ;(acet-geom-point-inside d lst dst)
          );or
     );and
     (progn
      (setq lst5 (append lst5 (list indx)));setq then totally inside the boundary
      (setq flag T) ;rk added 8-21
 
     );progn
     (progn
      (if (and (not flag)
               (acet-geom-point-inside (car lst) (list a b c d a) (+ dst dx dy))
          );and
          (setq lst4 (append lst4 (list indx))
                flag T
          );setq
      );if
 
     );progn else
 );if
 
 (setq j (+ j 1));setq
 );repeat
(acet-spinner)
(setq n (+ n 1));setq
);repeat
 
(setq lst nil)
(setq n 0)
(repeat (length lst4)
(setq   a (nth n lst4)
        b (cadr a)
        a (car a)
        a (nth a lst3)
        a (nth b a)
        a (acet-geom-point-rotate a bspnt rot)
      lst (append lst (list a));base point for rectang that intersects the boundary
);setq
(setq n (+ n 1));setq
);repeat
 
(setq lst2 nil)
(setq n 0)
(repeat (length lst5)
(setq    a (nth n lst5)
         b (cadr a)
         a (car a)
         a (nth a lst3)
         a (nth b a)
         a (acet-geom-point-rotate a bspnt rot)
      lst2 (append lst2 (list a));base point for a rectang that is within the boundary
);setq
(setq n (+ n 1));setq
);repeat
 
(list lst lst2);boundary intersecters and those totally within the boundary.
);defun do_tile_stuff2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes two points p1 and p2 and a list of points in lst.
;returns true if the segment formed by p1 and p2 intersects any of the
;segements formed by the points in lst
;
(defun poly_inters ( p1 p2 lst / len a b n flag)
 (setq len (length lst))
 (setq a (car lst))
 (if a
     (setq p1 (list (car p1) (cadr p1) (caddr a))
           p2 (list (car p2) (cadr p2) (caddr a))
     );setq
 );if
 (setq n 1)
 (while (< n len)
 (setq b (nth n lst));setq
 (if (setq flag (inters p1 p2 a b))
     (setq n len)
 );if
 (setq a b)
 (setq n (+ n 1));setq
 );while
 flag
);defun poly_inters
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;returns a list comprised of sub-lists of the form:
;(boundary island1 island2 island3...)
;where each element is a list of points along the entity.
;
;  - organize_islands
;     - organize_islands2
;
(defun organize_islands ( ss clipitres / a b c n j lst lst2 lst3 ss2 na )
 
(setq lst (organize_islands2 ss clipitres))
(setq n 0)
(repeat (length lst)
(setq   a (nth n lst)
        b (nth 2 a)
      ss2 (nth 1 a)
);setq
(if (and ss2
         (equal b (* 2 (/ b 2)))
    );and
    (progn
     (setq lst3 (list (nth 3 a)));setq
     (setq j 0)
     (repeat (sslength ss2)
     (setq   na (ssname ss2 j)
              c (assoc na lst)
              c (nth 3 c)
           lst3 (append lst3 (list c))
     );setq
     (setq j (+ j 1));setq
     );repeat
     (setq lst2 (append lst2 (list lst3)));setq
    );progn then it has islands
    (progn
     (if (equal b (* 2 (/ b 2)))
         (setq    b (list (nth 3 a))
               lst2 (append lst2 (list b))
         );setq then a boundary edge with no islands
     );if
    );progn else no islands
);if
(acet-spinner)
(setq n (+ n 1));setq
);repeat
 
lst2
);defun organize_islands
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a selection set of polylines and returns a list that contains
; sub-lists, each  of the form
;'(entname
;  sel_set_of_islands (nil if not islands are found)
;  nesting_index      (even means outer edge/odd means it's an island)
;  list of points along the ent
; )
;
;
;
(defun organize_islands2 ( ss clipitres / lst ss2 ss3 ss4 ss5 ss6 n j na a b c)
 
(setq lst (find_islands ss clipitres)
      ss2 (nth 1 lst) ;polylines that contain islands
      ss3 (nth 2 lst) ;polylines with no islands
      ss4 (nth 3 lst) ;polylines that are islands
      lst (car lst)
);setq
 
(setq n 0)
(repeat (sslength ss4)
(setq na (ssname ss4 n));setq
 (setq j 0)
 (repeat (length lst)
 (setq   a (nth j lst)
       ss5 (cadr a)
 );setq
 (if (and ss5
          (ssmemb na ss5)
     );and
     (setq   a (assoc na lst)
             b (list (nth 0 a)
                     (nth 1 a)
                     (+ (nth 2 a) 1) ;increment the nesting number
                     (nth 3 a)
               );list
           lst (subst b a lst)
     );setq
 );if
 (setq j (+ j 1));setq
 );repeat
(setq n (+ n 1));setq
);repeat
 
(setq n 0)
(repeat (length lst)
(setq a (nth n lst)
      c (nth 2 a)
);setq
(if (and (setq ss5 (nth 1 a))
         (equal c (* 2 (/ c 2)))
    );and
    (progn
     (setq ss6 (ssadd))
     (setq j 0)
     (repeat (sslength ss5)
      (setq na (ssname ss5 j)
             b (assoc na lst)
      );setq
      (if (equal (nth 2 b) (+ (nth 2 a) 1));odd and one greater than
          (setq ss6 (ssadd na ss6));setq then a valid island for this one
      );if
     (setq j (+ j 1));setq
     );repeat
     (setq   b (list (nth 0 a) ss6 (nth 2 a) (nth 3 a))
           lst (subst b a lst)
     );setq
    );progn then it's an outer edge with islands because it is even and has as an island ss
);if
(setq n (+ n 1));setq
);repeat
 
lst
);defun organize_islands2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;separate into three groups:
;1. polylines that contain islands
;2. polylines that do NOT contain islands.
;3. polylines that ARE islands
;
(defun find_islands ( ss clipitres / ss2 ss3 ss4 ss5 a na n lst lst2)
 
(if (not clipitres)
    (setq a (acet-geom-pixel-unit));setq
    (setq a clipitres);setq
);if
(setq ss2 (ssadd) ;polylines that contain islands
      ss3 (ssadd) ;polylines that do NOT contain islands.
      ss4 (ssadd) ;polylines that ARE islands
       ss (remove_duplicated_plines ss)
);setq
(setq n 0)
(repeat (sslength ss)
 
(setq   na (ssname ss n)
      lst2 (acet-geom-object-point-list na a)
      lst2 (acet-list-remove-adjacent-dups lst2)
);setq
;(setq lst3 (append lst3 (list lst2)));setq
(setq ss5 (wp_select lst2 ss));setq
 
(if (and ss5
         (> (sslength ss5) 0)
    );and
    (progn
     (setq ss2 (ssadd na ss2))
     (command "_.select" ss4 ss5 "")
     (setq ss4 (ssget "_p"))
    );progn then islands were found within na
    (progn
     (setq ss3 (ssadd na ss3));no islands
    );progn else
);if
(setq lst2 (list na ss5 0 lst2)
       lst (append lst (list lst2))
);setq
(setq n (+ n 1));setq
);repeat
 
(list lst ss2 ss3 ss4)
);defun find_islands
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove_duplicated_plines ( ss / pre_sel2 na lst lst2 n ss2)
 
 ;local function
 (defun pre_sel2 ( lst / lst2 n a)
  (setq n 0)
  (repeat (length lst)
  (setq a (nth n lst)
        a (list (acet-calc-round (car a) 0.00001)
                (acet-calc-round (cadr a) 0.00001)
          );list
  );setq
  (setq lst2 (append lst2 (list a)));setq
  (setq n (+ n 1));setq
  );repeat
  lst2
 );defun pre_sel2
 
 (setq ss2 (ssadd))
 (setq n 0)
 (repeat (sslength ss)
 (setq  na (ssname ss n)
       ;lst (acet-geom-vertex-list na)
       lst (acet-geom-object-point-list na nil)
       lst (pre_sel2 lst)
 );setq
 (if (not (member lst lst2))
     (setq  ss2 (ssadd na ss2)
           lst2 (append lst2 (list lst))
     );setq
 );if
 (setq n (+ n 1));setq
 );repeat
 ss2
);defun remove_duplicated_plines
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wp_select ( lst ss / na ss2 ss3 n)
 
(if (setq ss2 (ssget "_wp" lst '((0 . "*POLYLINE"))));setq
    (progn
     (if (> (sslength ss) (sslength ss2))
         (setq ss3 ss2
               ss2 ss
                ss ss3
         );setq
     );if
     (setq ss3 (ssadd))
     (setq n 0)
     (repeat (sslength ss)
      (setq na (ssname ss n))
      (if (ssmemb na ss2)
          (setq ss3 (ssadd na ss3))
      );if
      (setq n (+ n 1));setq
     );repeat
     (if (equal (sslength ss3) 0)
         (setq ss3 nil)
     );if
    );progn then
);if
 
ss3
);defun wp_select
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ave_y ( lst / n a)
 (setq a 0)
 (setq n 0)
 (repeat (length lst)
 (setq a (+ a (cadr (nth n lst))));setq
 (setq n (+ n 1));setq
 );repeat
(/ a (float (length lst)))
);defun ave_y
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rotate_pnts ( lst p1 ang / a n lst2)
 (setq n 0);setq
 (repeat (length lst)
 (setq    a (nth n lst)
          a (acet-geom-point-rotate a p1 ang)
       lst2 (append lst2 (list a))
 );setq
 (setq n (+ n 1));setq
 );repeat
lst2
);defun rotate_pnts
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun screen_sel ( flag / p1 p2 ss)
 (setq p1 (acet-geom-view-points)
       p2 (cadr p1)
       p1 (car p1)
 );setq
 (if flag
     (setq ss (ssget "_c" p1 p2 '((-4 . "<OR")
                                  (0 . "ELLIPSE")
                                  (0 . "TEXT")
                                  (0 . "ATTDEF")
                                  (0 . "MTEXT")
                                  (-4 . "OR>")
                                 )
              );ssget
     );setq
     (setq ss (ssget "_c" p1 p2));setq else
 );if
 ss
);defun screen_sel
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;creates polyline rectangles around text objects in ss
;and creates polyline ellipses in place of ellipses.
;Returns a list of 2 elements
;each element will be a selection set or nil
;i.e. '(ellipse_ss, polyline_ss)
;
(defun pre_boundary ( ss ss3 ss4 / ss5 a p1 lst j na na2 e1 n
                    );args and locals
 
 (setq na2 (entlast))
 (if (not ss3) (setq ss3 (ssadd)))
 (if (not ss4) (setq ss4 (ssadd)))
 (setq ss5 (ssadd))
 (if ss
     (progn
           (setq n 0)
           (repeat (sslength ss)
           (setq na (ssname ss n)
                 e1 (entget na)
           );setq
           (cond
            ((equal "ELLIPSE" (cdr (assoc 0 e1)))
             (acet-ucs-cmd (list "_ob" na))
             (setq ss3 (ssadd na ss3)
                   ss5 (ssadd na ss5)
                   lst (acet-geom-object-point-list na #clipitres) ;using a global here
             );setq
             (command "._pline")
             (setq j 0);setq
             (repeat (length lst)
             (setq a (nth j lst))
              (command (nth j lst))
             (setq j (+ j 1));setq
             );repeat
             (command "")
             (acet-ucs-cmd (list "_p"))
            );cond #1
            ((equal "MTEXT" (cdr (assoc 0 e1)))
             (acet-ucs-to-object na)
             (setq p1 (acet-geom-object-point-list na nil));setq
             (command "_.pline" (car p1) (cadr p1) (caddr p1) (nth 3 p1) "_cl")
             (acet-ucs-cmd (list "_p"))
            );cond #2
            ((or (equal "TEXT" (cdr (assoc 0 e1)))
                 (equal "ATTDEF" (cdr (assoc 0 e1)))
             );or
             (acet-ucs-cmd (list "_ob" na))
             (setq p1 (acet-geom-object-point-list na nil));setq
             (command "_.pline" (car p1) (cadr p1) (caddr p1) (nth 3 p1) "_cl")
             (acet-ucs-cmd (list "_p"))
            );cond #3
           );cond close
           (setq n (+ n 1));setq
           );repeat
           (if (not na2)
               (setq na2 (entnext))
           );if
           (while (setq na2 (entnext na2))
            (setq ss4 (ssadd na2 ss4))
           );while
           ;(setq ss4 (acet-ss-new na2))
           (if (and ss5
                    (> (sslength ss5) 0)
               );and
               (acet-ss-visible ss5 1)
           );if
     );progn then
 );if
 
(list ss3 ss4)
);defun pre_boundary
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
(defun get_boundary_plines ( / underscore na a ss ss2 ss3 ss4)
 
 ;local function that adds an underscore to string provided as argument
 (defun underscore ( a /)
  (if (and a
           (equal 'STR (type a))
           (not (equal "_" (substr a 1 1)))
      );and
      (setq a (strcat "_" a))
  );if
  a
 );defun underscore
 
(princ "\nSelecting visible objects for boundary detection...")
(setq ss (screen_sel T))
(if ss
    (setq ss3 (pre_boundary ss ss3 ss4)
          ss4 (cadr ss3)
          ss3 (car ss3)
    );setq
);if
(princ "Done.\n")
(command "_.-boundary" "_a" "_o" "_p" "_x" "")
(if (not (setq na (entlast)))
    (setq na (entnext))
);if
(if (cadr (assoc "HIGHLIGHT" (car acet:sysvar-list)))
    (setvar "highlight" (cadr (assoc "HIGHLIGHT" (car acet:sysvar-list))))
    (setvar "highlight" 1)
);if
(setq a T)
(command "_.-boundary")
(while (and a
            (wcmatch (getvar "cmdnames") "*BOUNDARY*")
       );and
(initget "Advanced _Advanced")
(setq a (getpoint "\nSpecify an option [Advanced options] <Internal point>: "));setq
 
(if (equal a "Advanced")
    (progn
     (command "_A")
     (while (not (equal a "_x"))
      (initget "Boundary Island eXit _Boundary Island eXit")
      (setq a (getkword "\nEnter an option [Boundary set/Island detection/eXit] <eXit>: "))
      (if (or (equal a "eXit")
              (not a)
          );or
          (setq a "_x")
          (setq a (underscore a))
      );if
      (command a)
      (cond
       ((equal a "_Boundary")
        (progn
         ;;(princ "\nSpecify candidate set for boundary")
         (initget "New Everything _New Everything")
         (setq a (getkword "\nSpecify candidate set for boundary [New/Everything] <Everything>: "))
         (if (not a)
             (setq a "_Everything")
             (setq a (underscore a))
         );if
         (command "_n" "" "_x" "");get out of the boundary command
         (if (equal a "_New")
             (progn
              (setq ss2 (ssget));setq
              (princ "\nProccessing objects for boundary detection...")
              (if ss2
                  (progn
                   ;(command "_n" "" "_x" "")
                   (setq ss3 (pre_boundary ss2 ss3 ss4)
                         ss4 (cadr ss3)
                         ss3 (car ss3)
                   );setq
                   (command "_.-boundary" "_A" "_B" a ss2 "")
                  );progn
                  (progn
                   (princ "\nNothing found.")
                   (command "_.-boundary" "_A")
                  );progn else
              );if
             );progn
             (progn
              (if (equal a "_Everything")
                  (progn
                   (princ "\nSelecting everything visible...")
                   (setq ss2 (screen_sel T))
                   (if ss2
                       (progn
                        ;(command "_n" "" "_x" "")
                        (setq ss3 (pre_boundary ss2 ss3 ss4));setq
                        (setq ss4 (cadr ss3)
                              ss3 (car ss3)
                        );setq
                       );progn then
                       ;(princ "\nNothing found")
                   );if
                   (princ "\nAnalyzing the selected data...")
                   (command "_.-boundary" "_A" "_B" a)
                  );progn then
              );if
             );progn
         );if
         (princ "Done.")
        );progn
       );cond #1 Boundary
       ((equal a "_Island")
        (progn
         (initget "Yes No _Yes No")
         (setq a (getkword "\nDo you want island detection? [Yes/No] <Yes>: "))
         (if (not a)
             (setq a "_y")
             (setq a (underscore a))
         );if
         (command a)
         (if (equal a "_No")
             (progn
              (initget "Nearest +X -X +Y -Y Angle _Nearest +X -X +Y -Y Angle")
              (setq a (getkword "\nEnter an option [Nearest/+X/-X/+Y/-Y/Angle] <Nearest>: "))
              (if (not a)
                  (setq a "_Nearest")
                  (setq a (underscore a))
              );if
              (command a)
             );progn
         );if
        );progn
       );cond #2
      );cond close
     );while
    );progn then
    (progn
     (cond
      ((and a
            (equal 'LIST (type a))
       );and
       (command a)
       (princ "\n")
       (if (acet-str-m-find " SELECTED" (xstrcase (getvar "lastprompt")))
           (progn
            (initget "Yes No _Yes No")
            (setq a (getkword "\nDo you really want to do this? [Yes/No] <No> "))
            (if (equal "Yes" a)
                (command "_y")
                (command "_n")
            );if
           );progn then
       );if
      );cond #1
      ((not a)
       (command "")
      );cond #2
     );cond close
    );progn
);if
);while
 
 
(while (wcmatch (getvar "cmdnames") "*BOUNDARY*")
(princ (strcat "\n*Unable to create polyline boundary*"
               "\nBoundary detection in this command does not support certain"
               "\nobject types. See help on \"Superhatch\" for details."
       );strcat
);princ
 
(command nil)
);while
 
(if (and ss3
         (> (sslength ss3) 0)
    )
    (acet-ss-visible ss3 0) ;turn ellipses back on
);if
(if (and ss4
         (> (sslength ss4) 0)
    )
    (command "_.erase" ss4 "")
);if
 
(if (not na)
    (setq na (entnext))
);if
(if (and (setq ss (acet-ss-new na))
         (> (sslength ss) 0)
    );and
    (progn
     (command "_.select" ss "")
     (setq ss (ssget "_p" '((0 . "*POLYLINE"))))
     (if (not ss)
         (progn
          (setq ss nil)
          (princ "\nUnable to create polyline boundary.")
         );progn
     );if
    );progn then
    (setq ss nil)
);if
(setvar "highlight" 0)
 
ss
);defun get_boundary_plines
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun do_boundary_stuff ( clipitres ss / a x n lst lst2 lst3)
(if (and ss
         (> (sslength ss) 0)
    );and
    (progn
     (setq lst (organize_islands ss clipitres));setq
     (command "_.erase" ss "")
 
     (setq n 0)
     (repeat (length lst)
     (setq a (nth n lst))
     (acet-spinner)
     (if (> (length a) 1)
         (setq    a (scan_islands a)
               lst3 (cadr a)
                  a (car a)
                  x (acet-geom-list-extents (append x lst3))
         );setq
         (setq x (acet-geom-list-extents (append x (car a))));setq
     );if
     (setq lst2 (append lst2 a));setq
     (setq n (+ n 1));setq
     );repeat
    );progn then
    (princ "\nNo boundary information found.")
);if
 
(list lst2 x)
);defun do_boundary_stuff
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun scan_islands ( lst / n a b lst2 lst3 lst4 lst5 mnmx flag)
 
(setq n 0)
(repeat (length lst)
 (setq lst2 (nth n lst))
 (if (equal (car lst2) (last lst2))
     (setq lst2 (cdr lst2))
 );if
 (setq    b (acet-geom-list-extents lst2)
          a (car (car b))
          a (assoc a lst2)
          a (vl-position a lst2)
       lst3 lst2
 );setq
 (repeat a
  (setq lst3 (append (cdr lst3) (list (car lst3))));setq
 );repeat
 (setq lst3 (append lst3 (list (car lst3)))
        lst (subst lst3 (nth n lst) lst)
       lst4 (append lst4 (list b))
       lst5 (append lst5 (find_activity_points lst3))
       mnmx (acet-geom-list-extents (append mnmx b))
 );setq
 
(setq n (+ n 1));setq
);repeat
(setq lst5 (acet-list-isort lst5 0));setq
 
;- lst is a list of lists. Each is a closed loop of bounding points. The
;  first element of each is the coord with the minimum x value.
;- lst4 is a list of sublists that corrispond to the extents of each loop in lst.
;- lst5 is a single list of points that contains all activity points
;  sorted with respect to x.;
 
 
(setq lst2 nil);setq
(setq a (car lst5));setq
(setq n 1);setq
(repeat (- (length lst5) 1)
(setq b (nth n lst5));setq
(setq lst2 (append lst2
                   (list (cross_segs n a b lst lst4))
           );append
);setq
(setq a b);setq
(acet-spinner)
(setq n (+ n 1));setq
);repeat a and b are delta x segements;
 
 
(setq lst3 nil
      lst4 nil
      lst5 nil
      flag T
);setq
(while flag
(setq lst4 nil
      flag nil
);setq
 (setq n 0)
 (while (< n (length lst2))
 (setq lst3 (nth n lst2)
          a (car lst3)
          b (cadr lst3)
 );setq
 (if (and a b)
     (progn
 
      (if (not flag)
          (progn
           (if (not lst4)
               (setq lst4 (list a b)
                     lst3 (cdr (cdr lst3))
               );setq
               (progn
                (if (equal (last (car lst4)) (car a) 0.0001)
                    (setq lst4 (list (append (car lst4) a)
                                     (append (cadr lst4) b)
                               );list
                          lst3 (cdr (cdr lst3))
                    );setq then
                    (setq lst4 (append (reverse (car lst4)) (cadr lst4))
                          lst5 (append lst5 (list lst4))
                          lst4 nil
                          flag T
                    );setq
                );if
               );progn else
           );if
          );progn then
      );if
      (setq lst3 (reverse lst3)
            lst2 (subst lst3 (nth n lst2) lst2)
      );setq
     );progn then
 );if
 (setq n (+ n 1));setq
 );while
 
(if lst4
    (setq lst4 (append (reverse (car lst4)) (cadr lst4))
          lst5 (append lst5 (list lst4))
          lst4 nil
          flag T
    );setq
);if
);while
 
;(setq n 0)
;(repeat (length lst5)
;(pline (list (cons 62 (+ n 1))
;             (acet-list-remove-adjacent-dups (nth n lst5))
;       );list
;);pline
;(setq n (+ n 1));setq
;);repeat
 
(list lst5 mnmx)
);defun scan_islands
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes: lower and upper x bounding points and
; a list of point lists that form boundary loops.
;Returns: a list of sub lists.
;each sub-list is a list of coords that form
;a the portion of a boundary loop that passes between
;the lower and upper x bounds specified in the first two args.
;
(defun cross_segs ( j x1 x2 lst lst2 / lst3 a b n )
 
(setq n 0);setq
(repeat (length lst2)
(setq a (nth n lst2)
      b (cadr a)
      a (car a)
);setq
(if (or (and (>= (car a) (car x1))
             (<= (car a) (car x2))
        );and then a is between x1 and x2
        (and (>= (car b) (car x1))
             (<= (car b) (car x2))
        );and then b is between x1 and x2
        (and (>= (car x1) (car a))
             (<= (car x1) (car b))
        );and then x1 is between a and b, so x2 must be also
    );or
    (progn
     (setq    a (cross_segs3 x1 x2 (nth n lst) a b)
           lst3 (append lst3 a)
     );setq
    );progn then there is an intersection somewhere
);if
(setq n (+ n 1));setq
);repeat
 
(if lst3
    (progn
     (setq lst3 (acet-list-isort lst3 0)
           lst2 nil
     );setq
     (setq n 0)
     (repeat (length lst3)
     (setq    a (nth n lst3)
           lst2 (append lst2 (cdr a))
     );setq
     (setq n (+ n 1));setq
     );repeat
     (setq lst3 lst2
           lst2 nil
     );setq
    );progn
);if
 
lst3
);defun cross_segs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun cross_segs3 ( x1 x2 lst y1 y2 /
                     point_in vec2
                     x1 x2 x3 x4 a b c d flag flag2 pnt lst2 lst3 tmp cnd n j
                   )
 
 (defun point_in ( a x1 x2 / flag)
  (cond
   ((and (> (car a) (car x1))
         (< (car a) (car x2))
    );and
    (setq flag 0);setq
   );
  );cond
  flag
 );defun point_in
 
 (defun vec2 ( lst / )
  (while (and lst
              (equal (car (car lst)) (car (cadr lst)) 0.0001)
         );and
   (setq lst (cdr lst))
  );while
  (setq lst (reverse lst))
  (while (and lst
              (equal (car (car lst)) (car (cadr lst)) 0.0001)
         );and
   (setq lst (cdr lst))
  );while
  (setq lst (reverse lst))
 lst
 );defun vec2
 
(setq   x1 (list (car x1) (cadr y1) 0.0)
        x3 (list (car x1) (cadr y2) 0.0)
        x2 (list (car x2) (cadr y1) 0.0)
        x4 (list (car x2) (cadr y2) 0.0)
         a (car lst)
      flag (point_in a x1 x2)
);setq
(setq n 1)
(while (< n (length lst))
(setq     b (nth n lst)
      flag2 (point_in b x1 x2)
          c (inters a b x1 x3)
          d (inters a b x2 x4)
        pnt nil
);setq
(if (and (not lst2)
         (or flag
             (equal (car a) (car x1) 0.0001)
             (equal (car a) (car x2) 0.0001)
         );or
    );and
    (setq lst2 (list a))
);if
 
(if (and c d)
    (progn
     (if (< (distance a c) (distance a d))
         (setq pnt (list c d))
         (setq pnt (list d c))
     );if
    );progn then
    (progn
     (if (and c
              (not (equal c (last lst2) 0.0001))
         );and
         (setq pnt (list c)
               cnd 1
         );setq then
         (progn
          (if (and d
                   (not (equal d (last lst2) 0.0001))
              );and
              (setq pnt (list d)
                    cnd 2
              );setq then
          );if
         );progn else
     );if
     (if (and (not (member b pnt))
              flag2
         );and
         (setq pnt (append pnt (list b))
               cnd 3
         )
     );if
    );progn else
);if
 
(setq j 0)
(repeat (length pnt)
(setq tmp (nth j pnt))
(if (not (equal tmp (last lst2) 0.0001))
    (setq lst2 (append lst2 (list tmp)))
);if
(setq j (+ j 1));setq
);repeat
(if (and (not flag2)
         lst2
         (setq lst2 (vec2 lst2))
         (> (length lst2) 1)
    );and
    (progn
     (if (< (car (last lst2)) (car (car lst2)))
         (setq lst2 (reverse lst2))
     );if
     (setq lst2 (list (+ (cadr (car (acet-geom-list-extents lst2))) (ave_y lst2)) ;
                      lst2
                );list
           lst3 (append lst3 (list lst2))
           lst2 nil
     );setq
    );progn then
);if
 
(setq flag flag2
         a b
);setq
(setq n (+ n 1));setq
);while
(if (and lst2
         (setq lst2 (vec2 lst2))
         (> (length lst2) 1)
    );and
    (progn
     (if (< (car (last lst2)) (car (car lst2)))
         (setq lst2 (reverse lst2))
     );if
     (setq lst2 (list (+ (cadr (car (acet-geom-list-extents lst2)))  (ave_y lst2)) ;
                      lst2
                );list
           lst3 (append lst3 (list lst2))
           lst2 nil
     );setq
    );progn then
);if
 
lst3
);defun cross_segs3
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun find_activity_points ( lst / dx n a b c lst2)
 
(setq    a (car lst)
      lst2 (list a)
        dx 1.0
);setq
(setq n 1)
(repeat (- (length lst) 1)
(setq b (nth n lst)
      c (- (car b) (car a))
);setq
(if (not (equal c 0.0))
    (progn
     (if (not (equal (/ dx c)
                     (/ (abs dx) (abs c))
              );equal
         );not
         (setq lst2 (append lst2 (list a)));setq
     );if
     (setq dx c);setq
    );progn then the change in x is not 0
);if
(setq a b);setq
(setq n (+ n 1));setq
);repeat
 
(acet-list-isort lst2 0)
);defun find_activity_points
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a default block name and prompts the user to enter a block name.
;A block name is returned.
;
(defun get_blockname ( bna / a b c d)
 
(setq a (xstrcase bna))
(if (equal "*" (substr a 1 1))
    (setq a (substr a 2))
);if
(if (not (equal a ""))
    (setq b (acet-str-format "\nSpecify block name <%1>: "  a ));setq then
    (setq b "\nSpecify block name: ");setq else
);if
(setq c (getstring T b))
(if (equal c "~")
    (progn
     (if (setq d (ACET-FILE-READDIALOG b a "dwg;dwt" "Acet:SuperHatch" 1664));setq
         (setq c (acet-filename-path-remove (acet-filename-ext-remove d)));setq
         (exit)
     );if
    );progn
    (progn
     (if (or (not c)
             (equal c "")
         );or
         (setq c bna)
     );if
    );progn
);if
(setq c (acet-str-space-trim (xstrcase c)))
(if (and (not d)
         (not (member c (acet-table-name-list "block")))
    );and
    (progn
     (if (equal c (acet-filename-ext-remove c))
         (setq c (strcat c ".dwg"))
     );if
     (if (not (findfile c))
         (progn
          (setq c (strcat (acet-filename-ext-remove c) ".dwt"))
          (if (not (findfile c))
              (progn
               (alert "Cannot find that block!")
               (setq c (get_blockname bna))
              );progn
          );if
         );progn then try to find a template
     );if
    );progn then not in the block table
    (progn
     (if d
         (setq c d)
     );if
    );progn else
);if
 
c
);defun get_blockname


(acet-autoload2	'("Clipit.lsp"	(c_clipit na lst)))
(acet-autoload2	'("Clipit.lsp"	(wipeout_clipit na lst)))
(acet-autoload2	'("Ddins2.lsp"	(ddins2)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDMEV8dam6Rzc1Sgv1DC/4apIMhuPd3H0nS7z1n
;;; 1A4MrDBfD2+1t1mUsM1Jt8uTFfnZ9frCetuhYgMXXsgAgJ2wQOZmZ8obGCFOJU4i
;;; 2w+B69tFGZs8nA5pZAFhXtGs+G1Lr5gSTJzprvDeYi53VY5TCzT4EUr/Px6PeiVr
;;; 3XZc+HmTDQ3CgAXYCG/z+IWpaMMJzTlNbJqA5vVjFbRXTlpx/NrUCi9LRrMM3JIh
;;; B2Z60LA6PxesQ5qn22UnGvlfP1ptPUs1w5IeeuWvEA9op7FEVHoSIH02w54b7MFk
;;; Dk8lC33YagWVASPQBCmpS+rbrNJeOJn8kQB7frFtBEjxfZcA7UNQRFl6BsUrn1Jy
;;; D4lqMxNhpllgsU0tEzg4QD1i6bSSHbk0iEkMvo5zDu5M1mRWzMSEHGhfcMvPUqoO
;;; vr5vcY7nBe8NiMoDopJ/KUpIR15EJh6Tg36tukZVb3IlVA1W2woPG5vq/rjqANql
;;; z0H6499iUEpAQRP+fvQAzH3LusV4uHXHGW6kB+OBICYBAFGSJ1Y6WmID2ovF+wp8
;;; Vy85eRnk3j+DybXiuF3lcksRzhEnFyfX/Z6I8jNzMdd3WFsVfQbpaiPkiixzP7yY
;;; K65FFdBnuTH+hxx3xAxwLNvFbZ/qo+1IW+nf8bj8Z7Q80Abj1Leal7xRKRhO1ikf
;;; mfcJgQ==
;;; -----END-SIGNATURE-----