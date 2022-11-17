;;
;;  Layoutmerge.lsp - Move objects from one or more layouts to the current layout and remove 
;;                    the empty original layouts.
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
 
;; Autoload the lman.lsp file
(acet-autoload '("lman.lsp" "(bns_sl lstate)"))
(acet-autoload '("tblname.lsp" "(acet-ui-table-name-get lst)"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:layoutmerge ( / lst n )
 (acet-error-init 
  (list '(    "cmdecho" 0 
            "highlight" 0
              "ucsview" 0
               "osmode" 0
             "extnames" nil
               "cmddia" nil
             "plinewid" 0.0
         )
        T
  )
 )
 (if (or (= (getvar "cmddia") 0)
         (= 4 (logand 4 (getvar "cmdactive")))
     );or
     (setvar "cmddia" 0)
 );if
 (if (setq lst (acet-layoutmerge-ui))
     (progn
      (setq n (acet-layoutmerge lst))
      (princ (acet-str-format "\n%1 layouts merged." (itoa n)))
      (command "_.zoom" "_e")
     );progn
 );if
 (acet-error-restore)
);defun c:layoutmerge
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:-layoutmerge ( / lst n )
 (acet-error-init 
  (list '(    "cmdecho" 0 
            "highlight" 0
              "ucsview" 0
               "osmode" 0
             "extnames" nil
               "cmddia" 0
         )
        T
  )
 )
 (if (setq lst (acet-layoutmerge-ui))
     (progn
      (setq n (acet-layoutmerge lst))
      (princ (acet-str-format "\n%1 layouts merged." (itoa n)))
      (command "_.zoom" "_e")
     );progn then
 );if
 (acet-error-restore)
);defun c:-layoutmerge
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-layoutmerge-ui ( / lst3 layoutcount flags target del-flag lst )
 
 (setq lst3 (acet-ui-table-name-get
             (list "Specify layout(s) to merge"		;; title
                   ""					;; default
                   "acad_layout"			;; table/dict name
                   (+ 8 32)				;; flags: DisallowPick + MultipleSelect
                   nil                                  ;; filter
                   "acet-1508.hlp"			;; help file
                   "LAYOUTMERGE"			;; help topic
             );list
            )
 );setq
 (if lst3
     (progn
      (setq layoutcount (length (acet-dict-name-list "acad_layout")))
      (if (>= layoutcount 255)
          (setq flags 8)
          (setq flags (+ 1 8)) ;allow new
      );if
      (setq target (acet-ui-table-name-get
                    (list "Specify destination layout"		;; title
                          (getvar "ctab")			;; default
                          "acad_layout"				;; table/dict name
                          (+ 1 8)				;; flags: AllowNew + DisallowPick
                          nil                                   ;; filter
                          "acet-1508.hlp"			;; help file
                          "LAYOUTMERGE"				;; help topic
                    );list
                   )
      );setq
      (if target
          (progn
            (initget "Yes No")
            (if (/= "No"
                    (progn 
                     (initget "Yes No")
                     (getkword "\nDelete unused layouts? <Y>: ")
                    )
                )
                (setq del-flag T)
            );if
            (setq lst (list lst3 target del-flag))
          );progn then
      );if
     );progn then
 );if
 lst
);defun acet-layoutmerge-ui
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-layoutmerge ( alst / lst sourcelst target del-flag
                          layout temp ss na p1 p2 tmplst locked n laylst space a xt
                        )
 
 ;; Extract the individual arguments from the single argument list provided.
 (setq lst '( sourcelst target del-flag ))
 (setq n 0)
 (repeat (min (length lst) (length alst))
  (set (nth n lst) (nth n alst))
 (setq n (+ n 1));setq
 );repeat
 
 ;; convert to uppper case and
 ;; remove the current tab from the source list if present
 (setq sourcelst (mapcar 'xstrcase sourcelst)
       sourcelst (vl-remove (xstrcase target) sourcelst)
 );setq
 
 ;; Remove layouts that do not exist
 (foreach layout sourcelst
  (if (not (acet-dict-ename "acad_layout" layout))
      (setq sourcelst (vl-remove layout sourcelst))
  );if
 );foreach
 
 ;; tell the error handler to delete the temp files if called
 (if (and (equal (type *error*) 'LIST)
          (equal (cadr *error*) "acet-error")
     );and
     (setq *error* (append *error* 
                           (list 
                            '(foreach x tmplst (vl-file-delete x))
                            '(princ)
                           );list
                   );append
     );setq
 );if
 
 (acet-groups-to-xdata "ACET-LAYOUTMERGE-GROUP-") 	;; Save group data to xdata
 (acet-layoutmerge-tag-viewports)			;; mark on viewports with an xdata tag.
 
 (if (not (tblobjname "layer" "defpoints"))		;; Create defpoints if needed
     (command "_.-layer" "_new" "defpoints" "")
 );if
 (bns_sl "LAYOUTMERGE")					;; and Save the layer status
 (command "_.-layer" "_unlock" "*" "_thaw" "*" "")	;; and thaw and unlock everything
 
 
 (setq n 0)
 (repeat (length sourcelst)
  (setq layout (nth n sourcelst)
          temp (vl-filename-mktemp "ACET" (getvar "tempprefix") ".dwg")
  );setq
  (if (acet-layoutmerge-wblock layout temp)	;; wblock each layout to a temp file
      (progn
       (setq laylst (cons layout laylst));setq list of layouts to delete (id delete flag is true)
       (if (findfile temp)
           (setq tmplst (cons temp tmplst));setq
       );if
      );progn then wblock
  );if
  (setq n (+ n 1));setq
 );repeat layout in sourcelst
 
 (setq tmplst (reverse tmplst))
 
 ;; set the target layout current (create it if not present)
 (if (not (acet-dict-ename "ACAD_LAYOUT" target))
     (command "_.layout" "_new" target)
 );if
 (setvar "ctab" target)
 (if (and (= 0 (getvar "tilemode"))	;; go to paper space if needed
          (/= 1 (getvar "cvport"))
     );and
     (command "_.pspace")
 );if
 
 (if del-flag
     (progn
      (foreach layout laylst				;; delete the unneeded layouts
       (if (not (acet-str-equal layout "model"))
           (command "_.layout" "_delete" layout)
       );if
      );foreach
     );progn then delete the layouts
 );if
 
 (acet-ucs-cmd (list "_view"))
 
 ;; find the insertion point for the first layout to insert
 (if (and (setq ss (ssget "_x" (list (cons 410 target))));setq
          (setq p1 (acet-geom-ss-extents ss nil))
     );and
     (setq p2 (cadr p1)
           p1 (car p1)
           p1 (list (+ (car p2) (* 0.25 (distance p1 p2)))
                    (cadr p1)
              )
     );setq then
     (setq p1 (list 0.0 0.0))
 );if
 
 ;; loop through the list of temp filenames and insert them
 (setq n 0)
 (repeat (length tmplst)
  (setq   na (entlast)
        temp (nth n tmplst)
  );setq
  (command "_.-insert" (strcat "*" temp) p1 1 0);command
 
  ;; get the extents of the objects and save a view.
  (setq ss (acet-ss-new na))
  (if (and ss
           (> (sslength ss) 0)
           (setq p1 (acet-geom-ss-extents ss nil))
      );and
      (progn
       (setq p2 (cadr p1)
             p1 (car p1)
       );setq
       (command "_.zoom" "_w" p1 p2)
       (setq a (nth n sourcelst))
       (if (= 0 (getvar "extnames"))
           (setq a (acet-str-replace " " "_" a))
       );if
       (command "_.view" "_s" a)
       (setq p1 (list (+ (car p2) (* 0.25 (distance p1 p2)))
                      (cadr p1)
                )
       );setq
      );progn then
  );if
  (vl-file-delete temp)
 (setq n (+ n 1));setq
 );repeat
 
 (acet-ucs-cmd (list "_prev"))
 (bns_rl "LAYOUTMERGE")
 (bns_dl "LAYOUTMERGE")
 (acet-xdata-to-groups "ACET-LAYOUTMERGE-GROUP-")
 (acet-layoutmerge-restore-tagged-viewports)
 
 ;; put the error handler back
 (if (and (equal (type *error*) 'LIST)
          (equal (cadr *error*) "acet-error")
     );and
     (setq *error* (reverse (cdr (cdr (reverse *error*)))));setq then remove the princ and the foreach
 );if
 n
);defun layoutmerge
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-layoutmerge-wblock ( layout temp / space xv p1 p2 h ss xt flag na laylst )
 
  (setvar "ctab" layout)
  (if (= 0 (getvar "tilemode"))
      (progn
       (if (/= 1 (getvar "cvport"))
           (command "_.pspace")
       );if
       (setq space 1)
      );progn
      (setq space 0)
  );if
 
  (acet-ucs-cmd (list "_view"))
  (setq xv (trans (getvar "viewdir") 1 0 T)
        p1 (acet-geom-view-points)
        p2 (cadr p1)
        p1 (car p1)
         h (* 0.01 (distance p1 p2))
  );setq
 
 
  (if (setq ss (ssget "_x" 
                      (list 
                       '(-4 . "<AND")
                          (cons 410 (acet-str-esc-wildcards layout)) ;escape the wildcards for ssget
                          '(-4 . "<NOT") '(-4 . "<AND") 
                            '(0 . "VIEWPORT") '(69 . 1)  ;not the paper space viewport
                          '(-4 . "AND>") '(-4 . "NOT>")
                       '(-4 . "AND>")
                      );list
               );ssget
      );setq
      (progn
       ;; label the layout with a pline rectangle and a text object
       (setq na (entlast))
       (entmake
           (list
            '(0 . "TEXT")
             (cons 67 space)  (cons 8 "defpoints") ;(cons 410 layout)
             (list 10 (+ (car p1) (/ h 2.0)) (+ (cadr p1) (/ h 2.0)) 0.0)
             (cons 1 layout) (cons 40 h)
             (cons 50 (acet-geom-angle-trans 0.0 '(0.0 0.0 1.0) xv))
             (cons 210 xv)
           );list
       );entmake
       (if (not (equal na (entlast)))
           (setq ss (ssadd (entlast) ss)) ;then add the text to the selection set
       );if
       (setq na (entlast))
       (command "_.pline" p1 (list (car p2) (cadr p1)) p2 (list (car p1) (cadr p2)) "_c")
       (command "_.chprop" (entlast) "" "_lay" "defpoints" "")
       (if (not (equal na (entlast)))
           (setq ss (ssadd (entlast) ss)) ;then add the pline to the selection set
       );if
 
       (setq xt (acet-geom-ss-extents ss nil))
       (if xt
           (setq p1 (car xt))
       );if
       (command "_.wblock" temp "" p1 ss "")
      );progn then
  );if
  (acet-ucs-cmd (list "_prev"))
 
  (if (not (setq flag (findfile temp)))
      (progn
       (if ss
           (princ "\nTemp file creation failed.")
           (progn
            (setq flag T)
            (setq laylst (cons layout laylst)) ;; delete it anyway
            (princ "\nNo objects found in layout.")
           );progn
       );if
       (princ (acet-str-format "\nUnable to merge layout %1." layout))
      );progn then
  );if
 
  flag
);defun acet-layoutmerge-wblock
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-groups-to-xdata ( prefix / lst na e1 lst2 a na2 e2 xd j b name n )
 
 (setq lst (acet-dict-name-list "acad_group"))
 (setq n 0)
 (repeat (length lst)
 (setq    a (nth n lst)
         na (acet-dict-ename "acad_group" a)
         e1 (entget na)
          a (acet-str-replace "*" "" a)
       name (strcat prefix a)
          b (list
             (cons 1000 (nth n lst))		;the original group name
             (cons 1000 (cdr (assoc 300 e1)))   ;the description
             (cons 1070 (cdr (assoc 70 e1)))    ;unnamed
             (cons 1071 (cdr (assoc 71 e1)))    ;selectable
            );list
       lst2 (acet-list-m-assoc 340 e1)
 );setq
 (regapp name)
 
  (setq j 0)
  (repeat (length lst2)
  (setq na2 (cdr (nth j lst2))
         e2 (entget na2)
         xd (assoc -3 (entget na2 '("*")))
         xd (cdr xd)
         xd (acet-list-assoc-put (cons name b) xd) ;replace or add the xdata
         xd (cons -3 xd)
  );setq
 
  (setq e2 (append e2 (list xd)))
  (entmod e2)
  (setq j (+ j 1));setq
  );repeat  
 
 (setq n (+ n 1));setq
 );repeat
 
);defun acet-groups-to-xdata
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-xdata-to-groups ( prefix / ss na e1 xdlst n xd j lst2 x grpdata a name ac-group lst )
 
 (if (setq ss (ssget "_x" (list (list -3 
                                      (list (strcat prefix "*"))
                                );list
                          );list
              );ssget
     );setq
     (progn
      ;; loop through the selection set of all objects with group related xdata and build a list of
      ;; unique group names/definitions based on the data
      (setq n 0)
      (repeat (sslength ss)
      (setq    na (ssname ss n)
               e1 (entget na '("*"))
            xdlst (cdr (assoc -3 e1))
      );setq
       (setq j 0)
       (repeat (length xdlst)
       (setq       a (car (nth j xdlst)) ;the appid name
             grpdata (cdr (nth j xdlst)) ;raw xdata
       );setq
       (if (wcmatch (xstrcase a) (xstrcase (strcat prefix "*")))
           (progn
            ;;add the current ent to the list for the specified appid name
            (setq    x (cdr (cdr (assoc a lst2)))
                     x (append (list a grpdata na) x)
                  lst2 (acet-list-assoc-put x lst2)
            );setq
            ;; Update the item in lst so that it's only an appid so 
            ;; that the data can be removed later via entmod
            (setq xdlst (acet-list-assoc-put (list a) xdlst));setq
           );progn then
       );if
       (setq j (+ j 1));setq
       );repeat
 
       ;;;mod the ent to remove the xdata because we don't need it anymore 
       (if (and xdlst
                (not (member (cons -3 xdlst) e1)) ;xdata changed?
           );and
           (entmod (subst (cons -3 xdlst) (assoc -3 e1) e1)) 
       );if
      (setq n (+ n 1));setq
      );repeat
 
      ;; use the master list to actually entmake the groups
      (setq n 0)
      (repeat (length lst2)
       (setq       a (nth n lst2)
             grpdata (cadr a)            ;the properties of the group (selectable/description ...etc)
                 lst (cdr (cdr a))       ;the enames (strip the appidname and the group data)
                 lst (mapcar '(lambda (x) (cons 340 x)) lst)    ;add the 340 group code
                name (assoc 1000 grpdata)			;the original group name
             grpdata (vl-remove name grpdata)   		;remove the group name
                name (cdr name)
             grpdata (subst (cons 300 (cdr (assoc 1000 grpdata))) (assoc 1000 grpdata) grpdata) ;description
             grpdata (subst (cons 70 (cdr (assoc 1070 grpdata))) (assoc 1070 grpdata) grpdata)  ;unnamed flag
             grpdata (subst (cons 71 (cdr (assoc 1071 grpdata))) (assoc 1071 grpdata) grpdata)  ;selectable flag
                  e1 (dictsearch (namedobjdict) "ACAD_GROUP")
             ac-group (cdr (assoc -1 e1))
             grpdata (append (list '(0 . "GROUP") 	;; add the header
                                   '(100 . "AcDbGroup") 
                             );list
                             grpdata
                             lst  			;; add the entity names
                     );append
       );setq
       ;; if the specified group already exists then delete it
       (if (setq na (acet-dict-ename "acad_group" name))
           (progn
            (entdel na)
            (dictremove (cdr (assoc -1 e1)) name)
           );progn then
       );if
       (setq na (entmakex grpdata))                         ;Create the group
       (dictadd ac-group name na) 
 
      (setq n (+ n 1));setq
      );repeat
      (acet-appid-delete (strcat prefix "*")) ;remove the appids that were used to attach the group-related xdata
     );progn then
 );if
 
);defun acet-xdata-to-groups
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-layoutmerge-viewport-xdata-tag ( na / ob app lst lst2 gclst vlst )
 (vl-load-com)
 (setq  ob (vlax-ename->vla-object na)
       app "ACET-LAYOUTMERGE-VP-ON"
 );setq
 (if (not (tblobjname "appid" app))
     (regapp app)
 );if
 (setq   lst (list 1001 1070)
        lst2 (list (vlax-make-variant app vlax-vbString)
                   (vlax-make-variant 1 vlax-vbInteger)
             ) 
       gclst (vlax-make-safearray vlax-vbInteger 			;; initialize the arrays
                                  (cons 0 (- (length lst) 1))
             )
        vlst (vlax-make-safearray vlax-vbVariant 
                                  (cons 0 (- (length lst2) 1))
             )
 );setq
 
 (vlax-safearray-fill gclst lst)	;;fill the arrays
 (vlax-safearray-fill vlst lst2)
 
 (vla-setxdata ob gclst vlst)
 
);defun acet-layoutmerge-viewport-xdata-tag
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-layoutmerge-viewport-xdata-untag ( na / ob app lst lst2 gclst vlst )
 (vl-load-com)
 (setq  ob (vlax-ename->vla-object na)
       app "ACET-LAYOUTMERGE-VP-ON"
 );setq
 (if (not (tblobjname "appid" app))
     (regapp app)
 );if
 (setq   lst (list 1001)
        lst2 (list (vlax-make-variant app vlax-vbString)
             ) 
       gclst (vlax-make-safearray vlax-vbInteger 			;; initialize the arrays
                                  (cons 0 (- (length lst) 1))
             )
        vlst (vlax-make-safearray vlax-vbVariant 
                                  (cons 0 (- (length lst2) 1))
             )
 );setq
 
 (vlax-safearray-fill gclst lst)	;;fill the arrays
 (vlax-safearray-fill vlst lst2)
;(getstring "hey")
 (vla-setxdata ob gclst vlst)
;(getstring "hey2")
 
);defun acet-layoutmerge-viewport-xdata-untag
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mark on viewports with an xdata tag.
(defun acet-layoutmerge-tag-viewports ( / ss na e1 n )
 
  (setq ss (ssget "_x" '((0 . "VIEWPORT")
                        (-4 . "<AND")
                         (-4 . "<NOT") (68 . 0) (-4 . "NOT>") ;on
                         (-4 . "<NOT") (69 . 1) (-4 . "NOT>") ;not the paper space viewport
                        (-4 . "AND>")
                       )
           );ssget
  );setq
  (if ss
      (progn
       (setq n 0)
       (repeat (sslength ss)
        (setq na (ssname ss n)
              e1 (entget na)
        );setq
        (if (/= 0 (cdr (assoc 68 e1)))
            (acet-layoutmerge-viewport-xdata-tag na)
        );if
        (setq n (+ n 1));setq
       );repeat
      );progn then
  );if
);defun acet-layoutmerge-tag-viewports
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mark on viewports with an xdata tag.
(defun acet-layoutmerge-restore-tagged-viewports ( / app ss na e1 n )
  (vl-load-com)
  (setq app "ACET-LAYOUTMERGE-VP-ON"
         ss (ssget "_x" (list (list -3 (list app)))
            );ssget
  );setq
  (if ss
      (progn
       (setq n 0)
       (repeat (sslength ss)
       (setq na (ssname ss n)
             e1 (entget na)
       );setq
        (acet-layoutmerge-viewport-xdata-untag na)
        (if (= 1 (cdr (assoc 67 e1)))
            (vla-put-viewporton (vlax-ename->vla-object na) -1) ;turn the viewport back on.
        );if
       (setq n (+ n 1));setq
       );repeat
       (acet-appid-delete app)	;;delete the appid
      );progn then
  );if 
 
);defun acet-layoutmerge-restore-tagged-viewports


(acet-autoload2	'("Lman.lsp"	(bns_dl lstate)))
(acet-autoload2	'("Lman.lsp"	(bns_rl lstate)))
(acet-autoload2	'("Lman.lsp"	(bns_sl lstate)))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgBCdw4bp+O7PaXAAsEJvbXKUa9ydajQzVo8byKj
;;; Rv7bG9xauKS7MOmN5hMNXqIB01UvjOD1cvVwXva6uEFBl9O8lTB8n3h8AB8HsIq0
;;; v6vu+r+ElC5HNTGn1iDxOd1o0tAoSzG1NdTe1TAXzcmknoqUDRdCBgww19ZI0NNy
;;; 57bsUjSwhTO4EWKGDBkKT5X4RyZXWoI/8ByEnqL79XpHzvAPzlpqUbyWJQqFQOKZ
;;; TTseqHGGqngdcNUklX55LV4FXg744I1xs8tH1gWMlxVJKCpOXwX8qNvRrn6oOVtd
;;; 9/pCIXrtH7QbHz2Xb/6xnXz2pFYyHtoaUA5ubCGDoXzhRT/dQWIoXaUFuHcZ/5Sw
;;; S9wSXGn9FA8oPlJxg/OdWXGJBMtWj5NEactMUXrACBpBHA0+DvtxPXkJNLm/kgFO
;;; 0Ke8oYfuCPPq9uJY/V6AA4o1Ui+lEAwJP3iX5hB9Dg6lby6DRQkmnv6eNAWihkKt
;;; m6u2faCgyTpG0HGOnqxknWNA36T8CprfxBgEtt0i7/UPvBnD0zu5W7ctu6QgnfLT
;;; gCRFrwGZ7qgiZqkj1pVHHbNXVCJCThxZCfxy3nQ+xcxONPGG2gEy5u41budw9r0D
;;; hRvNHe1HDtro11mf/qK8KtJ55BF9WveipuWMUb8UKZLELhhs3ylJes/NC8PG9WaA
;;; THjc8g==
;;; -----END-SIGNATURE-----