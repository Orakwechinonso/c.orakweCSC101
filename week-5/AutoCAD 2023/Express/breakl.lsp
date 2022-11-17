;;
;;;
;;;    BREAKL.LSP - Written by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BREAKLINE
;This routine allows you to quickly create breaklines by selecting two points and
;optionaly a third point to specify a non-midpoint location for the break symbol.
; You can configure BREAKLINE settings such as break symbol to use, the size and
; extension distance by pressing "Enter" at the first prompt.
;   You can customize this feature in several ways:
;
;    Break symbol BLOCK -
;      The default break symbol is a drawing called BRKLINE.DWG that is
;    provided in the [ACADDIR..]\EXPRESS directory. However, if
;    you wish, you can use your own custom symbol by defining a block
;    you like and then setting the options in BREAKLINE to use that block.
;      There is only one requirement that must be met in order for BREAKLINE to
;    use your custom block. The block must contain two POINT objects that reside
;    on the DEFPOINTS layer. These points are used for aligning the symbol along
;    a breakline and for scaling it.
;     NOTES ON DEFPOINTS: If your drawing does not already have a defpoints layer
;    you can create it. Also note that points on the defpoints layer will not
;    show on plots and these points also will not display according to pdmode
;    settings. (Points on the defpoints layer always display as a dot.)
;
;    Break symbol SIZE -
;      In the options sub-prompt there is a "Size" option that allows you to control the
;    scale that the breakline symbol will appear at. This value is scaled by the current
;    dimscale setting in your drawing. (This analagous to the behavior of many DIM
;    variables such as DIMTXT and DIMEXE.)
;      The overall size of the symbol is affected by the distance between the
;    defpoints contained in the break symbol block that is used and the
;    relationship between dimscale and the BREAKLINE size setting.
;    The overall size is described by the expression:
;      A*S*D
;      Where A is the distance between the defpoints in the block as created.
;            S is the BREAKLINE size setting
;            D is the current DIMSCALE setting.
;
;     For example: Using the default block, BRKLINE.DWG, which has a distance between
;                  defpoints of 1, and a BREAKLINE size setting of 0.5 and dimscale
;                  is set at 48.
;                    The resulting distance between defpoints will be 24. (1 * 0.5 * 48)
;
;    Breakline extension distance -
;      BREAKLINE will create a line that extends beyond the two points picked by a
;    specified amount. This extension distance is similar to the DIMEXE extension
;    distance for dimension entities. Similarly, the value is also affected by
;    the current DIMSCALE setting.
;
(defun c:breakline ( / p1 p2 p3 x1 x2 ss lk flag sca size ds os)
 
(acet-error-init
 (list
  (list "HIGHLIGHT" 0
          "CMDECHO" 0
           "attreq" 0
         "limcheck" 0
           "osmode" (getvar "osmode")
          "celtype" "continuous"
  );list
  T
 );list
);acet-error-init
 
(sssetfirst nil nil)
 
(setq lk (acet-layer-unlock-all))
 
(bns_breakline_blk)
(setq size (bns_breakline_blksize))
(bns_breakline_blkexe)
(bns_breakline_print_settings)
 
 
(while (not flag)
 (initget "Block Size Extension eXit" 128)
 (setq p1 (getpoint "\nSpecify first point for breakline or [Block/Size/Extension]: "));setq
 (cond
  ((equal p1 "Block")
   (bns_breakline_getblkname nil)
  );cond #1
  ((equal p1 "Size")
   (bns_breakline_getblksize)
  );cond #2
  ((equal p1 "Extension")
   (bns_breakline_getblkexe)
  );cond #3
  ((equal (type p1) 'LIST)
   (setq flag T)
  );cond #4
  (T
   (princ "\nInvalid. Expects a point or Block, Size, or Extension.")
  );cond #5
 );cond close
);while
 
(if (equal (setq ds (getvar "dimscale")) 0.0)
    (setq ds 1.0)
);if
(setq  ss (bns_breakline_get_blk_info (bns_breakline_blk)) ;returns a list i.e.  (ss x1 x2)
       x1 (cadr ss)
       x2 (caddr ss)
       ss (car ss)
      sca (/ (* ds size) (distance x1 x2))
);setq
 
 
(setq p2 p1)
(while (< (distance p1 p2)
           (* (distance x1 x2) (abs sca))
       )
 (initget 1)
 (setq p2 (getpoint p1 "\nSpecify second point for breakline: "));setq
 (if (<= (distance p1 p2)
         (* (distance x1 x2) (abs sca))
     )
     (princ "\n*Invalid* Points are too close together to insert break symbol.")
 );if
);while
 
(if ss
    (progn
     (setq flag nil
             os (getvar "osmode")
     )
     (if (not (equal 512 (logand 512 os)))
         (setq os (+ os 512))
     );if
     (if (equal 16384 (logand 16384 os)) ;;;osnaps are off
         (setq os (- os 16384))
     );if
     (if (/= os (getvar "osmode"))
         (progn
          (acet-sysvar-set (list "OSMODE" os))
          (setq flag T)
         );progn then
     );if
     (command "_.line" p1 p2 "")
 
     (while (not p3)
      (setq p3 (getpoint "\nSpecify location for break symbol <Midpoint>: "));setq
      (if (not p3)
          (setq p3 (acet-geom-midpoint p1 p2))
          (progn
           (setq p3 (bns_perp_inters p1 p2 p3 1))
           (if (not p3)
               (princ "\n*Invalid* Point is not on the segment formed by points one and two.")
           );if
          );progn then
      );if
     );while
     (if flag
         (acet-sysvar-restore)
     );if
     (entdel (entlast))
     (bns_breakline p1 p2 p3 ss x1 x2)
 
    );progn then
);if
 
(if lk
    (command "_.layer" "_lock" lk "")
);if
 
(acet-error-restore)
);defun c:breakline
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_breakline ( p1 p2 p3 ss x1 x2 / d x3 size na na2 sca ds)
 
(setvar "osmode" 0)
(if (equal (setq ds (getvar "dimscale")) 0.0)
    (setq ds 1.0)
);if
 
(setq size (bns_breakline_blksize))
(setq x3 (acet-geom-midpoint x1 x2));setq
(command "_.move" ss "" x3 p3)
(setq  d (acet-geom-delta-vector x3 p3)
      x3 (acet-geom-vector-add d x3)
      x1 (acet-geom-vector-add d x1)
      x2 (acet-geom-vector-add d x2)
);setq
(command "_.rotate" ss "" p3 "_r" x1 x2 p2);command
(setq   d (- (angle p1 p2) (angle x1 x2))
       x1 (acet-geom-point-rotate x1 x3 d)
       x2 (acet-geom-point-rotate x2 x3 d)
      sca (/ (* ds size) (distance x1 x2))
);setq
(command "_.scale" ss "" p3 sca);command
(setq x1 (polar x3
                (angle x3 x1)
                (* sca (distance x1 x3))
         );polar
      x2 (polar x3
                (angle x3 x2)
                (* sca (distance x2 x3))
         );polar
      p1 (polar p1 (angle p2 p1) (* ds #bns_breakline_blkexe))
      p2 (polar p2 (angle p1 p2) (* ds #bns_breakline_blkexe))
);setq
(acet-ss-visible ss 0)
 
(command "_.pline" p1 x1 "")
(setq na (entlast))
(command "_.pline" x2 p2 "")
(setq na2 (entlast))
(command "_.pedit" na "_join" na na2 ss "" "_x")
(command "_.select" na na2 ss "")
 
(if (and (setq ss (ssget "_p"))
         (> (sslength ss) 1)
    );and
    (acet-group-make-anon (list ss) "BREAKLINE")
);if
);defun bns_breakline
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;initialize the default block name to use.
;performs some checking to ensure the block is valid.
(defun bns_breakline_blk ( / bna dwgna flag ss)
 
 (if (not #bns_breakline_blk)
     (progn
      (setq #bns_breakline_blk (acet-getvar (list "ACET.BREAKLINE-BLOCK")))
      (setq flag T) ;set a flag to show this is the first time this is used this session
     );progn
 );if
 (if (or (not #bns_breakline_blk)      ;set the default is non exists.
         (equal "" #bns_breakline_blk)
     );or
     (setq #bns_breakline_blk "BRKLINE.DWG");setq
 );if
 (acet-setvar (list "ACET.BREAKLINE-BLOCK" #bns_breakline_blk 2)) ;save in profile only
 
 (setq dwgna (xstrcase #bns_breakline_blk)
       dwgna (acet-str-space-trim dwgna)              ;the dwgname, possibly w/path
       dwgna (acet-filename-ext-remove dwgna)
 );setq
 (if (acet-str-m-find "=" dwgna)
     (setq dwgna (acet-str-to-list "=" dwgna)
             bna (car dwgna)
           dwgna (cadr dwgna)
     );setq then
     (setq bna (acet-filename-path-remove (acet-filename-ext-remove dwgna)));setq else the base blockname
 );if
 (if (not (snvalid bna))
     (progn
      (princ "\nInvalid block name. Reseting to default.")
      (setq #bns_breakline_blk "BRKLINE.DWG");setq
 
      (bns_breakline_getblkname "BRKLINE.DWG")
 
     );progn then
 );if
 (if (and (not (member bna (acet-table-name-list "block")))
          (not (findfile (strcat dwgna ".DWG")))
          (not (findfile (strcat dwgna ".DWT")))
     );and
     (progn
      (princ (strcat "\nCannot find \""
                     bna
                     "\" or \""
                     dwgna ".DWG" "."
             );strcat
      )
      (setq #bns_breakline_blk "BRKLINE.DWG");setq
 
      (bns_breakline_getblkname "BRKLINE.DWG")
     );progn then
 );if
 
 (acet-setvar (list "ACET.BREAKLINE-BLOCK" #bns_breakline_blk 2))
 
 (if flag
     (progn
 
      (setq ss (bns_breakline_get_blk_info #bns_breakline_blk)
            ss (car ss)
      );setq
      (if (and ss
               (> (sslength ss) 0)
          );and
          (command "_.erase" ss "")
      );if
     );progn
 );if
 
 #bns_breakline_blk
);defun bns_breakline_blk
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;initialize the block size to use
(defun bns_breakline_blksize ()
 (if (not #bns_breakline_blksize)
     (setq #bns_breakline_blksize (acet-getvar (list "ACET.BREAKLINE-BLKSIZE")))
 );if
 (if (not #bns_breakline_blksize)
     (setq #bns_breakline_blksize 0.5);setq
 );if
 (acet-setvar (list "ACET.BREAKLINE-BLKSIZE" #bns_breakline_blksize 2))
 #bns_breakline_blksize
);defun bns_breakline_blksize
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;initialize the extension distance to use
(defun bns_breakline_blkexe ()
 (if (not #bns_breakline_blkexe)
     (setq #bns_breakline_blkexe (acet-getvar (list "ACET.BREAKLINE-BLKEXE")))
 );if
 (if (not #bns_breakline_blkexe)
     (setq #bns_breakline_blkexe (getvar "dimexe"));setq
 );if
 (acet-setvar (list "ACET.BREAKLINE-BLKEXE" #bns_breakline_blkexe 2))
 #bns_breakline_blkexe
);defun bns_breakline_blkexe
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_breakline_print_settings ( / )
 
(princ (strcat "\nBlock= " #bns_breakline_blk
               ", Size= " (rtos #bns_breakline_blksize)
               ", Extension= " (rtos #bns_breakline_blkexe)
       );strcat
);princ
 
);defun bns_breakline_print_settings
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Handles prompting for breakline block name.
;
(defun bns_breakline_getblkname ( def / lst lst2 a b flag na )
 
(if (not def)
    (setq def (bns_breakline_blk))
);if
 
(setq  lst (acet-table-name-list '("block" 4 16))
      lst2 (acet-table-name-list "block")
);setq
 
(while (not flag)
 
(setq flag nil
         a (strcat "\nEnter the block name for breakline symbol <"
                    def
                    ">: "
           );strcat
         a (getstring T a)
         a (xstrcase (acet-str-space-trim a))
);setq
(if (equal a "")
    (setq a def);setq
);if
(cond
 ((and (acet-str-m-find "=" a)             ;the "=" syntax for inserting long filenames
       (setq b (acet-str-to-list "=" a))
       (setq a (cadr b))
       (setq b (car b))
       (setq a (acet-str-space-trim a)
             b (acet-str-space-trim b)
       );setq
       (if (equal a "")
           (setq a b)
           a
       );if
       (setq a (strcat (acet-filename-ext-remove a) ".DWG"))
       (snvalid b)
       (acet-file-find a)
  );and
   (setq flag (strcat b "=" a))
 );cond #1 contains =
 ((and (equal a "~")
       (setq a (ACET-FILE-READDIALOG "Breakline symbol block:" def "DWG" "Acet:BreakL" 1664))
       (setq a (xstrcase a)
             a (acet-filename-ext-remove a)
             b (acet-filename-path-remove a)
             a (strcat a ".DWG")
       );setq
       (snvalid b)
       (acet-file-find a)
  );and
   (setq flag a)
 );cond #2 enter a block name through filedia
 ((and a
       (not (acet-str-m-find "=" a))
       (not (equal a "~"))
       (setq a (acet-filename-ext-remove a))
       (setq b (acet-filename-path-remove a))
       (setq a (strcat a ".DWG"))
       (snvalid b)
       (or (acet-file-find a)
           (member b lst)
       );or
  );and
   (setq flag a)
 );cond #3
 ((and b
       (not (snvalid b))
       a
       (acet-file-find a)
  );and
  (princ "\nInvalid block name.")
  (if (member b lst2)
      (princ "\nExternaly referenced blocks are not allowed.")
      (princ (strcat "\nWarning: If you are trying to insert the file: "
                     a
                     "\nit must be inserted using the <block>=<filename> syntax."
             );strcat
      );princ then
  );if
 );cond #4
 ((and a
       (not (acet-file-find a))
  );and
  (princ (strcat "\nCannot find block " a "."))
 );cond #5
);cond close
 
(if flag
    (progn
     (setq na (entlast))
     (command "_.insert" flag)
     (acet-cmd-exit)
     ;(while (wcmatch (getvar "cmdnames") "*INSERT*") (command nil))
     (if (acet-str-m-find "=" flag)
         (setq flag (car (acet-str-to-list "=" flag)))
     );if
     (entmake (list '(0 . "INSERT") (cons 2 (vl-filename-base flag))
                    '(60 . 1)
                    '(10 0.0 0.0 0.0) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0)
              )
     );entmake
     (if (equal na (entlast))
         (setq flag nil)
         (progn
          (entdel (entlast))
          (setq #bns_breakline_blk flag)
          (acet-setvar (list "ACET.BREAKLINE-BLOCK" flag 3))
         );progn then
     );if
    );progn then
);if
 
);while
 
 
flag
);defun bns_breakline_getblkname
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;handles prompting for block insert size
;and also places new default value in the registry.
;returns the entered value or the deafult if accepted
(defun bns_breakline_getblksize ( / def a size)
 
(setq size (bns_breakline_blksize)
       def (rtos size)
);setq
(initget 6);non zero and no negative numbers
(setq a (getdist (strcat "\nBreakline symbol size <"
                         def
                         ">: "
                 );strcat
        );getdist
);setq
(if (not a)
    (setq #bns_breakline_blksize size);setq
    (setq #bns_breakline_blksize a);setq
);if
(acet-setvar (list "ACET.BREAKLINE-BLKSIZE" #bns_breakline_blksize 3))
#bns_breakline_blksize
);defun bns_breakline_getblksize
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;handles prompting for extension distance
;and also places new default value in the registry.
;returns the entered value or the deafult if accepted
(defun bns_breakline_getblkexe ( / def a exe)
 
(setq exe (bns_breakline_blkexe)
      def (rtos exe)
);setq
(initget 4);no negative numbers
(setq a (getdist (strcat "\nBreakline extension distance <"
                         def
                         ">: "
                 );strcat
        );getdist
);setq
(if (not a)
    (setq #bns_breakline_blkexe exe);setq
    (setq #bns_breakline_blkexe a);setq
);if
(acet-setvar (list "ACET.BREAKLINE-BLKEXE" #bns_breakline_blkexe 3))
#bns_breakline_blkexe
);defun bns_breakline_getblkexe
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a block name and returns a list containing a selection set and a list of
;two points.
(defun bns_breakline_get_blk_info ( bna / na na2 ss ss2 ss3 p1 p2 lst)
 
(setq p1 (acet-geom-view-points)
      p2 (cadr p1)
      p1 (car p1)
      p1 (polar p2 (angle p1 p2) (distance p1 p2))
      na (entlast)
);setq
(command "_.insert" bna p1 1 1 0)
(if (not (equal na (entlast)))
    (progn
     (command "_.explode" (entlast))
     (while (wcmatch (getvar "cmdnames") "*EXPLODE*")
      (command "")
     );while
     (setq ss (acet-ss-new na) ;(ssget "_p")
           p1 nil
           p2 nil
     );setq
     ;explode the selection set again in case there are spline/fit polylines inside.
     ;(they don't join very well.)
    );progn
);if
(if ss
    (progn
     (setq ss3 (ssget "_p" '((0 . "*POLYLINE"))));setq
     (setq na (entlast))
     (command "_.explode" ss3)
     (while (wcmatch (getvar "cmdnames") "*EXPLODE*")
      (command "")
     );while
     (if (and (setq ss3 (acet-ss-new na))
              (> (sslength ss3) 0)
         );and
         (progn
          (command "_.select" ss ss3 "")
          (setq ss (ssget "_p"));setq
         );progn then
         (command "_.select" ss "");explode may have cleared the selset so select it again
     );if                          ;for (ssget "p"..) call below
    );progn then
);if
(if ss
    (progn
     (acet-ss-visible ss 1)
     (setq ss2 (ssget "_p" '((0 . "POINT") (8 . "DEFPOINTS"))));setq
    );progn
);if
(if (and ss2
         (= (sslength ss2) 2)
    );and
    (progn
     (setq  na (ssname ss2 0)
            ss (ssdel na ss)
            p1 (cdr (assoc 10 (entget na)))
            p1 (trans p1 0 1)
     );setq
     (entdel na)
     (setq  na (ssname ss2 1)
            ss (ssdel na ss)
            p2 (cdr (assoc 10 (entget na)))
            p2 (trans p2 0 1)
     );setq
     (entdel na)
    );progn then
    (setq ss2 nil)
);if
(cond
 ((not ss)
  (princ "\nUnable to insert block.")
  (exit)
 )
 ((not ss2)
  (alert "Invalid block. \nBlock must contain two POINT objects on the DEFPOINTS layer.")
  (setq lst (bns_breakline_get_blk_info (bns_breakline_getblkname nil)))
 );cond #1
 ((equal p1 p2)
  (alert "Invalid block. \nBlock POINT objects must not be coincident.")
  (bns_breakline_getblkname nil)
  (setq lst (bns_breakline_get_blk_info (bns_breakline_getblkname nil)))
 );cond #2
 (T
  (setq lst (list ss p1 p2));setq
 );cond #3
);cond close
 
lst
);defun bns_breakline_get_blk_info
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes three points and a flag and returns the perpindicular intersection
;from point p3 to the segement p1-p2.
;The flag argument is an "onsegment" argument.
;flag=true means return the intersection only if it is on segment p1-p2
;flag=nil means return the intersection as if segment p1-p2 was infinite in length.
;
(defun bns_perp_inters ( p1 p2 p3 flag / p4 a d)
 
(setq  a (angle p1 p2)
       d (* 2.0 (distance (getvar "extmax") (getvar "extmin")))
      p4 (polar p3
                (+ a (/ pi 2.0))
                d
         )
      p3 (polar p3
                (- a (/ pi 2.0))
                d
         );polar
       a (inters p1 p2 p3 p4 flag)
);setq
 
a
);defun bns_perp_inters


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgCWAmMJJQfdEoSxiVgHFhI3F3ofXVZi83r8tweg
;;; uPItEvdDriQc3K62tUz5HoBblFWJGaD4CygsPEVSb7cl7acTOJzgU8WmnSYgR5WV
;;; GX//GDPUpxBTKMyW3EMrUhQc1XX2tvrUpIhuX272ojWCG54ZCYAsNA31wzSFlKoa
;;; q01kSgsV2WveTqZ2zrNwmlxUTQEOFRGeGuOb/nrK0zjed56wkCvIeg70dEqwcE9R
;;; Z/frKqHWbbWc0I2kzd6sdf7FXFSVMPIeHcULDNdhA8+wxLP34yYn0bq9CrwG8S29
;;; rfDgrAce7sBoCpRMN1pD3zDjuSWYgywLGxFMc1m8/17mHOF7v20/WGm+W3a2Medo
;;; 3w97k5qI9wLcfJJYFn8Lzam7bbRDd1/0pu5kQj7jZqPowGa5M5U6LTL14mEOeZMT
;;; U+KW12mjV9UtZBuoEYbLBCMDjM8FEmGEsySb5OCOeAo89xZoUuYkWTPMnBTuC8Ju
;;; 2IdXhJSwgQ7N4V+ATV64SjNri75e85Opg+CiVWwODFj4D9tGSCthaEsW+UXzm9l2
;;; M03euU01GXRmfZdSZZFHka+GEFOpBemXFxo0docpZG2UvNhtAZJurWVkjaFn5BVR
;;; IaA+FUi/adC/Vkz38YKkHwFiy/O50ouUinuwssdkFfPF4aAvfIow8iQYG5xz8+6/
;;; SKvScA==
;;; -----END-SIGNATURE-----