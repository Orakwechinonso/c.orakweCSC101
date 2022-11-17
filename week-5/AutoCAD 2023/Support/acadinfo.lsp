;;  acadinfo.lsp - report installation information
;;
;;  Copyright 2022 Autodesk, Inc.  All rights reserved.
;;
;;  Use of this software is subject to the terms of the Autodesk license 
;;  agreement provided at the time of installation or download, or which 
;;  otherwise accompanies this software in either electronic or hard copy form. 
;;
;;  ----------------------------------------------------------------
;;
;;  This file defines the ACADINFO command, which locates various 
;;  data items and generates an 'acadinfo.txt' file.  This file 
;;  can be used to analyze installation and setup problems.
;;

(vl-load-com)

(if (and (not (member "acapp.arx" (arx)))
         (setq fna (findtrustedfile "acapp.arx"))
    );and
    (arxload fna "\nLoad of 'acapp.arx' failed")
);if


(setq acet:acadinfo-version "24.2")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getAcadLspYear ( / filename currentYear year endYear name found )
 (if (not acet:acadinfo-productYear)
     (progn
       ;; Initialize variable: acet:acadinfo-productYear
       (setq currentYear (fix (/ (getvar "cdate") 10000))
                    year (+ currentYear 3)
                 endYear (- year 50)
       )
       (while (and (not found)
                   (> year endYear)
              )
          (setq name (strcat "acad" (itoa year)))
          (if (or (findfile (strcat name ".lsp"))
                  (findfile (strcat name ".fas"))
              )
              (setq found year)
          )
          (setq year (- year 1))
       );while
       (if found
           (setq acet:acadinfo-productYear (itoa found))
           (setq acet:acadinfo-productYear "")
       )
     );progn
 );if 
 acet:acadinfo-productYear
);defun getAcadLspYear

(defun acadProductLspFileName ()
 (strcat "acad" (getAcadLspYear) ".lsp")
)

(defun acadProductDocLspFileName ()
 (strcat "acad" (getAcadLspYear) "doc.lsp")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:acadinfo ( / fna fh op )

;initialize a bare bones error handler that will recover from a first try
;load_test failure and re-issue this command.

  (if (not Acet:Acadinfo-Olderr)
      (setq Acet:Acadinfo-Olderr *error*)
  );if

  (defun *error* ( msg / )
    (if Acet:Acadinfo-Error-On-Load-Test
      (progn
        (if fh
            (close fh)
        );if
        (setq *error* Acet:Acadinfo-Olderr
              Acet:Acadinfo-Olderr nil
        );setq
        (c:acadinfo)
        (setq Acet:Acadinfo-Error-On-Load-Test nil)
      );progn
      (princ msg)
    );if
  );defun *error*


  ;; prompt to open a text file to write info
  (if (not Acet:Acadinfo-Error-On-Load-Test)
    (progn
      (setq docDir (getvar "MYDOCUMENTSPREFIX"))
      (setq initFile (strcat docDir "\\acadinfo"))
      (setq fna (getfiled "Select File Name" initFile "txt" 33))
    )
  )

  (if Acet:Acadinfo-Error-On-Load-Test
    (setq op "a")
    (progn
       (textscr)
       (princ (strcat "\n"
                      "\nACADINFO is a utility for gathering information about"
                      "\nyour AutoCAD installation and current setup. The routine"
                      "\nwill examine your system and write a text file called"
                      "\n\'" fna "\'"
		              "\nto your hard drive."
              );strcat
       );princ
       (getstring "\nPress ENTER to continue or ESC to cancel... ")

       (princ "\n\nExamining your AutoCAD setup. Please wait...\n")
       (setq op "w")
    );progn then
  );if

  (if (setq fh (open fna op))
    (progn
      (close fh) (setq fh (open fna op)) ;close and re-open again in case of
                                         ;garbage echo from error recovery.

      (if (not Acet:Acadinfo-Error-On-Load-Test)
        (progn

         (princ "\Performing load tests...")

          (acet-acadinfo-do-header fh)

          (acet-acadinfo-do-general fh)

          (acet-acadinfo-do-express fh)

          (acet-acadinfo-do-fileloads fh)

          (write-line "Tests for successful load of LISP initialization files." fh)
          (write-line (acet-acadinfo-test-load (acadProductLspFileName)) fh)
          (write-line (acet-acadinfo-test-load (acadProductDocLspFileName)) fh)
          (write-line (acet-acadinfo-test-load "acettest.fas") fh)
          (write-line (acet-acadinfo-test-load "acetutil.fas") fh)
          (write-line (acet-acadinfo-test-load "acetmain.mnl") fh)
        );progn then
        (progn
          (write-line "" fh)
          (write-line "*****FAILURE during lisp file load tests.**** " fh)
          (write-line "One of the following files causes an error on load: " fh)
          (write-line (strcat "  " (acadProductLspFileName))  fh)
          (write-line (strcat "  " (acadProductDocLspFileName))  fh)
          (write-line "  acettest.fas" fh)
          (write-line "  acetutil.fas" fh)
          (write-line "  acetmain.mnl" fh)
        );progn else
      );if

      (write-line "" fh)
      (write-line (strcat "(arx) -> " (acet-acadinfo-item-to-string (arx))) fh)
      (write-line "" fh)

      (write-line " ------------------------- TYPELIB TEST -------------------------" fh)
      (write-line "" fh)
      (acet-acadinfo-check-typelib fh)
      (write-line "" fh)


      (write-line " ------------------- SYSTEM VARIABLE SETTINGS -------------------" fh)
      (write-line "|;" fh)

      (close fh)

      (acet-acadinfo-vars-to-scr fna -1);append

      (acet-acadinfo-lisp-dump fna)
      (princ "\nDone.")
    );progn then
    (princ "\nCannot open file for write.")
  );if

  (setq *error* olderr)

  (princ)
);defun c:acadinfo


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-do-header ( fh )
  (write-line ";|" fh)
  (write-line "" fh)
  (write-line (strcat " Using ACADINFO.LSP version " acet:acadinfo-version) fh)
  (write-line "" fh)
  (write-line " Your Name           : " fh)
  (write-line " Your E-mail Address : " fh)
  (write-line " Your Phone Number   : " fh)
  (write-line " Problem Description : " fh)
  (write-line "" fh)
  (write-line "" fh)
  (write-line "" fh)
  (write-line "" fh)
  (write-line "" fh)
  (write-line " Steps To Reproduce" fh)
  (write-line "   1. " fh)
  (write-line "   2. " fh)
  (write-line "   3. " fh)
  (write-line "   4. " fh)
  (write-line "   5. " fh)
  (write-line "   ..." fh)
  (write-line "" fh)
  (write-line "" fh)
)

(defun acet-acadinfo-do-general ( fh )
  (write-line " ---------------------- GENERAL INFORMATION ---------------------" fh)
  (write-line "" fh)
  (write-line (strcat "User Name: "
                (acet-acadinfo-item-to-string (getenv "USERNAME"))
              );strcat
              fh
  );write-line

  (write-line (strcat "Computer Name: "
                (acet-acadinfo-item-to-string (getenv "COMPUTERNAME"))
              );strcat
              fh
  );write-line

  (write-line (strcat "Platform: "
                (acet-acadinfo-item-to-string (getvar "PLATFORM"))
              );strcat
              fh
  );write-line

  (write-line "" fh)
)

(defun acet-acadinfo-do-fileloads (fh / a b)
  (write-line " ------------------- FILE LOADING INFORMATION -------------------" fh)
  (write-line "" fh)
  (setq a (findfile (acadProductLspFileName)));setq
  (setq a (strcat (acadProductLspFileName) " FOUND AT: " (acet-acadinfo-item-to-string a)));setq
  (write-line a fh)
  (setq a (findfile (acadProductDocLspFileName)));setq
  (setq a (strcat (acadProductDocLspFileName) " FOUND AT: " (acet-acadinfo-item-to-string a)));setq
  (write-line a fh)

  (write-line "" fh)
)

(defun acet-acadinfo-do-express (fh / a b)
  (write-line " ------------------ EXPRESS TOOLS INFORMATION -------------------" fh)
  (write-line "" fh)
  (setq a (findfile (acadProductDocLspFileName)));setq
  (setq b (acet-acadinfo-item-to-string (acet-acadinfo-acaddoc-lsp-check a)));setq
  (write-line (strcat (acadProductLspFileName) " Express Tools load: " b) fh) ;the line that loads Express

  (write-line a fh)
  (setq a (findfile "acetutil.arx")
       a (strcat "ACETUTIL.ARX FOUND AT: " (acet-acadinfo-item-to-string a))
  );setq
  (write-line a fh)

  (setq a (findfile "acettest.fas")
       a (strcat "ACETTEST.FAS FOUND AT: " (acet-acadinfo-item-to-string a))
  );setq
  (write-line a fh)

  (setq a (findfile "acetutil.fas")
       a (strcat "ACETUTIL.FAS FOUND AT: " (acet-acadinfo-item-to-string a))
  );setq
  (write-line a fh)

  (setq a (findfile "acetauto.lsp")
       a (strcat "ACETAUTO.LSP FOUND AT: " (acet-acadinfo-item-to-string a))
  );setq
  (write-line a fh)

  (setq a (findfile "acetmain.mnl")
       a (strcat "ACETMAIN.MNL FOUND AT: " (acet-acadinfo-item-to-string a))
  );setq
  (write-line a fh)


  (write-line "" fh)
  (write-line (strcat "(menugroup \"EXPRESS\") = " (acet-acadinfo-item-to-string (menugroup "EXPRESS"))) fh)
  (write-line "" fh)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-check-typelib ( fh / fna key1 key2)
  (setq key1 "HKEY_CLASSES_ROOT\\TypeLib\\{C094C1E2-57C6-11d2-85E3-080009A0C626}\\1.1\\0\\win32"
        key2 "HKEY_CLASSES_ROOT\\TypeLib\\{C094C1E2-57C6-11d2-85E3-080009A0C626}\\1.1\\9\\win32"
        key1 (vl-registry-read key1)
        key2 (vl-registry-read key2)
  );setq
  (cond
    ((not key1)
      (write-line "Cannot open registry key 0 for AcAx24enu.tlb check" fh)
    )
    ((not key2)
      (write-line "Cannot open registry key 9 for AcAx24enu.tlb check" fh)
    )
    ((not (findfile key1))
      (write-line (strcat "Cannot locate '" key1 "'.") fh)
    )
    ((not (findfile key2))
      (write-line (strcat "Cannot locate '" key2 "'.") fh)
    )
    (T
      (write-line "'AcAx24enu.tlb' file located." fh)
    )
  )
);defun acet-acadinfo-check-typelib

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:lspdump ( / fna )

(setq fna (getvar "dwgname")
      fna (substr fna 1 (- (strlen fna) 4))
      fna (strcat fna ".dmp")
);setq
(acet-acadinfo-lisp-dump fna)

(princ)
);defun c:lspdump

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:addvars2scr ( / fna )
 (setq fna (getfiled
             "Enter the name of the script to add your variable settings to"
             (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4))
             "SCR"
             0
           );getfiled
 );setq
 (acet-acadinfo-vars-to-scr fna 0)
 (princ)
);defun c:addvars2scr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:vars2scr ( / fna)
 (setq fna (getfiled
             "Enter the name of the script file to create"
             (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4))
             "SCR"
             1
           );getfiled
 );setq
 (acet-acadinfo-vars-to-scr fna 1)
 (princ)
);defun c:vars2scr


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-product-key ()
 (cond
  ( acet-reg-prodkey
    (acet-reg-prodkey)
  )
  ( bns_get-product-key
    (bns_get-product-key)
  )
  (T
   ""
  )
 );cond close
);defun acet-acadinfo-product-key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-test-load (fna / a)
  (if (setq a (findfile fna))
    (progn
      (setq Acet:Acadinfo-Error-On-Load-Test T) ;this is so the error handler can recover
      (setq a (load fna 99))
      (setq Acet:Acadinfo-Error-On-Load-Test nil)
      (if (not (equal a 99))
        (setq a (strcat "Load test passed. File: " fna));setq then
        (setq a (strcat "Load test *FAILED*. Load failed for file: " fna));setq else
      );if
    );progn then
    (setq a (strcat "Load test *FAILED*. File not found: " fna));setq else
  );if
  a
);defun acet-acadinfo-test-load

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-acaddoc-lsp-check ( fna / fh a b)
  (if fna
    (progn
      (if (setq fh (open fna "r"))
        (progn
          (while (setq b (read-line fh))
            (setq b (strcase b))
            (if (wcmatch b "*(LOAD \"ACETTEST.FAS*")
              (setq a b);setq then
            );if
          );while
          (close fh)
        );progn then
        (setq a (strcat "UNABLE TO READ: " (acadProductDocLspFileName)));setq else
      );if
    );progn then
    (setq a (strcat (acadProductDocLspFileName) " NOT FOUND!"))
  );if
  (if (not a)
    (setq a "No load statement for Express Tools.")
  )
  a
);defun acet-acadinfo-acaddoc-lsp-check



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-lisp-dump ( fna / fh lst lst2 lst3 n a b c)

(princ "\nFormatting and writing ouput. Please wait....")
(setq  lst (atoms-family 1)
       lst (acad_strlsort lst)
);setq
(setq n 0)
(repeat (length lst)
 (setq a (nth n lst)
       b (eval (read a))
       b (acet-acadinfo-pre-process-strings b)
       c (acet-acadinfo-item-to-string b)
       a (strcat "(setq " a "\t")
       c (strcat c ")")
 );setq
 (if (equal (type b) 'LIST)
     (setq c (strcat "'" c));setq then
 );if
 (setq a (strcat a c));setq
 (if (or (equal (type b) 'EXRXSUBR) ;arx
         (equal (type b) 'SUBR)     ;internal lisp
         (equal (type b) 'FILE)
         (equal (nth n lst) "PAUSE")
         (equal b (princ))          ;causes an error on load
     );or
     (setq    a (strcat ";" a)
           lst3 (append lst3 (list a))
     );setq then
     (setq lst2 (append lst2 (list a)));setq else
 );if
(acet-acadinfo-spinner)
(setq n (+ n 1));setq
);repeat

(if (setq fh (open fna "a"));setq
    (progn
  (acet-acadinfo-spinner)
     (setq n 0);setq
     (repeat (length lst3)          ;commented lisp
      (setq a (nth n lst3));setq
      (write-line a fh)
      (setq n (+ n 1));setq
     );repeat
  (acet-acadinfo-spinner)

     (setq n 0);setq               ;active lisp
     (repeat (length lst2)
      (setq a (nth n lst2));setq
      (write-line a fh)
      (setq n (+ n 1));setq
     );repeat
     (write-line "(princ)" fh)
     (close fh)
  (acet-acadinfo-spinner)
     (princ (strcat "\n\nOutput written to: " (acet-acadinfo-item-to-string (findfile fna)) ))
    );progn then
    (princ "\nWrite failed.")
);if

);defun acet-acadinfo-lisp-dump


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-pre-process-strings ( lst / a b lst2 n flag)

(cond
 ((equal (type lst) 'LIST)
     (if (not (equal (type (cdr lst)) 'LIST)) ;then dotted pair
         (progn
          (setq flag T
                   a (car lst)
                   b (cdr lst)
          );setq
          (if (or (equal (type a) 'ENAME)
                  (equal (type a) 'PICKSET)
              );or
              (setq a (acet-acadinfo-item-to-string a))
          );if
          (if (or (equal (type b) 'ENAME)
                  (equal (type b) 'PICKSET)
              );or
              (setq b (acet-acadinfo-item-to-string b))
          );if
          (setq lst (list a b));setq
         );progn then dotted pair
     );if
     (setq n 0);setq
     (repeat (length lst)
     (setq a (nth n lst));setq
     (cond
      ((equal (type a) 'LIST)
       (setq a (acet-acadinfo-pre-process-strings a));setq
      );cond #1
      ((equal (type a) 'STR)
       (setq a (acet-acadinfo-str-replace (chr 34)
                           "'" ;(strcat (chr 92) (chr 34))
                           a
               )
             a (acet-acadinfo-str-replace "\\" "/" a)
       );setq
      );cond #2
      ((or (equal (type a) 'ENAME)
           (equal (type a) 'PICKSET)
       );or
       (setq a (acet-acadinfo-item-to-string a));convert the entname or selection set to a string
      );cond #3
     );cond close
     (setq lst2 (append lst2 (list a)));setq
     (setq n (+ n 1));setq
     );repeat
     (if flag
         (setq lst2 (cons (car lst2) (cadr lst2)));setq
     );if
 );cond #1 then list
 ((equal (type lst) 'STR)
         (setq    a lst
                  a (acet-acadinfo-str-replace (chr 34)
                                "'" ;(strcat (chr 92) (chr 34))
                                a
                    )
                  a (acet-acadinfo-str-replace "\\" "/" a)
               lst2 a
         );setq then
 );cond #2
 ((or (equal (type lst) 'ENAME)
      (equal (type lst) 'PICKSET)
  );or
  (setq lst2 (acet-acadinfo-item-to-string lst));convert the entname or selection set to a string
 );cond #3
 (T
  (setq lst2 lst);setq else
 );cond #4
);cond close
lst2
);defun acet-acadinfo-pre-process-strings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-vars-to-scr ( fna flag / fna fh lst lst2 lst3 lst4 n a b)
  (princ "\nWriting AutoCAD system variable information...")

  (if (equal flag 0)
      (progn
       (setq fh (open fna "r"));setq
       (while (setq a (read-line fh))
        (setq lst (append lst (list a)));setq
       );while
       (close fh)
      );progn then read the existing file
  );if

  ;lst2 is the read-only vars
  (setq lst2
    (list
      "ACADPREFIX"
      "ACADVER"
      "ADCSTATE"
      "AREA"
      "ASSISTSTATE"
      "BACKZ"
      "BLOCKEDITOR"
      "CDATE"
      "CLEANSCREENSTATE"
      "CMDACTIVE"
      "CMDNAMES"
      "CPLOTSTYLE"
      "CPROFILE"
      "CPUTICKS"
      "CURRENTPROFILE"
      "DATE"
      "DBCSTATE"
      "DBMOD"
      "DEFLPLSTYLE"
      "DEFPLSTYLE"
      "DIASTAT"
      "DIMSTYLE"
      "DISTANCE"
      "DRSTATE"
      "DWGCODEPAGE"
      "DWGNAME"
      "DWGPREFIX"
      "DWGTITLED"
      "EXTMAX"
      "EXTMIN"
      "FRONTZ"
      "FULLOPEN"
      "HANDLES"
      "LASTANGLE"
      "LASTPROMPT"
      "LENSLENGTH"
      "LOCALE"
      "LOCALROOTPREFIX"
      "LOGINNAME"
      "MENUNAME"
      "MILLISECS"
      "MSMSTATE"
      "MYDOCUMENTSPREFIX"
      "NODENAME"
      "OPMSTATE"
      "PERIMETER"
      "PFACEVMAX"
      "PLATFORM"
      "POPUPS"
      "PRODUCT"
      "PROGRAM"
      "PSTYLEMODE"
      "REFEDITNAME"
      "ROAMABLEROOTPREFIX"
      "SAVEFILE"
      "SAVENAME"
      "SCREENBOXES"
      "SCREENMODE"
      "SCREENSIZE"
      "SSFOUND"
      "SSMSTATE"
      "SYSCODEPAGE"
      "TARGET"
      "TDCREATE"
      "TDINDWG"
      "TDUCREATE"
      "TDUPDATE"
      "TDUSRTIMER"
      "TDUUPDATE"
      "TEMPPREFIX"
      "TPSTATE"
      "UCSNAME"
      "UCSORG"
      "UCSXDIR"
      "UCSYDIR"
      "UNDOCTL"
      "UNDOCTL"
      "UNDOMARKS"
      "VIEWCTR"
      "VIEWDIR"
      "VIEWMODE"
      "VIEWSIZE"
      "VIEWTWIST"
      "VPMAXIMIZEDSTATE"
      "VSMAX"
      "VSMIN"
      "WORLDUCS"
      "WRITESTAT"
      "_PKSER"
      "_SERVER"
      "_VERNUM"
    );lst2 is the read-only vars

          lst3
    (list
      "ACADLSPASDOC"
      "AFLAGS"
      "ANGBASE"
      "ANGDIR"
      "APBOX"
      "APERTURE"
      "ATTDIA"
      "ATTMODE"
      "ATTREQ"
      "AUDITCTL"
      "AUNITS"
      "AUPREC"
      "AUTOSNAP"
      "AUXSTAT"
      "AXISUNIT"
      "BACKGROUNDPLOT"
      "BACTIONCOLOR"
      "BDEPENDENCYHIGHLIGHT"
      "BGRIPOBJSIZE"
      "BINDTYPE"
      "BLIPMODE"
      "BPARAMETERCOLOR"
      "BPARAMETERSIZE"
      "BTMARKDISPLAY"
      "BVMODE"
      "CALCINPUT"
      "CECOLOR"
      "CIPMODE"
      "CELTSCALE"
      "CELTYPE"
      "CELWEIGHT"
      "CHAMFERA"
      "CHAMFERB"
      "CHAMFERC"
      "CHAMFERD"
      "CHAMMODE"
      "CIRCLERAD"
      "CLAYER"
      "CMDDIA"
      "CMDECHO"
      "CMDINPUTHISTORYMAX"
      "CMLJUST"
      "CMLSCALE"
      "CMLSTYLE"
      "COMPASS"
      "COORDS"
      "CPLOTSTYLE"
      "CTAB"
      "CTABLESTYLE"
      "CURSORSIZE"
      "CVPORT"
      "DCTCUST"
      "DCTMAIN"
      "DELOBJ"
      "DEMANDLOAD"
      "DIMADEC"
      "DIMALT"
      "DIMALTD"
      "DIMALTF"
      "DIMALTRND"
      "DIMALTTD"
      "DIMALTTZ"
      "DIMALTU"
      "DIMALTZ"
      "DIMAPOST"
      "DIMASSOC"
      "DIMASZ"
      "DIMATFIT"
      "DIMAUNIT"
      "DIMAZIN"
      "DIMBLK"
      "DIMBLK1"
      "DIMBLK2"
      "DIMCEN"
      "DIMCLRD"
      "DIMCLRE"
      "DIMCLRT"
      "DIMDEC"
      "DIMDLE"
      "DIMDLI"
      "DIMDSEP"
      "DIMEXE"
      "DIMEXO"
      "DIMFIT"
      "DIMFRAC"
      "DIMGAP"
      "DIMJUST"
      "DIMLDRBLK"
      "DIMLFAC"
      "DIMLIM"
      "DIMLUNIT"
      "DIMLWD"
      "DIMLWE"
      "DIMPOST"
      "DIMRND"
      "DIMSAH"
      "DIMSCALE"
      "DIMSD1"
      "DIMSD2"
      "DIMSE1"
      "DIMSE2"
      "DIMSHO"
      "DIMSOXD"
      "DIMTAD"
      "DIMTDEC"
      "DIMTFAC"
      "DIMTIH"
      "DIMTIX"
      "DIMTM"
      "DIMTMOVE"
      "DIMTOFL"
      "DIMTOH"
      "DIMTOL"
      "DIMTOLJ"
      "DIMTP"
      "DIMTSZ"
      "DIMTVP"
      "DIMTXSTY"
      "DIMTXT"
      "DIMTZIN"
      "DIMUNIT"
      "DIMUPT"
      "DIMZIN"
      "DISPSILH"
      "DONUTID"
      "DONUTOD"
      "DRAGMODE"
      "DRAGP1"
      "DRAGP2"
      "DRAWORDERCTL"
      "DWGCHECK"
      "DYNDIGRIP"
      "DYNDIVIS"
      "DYNMODE"
      "DYNPICOORDS"
      "DYNPIFORMAT"
      "DYNPIVIS"
      "DYNPROMPT"
      "DYNTOOLTIPS"
      "EDGEMODE"
      "ELEVATION"
      "EXPERT"
      "EXPLMODE"
      "EXTNAMES"
      "FACETRATIO"
      "FACETRES"
      "FIELDDISPLAY"
      "FIELDEVAL"
      "FILEDIA"
      "FILLETRAD"
      "FILLMODE"
      "FONTALT"
      "FONTMAP"
      "GFANG"
      "GFCLR1"
      "GFCLR2"
      "GFCLRLUM"
      "GFCLRSTATE"
      "GFNAME"
      "GFSHIFT"
      "GRIDMODE"
      "GRIDUNIT"
      "GRIPBLOCK"
      "GRIPCOLOR"
      "GRIPHOT"
      "GRIPHOVER"
      "GRIPOBJLIMIT"
      "GRIPS"
      "GRIPSIZE"
      "GRIPTIPS"
      "HALOGAP"
      "HIDEPRECISION"
      "HIDETEXT"
      "HIGHLIGHT"
      "HPANG"
      "HPASSOC"
      "HPBOUND"
      "HPDOUBLE"
      "HPDRAWORDER"
      "HPGAPTOL"
      "HPINHERIT"
      "HPNAME"
      "HPOBJWARNING"
      "HPORIGIN"
      "HPORIGINMODE"
      "HPSCALE"
      "HPSEPARATE"
      "HPSPACE"
      "HYPERLINKBASE"
      "IMAGEHLT"
      "INDEXCTL"
      "INETLOCATION"
      "INPUTHISTORYMODE"
      "INSBASE"
      "INSNAME"
      "INSUNITS"
      "INSUNITSDEFSOURCE"
      "INSUNITSDEFTARGET"
      "INTERSECTIONCOLOR"
      "INTERSECTIONDISPLAY"
      "ISAVEBAK"
      "ISAVEPERCENT"
      "ISOLINES"
      "LASTPOINT"
      "LAYOUTREGENCTL"
      "LAZYLOAD"
      "LIMCHECK"
      "LIMMAX"
      "LIMMIN"
      "LISPINIT"
      "LOCKUI"
      "LOGFILEMODE"
      "LOGFILEPATH"
      "LTSCALE"
      "LUNITS"
      "LUPREC"
      "LWDEFAULT"
      "LWDISPLAY"
      "LWUNITS"
      "MAXACTVP"
      "MAXSORT"
      "MBUTTONPAN"
      "MEASUREINIT"
      "MEASUREMENT"
      "MENUCTL"
      "MENUECHO"
      "MIRRTEXT"
      "MODEMACRO"
      "MSOLESCALE"
      "MTEXTED"
      "MTEXTFIXED"
      "MTJIGSTRING"
      "NOMUTT"
      "OBSCUREDCOLOR"
      "OBSCUREDLTYPE"
      "OFFSETDIST"
      "OFFSETGAPTYPE"
      "OLEFRAME"
      "OLEHIDE"
      "OLEQUALITY"
      "OLESTARTUP"
      "ORTHOMODE"
      "OSMODE"
      "OSNAPCOORD"
      "OSNAPHATCH"
      "OSNAPNODELEGACY"
      "OSNAPZ"
      "PALETTEOPAQUE"
      "PAPERUPDATE"
      "PDMODE"
      "PDSIZE"
      "PEDITACCEPT"
      "PELLIPSE"
      "PHANDLE"
      "PICKADD"
      "PICKAUTO"
      "PICKBOX"
      "PICKDRAG"
      "PICKFIRST"
      "PICKSTYLE"
      "PLINEGEN"
      "PLINETYPE"
      "PLINEWID"
      "PLOTID"
      "PLOTLEGACY"
      "PLOTOFFSET"
      "PLOTROTMODE"
      "PLOTTER"
      "PLQUIET"
      "POLARADDANG"
      "POLARANG"
      "POLARDIST"
      "POLARMODE"
      "POLYSIDES"
      "PREVIEWFILTER"
      "PROJECTNAME"
      "PROJMODE"
      "PROXYGRAPHICS"
      "PROXYNOTICE"
      "PROXYSHOW"
      "PSLTSCALE"
      "PSPROLOG"
      "PSQUALITY"
      "PSTYLEPOLICY"
      "PSVPSCALE"
      "PUCSBASE"
      "QTEXTMODE"
      "QUEUEDREGENMAX"
      "RASTERDPI"
      "RASTERPREVIEW"
      "RECOVERYMODE"
      "REGENMODE"
      "REMEMBERFOLDERS"
      "REPORTERROR"
      "RTDISPLAY"
      "SAVEFILEPATH"
      "SAVETIME"
      "SDI"
      "SELECTIONAREA"
      "SELECTIONAREAOPACITY"
      "SELECTIONPREVIEW"
      "SHADEDGE"
      "SHADEDIF"
      "SHORTCUTMENU"
      "SHPNAME"
      "SIGWARN"
      "SKETCHINC"
      "SKPOLY"
      "SNAPANG"
      "SNAPBASE"
      "SNAPISOPAIR"
      "SNAPMODE"
      "SNAPSTYL"
      "SNAPTYPE"
      "SNAPUNIT"
      "SOLIDCHECK"
      "SORTENTS"
      "SPACESWITCH"
      "SPLFRAME"
      "SPLINESEGS"
      "SPLINETYPE"
      "SSLOCATE"
      "SSMAUTOOPEN"
      "SSMOPENREFRESH"
      "STANDARDSVIOLATION"
      "STARTUP"
      "STYLESHEET"
      "SURFTAB1"
      "SURFTAB2"
      "SURFTYPE"
      "SURFU"
      "SURFV"
      "TABMODE"
      "TBCUSTOMIZE"
      "TEMPOVERRIDES"
      "TEXTEVAL"
      "TEXTFILL"
      "TEXTQLTY"
      "TEXTSIZE"
      "TEXTSTYLE"
      "THICKNESS"
      "TILEMODE"
      "TOOLTIPS"
      "TRACEWID"
      "TRACKPATH"
      "TRAYICONS"
      "TRAYNOTIFY"
      "TRAYTIMEOUT"
      "TREEDEPTH"
      "TREEMAX"
      "TRIMMODE"
      "TSPACEFAC"
      "TSPACETYPE"
      "TSTACKALIGN"
      "TSTACKSIZE"
      "UCSAXISANG"
      "UCSBASE"
      "UCSFOLLOW"
      "UCSICON"
      "UCSORTHO"
      "UCSVIEW"
      "UCSVP"
      "UNITMODE"
      "UPDATETHUMBNAIL"
      "VISRETAIN"
      "VTDURATION"
      "VTENABLE"
      "VTFPS"
      "WHIPARC"
      "WHIPTHREAD"
      "WMFBKGND"
      "WMFFOREGND"
      "WORLDVIEW"
      "XCLIPFRAME"
      "XEDIT"
      "XFADECTL"
      "XLOADCTL"
      "XLOADPATH"
      "XREFCTL"
      "XREFNOTIFY"
      "XREFTYPE"
      "ZOOMFACTOR"
      "_TOOLPALETTEPATH"
    );lst3 is the non-read-only vars

         lst4
    (list
      "CLAYER"      ;These are not read-only vars but they are vars that are                          
      "CPLOTSTYLE"  ;likely to cause problems if you try to                                           
      "CVPORT"      ;(load "acaddata.txt") with a different drawing                                   
      "DIMBLK"      ;open than the one that was open when the data file was                           
      "DIMBLK1"     ;created. The solution is to include them in the output                           
      "DIMBLK2"     ;but commented out.                                                               
      "DIMLDRBLK"
      "DIMTXSTY"
      "FONTALT"
      "FONTMAP"
      "HPNAME"
      "INSNAME"
      "LOGFILEMODE"
      "LOGFILENAME"
      "LOGFILEPATH"
      "MTEXTED"
      "PHANDLE"
      "PLOTID"
      "PLOTLEGACY"
      "PLOTTER"
      "PROJECTNAME"
      "PUCSBASE"
      "SHPNAME"
      "STYLESHEET"
      "TEXTSTYLE"
      "UCSAXISANG"
      "UCSBASE"
      "XLOADPATH"
    );list
  );setq


  (if (equal flag -1)
      (setq fh (open fna "a"));setq ;then append to end of file
      (setq fh (open fna "w"));setq else
  );if

  (setq n 0);setq

  (repeat (length lst2)
   (setq a (nth n lst2)
         b (getvar a)
   );setq
   (if (not (equal 'STR (type b)))
       (setq b (acet-acadinfo-item-to-string b));setq
   );if
   (write-line (strcat (chr 59) " read-only - " a " " b)
               fh
   );write-line
   (setq n (+ n 1));setq
  );repeat

  (setq n 0);setq
  (repeat (length lst3)
   (setq a (nth n lst3)
         b (getvar a)
   );setq
   (if (not (equal 'STR (type b)))
       (setq b (acet-acadinfo-item-to-string b));setq then
       (progn
        (if (and (equal b "")
                 (or (wcmatch a "FONT*")
                     (equal a "PROJECTNAME")
                 );or
            );and
            (setq b ".")
        );if
        (setq b (strcat "\"" b "\"")
              b (acet-acadinfo-str-replace "\\" "/" b)
        );setq
       );progn else
   );if
   (if (equal "(" (substr b 1 1))
       (setq b (strcat "'" b));setq
   );if
   (if (member a lst4)
       (write-line (strcat ";(setvar \"" ;commented to avoid load problems
                             a
                           "\" "
                             b
                           ")"
                   );strcat
                   fh
       );write-line then comment out
       (write-line (strcat "(setvar \""
                             a
                             "\" "
                             b
                             ")"
                   );strcat
                   fh
       );write-line else active lisp durring load.
   );if
   (setq n (+ n 1));setq
  );repeat

  (setq n 0);setq
  (repeat (length lst)
   (setq a (nth n lst));setq
   (write-line a fh)
   (setq n (+ n 1));setq
  );repeat

  (close fh)

);defun acet-acadinfo-vars-to-scr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;acet-acadinfo-str-replace function
;takes three string arguments:
;i.e. (acet-acadinfo-str-replace "old" "new" "the old car is cool!")
;returns "the new car is cool!"
;
(defun acet-acadinfo-str-replace ( b a c / d n )
(setq d (acet-acadinfo-str-find b c)
      n 0
);setq
(repeat (length d)
(setq c (strcat
         (substr c
                 1
                 (+
                    (*
                       (- (strlen a) (strlen b))
                       n
                    );mult
                    (- (nth n d) 1)
                 );plus
         );substr
         a
         (substr c
                 (+ ( nth n d)
                    (*
                      ( - (strlen a) (strlen b))
                      n
                   );mult
                   (strlen b)
                 );plus
         );substr
        );strcat
);setq
(setq n (+ n 1));setq
);repeat
c
);defun acet-acadinfo-str-replace

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**acet-acadinfo-str-find** STRing SEArch function that searches for a specified substring
;in another larger supplied string and then returns a list of the start
;positions of each occurrence of that string in the larger string
;
(defun acet-acadinfo-str-find (a b / c n)
(cond
((equal "" a)
 (setq c nil)
);cond #1
((not (equal (type b) (type "1")))
 (progn (print "ARGUMENT NOT A STRING!!!!")
       (print b)
       (setq c nil)
 );progn
);cond #2
( T
  (progn
   (setq n 1);setq
   (while (>=
             (+ (- (strlen b) n) 1)
             (strlen a)
           );test while arg.
   (if (equal
             (substr b n (strlen a))
             a
       );equal
       (setq c (append c (list n))
             n (-
                  (+ n
                     (strlen a)
                  );plus
                  1
               );minus
       );setq
    );if
    (setq n (+ n 1));setq
    );while
   );progn
 );cond #3
);cond close
c
);defun acet-acadinfo-str-find

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-item-to-string ( a / fh fna)
  (cond
    ((equal (type a) 'REAL)
      (setq a (rtos a 2 4))
      (if (equal (substr a 1 1) ".")
          (setq a (strcat "0" a))
      );if
    )
    ((equal (type a) 'STR)
      (setq a (strcat "\"" a "\""))
    )
    ((equal (type a) 'INT)
      (setq a (itoa a))
    )
    (T
      (setq fna (vl-filename-mktemp)
            fh  (open fna "w")
      );setq
      (print a fh)
      (close fh)
      (setq fh (open fna "r"));setq
      (setq a (read-line fh));setq
      (setq a (read-line fh));setq
      (close fh)
      (vl-file-delete fna)
    )
  );cond close
  a
);defun acet-acadinfo-item-to-string

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-acadinfo-spinner ( / )
  (if (not #spin)
      (setq #spin "-")
  );if
  (cond
   ((equal #spin "-") (setq #spin "\\"))
   ((equal #spin "\\") (setq #spin "|"))
   ((equal #spin "|") (setq #spin "/"))
   (T (setq #spin "-"))
  );cond close
  (princ (strcat (chr 8) #spin))
);defun acet-acadinfo-spinner

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAs9ktw7imTHPVU/tDEwdYWPuiPlkrhm6f2q2tC
;;; wQteJaKjLOuN/NGgn86zx/bHHm4/qik78Soh4Epi/yDM2/Bd3lqqiWIIAfHPyYxi
;;; pOj7k2am0NJS9rLojpdHU2ttC1qtFRI182GW/efOE47rkHtFcaucjA5hbEWHaFb7
;;; CW2q5HxDCbyNMGrx3owK+TM2EQ+HzSnYwhPpVqMPh4cgZHk93XmFsOxUadvrO9Hx
;;; YPREVxDBMNNicmAIbtnXPhith3tzmIXBB9RbMm4mc6FIoV3KnlaMCMUGUTKHP+K/
;;; Ce2UqHyik/V0lFOZHB5ACslnt7CkTWEEEvqux9kuk/lJfjlF0qkdc6b+IJwm1n5K
;;; e8L8w3KKpJhw+jYZecoU6BCn4I2C1NBBEzPG3PJALEhHvA6iBChAqISYXzfqOB/H
;;; 38n1kiNnxTe7msmLLy52gmQ/SLli79gjyzZ4jGNrr0CxxWzZXTlNCFSkJGKj8rTX
;;; xbvc15FXLf+t7Y6gPukKk8zWKia+2pe6x3zWUU7hXPwPEqveRgVovEUJ5jbvQzmL
;;; 9wDQdOa0ka5KTdQZ0YYqrMSKhL1XTiWJu8sfg8NBrbIG6zhyvr61uq9LmT0E0At7
;;; RflG/ciFF+8+TwtJDnY0xjp7v2quZogIh3tpo5BLp9t7NA8BU087Mo1936cPfwLs
;;; ACHYFA==
;;; -----END-SIGNATURE-----