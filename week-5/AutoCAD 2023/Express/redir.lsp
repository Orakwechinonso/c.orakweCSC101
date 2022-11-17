;;;
;;;    REDIR.LSP
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
 
(defun c:redir ( / path1 path2 mode)
 
(acet-error-init
 (list
  (list "cmdecho" 0
        "fontalt" (getvar "fontalt")
  )
  T
  '(setq #redir_datalist nil)
 );list
);acet-error-init
 
(if (and (not (acet-file-find-font (getvar "fontalt")))
         (findfile "txt.shx")
    );and
    (setvar "fontalt" "txt.shx")
);if
(setq mode (bns_get_cur_redirmode))
(bns_princ_redirmode mode)
(princ "\nFind and replace directory names")
(setq path1 "?")
(while (or (equal path1 "?")
           (equal path1 "")
       );or
 (setq path1 (getstring T "\nEnter old directory (use '*' for all), or ? <options>: ")
       path1 (acet-str-space-trim path1)
       path1 (acet-str-lr-trim "\"" path1)
       path1 (xstrcase path1)
 );setq
 (cond
  ((equal path1 "*")
   (setq path1 "*")
  )
  ((equal path1 "?")
   (textscr)
   (bns_list_file_refs)
   (setq path1 "?")
  )
  ((equal "" path1)
   (bns_get_redirmode nil)
  )
  ((or (wcmatch path1 "*` ")
       (wcmatch path1 "*`?*")
       (wcmatch path1 "*<*")
       (wcmatch path1 "*>*")
       (wcmatch path1 "*|*")
   );or
   (princ "\n*Invalid*")
   (if (or (wcmatch path1 "*` ")
           (wcmatch path1 "*`?*")
       );or
       (princ (strcat " Wild cards are only partially supported for search and replace."
                      "\n- Press return to set object type options."
                      "\n- Enter \"*\" to change all paths"
                      "\n- Enter \"?\" to list current file references by selected object types."
              )
       );princ
       (princ " character")
   );if
   (setq path1 "")
  )
 );cond close
);while
(if (not (equal path1 ""))
    (progn
     (while (not (acet-filename-valid path2))
      (setq path2 (getstring T
                             (acet-str-format "\nReplace \"%1\" with: " path1)
                  )
            path2 (acet-str-space-trim path2)
            path2 (acet-str-lr-trim "\"" path2)
            path2 (xstrcase path2)
      );setq
      (if (not (acet-filename-valid path2))
          (progn
           (princ "\n*Invalid*")
           (setq path2 nil)
          );progn then
      );if
     );while
     (bns_redir path1 path2)
    );progn then
);if
(setq #redir_datalist nil)
 
(acet-error-restore)
);defun c:redir
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir ( path1 path2 / stlst xrflst ilst rtlst mode)
 
(acet-error-init
 (list
  (list "cmdecho" 0)
  T
  '(setq #redir_datalist nil)
 );list
);acet-error-init
 
(setq  mode (bns_get_cur_redirmode) ;possible bits in mode:
                                    ;  1 styles/shapes
                                    ;  2 xrefs
                                    ;  4 images
                                    ;  8 rtext
      path1 (xstrcase path1)
);setq
 
(if (and (not (equal path1 ""))
         (acet-filename-valid path2)
    );and
    (progn
     (setq path1 (acet-str-replace "/" "\\" path1)
           path2 (xstrcase path2)
           path2 (acet-str-replace "/" "\\" path2)
     );setq
     (princ (acet-str-format "\nSearching for old dir: %1" path1))
     (princ (acet-str-format "\nin order to replace it with: %1" path2))
 
     (if (equal 2 (logand 2 mode))   ;redir paths to XREFS
         (setq xrflst (bns_redir_xrefs path1 path2));setq then
     );if
 
 
     (if (equal 1 (logand 1 mode)) ;redir paths for SHAPE and FONTS in the style table
         (setq stlst (bns_redir_styles path1 path2));setq
     );if
 
     (if (equal 4 (logand 4 mode))     ;redir paths for IMAGES
         (setq ilst (bns_redir_images path1 path2));setq then
     );if
 
     (if (equal 8 (logand 8 mode))     ;redir paths for RTEXT objects
         (setq rtlst (bns_redir_rtext path1 path2));setq then
     );if
 
     (if stlst
         (princ (acet-str-format "\n%1 style/shape records modified."  (itoa (fix (car stlst)))))
     );if
     (if ilst
         (princ (acet-str-format "\n%1 image references modified."  (itoa (fix (car ilst)))))
     );if
 
     (if xrflst
         (princ (acet-str-format "\n%1 xrefs modified."  (itoa (fix (car xrflst)))))
     );if
 
     (if rtlst
         (princ (acet-str-format "\n%1 rtext objects modified."  (itoa (fix (car rtlst)))))
     );if
 
     (if (and (not (equal (length (acet-table-name-list "block"))
                          (length (acet-table-name-list '("block" 4)))
                   );equal
              );not
              (or (and (car xrflst) (/= 0 (car xrflst)))
                  (and (car stlst) (/= 0 (car stlst)))
                  (and (car ilst) (/= 0 (car ilst)))
                  (and (car rtlst) (/= 0 (car rtlst)))
              );or
         );and
         (princ "\nChanges to some externally referenced objects may be temporary.")
     );if
    );progn then
    (progn
     (if (not (acet-filename-valid path2))
         (princ "\nInvalid new directory specification.")
         (princ "\nOld directory not specified.")
     );if
    );progn else print an error.
);if
 
(setq #redir_datalist nil)
(acet-error-restore)
);defun bns_redir
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_xrefs ( path1 path2 / lst n na e1 a b c d j k slst fna)
 
;(print "bns_redir_xrefs")(print "")
(setq lst (acet-table-name-list "block")
        j 0
        k 0
);setq
(setq n 0)
(repeat (length lst)
(setq na (tblobjname "block" (nth n lst)));setq
(if (and na
         (setq   e1 (entget na))
         (setq    a (cdr (assoc 70 e1))
               slst (list "XREF" (cdr (assoc 2 e1)))
         );setq
         (equal 4 (logand 4 a))
    );and
    (progn
     (if (not (setq fna (cdr (assoc 3 e1))))
         (setq fna (cdr (assoc 1 e1)));setq
     );if
     (setq b (xstrcase fna)
           b (acet-str-replace "/" "\\" b)
     );setq
     (if (equal "" (acet-filename-extension b))
         (setq b (strcat b ".DWG"))
     );if
     (setq c (bns_path_replace path1 path2 b));setq
     (if (and (not (acet-str-equal b c))
              (setq d (acet-file-find c))
         );and
         (progn
          (princ (bns_redir_format
                  (list (car slst)
                        (cadr slst)
                        (strcat fna " ->")
                        c
                  )
                 )
          );princ
          (command "_.xref" "_p" (cdr (assoc 2 e1)) c)
          (while (wcmatch (getvar "cmdnames") "*XREF*")
            (command "")
          );while
          (setq j (+ j 1));setq
         );progn
         (progn
          (if (not (acet-str-equal b c))
              (princ (acet-str-format "\nCannot find xref: %1." c ))
          );if
         );progn else
     );if
    );progn then
);if
(setq n (+ n 1));setq
);repeat
 
(list j)
);defun bns_redir_xrefs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_styles ( path1 path2 / e1 lst n a
                                        b c d j found str slst lst3 lst4
                        )
(acet-autoload '("bns_flt.lsp" "(bns_tbl_match tbl flt)"))
 
(setq lst (bns_tbl_match "style" '((-4 . "<NOT")
                                    (-4 . "&") (70 . 1)
                                   (-4 . "NOT>")
                                  )
          );bns_tbl_match
);setq
(setq j 0
      n 0
);setq
(repeat (length lst)
(setq  e1 (nth n lst)
      str (cdr (assoc 2 e1))
);setq
(if (equal 16 (logand 16 (cdr (assoc 70 e1))))
    (setq lst4 (append lst4 (list (cdr (assoc 2 e1))))
            e1 nil
    );setq then it's xref'd so let the bns_util.arx functions handle this one.
);if
(if (equal "" str)
    (setq slst (list "SHAPE" str));setq then
    (setq slst (list "STYLE" str));setq else
);if
(if (and (setq  a (cdr (assoc 3 e1)))
         (not (equal a ""))
    );and
    (progn
     (setq    a (xstrcase a)
              a (acet-str-replace "/" "\\" a)
           lst3 nil
              c (bns_path_replace path1 path2 a)
     );setq
     (setq found T)
     (if (and (not (acet-str-equal a c))
              (setq found (acet-file-find-font c))
         );and
         (progn
          (setq lst3 (append lst3
                      (list
                       (bns_redir_format
                        (list (car slst)
                              (cadr slst)
                              (strcat (cdr (assoc 3 e1)) " ->")
                              c
                        )
                       );bns_redir_format
                      );list
                     );append
          );setq
          (princ (car lst3))
          (setq j (+ j 1));setq
          (setq e1 (subst (cons 3 c) (assoc 3 e1) e1));setq
         );progn then
         (progn
          (if (and c
                   (not found)
              );and
              (princ (acet-str-format "\nCannot find font: %1" c))
          );if
          (setq c nil)
         );progn else
     );if
    );progn then
);if
(if (and (not (equal str ""))
         (setq b (cdr (assoc 4 e1)));setq
         (not (equal b ""))
    );and
    (progn
     (setq b (xstrcase b)
           b (acet-str-replace "/" "\\" b)
           d (bns_path_replace path1 path2 b)
     );setq
     (setq found T)
     (if (and (not (acet-str-equal b d))
              (acet-file-find-font d)
         );and
         (progn
          (setq lst3 (append lst3
                             (list
                              (bns_redir_format
                               (list (car slst)
                                     (cadr slst)
                                     (strcat (cdr (assoc 4 e1)) " ->")
                                     d
                               )
                              )
                             );list
                      );append
          );setq
          (princ (cadr lst3))
          (if (not c)
              (setq j (+ j 1));setq
          );if
          (setq e1 (subst (cons 4 d) (assoc 4 e1) e1));setq
         );progn then
         (progn
          (if (and d
                   (not found)
              );and
              (princ (acet-str-format "\nCannot find bigfont: %1" d))
          );if
          (setq d nil)
         );progn else
     );if
    );progn then
);if
 
(if (and e1
         (not (equal e1 (nth n lst)))
    );and
    (entmod e1)
);if
(setq n (+ n 1));setq
);repeat
(setq lst4 (bns_redir_styles2 path1 path2 lst4)
         j (list (fix (+ j (car lst4)))
                 0
           );list
);setq
 
j
);defun bns_redir_styles
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_images ( path1 path2 / owner lst lst2 lst3 k n na e1 a b j
                                        found slst)
 
;(setq lst3 (bns_get_namedobjdict_all));setq
(if (assoc "IMAGES" #redir_datalist)
    (setq lst3 (cdr (assoc "IMAGES" #redir_datalist)));setq
    (setq lst3 (bns_get_namedobjdict_all)
          #redir_datalist (append #redir_datalist
                                  (list (append (list "IMAGES") lst3))
                          );append
    );setq
);if
 
(setq j 0)
(setq k 0)
(repeat (length lst3)
 (setq owner (nth k lst3)
         lst (dictsearch (car owner) "ACAD_IMAGE_DICT")
        lst2 (acet-list-m-assoc 3 lst)
         lst (acet-list-m-assoc 350 lst)
 );setq
 (setq n 0)
 (repeat (length lst)
 (setq   na (cdr (nth n lst)))
 (setq
         e1 (entget na)
          a (cdr (assoc 1 e1))  ;filename
 );setq
 (if a
     (progn
      (setq    a (xstrcase a)
               a (acet-str-replace "/" "\\" a)
      );setq
      (if (equal "" (cadr owner))
          (setq slst (list "IMAGE" (cdr (nth n lst2))));setq then its local
          (setq slst (list "IMAGE"
                           (strcat (cadr owner) "|"
                                   (cdr (nth n lst2))
                           );strcat
                     );list else list as xrefed
          );setq
      );if
      (setq b (bns_path_replace path1 path2 a));setq else
     );progn then
 );if
 (setq found T)
 (if (and a
          (not (acet-str-equal a b))
          (setq found (acet-file-find-image b));setq
          (setq e1 (subst (cons 1 b) (assoc 1 e1) e1));setq
          (entmod e1)
     );and
     (progn
      (princ (bns_redir_format
              (list (car slst)
                    (cadr slst)
                    (strcat a " ->")
                    b
              )
             )
      );princ
      (setq j (+ j 1));setq
     );progn then
     (progn
      (if (not found)
          (princ (acet-str-format "\nCannot find image: %1" b))
      );if
     );progn
 );if
 (setq n (+ n 1));setq
 );repeat
 
(setq k (+ k 1));setq
);repeat
 
(list j)
);defun bns_redir_images
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of sublists
;((entityname ownerblockname) ...)
;
(defun bns_get_namedobjdict_all ( / lst a b c j n lst2 lst3 lst4)
 
(princ "\nSearching for nested image references...")
(setq lst (bns_tbl_match "block"
                         '(
                           (-4 . "<OR")
                            (-4 . "&") (70 . 4)
                            (-4 . "&") (70 . 16)
                           (-4 . "OR>")
                          ) ; xref blocks
          )
);setq
(setq n 0)
(repeat (length lst)
 (setq    a (nth n lst)
          a (cdr (assoc 2 a));the block name
       lst2 (append lst2 (list a))
 );setq
 (setq n (+ n 1));setq
);repeat
(setq  lst lst2
      lst2 nil
);setq
 
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq the block name
(if (not (member a lst4))
    (setq lst2 (bns_blk_match a                ;block name
                              '((0 . "IMAGE")) ;filter
                              nil              ;internal use argument
                              nil ;T                ; search nested block inserts
               )
          lst2 (car lst2)
    );setq then
    (setq lst2 nil);setq else already searched this xref as nested under another xref
);if
 (setq j 0)
 (repeat (length lst2)
  (setq b (car (nth j lst2))        ;the image
        c (cadr (nth j lst2))       ;the owner block
        b (cdr (assoc 340 b))       ;the imagedef entname
  )
  (setq
        b (entget b)                ;the imagedef
        b (cdr (assoc 330 b))       ;the owner dictionary entname
  )
  (setq
        b (entget b)                ;the owner dictionary
        b (cdr (assoc 330 b))       ;the namedobjdict owner
  );setq
  (if (wcmatch c "*|*")
      (setq c (car (acet-str-to-list "|" c)))
      (setq c a)
  );if
  (setq b (list b c))           ;the namedobjdict and the block it came from
 
  (if (not (member c lst4))
      (setq lst4 (append lst4 (list c)));setq
  );if
 
  (if (equal (float (/ j 100)) (/ (float j) 100.0))
      (acet-spinner)
  );if
  (if (not (assoc (car b) lst3))
      (setq lst3 (append lst3 (list b)));setq then
  );if
  (setq j (+ j 1));setq
 );repeat
 
(acet-spinner)
 
(setq n (+ n 1));setq
);repeat through the xrefd blocks
 
(if (not (assoc (namedobjdict) lst3))
    (setq lst3 (append (list (list (namedobjdict) ""))
                       lst3
               );append
    );setq then add the local dictionary
);if
(princ "Done.")
 
(setq a "")
(repeat 80 (setq a (strcat a (chr 8))))
(princ a)
(princ (strcat "                                                      "))
 
lst3
);defun bns_get_namedobjdict_all
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_get_rtext_list ( / a lst na ss n)
 
(princ "\nSearching for rtext objects...")
 
(setq
       lst (bns_blktbl_match '((0 . "RTEXT")
                              (70 . 0)
                             )
           );bns_blktbl_match
);setq
(setq ss (ssget "_x" '((0 . "RTEXT")
                       (70 . 0)
                      )
         );ssget
);setq
(if (not ss)
    (setq ss (ssadd))
);if
(setq n 0)
(repeat (sslength ss)
 (setq na (ssname ss n))
 (setq lst (append lst (list (list na))));setq
 (setq n (+ n 1));setq
);repeat
 
(setq a "")
(repeat 80 (setq a (strcat a (chr 8))))
(princ a)
(princ (strcat "                                                      "))
 
lst
);defun bns_redir_get_rtext_list
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_rtext ( path1 path2 / found lst str lst2 na e1 n a b c j k)
 
(if (assoc "RTEXT" #redir_datalist)
    (setq lst (cdr (assoc "RTEXT" #redir_datalist)));setq
    (setq lst (bns_redir_get_rtext_list)
          #redir_datalist (append #redir_datalist
                                  (list (append (list "RTEXT") lst))
                          );append
    );setq
);if
 
(setq  str (acet-layer-unlock-all)
      lst2 (acet-table-name-list (list "block" 4 16));list of local block names
        k 0
);setq
(setq j 0)
(setq n 0)
(repeat (length lst)
(setq  c (nth n lst)
      na (car c)
       c (cadr c)
)
(setq
      e1 (entget na)
       a (cdr (assoc 1 e1))
);setq
(if a
    (progn
     (setq a (xstrcase a)
           a (acet-str-replace "/" "\\" a)
           b (bns_path_replace path1 path2 a)
     );setq
     (setq found T)
     (if (and (not (equal (xstrcase b) (xstrcase a)))
              (setq found (findfile b))
              (entmod (subst (cons 1 b) (assoc 1 e1) e1));then modify it.
         );and
         (progn
          (if c
              (progn
               (if (not (member c lst2))
                   (setq c (strcat "(xref/block " c ")")
                         k (+ k 1)
                   );setq
                   (setq c (strcat "(block " c ")"));setq
               );if
              );progn
              (setq c "")
          );if
          (princ (bns_redir_format
                  (list "RTEXT"
                        c
                        (strcat (cdr (assoc 1 e1)) " ->")
                        b
                  )
                 )
          );princ
          (setq j (+ j 1))
         );progn then
         (progn
          (if (not found)
              (princ (acet-str-format "\nCannot find rtext file: %1" b))
          );if
         );progn
     );if
    );progn then
);if
 
(setq n (+ n 1));setq
);repeat
 
(if str
    (command "_.-layer" "_lock" str "")
);if
 
(list j k)
);defun bns_redir_rtext
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_list_file_refs ( / str mode )
 
(setq str (getstring T "\nEnter file references to list <*>: "));setq
(if (or (not str)
        (equal str "")
    );or
    (setq str "*")
);if
 
(setq mode (bns_get_cur_redirmode))
(princ "\n")
(bns_princ_redirmode mode)
(princ (bns_redir_format (list " TYPE" " NAME" " FILE")))
(princ "\n--------------------------------------------------------------------------")
 
(if (equal 1 (logand 1 mode))
    (progn
     (bns_redir_list_style_file_refs str) ;the styles
     (bns_redir_list_shape_file_refs str) ;the styles
 
     (princ "\n")
    );progn
);if
(if (equal 2 (logand 2 mode))
    (progn
     (bns_redir_list_table_file_refs "block" str) ;the xrefs
     (princ "\n")
    );progn
);if
(if (equal 4 (logand 4 mode))                     ;the images
    (progn
     (bns_redir_list_images str)
     (princ "\n")
    );progn
);if
(if (equal 8 (logand 8 mode))                     ;the RTEXT objects
    (progn
     (bns_redir_list_rtext str)
     (princ "\n")
    );progn
);if
 
);defun bns_list_file_refs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_list_style_file_refs ( wc / lst e1 n j a b str slst lst3
                                      )
 
(setq lst (bns_tbl_match "style" '((-4 . "<NOT")
                                    (-4 . "&") (70 . 1)
                                   (-4 . "NOT>")
                                  )
          );bns_tbl_match
);setq
(setq j 0
      n 0
);setq
(repeat (length lst)
(setq  e1 (nth n lst)
      str (cdr (assoc 2 e1))
);setq
(if (equal "" str)
    (setq slst (list "SHAPE" str));setq then
    (setq slst (list "STYLE" str));setq else
);if
(if (and (setq a (cdr (assoc 3 e1)))
         (not (equal a ""))
    );and
    (progn
     (setq    a (xstrcase a)
              a (acet-str-replace "/" "\\" a)
           lst3 nil
     );setq
     ;a-c reg font
     ;b-d big font
     (if (wcmatch (xstrcase a) (xstrcase wc))
         (progn
          (setq lst3 (append lst3
                      (list
                       (bns_redir_format
                        (list (car slst)
                              (cadr slst)
                              a
                        )
                       );bns_redir_format
                      );list
                     );append
          );setq
          (princ (last lst3))
         );progn then
     );if
    );progn
);if
(if (and (not (equal str ""))
         (setq b (cdr (assoc 4 e1)))
         (not (equal b ""))
    );and
    (progn
     (setq b (xstrcase b)
           b (acet-str-replace "/" "\\" b)
     );setq
     (if (wcmatch (xstrcase b) (xstrcase wc))
         (progn
          (setq lst3 (append lst3
                             (list
                              (bns_redir_format
                               (list (car slst)
                                     (cadr slst)
                                     b
                               )
                              )
                             );list
                     );append
          );setq
          (princ (last lst3))
         );progn then
     );if
    );progn then
);if
(setq n (+ n 1));setq
);repeat
 
);defun bns_redir_list_style_file_refs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_list_rtext ( str / na e1 n a b lst lst2)
 
 
(if (assoc "RTEXT" #redir_datalist)
    (setq lst (cdr (assoc "RTEXT" #redir_datalist)));setq
    (setq lst (bns_redir_get_rtext_list)
          #redir_datalist (append #redir_datalist
                                  (list (append (list "RTEXT") lst))
                          );append
    );setq
);if
 
(setq lst2 (acet-table-name-list (list "block" 4 16));list of local block names
);setq
 
(setq n 0)
(repeat (length lst)
(setq  b (nth n lst)
      na (car b)
       b (cadr b)
)
(setq
      e1 (entget na)
       a (cdr (assoc 1 e1))
);setq
(if (and a
         ;(not (equal "$" (substr a 1 1)))
         (wcmatch (xstrcase a) (xstrcase str))
    );and
    (progn
     (if b
         (progn
          (if (not (member b lst2))
              (setq b (strcat "(xref/block " b ")"));setq
              (setq b (strcat "(block " b ")"));setq
          );if
         );progn
         (setq b "")
     );if
     (princ (bns_redir_format
              (list "RTEXT"
                    b
                    a
              )
            )
     );princ
    );progn then
);if
(setq n (+ n 1));setq
);repeat
 
);defun bns_redir_list_rtext
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_list_images ( wc / lst lst2 lst3 lst4 owner n j na e1 a slst)
 
(if (assoc "IMAGES" #redir_datalist)
    (setq lst4 (cdr (assoc "IMAGES" #redir_datalist)));setq
    (setq lst4 (bns_get_namedobjdict_all)
          #redir_datalist (append #redir_datalist
                                  (list (append (list "IMAGES") lst4))
                          );append
    );setq
);if
 
(setq j 0)
(repeat (length lst4)
 (setq    wc (xstrcase wc)
       owner (nth j lst4)
         lst (dictsearch (car owner) "ACAD_IMAGE_DICT")
        lst2 (acet-list-m-assoc 3 lst)
         lst (acet-list-m-assoc 350 lst)
 );setq
 (setq n 0)
 (repeat (length lst)
 (setq   na (cdr (nth n lst))
)
(setq
         e1 (entget na)
          a (cdr (assoc 1 e1))
          a (xstrcase a)
          a (acet-str-replace "/" "\\" a)
 );setq
 (if (equal "" (cadr owner))
     (setq slst (list "IMAGE " (cdr (nth n lst2))));setq then
     (setq slst (list "IMAGE "
                      (strcat (cadr owner) "|" (cdr (nth n lst2)))
                )
     );setq else
 );if
 (if (wcmatch (xstrcase (cdr (assoc 1 e1))) (xstrcase wc))
     (setq lst3 (append lst3
                        (list (bns_redir_format
                                (list (car slst)
                                      (cadr slst)
                                      (cdr (assoc 1 e1))
                                )
                              )
                        )
                );append
     );setq then
 );if
 (setq n (+ n 1));setq
 );repeat
 
(setq j (+ j 1));setq
);repeat
(if lst3
    (setq lst3 (acad_strlsort lst3))
);if
 
(while lst3
 (princ (car lst3))
 (setq lst3 (cdr lst3));setq
);while
;(princ "\n")
 
);defun bns_redir_list_images
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_list_table_file_refs ( tbl wc / tbl2 a na e1 n lst lst3)
 
(if (not wc)
    (setq wc "*")
);if
(setq tbl (xstrcase tbl)
       wc (xstrcase wc)
      lst (acet-table-name-list tbl)
);setq
(setq n 0)
(repeat (length lst)
(setq  a (nth n lst)
      na (tblobjname tbl a)
      e1 (entget na)
       a (cdr (assoc 3 e1))
)
(if (not a)
    (setq a (cdr (assoc 1 e1)));setq then fix for xref group code change
);if
(if (and (equal tbl "STYLE")
         (equal (cdr (assoc 2 e1)) "")
         (equal 1 (cdr (assoc 70 e1)))
    );and
    (setq tbl2 "SHAPE")
    (progn
     (if (equal "BLOCK" (xstrcase tbl))
         (setq tbl2 "XREF")
         (setq tbl2 tbl)
     );if
    );progn else
);if
(if a
    (progn
     (if (and (not (equal a ""))
              (wcmatch (xstrcase a) wc)
              (or (and (equal "BLOCK" (xstrcase tbl))
                       ;(cdr (assoc 1 e1))
                       (equal 4 (logand 4 (cdr (assoc 70 e1))))
                  );and
                  (equal "STYLE" (xstrcase tbl))
              );or
         );and
         (setq lst3 (append lst3
                            (list (bns_redir_format
                                   (list tbl2
                                         (cdr (assoc 2 e1))
                                         a
                                   );list
                                  );bns_redir_format
                            );list
                    );append
         );setq then
     );if
     (if (and (equal tbl "STYLE")
              (not (equal "" (cdr (assoc 4 e1))))
         );and
         (setq lst3 (append lst3
                            (list (bns_redir_format
                                   (list tbl2
                                         (cdr (assoc 2 e1))
                                         (cdr (assoc 4 e1))
                                   );list
                                  );bns_redir_format
                            );list
                    );append
         );setq then it's a style and we need to list the bigfont too.
     );if
    );progn then
);if
(setq n (+ n 1));setq
);repeat
(if lst3
    (setq lst3 (acad_strlsort lst3))
);if
(while lst3
 (princ (car lst3))
 (setq lst3 (cdr lst3));setq
);while
;(princ "\n")
 
);defun bns_redir_list_table_file_refs
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:redirmode ( / ) ;dialog unless script or cmddia=0
 (acet-error-init nil)
 (bns_get_redirmode nil)
 (acet-error-restore)
);defun c:redirmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:-redirmode ( / ) ;command line
 (acet-error-init nil)
 (bns_get_redirmode T)
 (acet-error-restore)
);defun c:-redirmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_get_redirmode ( flag / mode)
 
(setq mode (bns_get_cur_redirmode))
(if (or flag
        (equal 0 (getvar "cmddia"))
        (equal 4 (logand 4 (getvar "cmdactive")))
    );or
    (bns_get_redirmode_cmd)
    (bns_get_redirmode_dd)
);if
 
);defun bns_get_redirmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bns_get_redirmode_cmd
;prompts for redirmode at the command line.
;sets the environment variable BNS_REDIRMODE
;The bns_redirmode variable controls the type of objects
;that the bns_redir function performs a search and replace on.
;it is a sum of the following:
;1 styles/shapes
;2 xrefs
;4 images
;8 rtext
;
(defun bns_get_redirmode_cmd ( / curmode mode lst lst2 a b n flag)
 
(setq curmode (bns_get_cur_redirmode)
         mode curmode
);setq
(bns_princ_redirmode mode)
(setq lst (list '(1 "STYLES")
                '(2 "XREFS")
                '(4 "IMAGES")
                '(8 "RTEXT")
          );list
);setq
(while (not flag)
 (setq a (getstring "\nReplace directories in Xrefs,Styles,Images,Rtext. <current>: "))
 (if (not (equal a ""))
     (progn
      (setq    a (xstrcase a)
            lst2 (acet-str-to-list "," a)
            mode 0
            flag nil
      );setq
      (while lst2           ;while parsing the input
       (setq    b (car lst2)
             lst2 (cdr lst2)
       );setq
       (setq n 0)
       (repeat (length lst) ;repeat through valid options looking for matches to input
       (setq a (nth n lst))
        (if (and (not (equal b ""))
                 (wcmatch (cadr a) (strcat b "*"))
            );and
            (setq mode (+ mode (car a))
                  flag T
            );setq
        );if
       (setq n (+ n 1));setq
       );repeat
      );while
     );progn then
     (setq flag T)
 );if
 (if (not flag)
     (progn
       (princ "*Invalid*")
       (setq mode curmode)
     );progn
     (progn
      (setenv "BNS_REDIRMODE" (itoa mode))
      (bns_princ_redirmode mode)
     );progn
 );if
);while
 
mode
);defun bns_get_redirmode_cmd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bns_get_redirmode_dd
;prompts for redirmode using a dcl dialog with check boxes.
;sets the environment variable BNS_REDIRMODE
;The bns_redirmode variable controls the type of objects
;that the bns_redir function performs a search and replace on.
;it is a sum of the following:
;1 styles/shapes
;2 xrefs
;4 images
;8 rtext
;
(defun bns_get_redirmode_dd ( / iv flag set_bit mode)
 
 (setq mode (bns_get_cur_redirmode))
 (if (> (setq iv (load_dialog (getfileET "redir.dcl")));setq
        0
     );test
     (progn
      (if (new_dialog "redirmode" iv)
          (progn
           (if (equal 1 (logand 1 mode))
               (set_tile "styles" "1")
               (set_tile "styles" "0")
           );if
           (if (equal 2 (logand 2 mode))
               (set_tile "xrefs" "1")
               (set_tile "xrefs" "0")
           );if
           (if (equal 4 (logand 4 mode))
               (set_tile "images" "1")
               (set_tile "images" "0")
           );if
           (if (equal 8 (logand 8 mode))
               (set_tile "rtext" "1")
               (set_tile "rtext" "0")
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
 
           (action_tile "styles" "(setq mode (set_bit 1 mode $value))")
           (action_tile "xrefs"  "(setq mode (set_bit 2 mode $value))")
           (action_tile "images" "(setq mode (set_bit 4 mode $value))")
           (action_tile "rtext"  "(setq mode (set_bit 8 mode $value))")
 
           (action_tile "accept" "(done_dialog 1)")
           (action_tile "cancel" "(done_dialog 0)")
           (action_tile "help" "(acet-help \"REDIRMODE\")")
 
           (setq flag (start_dialog));setq ;START_DIALOG MAKES THE BUTTONS ACTIVE
           (if (and (equal flag 1)
                    (> mode 0)
               );and
               (setenv "BNS_REDIRMODE" (itoa mode))
               (setq mode (bns_get_cur_redirmode));setq else
           );if
          );progn then initialize the tiles and activate the dialog box
          (alert "Unable to display dialog box")
      );if new dialog
      (unload_dialog iv);unload it when done
     );progn then
     (alert "Unable to load dialog box");else
 );if load
 
(bns_princ_redirmode mode)
mode
);defun bns_get_redirmode_dd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Gets the current redirmode setting from the environment var "bns_redirmode"
;Returns an a bit sum integer. See header for bns_get_redirmode for more details.
;
(defun bns_get_cur_redirmode ( / mode )
 (if (not (setq mode (getenv "BNS_REDIRMODE")))
     (progn
      (setq mode (+ 1 2 4 8))
      (setenv "BNS_REDIRMODE" (itoa mode))
     );progn
     (setq mode (atoi mode))
 );if
mode
);defun bns_get_cur_redirmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_princ_redirmode ( mode / lst n a b )
 
(if (not mode)
    (setq mode (bns_get_cur_redirmode))
);if
(setq lst (list '(1 "Styles")
                '(2 "Xrefs")
                '(4 "Images")
                '(8 "Rtext")
          );list
);setq
(setq b "")
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
(if (equal (car a) (logand (car a) mode))
    (setq b (strcat b "," (cadr a)));setq
);if
(setq n (+ n 1));setq
);repeat
(if (equal (substr b 2) "")
    (princ "\nCurrent REDIRMODE: None");then
    (princ (acet-str-format "\nCurrent REDIRMODE: %1" (substr b 2)));else
);if
(substr b 2)
);defun bns_princ_redirmode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;following functions taken from bns_util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_format ( lst / a b n j str)
(setq str "          "
      str (strcat str str str str str str str)
        j (/ (strlen str) (length lst))
        b ""
);setq
 
(setq n 0)
(repeat (length lst)
(setq a (nth n lst)
      a (strcat a
                (substr str
                        1
                        (max 0
                             (- j (strlen a))
                        );max
                );substr
        );strcat
       b (strcat b  " " a)
);setq
 
(setq n (+ n 1));setq
);repeat
(setq b (strcat "\n" (substr b 2)))
 
b
);defun bns_redir_format
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;handle shapes and xref'ed styles
;
 
(defun bns_redir_styles2 ( path1 path2 lst / app doc sty styob
                                             a c j found str slst lst3
                        )
 (vl-load-com)
(setq app (vlax-get-acad-object)
      doc (vla-get-activedocument app)
      sty (vla-get-textstyles doc)
);setq
 
(setq j 0);setq
(vlax-for styob sty
      (setq a (vla-get-fontfile styob));setq
      (if a
          (setq    a (xstrcase a)
                   a (acet-str-replace "/" "\\" a)
                 str (vla-get-name styob)
                lst3 nil
                   c (bns_path_replace path1 path2 a)
          );setq
      );if
 
      (setq found T)
      (if (and a
               (or (equal "" str)
                   (member str lst)
               );or
               (not (equal a c))
               (setq found (acet-file-find-font c))
          );and
          (progn
           (if (equal str "")
               (setq slst (list "SHAPE" str));setq then
               (setq slst (list "STYLE" str));setq then
           );if
           (setq lst3 (append lst3
                       (list
                        (bns_redir_format
                         (list (car slst)
                               (cadr slst)
                               (strcat (vla-get-fontfile styob) " ->")
                               c
                         )
                        );bns_redir_format
                       );list
                      );append
           );setq
          );progn then
          (progn
           (if (and c
                    (not found)
               );and
               (princ (strcat "\nCannot find shape file: " c))
           );if
           (setq c nil)
          );progn else
      );if
      (if c
          (progn
           (princ (car lst3))
           (setq j (+ j 1));setq
           (vla-put-fontfile styob c)
          );progn then
      );if
);vlax-for
 
(list j 0)
);defun bns_redir_styles2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_redir_list_shape_file_refs ( wc / app doc sty styob
                                             a str slst lst3
                                      )
 (vl-load-com)
(setq app (vlax-get-acad-object)
      doc (vla-get-activedocument app)
      sty (vla-get-textstyles doc)
);setq
 
(vlax-for styob sty
 
 (setq a (vla-get-fontfile styob));setq
   (if a
       (setq    a (xstrcase a)
                a (acet-str-replace "/" "\\" a)
              str (vla-get-name styob)
             lst3 nil
       );setq
   );if
   ;a-c reg font
   (if (and a
            (equal "" str)
            (wcmatch (xstrcase a) (xstrcase wc))
       );and
       (progn
        (setq slst (list "SHAPE" str));setq then
        (setq lst3 (append lst3
                    (list
                     (bns_redir_format
                      (list (car slst)
                            (cadr slst)
                            a
                      )
                     );bns_redir_format
                    );list
                   );append
        );setq
        (princ (last lst3))
       );progn then
   );if
);vlax-for
 
 
);defun bns_redir_list_shape_file_refs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;old=path1
;new=path2
(defun bns_path_replace ( path1 path2 fna / a b)
 
(setq  fna (xstrcase fna)
       fna (acet-str-space-trim fna)
         a (acet-filename-directory fna)        ;drive/directory
         b (acet-filename-path-remove fna)      ;base filename
         a (acet-str-replace "/" "\\" a)
);setq
(if (equal path1 "*")
    (setq a path2);setq
    (setq a (acet-str-replace path1 path2 a));setq
);if
(if (and (not (equal a ""))
         (not (equal "\\" (substr a (strlen a) 1)))
         (not (equal ":" (substr a (strlen a) 1)))
    );and
    (setq a (strcat a "\\"));setq then
);if
 
(setq fna (strcat a b));setq
 
fna
);defun bns_path_replace


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAGEwGhc/b3AyqkRiSLj6cEv5JQTeBBpjukL7q8
;;; 7biEEAy0U4s8XJ0VUCqlNhUSdbbksnnhayuot+7mSbLYxIDGqo1C1IjP1jWnxbjQ
;;; W34qdddtJt9ts0xKGSgXUcmGhiGZu7bsL4EBIUAdhu8h48WZd4Lv4EN2NnfvpLZH
;;; w9dp36sOAlM8pBV2CPC8xlVP6WCA26S8/GJTbsyogPiuFOM7q4zAmX7y8sOJaA9v
;;; r1odT7klPneSjUwSvSo1rGptNHlteMdxKTKwwGbTnfom+iiN0EZOeqJvVa2qZJA3
;;; JG6n/fP3EwDJqXVruYQImw+vxzvf1p+P0y6ChP7WahSqzJKPpxVdUHuWoeope3ax
;;; ZBKBY6b4vnsv3JRbBydxFREHj5EaFBUYBo3M/n2Gt05kyNUlHzHp2w9ke4r/uirA
;;; pXRvSD8rkjsoD2DLLGCOqwG0uSWw8ENwuP0T63GZSKcb0RivLaLBlGw5PGht7H6H
;;; qsmTiQP2KOoMJPbDVL/Au91iAmCbfmpu4wxKe5p3oPNijJGW42VFtmxAUu/JRBoG
;;; llhxwKTyGaXE+CPZ2o3nyo808nEGCeGLm1QbCszl4N0o15kWViq3pbtn14GIQHeJ
;;; 0iC7E3EKdARMp9gJh4I7vrU25tYHg8i1ucEJu+s2xpneaqSONWCIFgyf8SFEJ4a7
;;; 4GY0ww==
;;; -----END-SIGNATURE-----