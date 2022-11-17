;;
;;;
;;;    ACETURL.LSP
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
;;  Paul Vine - 4/7/1999 9:40 pm
;;  PORTED TO TAHOE
;;  -Moved CHURLS, REPURLS and avip_url_dcl from AVIP_URL.LSP to here.
;;  -Changed all calls from avip_url.dcl to aceturl.dcl
;;  -Changed bns* error handlers to acet* error handlers
;;  -Changed bns_ssstrip to acet-str-space-trim
;;  -changed os-beep to acet-sys-beep
;;  -changed str-subst to acet-str-replace
;;  -changed strcat to acet-str-format
;;  -Fixed 163692 by removing restriction on spaces in URLs
 
(acet-init-fas-lib T nil)
 
;;  load AI_UTILS
(cond
  ((and ai_dcl (listp ai_dcl)))
  ((not (load (getfileSupportLANG "ai_utils.lsp") nil))
    (acet-alert "Cannot load AI_UTILS.")(exit) )
)
;;  load DWFOUT
(cond
  ((= (type geturl) 'EXRXSUBR))
  ((not (arxload "acapp" nil))
    (acet-alert "Cannot load ACHLNKUI.")(exit) )
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;imported from avip_url for tahoe port.
(defun avip_url_dcl (/ dcl)
  (if (< (setq dcl (load_dialog (getfileET "aceturl.dcl"))) 0)
    (progn
      (alert "Cannot load 'aceturl.dcl'.")
      (exit)
    )
  )
 
  dcl
)
 
(defun c:showurls ( / clayer vpna vplocked )
  (acet-error-init
    (list (list "cmdecho" 0
                "ucsfollow" 0
                "limcheck" 0
          )
          T
    );list
  );acet-error-init
 
  (acet-arxload-or-bust "acetutil.arx")
 
  (if (acet-layer-locked (getvar "clayer"))
    (progn
      (setq clayer (getvar "clayer"))
      (command "_.-layer" "_unlock" clayer "")
    );progn then
  );if
  (if (setq vpna (acet-currentviewport-ename))
      (setq vplocked (acet-viewport-lock-set vpna nil)) 
  );if
  (command "_.view" "_save" "bns_urlview")
 
  (bns_showurls_dd)
 
  (command "_.view" "_restore" "bns_urlview"
           "_.view" "_delete" "bns_urlview"
  )
  (if vplocked
      (acet-viewport-lock-set vpna T)
  );if
  (if clayer
    (command "_.-layer" "_lock" clayer "")
  );if
 
  (acet-error-restore)
);defun c:showurls
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun bns_showurls_dd ( / lst lst2 a n iv flag cspace vis entry pos len inc
                           clyron expert sellst)
 
  (cond
    ;; check the view
    ((equal 1 (logand 1 (getvar "viewmode")))
      (princ "\n  That command may not be invoked in a perspective view  ")
    )
    ;; find the URLs in the drawing
    ((not (setq lst (bns_find_urls)))
      (princ "\nNo URLs found.")
    )
    ;; load the DCL
    ((< (setq iv (load_dialog (getfileET "aceturl.dcl"))) 1)
     (alert "Unable to load dialog box");else
    )
    ;; continue normal processing
    (t
      ;; is the current layer off?
      (setq clyron -1)
      (if (bns_layer_off (getvar "clayer"))
        (progn
          (if (= (setq clyron (bns_yn_dlg "layeroff" iv)) 1)
            (command "._layer" "_on" (getvar "clayer") "")
          ) ;if
        ) ;progn
      ) ;if
 
      (setq cspace (if (> (getvar "cvport") 1) "MS" "PS"))
      (setq flag 2)
      ;; begin the while loop
      (while (> flag 0)
        ;; starting position for the dialog box
        (if (not #bns_url_dlg_pnt)
          (setq #bns_url_dlg_pnt '(-1 -1))
        );if
        ;; preselect the element to highlight
        (if (new_dialog "showurls" iv "" #bns_url_dlg_pnt)
          (progn
            (if (> flag 1)
              (progn
                ;; sort the URL list and nil the string list
                (setq lst (acet-list-isort lst 1)
                      lst2 nil)
                ;; create the URL strings for the list_box
                (foreach mbr lst
                  (setq a (acet-str-format "%1\t%2\t%3" (cadr mbr) (caddr mbr) (last mbr)))
                  (setq lst2 (append lst2 (list a)));setq
                ) ;foreach
              ) ;progn
            ) ;if
            (acet-dcl-list-make "url_list" lst2)
            (acet-dcl-list-make "lst_hdr" (list "URL Name\tEntity Type\tSpace"))
            ;; preselect the elements to highlight
            (if #bns_showurls
              (progn
                (setq n ""
                      len (length #bns_showurls)
                      inc -1)
                (while (< (setq inc (1+ inc)) len)
                  (if (setq entry (assoc (nth inc #bns_showurls) lst))
                    (if (setq pos (itoa (vl-position entry lst)))
                      (progn
                        (setq n (acet-str-format "%1%2" n pos))
                        (if (< (1+ inc) len)
                          (setq n (acet-str-format "%1 " n ))
                        ) ;if
                      ) ;progn
                    ) ;if
                  ) ;if
                ) ;while
                ;; if, at this point, n is an empty string it is because
                ;; none of the saved ent names exist any longer
                (if (= n "")
                  (setq n "0")
                ) ;if
              ) ;progn
              (setq n "0")
            ) ;if
            (setq vis (bns_liststatus n cspace lst2 clyron))
            (set_tile "url_list" n)
            (mode_tile "url_list" 2)
            (action_tile "url_list"
                 (strcat  "(setq n $value)"
                          "(setq vis (bns_liststatus $value cspace lst2 clyron))"
                          "(if (and (equal $reason 4) (not (= vis 1)))"
                            "(setq #bns_url_dlg_pnt (done_dialog 1))"
                          ")"
                 );strcat
            );action_tile
 
            (action_tile "accept"
                         "(setq #bns_url_dlg_pnt (done_dialog 1))"
            )
            (action_tile "cancel"
                         "(setq #bns_url_dlg_pnt (done_dialog 0))"
            )
            (action_tile "edit" "(done_dialog 2)")
            (action_tile "replace" "(done_dialog 3)")
            (action_tile "help" "(acet-help \"showurls\")")
 
            (setq flag (start_dialog));setq ;START_DIALOG MAKES THE BUTTONS ACTIVE
            (cond
              ((equal flag 1)
                (bns_showurl (nth (atoi n) lst))
              )
              ((equal flag 2)
                (setq lst (bns_churl n lst iv))
              )
              ((equal flag 3)
                (setq lst (bns_repurl n lst iv))
              )
            );cond
            ;; create the list of items for highlighting on return through the loop
            (setq #bns_showurls nil
                  sellst (acet-str-to-list " " n)
            )
            (foreach mbr sellst
              (setq entry (car (nth (atoi mbr) lst)))
              (setq #bns_showurls (append #bns_showurls (list entry)))
            ) ;foreach
          );progn
          (progn
            (alert "Unable to display dialog box")
            (setq flag 0)
          ) ;progn
        );if new dialog
      ) ;while
      (if (equal clyron 1)
        (progn
          (setq expert (getvar "EXPERT"))
          (setvar "EXPERT" 5)
          (command "._layer" "_off" (getvar "clayer") "")
          (setvar "EXPERT" expert)
        ) ;progn
      ) ;if
      (unload_dialog iv);unload it when done
    )
  );cond
);defun bns_showurls_dd
 
 
; ==========================================================================
;      Task: Sets the appropriate mode_tile option for the accept button
; Called By: bns_showurls_dd
; Arguments: list_box selection, current space, URL string list, change to clayer
;   Returns: 1 or 0
; ==========================================================================
 
(defun bns_liststatus (val cspace lst2 clyron / mode)
  (if (equal clyron 0)
    (setq mode 1)
    ;; if there is more than one item selected
    (if (wcmatch val "* *")
      ;; disable the button
      (setq mode 1)
      ;; if the selected item is not in the same space
      (if (not (equal cspace (last (acet-str-to-list "\t" (nth (read val) lst2)))))
        ;; disable the button
        (setq mode 1)
        ;; otherwise enable the button
        (setq mode 0)
      );if
    );if
  ) ;if
  (mode_tile "accept" mode)
  mode
) ;defun bns_liststatus
 
(defun bns_layer_off ( la / na e1)
 (setq na (tblobjname "layer" la)
       e1 (entget na)
 );setq
 (< (cdr (assoc 62 e1)) 0)
);defun acet-layer-locked
 
(defun bns_yn_dlg (dlg dclid)
  (if (new_dialog dlg dclid)
    (progn
      (action_tile "accept" "(done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (start_dialog)
    ) ;progn
  ) ;if
)
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a list of the form (entname url_string)
;and zooms to the extents of the entity
;
(defun bns_showurl ( lst / na e1 p1 p2 space cspace vp flag zflag )
 
(if (and (setq na (car lst));setq
         (setq e1 (entget na));setq
         (setq p1 (acet-ent-geomextents na)
               p2 (cadr p1)
               p1 (car p1)
         );setq
    );and
    (progn
     (if (not (setq space (cdr (assoc 67 e1)))) ;set the space of the object
         (setq space 0)
     );if
     (if (equal (getvar "cvport") 1) ;set the current space
         (setq cspace 1)
         (setq cspace 0)
     );if
     (setq flag T)
     (if (not (equal space cspace)) ;get in the proper space to zoom to the object
         (progn
          (if (not (equal (getvar "tilemode") 0))
              (setvar "tilemode" 0)
          );if
          (if (equal space 0)
              (progn
               (if (car (setq vp (acet-viewport-next-pickable)))
                   (progn
                    (command "_.mspace")
                    (setvar "cvport" (car vp))
                    (if (cadr vp)
                        (princ (cadr vp))
                    );if
                    (princ "\nSwitching to Model space")
                   );progn then do it
                   (progn
                    (princ (cadr vp));else explain why we can't go to model space
                                     ;(usually perspective or no actvie viewports available)
                    (setq flag nil)  ;abort the zoom window operation
                   );progn else
               );if
              );progn then switch to model space if possible
              (progn
               (command "_.pspace")
               (princ "\nSwitching to Paper space")
              );progn else switch to paper space
          );if
         );progn then need to switch spaces
     );if
     (if flag
         (progn
          (setq p1 (trans p1 0 1)
                p2 (trans p2 0 1)
          );setq
          (if (setq zflag (acet-geom-zoom-for-select (list p1 p2)))
              (command "_.zoom" "_w" (car zflag) (cadr zflag))
          );if
          (acet-blink-and-show-object (list na 6))
         );progn then OK to zoom
     );if
    );progn then got a valid entity and got it's extents.
    (princ "\nUnable to determine the extents of the specified object.")
);if
 
);defun bns_showurl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of  sub-lists
;each of the form:
;'(url_string entname etype space_string)
;
(defun bns_find_urls ( / ss na e1 xd a b n lst space)
 
(if (and (tblobjname "appid" "pe_url")
         (setq ss (ssget "_x"
                         '((-3  ("PE_URL")))
                  );ssget
         );setq
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
      (setq na (ssname ss n)
            e1 (entget na '("PE_URL"))
            xd (cdr (assoc -3 e1))
            xd (cdr (assoc "PE_URL" xd))
             a (cdr (assoc 1000 xd))
             b (cdr (assoc 0 e1))
      );setq
      (if (not (setq space (cdr (assoc 67 e1)))) ;set the space of the object
          (setq space 0)
      );if
      (if (equal space 1)
          (setq space "PS")
          (setq space "MS")
      );if
      (if a
          (setq lst (append lst (list (list na a b space))));setq then
      );if
      (setq n (+ n 1));setq
     );repeat
    );progn then
);if
 
lst
);defun bns_find_urls
 
 
(defun bns_churl (selstr lst dcl / obj url entry sellst newentry wrklst flag
                                   cnt num val inc lockcnt onOk)
 
  ;;  define action handler
  (defun onOk ()
    ;;  set new url on OK
    (setq val (get_tile "url")) ;setq
   ;;removed restrictin of spaces in URL -- not valid when hyperlinking other docs on local system
    ;(if (wcmatch val "* *")
    ;  (progn
    ;    (mode_tile "url" 2)
    ;    (acet-sys-beep 1)
    ;    (set_tile "error" "Spaces are not permitted in the URL name!")
    ;  ) ;progn
      (setq #bns_churl_dlg_pnt (done_dialog 1))
   ; ) ;if
  );defun onOk
 
  ;; convert selection string to a list of indexes
  (setq sellst (acet-str-to-list " " selstr))
  (setq wrklst lst
        flag 1
        lockcnt 0)
 
  (if (not #bns_churl_dlg_pnt)
    (setq #bns_churl_dlg_pnt '(-1 -1))
  );if
  ;;  load dialog
  (if (new_dialog "churl" dcl "" #bns_churl_dlg_pnt)
    (progn
      (setq obj (car (nth (atoi (nth 0 sellst)) wrklst))
            url (geturl obj)
            url (if url url "")
      )
      (set_tile "url" (acet-str-space-trim url))
      (action_tile "accept" "(onOk)")
      (action_tile "cancel" "(setq #bns_churl_dlg_pnt (done_dialog 0))")
      ;;  run the dialog
      (setq flag (start_dialog))
      (if (equal flag 1)
        (progn
          (setq cnt (length sellst) inc -1)
          (while (< (setq inc (1+ inc)) cnt)
            (setq num (nth inc sellst)
                  entry (nth (atoi num) wrklst)
                  obj (car entry)
            )
            (if (acet-layer-locked (acet-dxf 8 (entget obj)))
              (setq lockcnt (1+ lockcnt))
              (progn
                ;; change the existing URL value
                (seturl obj val)
                ;; change string in the list member
                (setq newentry (acet-list-put-nth val entry 1))
                ;; add the new member to the list
                (setq wrklst (acet-list-put-nth newentry wrklst (atoi num)))
              ) ;progn
            ) ;if
          );while
          (if (> lockcnt 0)
            (princ (acet-str-format "\n%1 object(s) on a locked layer." (itoa lockcnt)))
          ) ;if
        ) ;progn
      ) ;if
    )
    (progn
      (alert "Cannot load dialog.")
      (setq flag 0)
    ) ;progn
  );if
 
  wrklst
);defun bns_churl
 
(defun bns_repurl (selstr lst dcl / obj url entry sellst newentry wrklst
                                    find replace num val cnt inc lockcnt onOk)
  ;;  define action handler
  (defun onOk ()
    ;;  extract strings
    (setq find (get_tile "find")
          replace (get_tile "replace")
    )
    ;;removed restrictin of spaces in URL -- not valid when hyperlinking other docs on local system
    ;(cond
    ;  ((wcmatch find "* *")
    ;    (mode_tile "find" 2)
    ;    (acet-sys-beep 1)
    ;    (set_tile "error" "Spaces are not permitted in the URL name!")
    ;  )
    ;  ((wcmatch replace "* *")
    ;    (mode_tile "replace" 2)
    ;    (acet-sys-beep 1)
    ;    (set_tile "error" "Spaces are not permitted in the URL name!")
    ;  )
    ;  (t
        (done_dialog 1)
     ; )
    ;) ;cond
  ) ; defun OK
 
  ;; convert selection string to a list of indexes
  (setq sellst (acet-str-to-list " " selstr))
  (setq wrklst lst
        lockcnt 0)
  ;;  load dialog
  (if (new_dialog "repurl" dcl)
    (progn
      ;;  init dialog
      (action_tile "accept" "(onOk)")
      (action_tile "cancel" "(done_dialog 0)")
      ;;  run the dialog
      (if (equal (start_dialog) 1)
        (if (and find replace)
          (progn
            (setq cnt (length sellst) inc -1)
            (while (< (setq inc (1+ inc)) cnt)
              (setq num (nth inc sellst)
                    entry (nth (atoi num) wrklst)
                    obj (car entry)
                    url (geturl obj)
                    url (if url url "")
              )
              (if (acet-layer-locked (acet-dxf 8 (entget obj)))
                (setq lockcnt (1+ lockcnt))
                (progn
                  (setq val (acet-str-replace find replace (acet-str-space-trim url)))
                  ;; test string length
                  (if (> (strlen val) 256)
                    (progn
                      (setq val (substr val 1 256)) ;setq
                      (princ "\nNew URL value exceeded 256 character limit!  The value has been truncated!")
                    ) ;progn
                  ) ;if
                  ;; replace the matching URL value
                  (seturl obj val)
                  ;; change string in the list member
                  (setq newentry (acet-list-put-nth val entry 1))
                  ;; add the new member to the list
                  (setq wrklst (acet-list-put-nth newentry wrklst (atoi num)))
                ) ;progn
              ) ;if
            );while
            (if (> lockcnt 0)
            (princ (acet-str-format "\n%1 object(s) on a locked layer." (itoa lockcnt)))
;;            (princ (strcat "\n" (itoa lockcnt)
;;                            " object(s) on a locked layer."))
            ) ;if
          )
        ) ;progn
      ) ;if
    )
    (progn
      (alert "Cannot load dialog.")
    ) ;progn
  );if
 
  wrklst
);defun bns_churl
 
 
;imported from avip_url for tahoe port.
(defun C:CHURLS (/ dcl ss obj len cnt url onOk etype flag espace)
  ;;  install error handler
  (acet-error-init nil)
 
  ;;  load
  (setq dcl (avip_url_dcl))
 
  ;;  define action handler
  (defun onOk (/ urlval)
    ;;  set new url on OK
    (setq urlval (get_tile "url")) ;setq
  ;;removed restrictin of spaces in URL -- not valid when hyperlinking other docs on local system
    ;(if (wcmatch urlval "* *")
    ;  (progn
     ;   (mode_tile "url" 2)
     ;   (acet-sys-beep 1)
     ;   (set_tile "error" "Spaces are not permitted in the URL name!")
     ; ) ;progn
     ; (progn
        (seturl obj (get_tile "url"))
        (setq #bns_churl_dlg_pnt (done_dialog 1))
     ; ) ;progn
   ; ) ;if
  )
 
  ;;  select and process objects
  (cond
    ((and (eq 1 (logand 1 (getvar "pickfirst")))
                 (setq ss (ssget "_i" '((-3  ("PE_URL"))))))
       (setq ss (ai_ssget ss))  ;; only if ss exists.
    )
    ((setq ss (ssget '((-3  ("PE_URL")))))
      (if ss (setq ss (ai_ssget ss)))
    )
  )
  ;;(setq ss (ai_aselect))
 
  ;;(if (setq ss (ai_aselect))
  (princ)
  (if (and ss (setq ss (car (bns_ss_mod ss 1 T))))
    (progn
      (if (not #bns_churl_dlg_pnt)
        (setq #bns_churl_dlg_pnt '(-1 -1))
      );if
      (setq len (sslength ss)
            cnt -1
            flag 1)
      (while (and (< (setq cnt (1+ cnt)) len) (> flag 0))
        (setq obj (ssname ss cnt)
              etype (cdr (assoc 0 (entget obj)))
              espace (cdr (assoc 67 (entget obj)))
              url (geturl obj)
              url (if url url "")
        )
        (redraw obj 3)
        ;;  load dialog
        (if (new_dialog "churl" dcl "" #bns_churl_dlg_pnt)
          (progn
            ;;  init dialog
            (set_tile "title"
              (acet-str-format  "Change URL for %1 in %2" etype (if (equal espace 1) "Paper Space" "Model Space"))
            )
            (set_tile "url" (acet-str-space-trim url))
            (action_tile "accept" "(onOk)")
            (action_tile "cancel" "(done_dialog 0)")
            ;;  run the dialog
            (setq flag (start_dialog))
          );progn
          (alert "Cannot load dialog.")
        )
        (redraw obj 4)
      )
    )
  )
 
  (unload_dialog dcl)
  (acet-error-restore)
  (princ)
);defun c:churls
 
;imported from avip_url for tahoe port.
 
(defun C:REPURLS (/ dcl ss url onOk find replace obj len cnt newurl changed )
  ;;  install error handler
  (acet-error-init nil)
 
  ;;  load
  (setq dcl (avip_url_dcl))
 
  ;;  select and process objects
  (cond
    ((and (eq 1 (logand 1 (getvar "pickfirst")))
                 (setq ss (ssget "_i" '((-3  ("PE_URL"))))))
       (setq ss (ai_ssget ss))  ;; only if ss exists.
    )
    ((setq ss (ssget '((-3  ("PE_URL")))))
      (if ss (setq ss (ai_ssget ss)))
    )
  )
 
  ;;(if (setq ss (ai_aselect))
  ;; filter out the locked layers
  (if ss
    (setq ss (car (bns_ss_mod ss 1 T)))
  ) ;if
  (if ss
    (progn
      ;;  define action handler
      (defun onOk ()            ;;this function is duped in aceturl
        ;;  extract strings
        (setq find (get_tile "find")
              replace (get_tile "replace")
        )
   ;;removed restrictin of spaces in URL -- not valid when hyperlinking other docs on local system
        ;(cond
        ;  ((wcmatch find "* *")
        ;    (mode_tile "find" 2)
        ;    (acet-sys-beep 1)
        ;    (set_tile "error" "Spaces are not permitted in the URL name!")
        ;  )
        ;  ((wcmatch replace "* *")
        ;    (mode_tile "replace" 2)
        ;    (acet-sys-beep 1)
        ;    (set_tile "error" "Spaces are not permitted in the URL name!")
        ;  )
        ;  (t
            (done_dialog)
        ; )
        ;) ;cond
      ) ; defun OK
 
      ;;  get find/replace strings from dialog
      (if (new_dialog "repurl" dcl)
        (progn
          ;;  run the dialog
          (action_tile "accept" "(onOk)")
          (start_dialog)
          (unload_dialog dcl)
 
          ;;  perform replace if valid strings
          (if (and find replace)
            (progn
              (setq len (sslength ss)
                    cnt 0
                    changed 0)
              (while (> len 0)
                (setq obj (ssname ss (setq len (1- len)))
                      url (geturl obj)
                      url (if url url "")
                )
 
               (setq newurl (acet-str-replace find replace (acet-str-space-trim url))) ;used to be bns_sstrip
 
               (if (not (equal newurl url))
                  (progn
                    ;;(seturl obj (str-subst find replace url))
                    (if (> (strlen newurl) 256)
                      (progn
                        (setq newurl (substr newurl 1 256)) ;setq
                        (princ "\nNew URL value exceeded 256 character limit!  The value has been truncated!")
                      ) ;progn
                    ) ;if
                    (seturl obj newurl)
                    (setq changed (1+ changed))
                  ) ;progn
                ) ;if
              )
              (princ  (acet-str-format  "\nReplaced values in %1 of %2 URLs" (itoa changed) (itoa (sslength ss)) ))
            )
            (princ "\nNo search string was provided!")
 
          )
        )
        (alert "Cannot load dialog.")
      )
    )
  )
  (acet-error-restore)
  (princ)
);defun repurls


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDPPCdfS3e+mSQAUMIYdg7hkBBm3ctoav5a1Gjr
;;; 4dCbQsd5seBfZJjlizZ6LDA+kwtaFvY6r5XdYIZeboq++ljiGCQZU4iJOMX1qr/c
;;; IKdIpHWd2b9/13XpZo3E/bN4mD5Zhuhg0/e7UVjwbNrchkZTCFNfHq8jS0Dtmr53
;;; PH3ox7alWNVBHDjmg9cWGeEZr2AbcgBdYsr3VJx4TMpAWXwi7QChZ37cD4Q/SB39
;;; OnSyF5KYLp4i2o0O0MrG3e0MPmrLS9Y2nJjLBCdVJIIT94SC48dTTsys9GmGbUo4
;;; rs3YXORYwqXb4SWhzfw1eI0z8Y3g8BWM4ZmAFrbPeVCP0GiKObOb/EDemRdfcKKG
;;; tNeb1hf3VD9VjCU61ASfp+11til5utHvEYZ9XFSWyaA2AdLqoMhXp14OHoAUwwRA
;;; twEliTzoiO4UuI+R5+42B+cOpdTMOCHH8eqDGS8YoyCvhqrkiLqtT1zW1gWKpgmJ
;;; qcV18fZz5gbR4iM9pF9EVACQMdJMZDqQSIHVg+c/TBWoG7OEO2i2hHu3/BI/kDmn
;;; lxzDxt2FXVu469osCgRGTK8RoKS96q8ZvjYnvFqNx3IlZGluNX2us7cXiM+0DNOw
;;; AJ2Dtv2bWQXDjKsHLd3Epv7IkvM31WDUXsfQrSGzKr6X3ybgF/FBrJHKNO/+zo5F
;;; 1VuttA==
;;; -----END-SIGNATURE-----