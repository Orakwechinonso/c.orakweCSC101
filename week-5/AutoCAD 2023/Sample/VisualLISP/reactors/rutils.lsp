;;;                                                                    ;
;;;  RUTILS.LSP                                                        ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998, 1999    ;
;;;  by Autodesk, Inc. All Rights Reserved.                            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;                                                                    ;
;;;  Incorporation of any part of this software into other software,   ;
;;;  except when such incorporation is exclusively for your own use    ;
;;;  or for use by others in your organization in the performance of   ;
;;;  their normal duties, is prohibited without the prior written      ;
;;;  consent of Autodesk, Inc.                                         ;
;;;                                                                    ;
;;;  Copying, modification and distribution of this software or any    ;
;;;  part thereof in any form except as expressly provided herein is   ;
;;;  prohibited without the prior written consent of Autodesk, Inc.    ;
;;;                                                                    ;
;;;  AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      ;
;;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           ;
;;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       ;
;;;  INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          ;
;;;  WILL BE UNINTERRUPTED OR ERROR FREE.                              ;
;;;                                                                    ;
;;;  Restricted Rights for US Government Users.  This software         ;
;;;  and Documentation are provided with RESTRICTED RIGHTS for US      ;
;;;  US Government users.  Use, duplication, or disclosure by the      ;
;;;  Government is subject to restrictions as set forth in FAR         ;
;;;  12.212 (Commercial Computer Software-Restricted Rights) and       ;
;;;  DFAR 227.7202 (Rights in Technical Data and Computer Software),   ;
;;;  as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       ;
;;;  Parkway, San Rafael, California 94903.                            ;
;;;                                                                    ;

;;;--------------------------------------------------------------------;
;;; General Note:  THIS FILE IS A MEMBER OF THE REAC-TST PROJECT       ;
;;;--------------------------------------------------------------------;
;;; General Note: This file creates and uses global variables. Their   ;
;;;               definition and descriptions follow.                  ;
;;;                                                                    ;
;;;                                                                    ;
;;; Global Names                Description                            ;
;;;---------------------------|----------------------------------------;
;;; *current-model-space*     | Vla model space object identifier      ;
;;;                           | value could be:                        ;
;;;                           | #<VLA-OBJECT IAcadModelSpace 027a34c0> ;
;;;---------------------------|----------------------------------------;
;;; *current-paper-space*     | Vla model space object identifier      ;
;;;                           | value could be:                        ;
;;;                           | #<VLA-OBJECT IAcadPaperSpace 03131e0c> ;
;;;---------------------------|----------------------------------------;
;;;  This file contains useful utility functions for the REAC-TST      ;
;;;  project and can be useful for any project.                        ;
;;;                                                                    ;
;;;  For a description of the entire project and a listing of the      ;
;;;  AutoCAD commands defined within it, see the source code file      ;
;;;  REAC-TST.PRJ.                                                     ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;       Function:  _REACTOR-MAKE-SAME-PROPERTIES                     ;
;;;                                                                    ;
;;;    Description:  This function is used to modify a collection of   ;
;;;                  vla objects to share the same properties.         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            obj1 = a valid vla object To be used as the source      ;
;;;                   object to get properties from.                   ;
;;;            obj2 = a valid vla object to be used as the target      ;
;;;                   objects to place the properties from obj1.       ;
;;;   property-list = a list of properties to be modified.             ;
;;;                                                                    ;
;;; Returned Value:  A vla object with updated properties.             ;
;;;		                                                       ;
;;;          Usage:                                                    ;
;;;                (_reactor-make-same-properties                      ;
;;;                      obj1                                          ;
;;;                      obj2                                          ;
;;;                     property-list)                                 ;
;;;--------------------------------------------------------------------;
(defun _reactor-make-same-properties
       (obj1 obj2 property-list / new-value)
  (if (and (eq 'VLA-OBJECT (type obj2))
           (vlax-write-enabled-p obj2)  ; test if object can be modified
           (vlax-read-enabled-p obj1)   ; test if object can be read
      )
    (foreach property property-list
      (if (and (vlax-property-available-p obj1 property)
               (vlax-property-available-p obj2 property)
               (not                     ; don't modify if equal
                 (equal (setq new-value (vlax-get obj1 property))
                        (vlax-get obj2 property)
                 )
               )
          )
        (vlax-put obj2 property new-value)
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  _REACTOR-MAKE-SAME-PROPERTIES-LIST                ;
;;;                                                                    ;
;;;    Description:  This function is used to modify a collection of   ;
;;;                  vla objects to share the same properties.         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      _reactor-make-same-properties                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;       obj-list  = a valid list of vla objects to be modified with  ;
;;;                   the same property list.                          ;
;;;   property-list = a list of properties to be modified.             ;
;;;                                                                    ;
;;; Returned Value:  A vla object                                      ;
;;;		                                                       ;
;;;          Usage:                                                    ;
;;;                (_reactor-make-same-properties-list                 ;
;;;			    notifier                                   ;
;;;			    (vlr-data reactor)                         ;
;;;			    '("Center")                                ;
;;;			  )                                            ;
;;;--------------------------------------------------------------------;
(defun _reactor-make-same-properties-list
       (notifier obj-list property-list)
  (foreach obj obj-list
    (_reactor-make-same-properties notifier obj property-list)
  )
)


;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-MAKE-SAME-RADIUS                          ;
;;;                                                                    ;
;;;    Description:  This function is used as a call back function to  ;
;;;                  an event. It is responsible in modifying the      ;
;;;                  radius of a circle.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      _reactor-make-same-properties-list            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   Filled in by the reactor invoked.                ;
;;;         arg-list  = a list of arguments.                           ;
;;;                   Filled in by the reactor invoked.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (reactor-make-same-radius                           ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun reactor-make-same-radius (notifier reactor arg-list)
  (_reactor-make-same-properties-list
    notifier
    (vlr-data reactor)
    '("Radius")
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-MAKE-SAME-CENTER                          ;
;;;                                                                    ;
;;;    Description:  This function is used as a call back function to  ;
;;;                  an event. It is responsible in modifying the      ;
;;;                  center of a circle.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      _reactor-make-same-properties-list            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   Filled in by the reactor invoked.                ;
;;;         arg-list  = a list of arguments.                           ;
;;;                   Filled in by the reactor invoked.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (reactor-make-same-center                           ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun reactor-make-same-center (notifier reactor arg-list)
  (_reactor-make-same-properties-list
    notifier
    (vlr-data reactor)
    '("Center")
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-MAKE-SAME-RADIUS-COLOR                    ;
;;;                                                                    ;
;;;    Description:  This function is used as a call back function to  ;
;;;                  an event. It is responsible in modifying the      ;
;;;                  radius and color of a circle.                     ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      _reactor-make-same-properties-list            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   Filled in by the reactor invoked.                ;
;;;         arg-list  = a list of arguments.                           ;
;;;                   Filled in by the reactor invoked.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (reactor-make-same-radius-color                     ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun reactor-make-same-radius-color (notifier reactor arg-list)
  (_reactor-make-same-properties-list
    notifier
    (vlr-data reactor)
    '("Radius" "Color")
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Object Property Utilities                                         ;
;;;--------------------------------------------------------------------;
;;;       Function:  SAVE-OBJECT-PROPERTIES                            ;
;;;                                                                    ;
;;;    Description:  Saves a property in the object extension          ;
;;;                  dictionary.  The property value is saved to the   ;
;;;                  reactor data.                                     ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      save-object-properties                        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      vla-object = a valid vla object.                              ;
;;;                                                                    ;
;;;   property-list = a list of properties to place in the             ;
;;;                   objects dictionary.                              ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;		                                                       ;
;;;          Usage:                                                    ;
;;;                (save-object-properties                             ;
;;;                      Vla-Object                                    ;
;;;                      properties-list                               ;
;;;                  )                                                 ;
;;;--------------------------------------------------------------------;
(defun save-object-properties (vla-obj property-list)
  (if (and (eq 'VLA-OBJECT (type vla-obj))
           (vlax-read-enabled-p vla-obj)
      )
    (foreach property property-list
      (if (vlax-property-available-p vla-obj property)
        (vlax-ldata-put
          vla-obj
          property
          (vlax-get vla-obj property)
        )
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-SAVE-CENTER                               ;
;;;                                                                    ;
;;;    Description:  This function is used as a call back function to  ;
;;;                  an event.                                         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      save-object-properties                        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   (this argument is ignored) but must be supplied. ;
;;;         arg-list  = a list of arguments.                           ;
;;;                   (this argument is ignored) but must be supplied. ;
;;;                                                                    ;
;;; Returned Value:  A vlr object reactor.                             ;
;;;                  such as:                                          ;
;;;			#<VLR-XXXXXX-reactor>                          ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (reactor-save-center                                ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun reactor-save-center (notifier reactor arg-list)
  (save-object-properties notifier '("Center"))
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-SAVE-CENTER-COLOR                         ;
;;;                                                                    ;
;;;    Description:  This function is used as a call back function to  ;
;;;                  an event.                                         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      save-object-properties                        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   (this argument is ignored) but must be supplied. ;
;;;         arg-list  = a list of arguments.                           ;
;;;                   (this argument is ignored) but must be supplied. ;
;;;                                                                    ;
;;; Returned Value:  A vlr object reactor.                             ;
;;;                  such as:                                          ;
;;;			#<VLR-XXXXXX-reactor>                          ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (reactor-save-center-color                          ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun reactor-save-center-color (notifier reactor arg-list)
  (save-object-properties notifier '("Center" "Color"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sometimes it is useful to place reactors into selected objects     ;
;;; Here are some constructors to work with                            ;
;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-SAME-REACTOR                               ;
;;;                                                                    ;
;;;    Description:  This creates a duplicate modified event for a     ;
;;;                  list of vla-objects.                              ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         obj-list  a valid list of vla objects.                     ;
;;;        reaction = a valid function to invoke as a call back.       ;
;;;                                                                    ;
;;; Returned Value:  A vlr object reactor.                             ;
;;;                  such as:                                          ;
;;;			#<VLR-Object-reactor>                          ;
;;;		                                                       ;
;;;          Usage:  Where ac1 and ac2 are valid vla-object and        ;
;;;                  reaction is a function call back.                 ;
;;;                 (setq r                                            ;
;;;                     (create-same-reactor (list ac1 ac2)            ;
;;;                       'reactor-save-center-color))                 ;
;;;--------------------------------------------------------------------;
(defun create-same-reactor (obj-list reaction)
  (vlr-object-reactor
    obj-list
    obj-list
    (list (cons :vlr-modified reaction))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-SAFE-PROPERTY-REACTOR                      ;
;;;                                                                    ;
;;;    Description:  This creates a duplicate objectclosed event for   ;
;;;                  a vla-object.                                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             obj = a valid vla object                               ;
;;;        reaction = a valid function to invoke as a call back.       ;
;;;                                                                    ;
;;; Returned Value:  A vlr object reactor.                             ;
;;;                  such as:                                          ;
;;;			#<VLR-Object-reactor>                          ;
;;;		                                                       ;
;;;          Usage:  Where ac1 is a valid vla object and               ;
;;;                  reaction is a function call back.                 ;
;;;                 (setq r                                            ;
;;;                     (create-safe-property-reactor ac1              ;
;;;                       'reactor-save-center-color))                 ;
;;;--------------------------------------------------------------------;
(defun create-safe-property-reactor (obj reaction)
  (vlr-object-reactor
    obj
    nil
    (list (cons :vlr-objectclosed reaction))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selection Set Manipulations                                        ;
;;;--------------------------------------------------------------------;
;;;       Function:  SSGET->VLA-LIST                                   ;
;;;                                                                    ;
;;;    Description:  This function converts a valid ACAD selction      ;
;;;                  set to a list of vla-objects.                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        selection-list = a valid ACAD selection set returned by     ;
;;;                         ssget.                                     ;
;;;                                                                    ;
;;; Returned Value:  A list of all circles as vla-objects              ;
;;;                  such as:                                          ;
;;;		      (                                                ;
;;;			#<VLA-OBJECT IAcadCircle 01b4211c>             ;
;;;			#<VLA-OBJECT IAcadCircle 01b42790>             ;
;;;			#<VLA-OBJECT IAcadCircle 01b429a0>             ;
;;;		      )                                                ;
;;;		                                                       ;
;;;          Usage: (ssget->vla-list (ssget))                          ;
;;;--------------------------------------------------------------------;
(defun ssget->vla-list (selection-set / index vla-list)
  (setq index (if selection-set
                (1- (sslength selection-set))
                -1
              )
  )
  (while (>= index 0)
    (setq vla-list (cons (vlax-ename->vla-object (ssname selection-set index))
                         vla-list
                   )
          index    (1- index)
    )
  )
  vla-list
)

;;;--------------------------------------------------------------------;
;;;       Function:  VLASEL                                            ;
;;;                                                                    ;
;;;    Description:  This function mimics the AutoLISP entsel function.;
;;;                  The difference is that the return value is a      ;
;;;                  vla-object.                                       ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A the selected item as a vla-object               ;
;;;                  such as:                                          ;
;;;		      #<VLA-OBJECT IAcadCircle 01b42790>               ;
;;;		                                                       ;
;;;          Usage:  (vlasel)                                          ;
;;;--------------------------------------------------------------------;
(defun vlasel (/ sel)
  (if (setq sel (entsel))
    (vlax-ename->vla-object (car sel))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  VLA-SEL                                           ;
;;;                                                                    ;
;;;    Description:  This function mimics the AutoLISP entsel function.;
;;;                  The difference is that the return value is a      ;
;;;                  vla-object.                                       ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           message = A string or nil. If nil the entsel             ;
;;;                     function is called without arguments.          ;
;;;                     If this argument not nil and is a string, the  ;
;;;                     entsel function is passed the string value.    ;
;;;                                                                    ;
;;; Returned Value:  A the selected item as a vla-object               ;
;;;                  such as:                                          ;
;;;		      #<VLA-OBJECT IAcadCircle 01b42790>               ;
;;;		                                                       ;
;;;          Usage:  (vla-sel "\nSelect an Object:") or (vla-sel nil)  ;
;;;--------------------------------------------------------------------;
(defun vla-sel (message / sel)
  (if (setq sel (if (equal (type message) 'STR)
                  (entsel message)
                  (entsel)
                )
      )
    (vlax-ename->vla-object (car sel))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Special Selection Set Utilities                                   ;
;;;--------------------------------------------------------------------;
;;;       Function:  SELECT-VLA-CIRCLES                                ;
;;;                                                                    ;
;;;    Description:  This function prompt the user to select objects   ;
;;;                  from the ACAD screen and applies a filter to the  ;
;;;                  selection set.                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A list of all circles as vla-objects              ;
;;;                  such as:                                          ;
;;;		      (                                                ;
;;;			#<VLA-OBJECT IAcadCircle 01b4211c>             ;
;;;			#<VLA-OBJECT IAcadCircle 01b42790>             ;
;;;			#<VLA-OBJECT IAcadCircle 01b429a0>             ;
;;;		      )                                                ;
;;;		                                                       ;
;;;          Usage: (select-vla-circles)                               ;
;;;--------------------------------------------------------------------;
(defun select-vla-circles ()
  (ssget->vla-list (ssget '((0 . "CIRCLE"))))
)

;;;--------------------------------------------------------------------;
;;;       Function:  SELECT-VLA-CIRCLES-ARC                            ;
;;;                                                                    ;
;;;    Description:  This function prompt the user to select objects   ;
;;;                  from the ACAD screen and applies a filter to the  ;
;;;                  selection set.                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A list of all circles or arcs as vla-objects      ;
;;;                  such as:                                          ;
;;;		      (                                                ;
;;;			#<VLA-OBJECT IAcadCircle 01b4211c>             ;
;;;			#<VLA-OBJECT IAcadCircle 01b42790>             ;
;;;			#<VLA-OBJECT IAcadCircle 01b429a0>             ;
;;;		      )                                                ;
;;;		                                                       ;
;;;          Usage: (select-vla-circles-arc)                           ;
;;;--------------------------------------------------------------------;
(defun select-vla-circles-arc ()
  (ssget->vla-list
    (ssget
      '((-4 . "<OR") (0 . "CIRCLE") (0 . "ARC") (-4 . "OR>"))
    )
  )
)


;;;--------------------------------------------------------------------;
;;;       Function:  REMOVE-FROM-ALL-REACTORS                          ;
;;;                                                                    ;
;;;    Description:  This function removes all associations with       ;
;;;                  any object reactor that pertains to the object.   ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           vla-obj  =  a valid vla object                           ;
;;;                                                                    ;
;;; Returned Value:  The last reactor which was modified.              ;
;;;		                                                       ;
;;;          Usage: (remove-from-all-reactors  my-vla-object )         ;
;;;--------------------------------------------------------------------;
(defun remove-from-all-reactors (vla-obj)
  (foreach reactor (vlr-reactors :vlr-object-reactor)
    (vlr-owner-remove reactor vla-obj)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Geometry Utilities                                                ;
;;;--------------------------------------------------------------------;
;;;       Function:  ADD-VECTOR                                        ;
;;;                                                                    ;
;;;    Description:  This function returns the addition of             ;
;;;                  two vectors.                                      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               v1   =  a valid vector list such as:                 ;
;;;                       '( 5 5 5 )                                   ;
;;;               v2   =  a valid vector list such as:                 ;
;;;                       '( 2 2 2 )                                   ;
;;;                                                                    ;
;;; Returned Value:  A vector list with the subtraction performed      ;
;;;                  from v1 and v2.                                   ;
;;;			(add-vector '(5 5 5 ) '(2 2 2))                ;
;;; 					Returns:                       ;
;;;					(7 7 7)                        ;
;;;		                                                       ;
;;;          Usage: (add-vector '(5 5 5 ) '(2 2 2 ))                   ;
;;;--------------------------------------------------------------------;
(defun add-vector (v1 v2)

 (if (eq (type v1) 'VARIANT)
   (if (> (vlax-variant-type v1) 8192)
     (setq v1 (vlax-safearray->list (vlax-variant-value v1)))
   )
 )

(if (eq (type v2) 'VARIANT)
   (if (> (vlax-variant-type v2) 8192)
     (setq v2 (vlax-safearray->list (vlax-variant-value v2)))
   )
 )
  (mapcar '+ v1 v2))

;;;--------------------------------------------------------------------;
;;;       Function:  SUBTRACT-VECTOR                                   ;
;;;                                                                    ;
;;;    Description:  This function returns the subtraction of two      ;
;;;                  vectors.                                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               v1   =  a valid vector list such as:                 ;
;;;                       '( 5 5 5 )                                   ;
;;;               v2   =  a valid vector list such as:                 ;
;;;                       '( 1 1 1 )                                   ;
;;;                                                                    ;
;;; Returned Value:  A vector list with the subtraction performed      ;
;;;                  from v1 and v2.                                   ;
;;;			(subtract-vector '(5 5 5 ) '(1 1 1))           ;
;;; 					Returns:                       ;
;;;					(4 4 4)                        ;
;;;		                                                       ;
;;;          Usage: (subtract-vector '(5 5 5 ) '(1 1 1))               ;
;;;--------------------------------------------------------------------;
(defun subtract-vector (v1 v2) (vlax-3d-point (mapcar '- v1 v2)))

;;;--------------------------------------------------------------------;
;;;       Function:  MULT-BY-SCALAR                                    ;
;;;                                                                    ;
;;;    Description:  This function returns the multiplication of       ;
;;;                  a vector to a number.                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      mult-by-scalar                                ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               vect =  a valid vector list such as:                 ;
;;;                       '( 5 5 5 )                                   ;
;;;             scalar = a valid number                                ;
;;;                                                                    ;
;;; Returned Value:  A vector list with the multiplication of the      ;
;;;                  scalar argument with the supplied vector list.    ;
;;;			(mult-by-scalar '(5 5 5 ) 12)                  ;
;;; 					Returns:                       ;
;;;					(60 60 60)                     ;
;;;		                                                       ;
;;;          Usage: (mult-by-scalar '(5 5 5 ) 12)                      ;
;;;--------------------------------------------------------------------;
(defun mult-by-scalar (vect scalar / sv TransDataA TransData)

  (if (> (vlax-variant-type vect) 8192)
    (setq vect (vlax-safearray->list (vlax-variant-value vect)))
  )

  (setq sv (if (null vect)
    nil
    (cons (* scalar (car vect))
          (mult-by-scalar (cdr vect) scalar)
    )
  ))

;; Convert to a Variant Array of Doubles here ->
 (setq TransDataA (vlax-make-safearray vlax-vbDouble (cons 0 3)))
 (vlax-safearray-fill TransDataA sv)
 (setq TransData (vlax-make-variant TransDataA (logior vlax-vbarray vlax-vbDouble)))
)

;;;--------------------------------------------------------------------;
;;;       Function:  UNIT-VECTOR                                       ;
;;;                                                                    ;
;;;    Description:  This function returns the normal for the          ;
;;;                  vector supplied.                                  ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      mult-by-scalar                                ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               vect =  a valid vector list such as:                 ;
;;;                       '( 5 5 5 )                                   ;
;;;                                                                    ;
;;; Returned Value:  If the vector supplied is not '(0 0 0 )           ;
;;;                  A one unit vector is returned. Otherwise nil      ;
;;;                  is returned.                                      ;
;;;                                                                    ;
;;;			(unit-vector  '( 5 5 5))                       ;
;;;				Returns:                               ;
;;;				 (0.57735 0.57735 0.57735)             ;
;;;		                                                       ;
;;;          Usage: (unit-vector  '( 5 5 5))                           ;
;;;--------------------------------------------------------------------;
(defun unit-vector (vect / uv TransDataA TransData)

  (if (> (vlax-variant-type vect) 8192)
    (setq vect (vlax-safearray->list (vlax-variant-value vect)))
  )

  ;;; test for (0 0 0 )
  (setq uv (if (not (vl-every (function (lambda (x) (zerop x))) vect))
    (mult-by-scalar vect (/ 1.0 (vector-norm vect)))
    nil
    ))

;; Convert to a Variant Array of Doubles here ->
(if uv (progn
 (setq TransDataA (vlax-make-safearray vlax-vbDouble (cons 0 3)))
 (vlax-safearray-fill TransDataA uv)
 (setq TransData (vlax-make-variant TransDataA (logior vlax-vbarray vlax-vbDouble)))
))

)

;;;--------------------------------------------------------------------;
;;;       Function:  VECTOR-NORM                                       ;
;;;                                                                    ;
;;;    Description:  This function returns the normal for the          ;
;;;                  vector supplied.                                  ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      trace-vector                                  ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               vect =  a valid vector list such as:                 ;
;;;                       '( 5 5 5 )                                   ;
;;;                                                                    ;
;;; Returned Value:  A number representing the normal of the vector.   ;
;;;		                                                       ;
;;;          Usage: (vector-norm '( 5 5 5))                            ;
;;;--------------------------------------------------------------------;
(defun vector-norm (vect / nv TransDataA TransData)

  (if (> (vlax-variant-type vect) 8192)
    (setq vect (vlax-safearray->list (vlax-variant-value vect)))
  )

  (setq nv (sqrt (trace-vector (mapcar '* vect vect))))

;; Convert to a Variant Array of Doubles here ->
(if nv (progn
 (setq TransDataA (vlax-make-safearray vlax-vbDouble (cons 0 3)))
 (vlax-safearray-fill TransDataA nv)
 (setq TransData (vlax-make-variant TransDataA (logior vlax-vbarray vlax-vbDouble)))
))

)


;;;--------------------------------------------------------------------;
;;;       Function:  TRACE-VECTOR                                      ;
;;;                                                                    ;
;;;    Description:  This function supplies the                        ;
;;;                  Sum of all the elements of a vector.              ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               vect =  a valid vector list such as:                 ;
;;;                       '( 5 5 5 )                                   ;
;;;                                                                    ;
;;; Returned Value:  A number representing the xxxxxx of the vector.   ;
;;;		                                                       ;
;;;          Usage: (trace-vector '( 5 5 5))                           ;
;;;--------------------------------------------------------------------;
(defun trace-vector (vect)

  (if (> (vlax-variant-type vect) 8192)
    (setq vect (vlax-safearray->list (vlax-variant-value vect)))
  )

  (if (null vect)
    0
    (+ (car vect) (trace-vector (cdr vect)))
  )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Matrix Operations                                                 ;
;;;--------------------------------------------------------------------;
;;;       Function:  CHECK-VECTOR-ELEM                                 ;
;;;                                                                    ;
;;;    Description:  This function check the integrity of the elem     ;
;;;                  argument. This guarantees a number value for      ;
;;;                  functions that require this check.                ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      check-vector-elem                             ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               elem =  a valid number or nil                        ;
;;;                                                                    ;
;;; Returned Value:  A number. If the argument elem is nil.            ;
;;;                  check-vector returns 0 otherwise it returns the   ;
;;;                  argument.                                         ;
;;;		                                                       ;
;;;          Usage: (check-vector-elem 0)                              ;
;;;--------------------------------------------------------------------;
(defun check-vector-elem (elem)
  (if (null elem)
    0
    elem
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-TRANSLATION-MATRIX                           ;
;;;                                                                    ;
;;;    Description:  This function converts a variant vector list      ;
;;;                  (a list of three numbers) into a vector matrix.   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                                                                    ;
;;;                                                                    ;
;;;                  Example:  A vector list '( 5 5 5 ) is passed to   ;
;;;                  make-translation-matrix. The function then        ;
;;;                  translates this value to a matrix list.           ;
;;;                  using the following logic.                        ;
;;;                                                                    ;
;;;			make a translation matrix from                 ;
;;;			1,2 or 3 dim vector v represented as:          ;
;;;			 	list (x), (x y) or (x y z)             ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             vector =  a valid vector list such as:                 ;
;;;                       '( 5 5 5) or '( 1.2 4.5 200.00)              ;
;;;      or vector =  a valid safearray variant vector list of doubles ;
;;;                                                                    ;
;;; Returned Value:  A matrix List such as:                            ;
;;;		      (make-translation-matrix '( 5 5 5 ))             ;
;;;		                                                       ;
;;;		            Returns List In A Variant Array:           ;
;;;				((1.0 0.0 0.0 5.0)                     ;
;;;				  (0.0 1.0 0.0 5.0)                    ;
;;;				  (0.0 0.0 1.0 5.0)                    ;
;;;				  (0.0 0.0 0.0 1.0)                    ;
;;;				)                                      ;
;;;		                                                       ;
;;;       Usage: (make-translation-matrix '( 5 5 5 ))   or             ;
;;;              (make-translation-matrix (vlax-3d-point '( 5 5 5 )))  ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun make-translation-matrix (vector)

  (if (> (vlax-variant-type vector) 8192)
    (setq vector (vlax-safearray->list (vlax-variant-value vector)))
  )

  (setq tm (vlax-tmatrix
    (list (list 1 0 0 (car vector))
          (list 0 1 0 (check-vector-elem (cadr vector)))
          (list 0 0 1 (check-vector-elem (caddr vector)))
          '(0 0 0 1)
    )
  ))

;; Convert to a Variant Array of Doubles here ->
 (setq TransDataA (vlax-make-safearray vlax-vbDouble (cons 0 3) (cons 0 3)))
 (vlax-safearray-fill TransDataA tm)
 (setq TransData (vlax-make-variant TransDataA (logior vlax-vbarray vlax-vbDouble)))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vla-Object Transformation Functions                                ;
;;;--------------------------------------------------------------------;
;;;       Function:  TRANSLATE-VLA-OBJECT                              ;
;;;                                                                    ;
;;;    Description:  This function translates the current              ;
;;;                  transformation values of an object by a supplied  ;
;;;                  vector list.  This vector list is a list of three ;
;;;                  numbers which determine the new values for the    ;
;;;                  existing transformation value.                    ;
;;;                  Translate-Vla-Object is similar to                ;
;;;                  translate-object except this function performs    ;
;;;                  error checking before passing the information     ;
;;;                  to translate-object.                              ;
;;;                                                                    ;
;;;                  Note: This function performs                      ;
;;;                        error checking.                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      translate-object                              ;
;;;                                                                    ;
;;;                  Example:  A line beginning is anchored at 0,0,0.  ;
;;;                  Its ending point is 1,0,0. The transformation     ;
;;;                  value is '(5 5 5). Hence add 5 to the X value, 5  ;
;;;                  to the Y value and 5 to the Z value. The result   ;
;;;                  will be:                                          ;
;;;                       The beginning point will have 5,5,5          ;
;;;                       The ending point will have 6,5,5             ;
;;;                                                                    ;
;;;                  The example above demonstrates a different method ;
;;;                  for moving an object.                             ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           vla-obj  =  a vla object that can contain                ;
;;;                       transformation verctors.                     ;
;;; translation-vector =  a valid vector list such as:                 ;
;;;                       '( 5 5 5) or '( 1.2 4.5 200.00)              ;
;;;                                                                    ;
;;;                                                                    ;
;;; Returned Value:  A vla object                                      ;
;;;                                                                    ;
;;;          Usage: (translate-vla-object vla-Object '( 5 5 5))        ;
;;;--------------------------------------------------------------------;
(defun translate-vla-object (vla-obj translation-vector)
  (if (and vla-obj                          ;; is the vla-object
                                            ;; argument not nil?
           
           (eq 'VLA-OBJECT (type vla-obj))  ;; is it a vla-object?
           
           (vlax-write-enabled-p vla-obj)   ;; test if object
                                            ;; can be modified
      )
    (translate-object vla-obj translation-vector) ;; ok safe to
    						  ;; transform
    						  ;; the vectors.
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  TRANSLATE-OBJECT                                  ;
;;;                                                                    ;
;;;    Description:  This function translates the current              ;
;;;                  transformation values of an object by a supplied  ;
;;;                  vector list.  This vector list is a list of three ;
;;;                  numbers which determine the new values for the    ;
;;;                  existing transformation value.                    ;
;;;                  Translate-Object is similar to                    ;
;;;                  translate-vla-object except this function DOES    ;
;;;                  NOT perform error checking before passing the     ;
;;;                  information to make-translation-matrix.           ;
;;;                                                                    ;
;;;                  Note: This function DOES NOT performs             ;
;;;                        error checking.                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      make-translation-matrix                       ;
;;;                                                                    ;
;;;                  Example:  A line beginning is anchored at 0,0,0.  ;
;;;                  Its ending point is 1,0,0. The transformation     ;
;;;                  value is '(5 5 5). Hence add 5 to the X value, 5  ;
;;;                  to the Y value and 5 to the Z value. The result   ;
;;;                  will be:                                          ;
;;;                       The beginning point will have 5,5,5          ;
;;;                       The ending point will have 6,5,5             ;
;;;                                                                    ;
;;;                  The example above demonstrates a different method ;
;;;                  for moving an object.                             ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           vla-obj  =  a vla object that can contain                ;
;;;                       transformation vectors.                      ;
;;; translation-vector =  a valid vector list such as:                 ;
;;;                       '( 5 5 5) or '( 1.2 4.5 200.00)              ;
;;;                                                                    ;
;;;                                                                    ;
;;; Returned Value:  A vla object                                      ;
;;;                                                                    ;
;;;          Usage: (translate-object vla-Object '( 5 5 5))            ;
;;;--------------------------------------------------------------------;
(defun translate-object (obj translation-vector)
  (vla-transformby
    obj
    (make-translation-matrix translation-vector)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-MODEL-SPACE                                   ;
;;;                                                                    ;
;;;    Description:  This function test if the global variable         ;
;;;                  *current-model-space* is set. If it is the        ;
;;;                  current value of *current-model-space* is         ;
;;;                  returned. Otherwise the value of the global       ;
;;;                  variable *current-model-space* is created.        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A vla model space object                          ;
;;;                  is returned such as:                              ;
;;;                  #<VLA-OBJECT IAcadModelSpace 027a34c0>            ;
;;;                                                                    ;
;;;          Usage: (get-model-space)                                  ;
;;;--------------------------------------------------------------------;
(defun get-model-space (/ tmp)
  (cond (*current-model-space* *current-model-space*)
        ((and (setq tmp (vlax-get-acad-object))
              (setq tmp (vla-get-activedocument tmp))
              (setq tmp (vla-get-modelspace tmp))
         )
         (setq *current-model-space* tmp)
        )
        (t nil)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-PAPER-SPACE                                   ;
;;;                                                                    ;
;;;    Description:  This function test if the global variable         ;
;;;                  *current-paper-space* is set. If it is the        ;
;;;                  current value of *current-paper-space* is         ;
;;;                  returned. Otherwise the value of the global       ;
;;;                  variable *current-paper-space* is created.        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A vla paper space object                          ;
;;;                  is returned such as:                              ;
;;;                  #<VLA-OBJECT IAcadPaperSpace 03131e0c>            ;
;;;                                                                    ;
;;;          Usage: (get-paper-space)                                  ;
;;;--------------------------------------------------------------------;
(defun get-paper-space (/ tmp)
  (cond (*current-paper-space* *current-paper-space*)
        ((and (setq tmp (vlax-get-acad-object))
              (setq tmp (vla-get-activedocument tmp))
              (setq tmp (vla-get-paperspace tmp))
         )
         (setq *current-paper-space* tmp)
        )
        (t nil)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Line Type Functions                                                ;
;;;--------------------------------------------------------------------;
;;;       Function:  LOAD-LINE-TYPES                                   ;
;;;                                                                    ;
;;;    Description:  This searches a linetype collection object and    ;
;;;                  determines if the linetype is present in the      ;
;;;                  collection.                                       ;
;;;                                                                    ;
;;;                  Note that l-obj is a "local" variable within the  ;
;;;                  scope of the vlax-for function because it is      ;
;;;                  used within a "for" expression.                   ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           line-type  =  A string which denotes the linetype        ;
;;;                         to search for in the line-type-collection  ;
;;;                         argument.                                  ;
;;; line-type-collection =  a vla collection object which contains     ;
;;;                         the current linetypes loaded in ACAD.      ;
;;;                                                                    ;
;;; Returned Value:  If the linetype is found a vla linetype object    ;
;;;                  is returned such as:                              ;
;;;                  #<VLA-OBJECT IAcadLineType 03fe0b00>              ;
;;;                  If the linetype search fails this function        ;
;;;                  returns nil.                                      ;
;;;                                                                    ;
;;;          Usage: (load-line-types "CENTER" "acad.lin")              ;
;;;--------------------------------------------------------------------;
(defun find-line-type (line-type line-type-collection / res)
  (setq line-type (strcase line-type))
  (vlax-for l-obj line-type-collection
    (if (= (strcase (vla-get-name l-obj)) line-type)
      (setq res l-obj)
    )
  )
  res
)

;;;--------------------------------------------------------------------;
;;;       Function:  LOAD-LINE-TYPES                                   ;
;;;                                                                    ;
;;;    Description:  This function creates a specified umber of        ;
;;;                  circles in model space.                           ;
;;;                  Required Functions:                               ;
;;;                         find-line-type                             ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      line-type  =  A string which denotes the linetype to load.    ;
;;;      file-name  =  A string which denotes the linetype file to     ;
;;;                    which to load the requested linetype.           ;
;;;                                                                    ;
;;; Returned Value:  A vla linetype object objects such as:            ;
;;;                  #<VLA-OBJECT IAcadLineType 03fe0b00>              ;
;;;                                                                    ;
;;;          Usage: (load-line-types "CENTER" "acad.lin")              ;
;;;--------------------------------------------------------------------;
(defun load-line-types (line-type file-name / tmp res)
  (if (and (setq tmp (vlax-get-acad-object))
           (setq tmp (vla-get-activedocument tmp))
           (setq tmp (vla-get-linetypes tmp)) 	;; linetypes is the last
           					;; set and the current
           					;; linetype collection
      )
    (if (setq res (find-line-type line-type tmp))
      res
      (progn
		 ;; load the linetype
        (vla-load tmp line-type file-name)
		 ;; since the vla-load function returns nil
		 ;; we force the following function to test if
		 ;; the load was successful. If success the
		 ;; return the vla linetype object
        (if (vla-item tmp line-type)

          (vla-item tmp line-type)

          ;; Nothing was loaded so we return nil
          nil
        )   ;; _test to see if the line was loaded
      )     ;; evaluate when the linetype is not loaded in acad
    )       ;; end if for check if linetype is loaded
    nil
  )         ;; end if for various calls to ACAD
) ;;_end function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Circle Functions                                                 ;
;;;--------------------------------------------------------------------;
;;;       Function:  GET-CENTER                                        ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for a center point ;
;;;                  User input is curtailed via a call to initget     ;
;;;                  whose sum of the bit values determine the         ;
;;;                  behavior of this function.                        ;
;;;                                                                    ;
;;;                Bit value	Description                            ;
;;;                                                                    ;
;;;                1           Prevents the user from responding       ;
;;;                            to the request by entering              ;
;;;                            only ENTER.                             ;
;;;                                                                    ;
;;;      Arguments:   none                                             ;
;;;                                                                    ;
;;; Returned Value:  a list of three reals denoting a point            ;
;;;                                                                    ;
;;;          Usage: (get-center)                                       ;
;;;--------------------------------------------------------------------;
(defun get-center ()
  (initget 1)
  (getpoint "\nSelect center")
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-RADIUS                                        ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for a radius from  ;
;;;                  a known point. User input is curtailed via a call ;
;;;                  to initget whose sum of the bit values determine  ;
;;;                  the behavior of this function.                    ;
;;;                                                                    ;
;;;                Bit value	Description                            ;
;;;                                                                    ;
;;;                1           Prevents the user from responding       ;
;;;                            to the request by entering              ;
;;;                            only ENTER.                             ;
;;;                                                                    ;
;;;                2           Prevents the user from responding       ;
;;;                            to the request by entering zero.        ;
;;;                                                                    ;
;;;                4           Prevents the user from responding       ;
;;;                            to the request by entering a            ;
;;;                            negative value.                         ;
;;;                                                                    ;
;;;                32          Uses dashed lines when drawing          ;
;;;                            rubber-band line or box. For those      ;
;;;                            functions with which the user can       ;
;;;                            specify a point by selecting a          ;
;;;                            location on the graphics screen,        ;
;;;                            this bit value causes the               ;
;;;                            rubber-band line or box to be           ;
;;;                            dashed instead of solid.                ;
;;;                            (Some display drivers use a             ;
;;;                            distinctive color instead of            ;
;;;                            dashed lines.)                          ;
;;;                            If the system variable POPUPS           ;
;;;                            is 0, AutoCAD ignores this bit.         ;
;;;                                                                    ;
;;;                64          Prohibits input of a Z                  ;
;;;                            coordinate to the getdist               ;
;;;                            function; lets an application           ;
;;;                            ensure that this function returns       ;
;;;                            a 2D distance.                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           point = a list of three reals that denotes where the     ;
;;;                   rubber-banding visual aid will commence.         ;
;;;                                                                    ;
;;; Returned Value:  a real number denoting a distance                 ;
;;;                                                                    ;
;;;          Usage: (get-radius '(0 0 0 ))                             ;
;;;--------------------------------------------------------------------;
(defun get-radius (point)
;| see above for the bit values used = (+ 1 2 4 32 64) |;

(if (eq (type point) 'VARIANT)
   (if (> (vlax-variant-type point) 8192)
    (setq point (vlax-safearray->list (vlax-variant-value point)))
  )
)
  (initget 103)
  (getdist point "\nSelect radius: ")
)

;;;--------------------------------------------------------------------;
;;;       Function:  ADD-CIRCLE                                        ;
;;;                                                                    ;
;;;    Description:  This function creates a circle in model space.    ;
;;;                  Required Functions:                               ;
;;;                         get-center                                 ;
;;;                         get-radius                                 ;
;;;                                                                    ;
;;;      Arguments:   none                                             ;
;;;                                                                    ;
;;; Returned Value:  vla circle object such as:                        ;
;;;                     #<VLA-OBJECT IAcadCircle 03131694>             ;
;;;                                                                    ;
;;;          Usage: (add-circle)                                       ;
;;;--------------------------------------------------------------------;
(defun add-circle (/ center radius)
  (setq center (vlax-3d-point (get-center))
        radius (get-radius center)
  )
  (vla-addcircle (get-model-space) center radius)
)

;;;--------------------------------------------------------------------;
;;;       Function:  ADD-N-CIRCLE                                      ;
;;;                                                                    ;
;;;    Description:  This function creates a specified umber of        ;
;;;                  circles in model space.                           ;
;;;                  Required Functions:                               ;
;;;                         add-circle                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;              n  = An integer value representing how many circles   ;
;;;                   to place in model space.                         ;
;;;                                                                    ;
;;; Returned Value:  a list of n vla circle objects such as:           ;
;;;                    (                                               ;
;;;                        #<VLA-OBJECT IAcadCircle 03133298>          ;
;;;                        #<VLA-OBJECT IAcadCircle 03133e4c>          ;
;;;                        #<VLA-OBJECT IAcadCircle 031333ac>          ;
;;;                        #<VLA-OBJECT IAcadCircle 03130228>          ;
;;;                    )                                               ;
;;;                                                                    ;
;;;          Usage: (add-n-circles 4)                                  ;
;;;--------------------------------------------------------------------;
(defun add-n-circles (n / res-lst cl)
  (while (> n 0)
    (setq n  (1- n)
          cl (add-circle)
    )
    (if cl
      (setq res-lst (cons cl res-lst))
    )
  )
  res-lst
)

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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAsuFMP+GWUlMsj8mVlKzm+atx9dwQ2fjXda+5b
;;; YLIrzQ5h9pRSY3FALp6DX8juEbnS9ZNDUk1JIgjJ80s5FfuC9nnoN5YJRqHikYpI
;;; IoqdFzqzRWhjtdPT2Vy9spTTaiEZTmZd8OlLdKfESuiMLV9c8z+ZXASeu7FUsaQu
;;; MJvI2wgLG8RfFP2BbY2EoaUZLnlJPomDN1ielEXWQDHDKmJz4eidzeFhumofCq94
;;; I/YI1BqKGAzOLy7doZ2YddeWVAsVccRWnz0MQaLECCCLD2SBGIfldW6Gv1cgcDrD
;;; ETBQ4zfXe7wpg3jHAHH1AEE0vUP4FtJEjcdCcxtS0liyQRWGj286bO3VW1/x6cKy
;;; aeKauv64Q6HyW+vRVC2qhYxNP+IbMagIxFvcRmKUQyJcdyUeUXTY2OYlu61/JK1A
;;; is3Q6fhAnLZlK8xX9gRANCT0tqcVBvHV3HDZzcEXnirCPfiP3Gx67hoWjt5OvSyL
;;; kDLH4LYXoGt0FHzq+wJoxMpkdDdodjDibUv5dY6Vato0U7mq/1N9iSkizXIWDy0T
;;; sh5Oz97zPCBOnSe9EkoggGfqAQDqODnitoLMqqAQuCW8Yv9l+N/z1K5JVIcdwUsO
;;; E0t5E6G5SvNev3t5/TrbC8MKAHecyfXp1op9QfJL4RGh9sZSwX0MLS2tp2TJt3ad
;;; 2gcRYw==
;;; -----END-SIGNATURE-----