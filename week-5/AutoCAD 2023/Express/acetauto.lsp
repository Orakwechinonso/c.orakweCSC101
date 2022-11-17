;;
;;  acetauto.lsp - autoloading for Express Tools
;;
;;  Copyright © 2000 by Autodesk, Inc.
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
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;    Implements autoloading for Express Tool commands. 2001 Version
;;
;;-------------------------------------------------------------------------
;;

(acet-autoload2	'("edittime"  "edittime" nil "edittime" 1))
(acet-autoload2	'("acadinfo"	"acadinfo"	nil	"acadinfo" 2))
(acet-autoload2	'("acadinfo"	"addvars2scr"	nil	"addvars2scr" 2))
(acet-autoload2	'("acadinfo"	"lspdump"	nil	"lspdump" 2))
(acet-autoload2	'("acadinfo"	"vars2scr"	nil	"vars2scr" 2))
(acet-autoload2	'("acettxt"	"tcircle"	nil	"tcircle" 2))
(acet-autoload2	'("acettxt"	"tcount"	nil	"tcount" 2))
(acet-autoload2	'("acettxt"	"tjust"	nil	"tjust" 2))
(acet-autoload2	'("acettxt"	"torient"	nil	"torient" 2))
(acet-autoload2	'("aceturl"	"CHURLS"	nil	"CHURLS" 2))
(acet-autoload2	'("aceturl"	"REPURLS"	nil	"REPURLS" 2))
(acet-autoload2	'("aceturl"	"showurls"	nil	"showurls" 2))
(acet-autoload2	'("acetutil2.fas"	(acet-acadreactor-id-add e1 na2)))
(acet-autoload2	'("acetutil2.fas"	(acet-angle-equal a b fuz)))
(acet-autoload2	'("acetutil2.fas"	(acet-block-purge bna)))
(acet-autoload2	'("acetutil2.fas"	(acet-bs-strip a)))
(acet-autoload2	'("acetutil2.fas"	(acet-calc-tan a)))
(acet-autoload2	'("acetutil2.fas"	(acet-dcl-list-make a lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-dict-name-list dict)))
(acet-autoload2	'("acetutil2.fas"	(acet-dtor a)))
(acet-autoload2	'("acetutil2.fas"	(acet-explode ss)))
(acet-autoload2	'("acetutil2.fas"	(acet-filename-valid fna)))
(acet-autoload2	'("acetutil2.fas"	(acet-full-dwgname)))
(acet-autoload2	'("acetutil2.fas"	(acet-general-props-get-pairs e1)))
(acet-autoload2	'("acetutil2.fas"	(acet-general-props-set-pairs e1 lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-angle-to-ellipseAngle v e1)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-angle-trans ang frm to)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-arc-3p-d-angle p1 p2 p3)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-arc-bulge cent p1 dang)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-arc-center p1 p2 p3)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-arc-d-angle cent p1 p2)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-intersectwith na na2 flag)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-object-fuz na)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-object-normal-vector na)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-self-intersect lst flag2)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-vertex-list na)))
(acet-autoload2	'("acetutil2.fas"	(acet-geom-zoom-for-select lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-list-assoc-put new lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-list-assoc-remove a e1)))
(acet-autoload2	'("acetutil2.fas"	(acet-list-isort lst ind)))
(acet-autoload2	'("acetutil2.fas"	(acet-list-put-nth a lst n)))
(acet-autoload2	'("acetutil2.fas"	(acet-list-remove-adjacent-dups lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-list-remove-duplicates lst fuz)))
(acet-autoload2	'("acetutil2.fas"	(acet-list-to-ss lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-lwpline-make lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-pline-segment-list e1)))
(acet-autoload2	'("acetutil2.fas"	(acet-pline-segment-list-apply e1 lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-point-flat p1 xv xv2)))
(acet-autoload2	'("acetutil2.fas"	(acet-reg-tree-get keypath key)))
(acet-autoload2	'("acetutil2.fas"	(acet-reg-tree-set keypath lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-rtod a)))
(acet-autoload2	'("acetutil2.fas"	(acet-ss-filter arglst)))
(acet-autoload2	'("acetutil2.fas"	(acet-ss-filter-current-ucs ss printflag)))
(acet-autoload2	'("acetutil2.fas"	(acet-ss-filter-item ss a)))
(acet-autoload2	'("acetutil2.fas"	(acet-ss-filter-validate flt)))
(acet-autoload2	'("acetutil2.fas"	(acet-ss-scale-to-fit-base p1 p2 sf)))
(acet-autoload2	'("acetutil2.fas"	(acet-ss-ssget-filter ss flt)))
(acet-autoload2	'("acetutil2.fas"	(acet-ss-to-list ss)))
(acet-autoload2	'("acetutil2.fas"	(acet-ss-union SSLIST)))
(acet-autoload2	'("acetutil2.fas"	(acet-temp-segment p1 p2 p3 dflag)))
(acet-autoload2	'("acetutil2.fas"	(acet-ucs-capture-viewports)))
(acet-autoload2	'("acetutil2.fas"	(acet-ucs-restore-viewports lst)))
(acet-autoload2	'("acetutil2.fas"	(acet-ucs-set-z z)))
(acet-autoload2	'("acetutil2.fas"	(acet-ucs-to-object na)))
(acet-autoload2	'("acetutil2.fas"	(acet-ui-fence-select)))
(acet-autoload2	'("acetutil2.fas"	(acet-ui-getfile str def ext dgn flag)))
(acet-autoload2	'("acetutil2.fas"	(acet-ui-polygon-select dflag)))
(acet-autoload2	'("acetutil2.fas"	(acet-ui-single-select flt lockOk)))
(acet-autoload2	'("acetutil2.fas"	(acet-viewport-frozen-layer-list na)))
(acet-autoload2	'("acetutil2.fas"	(bns_blink_and_show_object na c)))
(acet-autoload2	'("acetutil2.fas"	(bns_blk_match blkname flt lst flag)))
(acet-autoload2	'("acetutil2.fas"	(bns_tbl_match table filter)))
(acet-autoload2	'("acetutil2.fas"	(bns_tjust ss just)))
(acet-autoload2	'("acetutil2.fas"	(isort lst index)))
(acet-autoload2	'("acetutil3.fas"	(acet-acadreactor-id-remove e1 na2)))
(acet-autoload2	'("acetutil3.fas"	(acet-acadreactor-id-remove-all e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-acadreactor-ids-get e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-acadreactor-id-swap newna oldna reflst)))
(acet-autoload2	'("acetutil3.fas"	(acet-alt-viewport ss na2)))
(acet-autoload2	'("acetutil3.fas"	(acet-block-make-anon ss bna)))
(acet-autoload2	'("acetutil3.fas"	(acet-file-find-font fna)))
(acet-autoload2	'("acetutil3.fas"	(acet-filename-associated-app fna)))
(acet-autoload2	'("acetutil3.fas"	(acet-general-props-get e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-general-props-set ss proplst)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-ellipseAngle-to-angle v e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-ellipse-arc-list e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-ellipse-point-at-angle ang e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-ellipse-to-pline e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-is-arc p1 p2 p3 p4 fuz)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-point-inside p1 lst dst)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-point-scale pnt p1 fact)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-rect-points a b)))
(acet-autoload2	'("acetutil3.fas"	(acet-geom-vector-parallel v1 v2)))
(acet-autoload2	'("acetutil3.fas"	(acet-group-make-anon LST DESC)))
(acet-autoload2	'("acetutil3.fas"	(acet-hatch-loopedge-make e1 lst)))
(acet-autoload2	'("acetutil3.fas"	(acet-hatch-loop-make e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-hatch-loop-make-data-get e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-list-assoc-append new lst)))
(acet-autoload2	'("acetutil3.fas"	(acet-list-remove-nth a lst)))
(acet-autoload2	'("acetutil3.fas"	(acet-pline-is-2d e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-plines-explode ss)))
(acet-autoload2	'("acetutil3.fas"	(acet-plines-explode-width-data-get e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-plines-rebuild plst)))
(acet-autoload2	'("acetutil3.fas"	(acet-plines-rebuild-apply-widths w1 w2 e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-plines-rebuild-set-elevation ss z)))
(acet-autoload2	'("acetutil3.fas"	(acet-re-make e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-ss-convert-ellipses-to-plines ss)))
(acet-autoload2	'("acetutil3.fas"	(acet-ss-redraw ss flag)))
(acet-autoload2	'("acetutil3.fas"	(acet-ss-scale-to-fit ss p1 p2 border)))
(acet-autoload2	'("acetutil3.fas"	(acet-ss-zoom-extents ss)))
(acet-autoload2	'("acetutil3.fas"	(acet-str-env-expand fna)))
(acet-autoload2	'("acetutil3.fas"	(acet-str-is-printable a)))
(acet-autoload2	'("acetutil3.fas"	(acet-str-list-select lst spec)))
(acet-autoload2	'("acetutil3.fas"	(acet-tjust ss a)))
(acet-autoload2	'("acetutil3.fas"	(acet-tjust-group-codes just)))
(acet-autoload2	'("acetutil3.fas"	(acet-tjust-keyword e1)))
(acet-autoload2	'("acetutil3.fas"	(acet-ui-drag p1 msg fun)))
(acet-autoload2	'("acetutil3.fas"	(acet-ui-entsel alst)))
(acet-autoload2	'("acetutil3.fas"	(acet-ui-getcorner p1)))
(acet-autoload2	'("acetutil3.fas"	(acet-ui-get-long-name msg)))
(acet-autoload2	'("acetutil3.fas"	(acet-ui-m-get-names alst)))
(acet-autoload2	'("acetutil3.fas"	(acet-viewport-lock-set na flag)))
(acet-autoload2	'("acetutil3.fas"	(acet-viewport-next-pickable)))
(acet-autoload2	'("acetutil3.fas"	(bns_blktbl_match flt)))
(acet-autoload2	'("acetutil3.fas"	(bns_ss_mod ss flag pr)))
(acet-autoload2	'("acetutil3.fas"	(bns_vp_on_screen na)))
(acet-autoload2	'("acetutil4.fas"	(acet-acad-refresh)))
(acet-autoload2	'("acetutil4.fas"	(acet-appid-delete spec)))
(acet-autoload2	'("acetutil4.fas"	(acet-blink-and-show-object alst)))
(acet-autoload2	'("acetutil4.fas"	(acet-calc-bitlist NUM)))
(acet-autoload2	'("acetutil4.fas"	(acet-editor-reactor-add rlst)))
(acet-autoload2	'("acetutil4.fas"	(acet-file-backup fna)))
(acet-autoload2	'("acetutil4.fas"	(acet-file-backup-delete)))
(acet-autoload2	'("acetutil4.fas"	(acet-file-backup-restore)))
(acet-autoload2	'("acetutil4.fas"	(acet-file-find fna)))
(acet-autoload2	'("acetutil4.fas"	(acet-file-find-image fna)))
(acet-autoload2	'("acetutil4.fas"	(acet-file-find-on-path fna path)))
(acet-autoload2	'("acetutil4.fas"	(acet-filename-supportpath-remove fna)))
(acet-autoload2	'("acetutil4.fas"	(acet-file-open fna flag)))
(acet-autoload2	'("acetutil4.fas"	(acet-geom-object-end-points e1)))
(acet-autoload2	'("acetutil4.fas"	(acet-geom-object-z-axis e1)))
(acet-autoload2	'("acetutil4.fas"	(acet-geom-trans-ucs-to-image lst e1 code)))
(acet-autoload2	'("acetutil4.fas"	(acet-list-group-by-assoc lst)))
(acet-autoload2	'("acetutil4.fas"	(acet-list-is-dotted-pair a)))
(acet-autoload2	'("acetutil4.fas"	(acet-list-split lst item)))
(acet-autoload2	'("acetutil4.fas"	(acet-path-to-list a)))
(acet-autoload2	'("acetutil4.fas"	(acet-pline-make lst)))
(acet-autoload2	'("acetutil4.fas"	(acet-pref-supportpath-list)))
(acet-autoload2	'("acetutil4.fas"	(acet-r12-dxf-convert ss)))
(acet-autoload2	'("acetutil4.fas"	(acet-reactor-exists rlst)))
(acet-autoload2	'("acetutil4.fas"	(acet-reactor-remove r)))
(acet-autoload2	'("acetutil4.fas"	(acet-ss-annotation-filter ss)))
(acet-autoload2	'("acetutil4.fas"	(acet-ss-intersection SS1 SS2)))
(acet-autoload2	'("acetutil4.fas"	(acet-ss-remove SS1 SS2)))
(acet-autoload2	'("acetutil4.fas"	(acet-ss-remove-dups ss fuz ignore)))
(acet-autoload2	'("acetutil4.fas"	(acet-ss-remove-dups-extents-list ss fuz)))
(acet-autoload2	'("acetutil4.fas"	(acet-ss-remove-dups-generalize-ent na fuz gclst)))
(acet-autoload2	'("acetutil4.fas"	(acet-ss-remove-dups-reverse lst)))
(acet-autoload2	'("acetutil4.fas"	(acet-ss-sort ss func)))
(acet-autoload2	'("acetutil4.fas"	(acet-sys-control-down)))
(acet-autoload2	'("acetutil4.fas"	(acet-sys-lmouse-down)))
(acet-autoload2	'("acetutil4.fas"	(acet-sys-shift-down)))
(acet-autoload2	'("acetutil4.fas"	(acet-ucs-set lst)))
(acet-autoload2	'("acetutil4.fas"	(acet-viewport-frozen-layer-list-set na laylst)))
(acet-autoload2	'("acetutil4.fas"	(acet-viewport-vpfreeze ss laylst)))
(acet-autoload2	'("acetutil4.fas"	(acet-wmfin FIL)))
(acet-autoload2	'("acetutil4.fas"	(acet-xdata-get LST)))
(acet-autoload2	'("acetutil4.fas"	(acet-xdata-set LST)))
(acet-autoload2	'("acetutil4.fas"	(acet-xref-cproject-path proj)))
(acet-autoload2	'("acetutil4.fas"	(bns_annotation_ss ss)))
(acet-autoload2	'("acetutil4.fas"	(bns_groups_sel lst)))
(acet-autoload2	'("acetutil4.fas"	(bns_groups_unsel)))
(acet-autoload2	'("acetutil4.fas"	(bns_truncate_2_view a b x y x2 y2)))
(acet-autoload2	'("acetutil4.fas"	(acet-insert-attrib-set na lst bquiet)))
(acet-autoload2	'("acetutil4.fas"	(acet-insert-attrib-get lst)))
(acet-autoload2	'("acetutil4.fas"	(bns_mod_att na lst)))
(acet-autoload2	'("acetutil4.fas"	(bns_get_att lst)))
(acet-autoload2	'("acetutil4.fas"	(acet-insert-attrib-mod na lst bquiet)))
(acet-autoload2	'("ai_utils.lsp"	(ai_abort app msg)))
(acet-autoload2	'("ai_utils.lsp"	(ai_acadapp)))
(acet-autoload2	'("ai_utils.lsp"	(ai_alert msg)))
(acet-autoload2	'("ai_utils.lsp"	(ai_angle value error_msg)))
(acet-autoload2	'("ai_utils.lsp"	(ai_angtos val)))
(acet-autoload2	'("ai_utils.lsp"	(ai_aselect)))
(acet-autoload2	'("ai_utils.lsp"	(ai_aselect1 msg)))
(acet-autoload2	'("ai_utils.lsp"	(ai_autossget1 msg)))
(acet-autoload2	'("ai_utils.lsp"	(ai_beep)))
(acet-autoload2	'("ai_utils.lsp"	(ai_common_state ss_ename)))
(acet-autoload2	'("ai_utils.lsp"	(ai_dcl dcl_file)))
(acet-autoload2	'("ai_utils.lsp"	(ai_entity_locked ename message)))
(acet-autoload2	'("ai_utils.lsp"	(ai_error s)))
(acet-autoload2	'("ai_utils.lsp"	(ai_helpfile)))
(acet-autoload2	'("ai_utils.lsp"	(ai_notrans)))
(acet-autoload2	'("ai_utils.lsp"	(ai_num value error_msg range)))
(acet-autoload2	'("ai_utils.lsp"	(ai_return value)))
(acet-autoload2	'("ai_utils.lsp"	(ai_rtos val)))
(acet-autoload2	'("ai_utils.lsp"	(ai_ssget ss)))
(acet-autoload2	'("ai_utils.lsp"	(ai_sslength ss)))
(acet-autoload2	'("ai_utils.lsp"	(ai_strltrim s)))
(acet-autoload2	'("ai_utils.lsp"	(ai_strrtrim s)))
(acet-autoload2	'("ai_utils.lsp"	(ai_strtrim s)))
(acet-autoload2	'("ai_utils.lsp"	(ai_table table_name bit)))
(acet-autoload2	'("ai_utils.lsp"	(ai_trans)))
(acet-autoload2	'("ai_utils.lsp"	(ai_transd)))
(acet-autoload2	'("ai_utils.lsp"	(ai_undo_off)))
(acet-autoload2	'("ai_utils.lsp"	(ai_undo_on)))
(acet-autoload2	'("ai_utils.lsp"	(ai_undo_pop)))
(acet-autoload2	'("ai_utils.lsp"	(ai_undo_push)))
(acet-autoload2	'("aliasedit"	"ALIASEDIT"	nil	"ALIASEDIT" 2))
(acet-autoload2	'("aspace"	"alignspace"	nil	"alignspace" 2))
(acet-autoload2	'("attout"	"attin"	nil	"attin" 2))
(acet-autoload2	'("attout"	"attout"	nil	"attout" 2))
(acet-autoload2	'("Blockq"	"block?"	nil	"blockq" 2))
(acet-autoload2	'("blocktoxref"	"blockreplace"	nil	"blockreplace" 2))
(acet-autoload2	'("blocktoxref"	"-blockreplace"	nil	"blockreplace" 2))
(acet-autoload2	'("blocktoxref"	"blocktoxref"	nil	"blocktoxref" 2))
(acet-autoload2	'("blocktoxref"	"-blocktoxref"	nil	"blocktoxref" 2))
(acet-autoload2	'("Breakl"	"breakline"	nil	"breakline" 2))
(acet-autoload2	'("bscale"	"bscale"	nil	"bscale" 2))
(acet-autoload2	'("bscale"	"PSBSCALE"	nil	"PSBSCALE" 2))
(acet-autoload2	'("Burst"	"BURST"	nil	"BURST" 2))
(acet-autoload2	'("Clipit"	"clipit"	nil	"clipit" 2))
(acet-autoload2	'("copym"	"copym"	nil	"copym" 2))
(acet-autoload2	'("Count"	"bcount"	nil	"bcount" 2))
(acet-autoload2	'("dimassoc"	"dimreassoc"	nil	"dimreassoc" 2))
(acet-autoload2	'("etbug"	"etbug"	nil	"etbug" 2))
(acet-autoload2	'("exoffset"	"exOffset"	nil	"exOffset" 2))
(acet-autoload2	'("explan"	"explan"	nil	"explan" 2))
(acet-autoload2	'("fastsel"	"fastsel"	nil	"fastselect" 2))
(acet-autoload2	'("fastsel"	"fs"	nil	"fastselect" 2))
(acet-autoload2	'("fastsel"	"fsmode"	nil	"fastselect" 2))
(acet-autoload2	'("Getsel"	"GETSEL"	nil	"GETSEL" 2))
(acet-autoload2	'("ix_edit"	"imageapp"	nil	"imageapp" 2))
(acet-autoload2	'("ix_edit"	"imageedit"	nil	"imageedit" 2))
(acet-autoload2	'("Julian"	"date"	nil	"date" 2))
(acet-autoload2	'("layoutmerge"	"layoutmerge"	nil	"layoutmerge" 2))
(acet-autoload2	'("layoutmerge"	"-layoutmerge"	nil	"layoutmerge" 2))
(acet-autoload2	'("lspdata"	"lsp"	nil	"lsp" 2))
(acet-autoload2	'("lspsurf"	"LSPSURF"	nil	"LSPSURF" 2))
(acet-autoload2	'("Mkltype"	"mkltype"	nil	"mkltype" 2))
(acet-autoload2	'("Mkshape"	"mkshape"	nil	"mkshape" 2))
(acet-autoload2	'("MoveBak"	"MOVEBAK"	nil	"MOVEBAK" 2))
(acet-autoload2	'("Mstretch"	"mstretch"	nil	"mstretch" 2))
(acet-autoload2	'("plt2dwg"	"plt2dwg"	nil	"plt2dwg" 2))
(acet-autoload2	'("qquit"	"QQUIT"	nil	"QQUIT" 2))
(acet-autoload2	'("Redir"	"redir"	nil	"redir" 2))
(acet-autoload2	'("Redir"	"redirmode"	nil	"redirmode" 2))
(acet-autoload2	'("Redir"	"-redirmode"	nil	"redirmode" 2))
(acet-autoload2	'("revert"	"REVERT"	nil	"REVERT" 2))
(acet-autoload2	'("Rtext"	"RTEDIT"	nil	"RTEDIT" 2))
(acet-autoload2	'("Rtext"	"RTEXT"	nil	"RTEXT" 2))
(acet-autoload2	'("Rtext"	"RTEXTAPP"	nil	"RTEXTAPP" 2))
(acet-autoload2	'("rtucs"	"rtucs"	nil	"rtucs" 2))
(acet-autoload2	'("rtucs"	"acetucs-top" nil	"rtucs" 2))
(acet-autoload2	'("rtucs"	"acetucs-front" nil	"rtucs" 2))
(acet-autoload2	'("rtucs"	"acetucs-right" nil	"rtucs" 2))
(acet-autoload2	'("rtucs"	"acetucs-back" nil	"rtucs" 2))
(acet-autoload2	'("rtucs"	"acetucs-left" nil	"rtucs" 2))
(acet-autoload2	'("rtucs"	"acetucs-bottom" nil    "rtucs" 2))
(acet-autoload2	'("saveall"	"SAVEALL"	nil	"SAVEALL" 2))
(acet-autoload2	'("shp2blk"	"shp2blk"	nil	"shp2blk" 2))
(acet-autoload2	'("Sprhatch"	"imageoverlap"	nil	"imageoverlap" 2))
(acet-autoload2	'("Sprhatch"	"Superhatch"	nil	"Superhatch" 2))
(acet-autoload2	'("Sprhatch"	"tframes"	nil	"tframes" 2))
(acet-autoload2	'("tblname.lsp"	(acet-dict-filter-match tblname flt)))
(acet-autoload2	'("tblname.lsp"	(acet-is-symbol-table str)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-check-flags tblname flags flt)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-format tblname)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-get alst)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-get-cmd alst)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-get-cmd-list msg lst)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-get-dlg alst)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-get-ename tblname name)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-get-is-valid name tblname flags flt lst)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-get-item-list tblname flags flt)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-get-pickit alst)))
(acet-autoload2	'("tblname.lsp"	(acet-ui-table-name-object-data tblname e1 gcode)))
(acet-autoload2	'("tcase"	"tcase"	nil	"tcase" 2))
(acet-autoload2	'("tcase"	"-tcase"	nil	"tcase" 2))
(acet-autoload2	'("Textfit"	"textfit"	nil	"textfit" 2))
(acet-autoload2	'("Textfit"	"TFHELP"	nil	"TFHELP" 2))
(acet-autoload2	'("Textmask"	"textmask"	nil	"textmask" 2))
(acet-autoload2	'("Textmask"	"textunmask"	nil	"textunmask" 2))
(acet-autoload2	'("trex"	"trex"	nil	"trex" 2))
(acet-autoload2	'("Trexblk"	"bextend"	nil	"bextend" 2))
(acet-autoload2	'("Trexblk"	"btrim"	nil	"btrim" 2))
(acet-autoload2	'("tscale"	"psTscale"	nil	"psTscale" 2))
(acet-autoload2	'("tscale"	"tscale"	nil	"tscale" 2))
(acet-autoload2	'("tscale"	"tSpaceInvaders"	nil	"tSpaceInvaders" 2))
(acet-autoload2	'("Txtexp"	"txtexp"	nil	"txtexp" 2))
(acet-autoload2	'("vpscale"	"vpscale"	nil	"vpscale" 2))
(acet-autoload2	'("vpsync"	"vpsync"	nil	"vpsync" 2))
(acet-autoload2	'("Xdata"	"xdata"	nil	"xdata" 2))
(acet-autoload2	'("Xdata"	"XDLIST"	nil	"XDLIST" 2))
(acet-autoload2	'("Xlist"	"xlist"	nil	"xlist" 2))
(acet-autoload2	'("Xlist"	"-xlist"	nil	"xlist" 2))
(acet-autoload2	'("cdorder"	"cdorder"	nil	"cdorder" 2))
(acet-autoload2	'("cdorder"	"-cdorder"	nil	"cdorder" 2))
(acet-autoload2	'("Extrim"	"extrim"	nil	"extrim" 2))
(acet-autoload2	'("flatten"	"flatten"	nil	"flatten" 2))
(acet-autoload2	'("Gatte"	"gatte"	nil	"gatte" 2))
(acet-autoload2	'("Julian"	"date"	nil	"date" 2))
(acet-autoload2	'("Mpedit"	"mpedit"	nil	"mpedit" 2))
(acet-autoload2	'("Ssx"	"ssx"	nil	"ssx" 2))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgC7BS2PTsr7vdS3uaqGLVDWqimkkm/VDLNwrYbN
;;; 9nMF8kl0Gfx+xcTQCeOu5FQxuSuV5VSoveO+8fUgRQSMQ4WHQJUiNpUZQXgC/798
;;; JgWzl7hTU9eazyYStzZW/d6JgzavPyWpWggHoUUIAD7UAxnDD3sj3r0syHrTIB5q
;;; 3R7rPU5YcqfkwS+nRgRfAoPzbQtf+F6swA11LPB+2j8V5vxGdLDEMo9d3EA6AX0/
;;; KUj8ObkgpbanRwHD9OZl+htCNiCt7hXerZaTtH15ewGxh/XWgClUJpnwui5MBZMz
;;; CkFjThX+lY5ctO4Nr4SU8YVtqPbdyU2b0RYNXjU9CW6TX0+qh8tZp4b2ixNnSvw9
;;; Pv75JGmUEbqlypjyt+wyJpEOP7hvmr3oJg6UH7HCo6TlAmudeLcbbeTMC6ysZdKi
;;; xA2JWqoJysZy9NsJOSuby/7mxYORPMXek5VwegYJW3bIriswDWQTZgDFmtgmeA24
;;; jXBNxuY6n6pfmhyijxV2bvDfJf2i0FvU2vKL6HXIRSBK+NJjfDoVpvuhLcSqGc2I
;;; K2qAHDaFAEEPnTTPvmhjR7e3XJztU/tk54rjnAFwgt1r9a6Nao5DdPslsP44SYqf
;;; h9FugaLuGEDTy07ckZF/ESek0U34EBeVypb7Nlpwr7taMF5T2mdlLeP1FUNZ2MUY
;;; ypRYMg==
;;; -----END-SIGNATURE-----