//
//    CDORDER.DCL
//    Created by Randy Kintzley 6/24/98
//
//    Copyright © 1999 by Autodesk, Inc.
//
//    Your use of this software is governed by the terms and conditions of the
//    License Agreement you accepted prior to installation of this software.
//    Please note that pursuant to the License Agreement for this software,
//    "[c]opying of this computer program or its documentation except as
//    permitted by this License is copyright infringement under the laws of
//    your country.  If you copy this computer program without permission of
//    Autodesk, you are violating the law."
//
//    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
//    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
//    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
//    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
//    UNINTERRUPTED OR ERROR FREE.
//
//    Use, duplication, or disclosure by the U.S. Government is subject to
//    restrictions set forth in FAR 52.227-19 (Commercial Computer
//    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
//    (Rights in Technical Data and Computer Software), as applicable.
//
//  ----------------------------------------------------------------

cdorder : dialog
{
 label = "CDORDER - color draworder";
 : boxed_row {
     : list_box {
       width=14;
       key="color_list";
     }
   : column {
     : image {
       // action alignment aspect_ratio color fixed_height fixed_width height
       // is_enabled is_tab_stop key label mnemonic value width
       key ="color_image";
       aspect_ratio = 1;
       height = 4;
       width = 7;
       color = -15;
     }
     : spacer {}
     : spacer {}
     : button {
        fixed_width=true;
        width=17;
        key="up";
        label= "Move &Up";
      }
     : button {
        fixed_width=true;
        width=17;
        key="down";
        label= "Move &Down";
      }
     : spacer {}
     : spacer {}
   } // column
   : column {
     : radio_column {
       label="Re-ordering method";

       : radio_button {
         label ="Draworder";
         key="draworder";
       }
       : radio_button {
         label ="Handles";
         key="handles";
       }
     }
     : spacer {}
     : radio_column {
       label="Draworder location";
       key="location";
       : radio_button {
         label ="Front";
         key="front";
       }
       : radio_button {
         label ="Back";
         key="back";
       }
     }
     : spacer {}
     : toggle {
       label="Modify blocks";
       key="blocks";
     }
     : spacer {}
   } // column
 } // boxed_row
 : row
 {
     : spacer {}
     : button {
        fixed_width=true;
        width=11;
        key="accept";
        label= "OK";
      }
     : button {
        fixed_width=true;
        width=11;
        is_cancel=true;
        key="cancel";
        label= "Cancel";
      }
     : button {
        fixed_width=true;
        width=11;
        key="help";
        label= "&Help";
     }
     : spacer {}
 } // row
 : text_part
 {
   key="error";
   label="";
 }
}