//
//
//    TBLNAME.DCL
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

tblname : dialog
{
 label = "";
 key="dlgTitle";
 : column
 {
  : spacer {}
  : text_part
  {
    key="msg";
    label="";
  }
 
  : boxed_column {

     fixed_width=true;
     width=50;

     : list_box {
        key="name_list";
     }
     : row {
       : button {
         fixed_width=true;
         width=5;
         key="pickit";
         label= "&Pick<";
         alignment=left;
       }
       : edit_box {
         key="name";
         is_default=true;
         edit_limit=256;
         alignment=left;
         fixed_width=true;
         width=41;
       }
     } //row
     : spacer {}
  } // boxed column

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
        is_help=true;
     }
     : spacer {}
  } // row

  : text_part
  {
    key="error";
    label="";
  }
 } // column
}

//-------------------------------------------------------------------------
// Multiple select version
tblname_m : dialog
{
 label = "";
 key="dlgTitle";
 : column
 {
  : spacer {}
  : text_part
  {
    key="msg";
    label="";
  }
 
  : boxed_column {

     fixed_width=true;
     width=50;

     : list_box {
        key="name_list";
        multiple_select=true;
     }
     : row {
       : button {
         fixed_width=true;
         width=5;
         key="pickit";
         label= "&Pick<";
         alignment=left;
       }
//       : edit_box {
//         key="name";
//         is_default=true;
//         edit_limit=256;
//         alignment=left;
//         fixed_width=true;
//         width=41;
//       }
     } //row
     : spacer {}
  } // boxed column

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
        is_help=true;
     }
     : spacer {}
  } // row

  : text_part
  {
    key="error";
    label="";
  }
 } // column
}