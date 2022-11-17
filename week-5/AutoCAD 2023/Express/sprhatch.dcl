//
//
//    SPRHATCH.DCL
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

sprhatch : dialog {
  label = "SuperHatch";
  : column {
    : boxed_column {
      label="Pattern Type";
      //: column {
      : button {
        alignment=centered;
        fixed_width=true;
        width=22;
        key="image";
        label="&Image...";
      }
      : button {
        alignment=centered;
        fixed_width=true;
        width=22;
        key="block";
        label="&Block...";
      }
      : button {
        alignment=centered;
        fixed_width=true;
        width=22;
        key="xref";
        label="&Xref Attach...";
      }
      : button {
        alignment=centered;
        fixed_width=true;
        width=22;
        key="wipeout";
        label="&Wipeout...";
      }
      spacer;
      spacer;
      : button {
        alignment=centered;
        fixed_width=true;
        width=22;
        key="select";
        label= "&Select existing <";
      }
      //} // column
    } // boxed column
    spacer;
    : boxed_row {
      label = "Curve error tolerance:";
      : edit_box {
        key = "clipitres";
        fixed_width=true;
        width=20;
      } //edit_box
    } // boxed row
    spacer;
  } // column
  : row {
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
  } // row
  : text_part
  {
    key="error";
    label="";
  }
}
