//
//
//    DDINS2.DCL
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

ddins2 : dialog {
  label = "SuperHatch - Insert";
  : boxed_column {
    label = "Block";
    : row {
      : button {
        label = "Block...";
        key = "int_blocks";
        mnemonic = "B";
        width = 10;
        fixed_width = true;
      }
      : edit_box {
        label = "";
        key = "current_name";
        width = 36;
        edit_limit = 217;
      }
    }
    : row {
      : button {
        label = "File...";
        key = "ext_blocks";
        mnemonic = "F";
        width = 11.7;
        fixed_width = true;
      }
      : edit_box {
        key = "path_name";
        width = 36;
      }
    }
  }
  : boxed_column {
    label = "Options";
    : row {
      : toggle {
        label = "Specify Parameters on Screen";
        key = "on_screen";
        mnemonic = "S";
        fixed_width = true;
      }
    }
    : row {
      : boxed_column {
        label = "Insertion Point";
        : edit_box {
          label = "X:";
          mnemonic = "X";
          key = "x_pt";
          edit_width = 10;
        }
        : edit_box {
          label = "Y:";
          mnemonic = "Y";
          key = "y_pt";
          edit_width = 10;
        }
        : edit_box {
          label = "Z:";
          mnemonic = "Z";
          key = "z_pt";
          edit_width = 10;
        }
      }
      : boxed_column {
        label = "Scale";
        : edit_box {
          label = "X:";
          mnemonic = "X";
          key = "x_scale";
          edit_width = 10;
        }
        : edit_box {
          label = "Y:";
          mnemonic = "Y";
          key = "y_scale";
          edit_width = 10;
        }
        : edit_box {
          label = "Z:";
          mnemonic = "Z";
          key = "z_scale";
          edit_width = 10;
        }
      }
      : column {
        : boxed_column {
          label = "Rotation";
          fixed_height = true;
          : edit_box {
            label = "Angle:";
            key = "rotation";
            mnemonic = "A";
            edit_width = 6;
          }
        }
      }
    }
  }
  ok_cancel_help_errtile;
}


list_blocks : dialog {
  label = "Defined Blocks";
  : edit_box {
    label = "Pattern:";
    key = "pattern";
    mnemonic = "P";
    allow_accept=true;
    is_default=true;
  }
  : list_box {
    key = "bl_match";
    width = 32;
    allow_accept = true;
  }
  : edit_box {
    label = "Selection:";
    key = "selection";
    mnemonic = "S";
    allow_accept=true;
    edit_limit = 217;
  }
  spacer;
  ok_cancel_err;
}

blk_exists : dialog {
  label = "Warning";
  : paragraph {
    : text_part {
      label = "A Block with this name already exists in the drawing.";
    }
    : text_part {
      label = "Do you want to redefine it?";
    }
  }
  spacer_1;
  : row {
    fixed_width = true;
    alignment = centered;
    : button {
      label = "OK";
      key = "redefine";
      width = 8;
    }
    : spacer {
      width = 2;
    }
    : default_button {
      label = "Cancel";
      key = "no";
      width = 8;
    }
  }
}
