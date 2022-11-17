//
//
//    LMAN.DCL
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

lman : dialog {
  label = "Layer Manager: Save and Restore Layer Settings";
  key= "lman";
  : boxed_row {
    label = "Saved Layer states:";
    : list_box {
      //label = "Saved Layer states:";
      mnemonic = "L";
      width = 42;
      tabs = "34";
      key = "list_states";
      tab_truncate = true;
    }
    : column {
      // spacer_1;
      //spacer_1;
      : button {
        label = "Save...";
        mnemonic = "S";
        //fixed_width=true;
        //width=18;
        key = "saveas";
      }
      : button {
        label = "Edit...";
        mnemonic = "E";
        //fixed_width=true;
        //width=18;
        key = "edit";
      }
      // spacer_1;
      : button {
        label = "Rename...";
        mnemonic = "n";
        //fixed_width=true;
        //width=18;
        key = "rename";
      }
      : button {
        label = "Delete";
        mnemonic = "D";
        //fixed_width=true;
        //width=18;
        key = "delete";
      }
      // spacer_1;
      : button {
        label = "Import...";
        mnemonic = "I";
        //fixed_width=true;
        //width=18;
        key = "import";
      }
      : button {
        label = "eXport...";
        mnemonic = "X";
        //fixed_width=true;
        //width=18;
        key = "export";
      }
      : button {
        label = "Options...";
        fixed_width=true;
        width=18;
        mnemonic = "O";
        key = "options";
      }
    } //column
  } //box_row
  //: boxed_row {
  : text_part
  { label = "          ";
    key   = "msg";
  }
  //}
  : row {
    spacer_1;
    : button {
      label = "Restore";
      width=14;
      fixed_width=true;
      mnemonic = "R";
      key = "restore";
    }
    //spacer_1;
    : button {
      label = "Close";
      fixed_width=true;
      width=14;
      mnemonic = "C";
      key = "close";
      is_cancel =true;
    }
    : button {
      fixed_width=true;
      width=14;
      key="help";
      label= "&Help";
      is_help=true;
    }
    spacer_1;
  }
}

new_lman : dialog {
  label = "Layer state name";
  : text_part {
    key = "new_msg";
    value = "";
    width = 32;
  }
  : edit_box {
    //label = "New name:";
    key = "new_name";
    value = "";
    fixed_width=true;
    width = 32;
    edit_limit=256;
  }
  : row {
    spacer_1;
    : button {
      label = "OK";
      width=10;
      fixed_width=true;
      mnemonic = "A";
      is_default=true;
      key = "accept";
    }
    spacer_1;
    : button {
      label = "Cancel";
      fixed_width=true;
      width=10;
      mnemonic = "C";
      key = "cancel";
      is_cancel =true;
    }
    spacer_1;
  }
}

warning : dialog {
  label = "* Warning! *";
  spacer_1;
  //alignment = centered;
  : column {
    : text_part {
      key = "warn_msg";
      value = "";
      width = 42;
    }
    : text_part {
      key = "warn_msg2";
      value = "";
      width = 42;
    }
  }
  spacer_1;
  : row {
    spacer_1;
    : button {
      label = "Yes";
      width=10;
      fixed_width=true;
      mnemonic = "Y";
      key = "accept";
    }
    spacer_1;
    : button {
      label = "No";
      fixed_width=true;
      width=10;
      mnemonic = "N";
      key = "cancel";
      is_cancel =true;
    }
    spacer_1;
  }
}

lmanmode : dialog
{
  label = "Layer Manager: Restore Options";
  : boxed_column {

    label="Select properties to restore:";

    : toggle {
      label = "ON/OFF status";
      key = "onoff";
      mnemonic = "O";
      fixed_width = true;
    }
    : toggle {
      label = "Thaw/Freeze status";
      key = "thawfreeze";
      mnemonic = "F";
      fixed_width = true;
    }
    : toggle {
      label = "Thaw/Freeze in current Viewport status";
      key = "vpthawfreeze";
      mnemonic = "V";
      fixed_width = true;
    }
    : toggle {
      label = "Locked/Unlocked status";
      key = "lock";
      mnemonic = "L";
      fixed_width = true;
    }
    : toggle {
      label = "Color status";
      key = "color";
      mnemonic = "C";
      fixed_width = true;
    }
    : toggle {
      label = "Linetype status";
      key = "linetype";
      mnemonic = "t";
      fixed_width = true;
    }
    : toggle {
      label = "Line&weight status";
      key = "lineweight";
      fixed_width = true;
    }
    : toggle {
      label = "&Plot status";
      key = "plot";
      fixed_width = true;
    }
    : toggle {
      label = "Plot&Style status";
      key = "plotstyle";
      fixed_width = true;
    }
    spacer;
  } // boxed column
  : row
  {
    : button {
      fixed_width=true;
      width=11;
      key="accept";
      is_default=true;
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
  } // row
  : text_part
  {
    key="error";
    label="";
  }
}

lman_rename : dialog {
  label = "Rename Layer state";
  : row {
    : text_part {
      key = "txt1";
      value = "";
      width = 10;
    }
    : edit_box {
      key = "ed1";
      value = "";
      fixed_width=true;
      width = 32;
      edit_limit=256;
    }
  } //row
  : row {
    : text_part {
      key = "txt2";
      value = "";
      width = 10;
    }
    : edit_box {
      key = "ed2";
      value = "";
      fixed_width=true;
      width = 32;
      edit_limit=256;
    }
  } //row
  : row {
    spacer_1;
    : button {
      label = "OK";
      width=10;
      fixed_width=true;
      mnemonic = "A";
      is_default=true;
      key = "accept";
    }
    spacer_1;
    : button {
      label = "Cancel";
      fixed_width=true;
      width=10;
      mnemonic = "C";
      key = "cancel";
      is_cancel =true;
    }
    spacer_1;
  }
  : text_part
  {
    key="error";
    label="";
  }

}