//
//
//    ACETURL.DCL
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

showurls : dialog {
  label = "SHOWURLS - Show objects with attached URLs";
  fixed_width=true;
  width=82;
  : list_box {
    key = "lst_hdr";
    tabs="58 72";
    height = 2;
    fixed_height = true;
    tab_truncate = true;
  }
  : list_box {
    multiple_select=true;
    key="url_list";
    tabs="58 72";
    tab_truncate = true;
    is_default=true;
  }
  : row {
    : spacer {}
    : button {
      fixed_width=true;
      width=11;
      key="accept";
      label= "Show URL";
      mnemonic = "S";
    }
    : button {
      fixed_width=true;
      width=11;
      key="edit";
      label= "Edit";
      mnemonic = "E";
      is_default = true;
    }
    : button {
      fixed_width=true;
      width=11;
      key="replace";
      label= "Replace";
      mnemonic = "R";
    }
    : button {
      fixed_width=true;
      width=11;
      is_cancel=true;
      key="cancel";
      label= "Close";
      mnemonic = "C";
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
  : text_part {
    key="error";
    label="";
  }
}

churl : dialog {
  key = "title";
  label = "Change URL for All Selected Items";
  initial_focus = "url";
  : edit_box {
    label = "URL:";
    key = "url";
    edit_width = 50;
    edit_limit = 250;
    allow_accept = true;
  }
  ok_cancel_err;
}

repurl : dialog {
  key = "title";
  label = "Replace URL text";
  initial_focus = "find";
  : edit_box {
    label = "Find what:";
    key = "find";
    edit_width = 50;
    edit_limit = 250;
  }
  : edit_box {
    label = "Replace with:";
    key = "replace";
    edit_width = 50;
    edit_limit = 250;
    allow_accept = true;
  }
  ok_cancel_err;
}

layeroff : dialog {
  label = "SHOWURLS Warning";
  children_alignment = centered;
  initial_focus = "accept";
  : text { label = "The current layer is OFF!";}
  : text { label = "It must be ON to show URLs!";}
  : text { label = "";}
  : text { label = "Turn the current layer ON?";}
  yes_no;
}

yes_button : retirement_button {
  label           = "  Yes  ";
  key             = "accept";
  is_default      = true;
}

no_button : retirement_button {
  label           = "   No  ";
  key             = "cancel";
  is_cancel       = true;
}

yes_no : column {
  : row {
    fixed_width = true;
    alignment = centered;
    yes_button;
    : spacer { width = 2;}
    no_button;
  }
}


