//
//
//    YES_NO.DCL
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

bns_yes_no60_15 : dialog {
  label = "";
  key = "title";
  width=60;
  height=15;

  : column {
    : text_part {
      label = "";
      key = "msg0";
    }
    : text_part {
      label = "";
      key = "msg1";
    }
    : text_part {
      label = "";
      key = "msg2";
    }
    : text_part {
      label = "";
      key = "msg3";
    }
    : text_part {
      label = "";
      key = "msg4";
    }
    : text_part {
      label = "";
      key = "msg5";
    }
    : text_part {
      label = "";
      key = "msg6";
    }
    : text_part {
      label = "";
      key = "msg7";
    }
  }
  : row {
    : spacer {}
    : spacer {}
    : spacer {}
    : button {
      label = " Yes ";
      mnemonic = "Y";
      key = "accept";
      is_default=true;
      fixed_width=true;
      width=12;
    }
    : button {
      label = " No  ";
      mnemonic = "N";
      key = "cancel";
      is_cancel=true;
      fixed_width=true;
      width=12;
    }
    : spacer {}
    : spacer {}
    : spacer {}
  }
}

bns_yes_no40_10 : dialog {
  label = "";
  key = "title";
  width=40;
  height=10;

  : column {
    : text_part {
      label = "";
      key = "msg0";
    }
    : text_part {
      label = "";
      key = "msg1";
    }
    : text_part {
      label = "";
      key = "msg2";
    }
    : text_part {
      label = "";
      key = "msg3";
    }
    : text_part {
      label = "";
      key = "msg4";
    }
    : text_part {
      label = "";
      key = "msg5";
    }
    : text_part {
      label = "";
      key = "msg6";
    }
    : text_part {
      label = "";
      key = "msg7";
    }
  }
  : row {
    : spacer {}
    : spacer {}
    : spacer {}
    : button {
      label = " Yes ";
      mnemonic = "Y";
      key = "accept";
      is_default=true;
      fixed_width=true;
      width=12;
    }
    : button {
      label = " No  ";
      mnemonic = "N";
      key = "cancel";
      is_cancel=true;
      fixed_width=true;
      width=12;
    }
    : spacer {}
    : spacer {}
    : spacer {}
  }
}

bns_yes_no20_5 : dialog {
  label = "";
  key = "title";
  width=20;
  height=5;

  : column {
    : text_part {
      label = "";
      key = "msg0";
    }
    : text_part {
      label = "";
      key = "msg1";
    }
    : text_part {
      label = "";
      key = "msg2";
    }
    : text_part {
      label = "";
      key = "msg3";
    }
    : text_part {
      label = "";
      key = "msg4";
    }
    : text_part {
      label = "";
      key = "msg5";
    }
  }
  : row {
    : spacer {}
    : button {
      label = " Yes ";
      mnemonic = "Y";
      key = "accept";
      is_default=true;
      fixed_width=true;
      width=12;
    }
    : button {
      label = " No  ";
      mnemonic = "N";
      key = "cancel";
      is_cancel=true;
      fixed_width=true;
      width=12;
    }
    : spacer {}
  }
}
