//
//
//    XLIST.DCL
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

xlistblock : dialog {
  label = "Xref/Block Nested Object List";
  : row {
    spacer; spacer;
    : column {
      : text {
        label = "Object:";
        //width = 12;
        //fixed_width = true;
      }
      : text {
        label = "Block Name:";
      }
      : text {
        label = "Layer:";
      }
      : text  {
        label = "Color:";
      }
      : text {
        label = "Linetype:";
      }
    }
    : column {
      : text {
        key = "sObjectType";
        //width = 35;
        //fixed_width = true;
      }
      : text {
        key = "sBlockname";
      }
      : text {
        key = "sLayer";
        width = 31;
      }
      : text {
        key = "sColor";
      }
      : text {
        key = "sLineType";
      }
    }
  }
  ok_only;
}



xlisttext : dialog {
  label = "Xref/Block Nested Object List";
  : row {
    spacer; spacer;
    : column {
      : text {
        label = "Object:";
        //width = 12;
        //fixed_width = true;
      }
      : text {
        label = "Style Name:";
      }
      : text {
        label = "Layer:";
      }
      : text  {
        label = "Color:";
      }
      : text {
        label = "Linetype:";
      }
    }
    : column {
      : text {
        key = "sObjectType";
        //width = 35;
        //fixed_width = true;
      }
      : text {
        key = "sStyleName";
      }
      : text {
        key = "sLayer";
        width = 31;
      }
      : text {
        key = "sColor";
      }
      : text {
        key = "sLineType";
      }
    }
  }
  ok_only;
}



xlist : dialog {
  label = "Xref/Block Nested Object List";
  : row {
    spacer; spacer;
    : column {
      : text {
        label = "Object:";
        //width = 12;
        //fixed_width = true;
      }
      : text {
        label = "Layer:";
      }
      : text  {
        label = "Color:";
      }
      : text {
        label = "Linetype:";
      }
    }
    : column {
      : text {
        key = "sObjectType";
        //width = 35;
        //fixed_width = true;
      }
      : text {
        key = "sLayer";
        width = 31;
      }
      : text {
        key = "sColor";
      }
      : text {
        key = "sLineType";
      }
    }
  }
  ok_only;
}
