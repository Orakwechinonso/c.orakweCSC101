//                                                                     
//   GETPARAM.DCL                                                      
//                                                                     
//   Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998, 1999              
//   by Autodesk, Inc. All Rights Reserved.                            
//                                                                     
//   You are hereby granted permission to use, copy and modify this    
//   software without charge, provided you do so exclusively for       
//   your own use or for use by others in your organization in the     
//   performance of their normal duties, and provided further that     
//   the above copyright notice appears in all copies and both that    
//   copyright notice and the limited warranty and restricted rights   
//   notice below appear in all supporting documentation.              
//                                                                     
//   Incorporation of any part of this software into other software,   
//   except when such incorporation is exclusively for your own use    
//   or for use by others in your organization in the performance of   
//   their normal duties, is prohibited without the prior written      
//   consent of Autodesk, Inc.                                         
//                                                                     
//   Copying, modification and distribution of this software or any    
//   part thereof in any form except as expressly provided herein is   
//   prohibited without the prior written consent of Autodesk, Inc.    
//                                                                     
//   AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      
//   AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           
//   MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       
//   INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          
//   WILL BE UNINTERRUPTED OR ERROR FREE.                              
//                                                                     
//   Restricted Rights for US Government Users.  This software         
//   and Documentation are provided with RESTRICTED RIGHTS for US      
//   US Government users.  Use, duplication, or disclosure by the      
//   Government is subject to restrictions as set forth in FAR         
//   12.212 (Commercial Computer Software-Restricted Rights) and       
//   DFAR 227.7202 (Rights in Technical Data and Computer Software),   
//   as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       
//   Parkway, San Rafael, California 94903.                            
//                                                                     

// --------------------------------------------------------------------
//  General Note:  This dialog file is called by RCTR.LSP              
//                                                                     
// --------------------------------------------------------------------
dcl_settings : default_dcl_settings { audit_level = 3; }

DLG_GetParams : dialog 
{
	label = "Choose parameters";
        key = "key-choose-pars";
        initial_focus = "br-distance";
  	: button {
        	label = "Select curve";
        	key = "key-Select-curve";
        	mnemonic = "S";
        	action = "(GetParams-dlg-callback t 10)";
	}
    	: edit_box {
       		label = "Color";
       		value = "1";
       		key = "key-Color";
       		mnemonic = "C";
       		action = "(GetParams-dlg-callback nil 12)";
    	}
  	: boxed_column {
    		label="Choose cirlce number and radius";
	    	: edit_box {
        		label = "Number";
        		value = "3";
        		key = "key-Number";
        		mnemonic = "N";
        		action = "(GetParams-dlg-callback nil 20)";
    		}
	    	: edit_box {
        		label = "Radius";
        		value = "0.0";
        		key = "key-Radius";
        		mnemonic = "R";
        		action = "(GetParams-dlg-callback nil 30)";
    		}
  		: button {
        		label = "Get radius";
        		key = "key-Get-radius";
        		mnemonic = "G";
        		action = "(GetParams-dlg-callback t 40)";
    		}
   	} 
  	: toggle {
        	label = "Persistent reactors";
        	key = "key-Persistent-reactors";
        	mnemonic = "P";
        	action = "(GetParams-dlg-callback nil 50)";
	}
    	ok_cancel;
}
