
//////////////////////////////////////////////////////////////////////////////
//---end of generated typescript--- 
//---start of generated typescript---
// 
//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright 2012 by Autodesk, Inc.
//
// The information contained herein is confidential, proprietary to Autodesk,
// Inc., and considered a trade secret as defined in section 499C of the
// penal code of the State of California.  Use of this information by anyone
// other than authorized employees of Autodesk, Inc. is granted only under a
// written non-disclosure agreement, expressly prescribing the scope and
// manner of such use.
//
//////////////////////////////////////////////////////////////////////////////
// Include TS files
///<reference path="CommonTypes.ts"/>
var AcBlockLibrary;
(function (AcBlockLibrary) {
    var Interop;
    (function (Interop) {
        var getIsBlockLibrariesExisted = function() {
            return new Promise( (resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockLibrary_Interop.getIsBlockLibrariesExisted',
                    functionParams: {}
                }), (result) => {
                    if (typeof(resolve) !== 'function') {
                        return;
                    }
                    var resObj = JSON.parse(result);
                    if (typeof (apiVersion) === 'function' && apiVersion() > 2) {
                        resolve(resObj.retValue);
                    } else {
                        if (resObj.hasOwnProperty("retValue"))
                            resolve(JSON.parse(resObj.retValue));
                        else
                            resolve(resObj);
                    }
                }, (result) => {
                    if (typeof(reject) !== 'function') {
                        return;
                    }
                    reject(JSON.stringify(result.retValue));
                });
            } );
        };
        Interop.getIsBlockLibrariesExisted = getIsBlockLibrariesExisted;
        var closeBrowser = function () {
            execAsync(JSON.stringify({
                functionName: 'AcBlockLibrary_Interop.closeBrowser',
                functionParams: {}
            }), () => {}, (result) => {
                if (typeof(reject) !== 'function') {
                    return;
                }
                reject(JSON.stringify(result.retValue));
            });
        };
        Interop.closeBrowser = closeBrowser;
        var getColorTheme = function () {
            return new Promise( (resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockLibrary_Interop.getColorTheme',
                    functionParams: {}
                }), (result) => {
                    if (typeof(resolve) !== "function") {
                        return;
                    }
                    var resObj = JSON.parse(result);
                    if (typeof (apiVersion) === 'function' && apiVersion() > 2) {
                        resolve(resObj.retValue);
                    } else {
                        if (resObj.hasOwnProperty("retValue"))
                            resolve(JSON.parse(resObj.retValue));
                        else
                            resolve(resObj);
                    }
                }, (result) => {
                    if (typeof(reject) !== 'function') {
                        return;
                    }
                    reject(JSON.stringify(result.retValue));
                });
            })
        };
        Interop.getColorTheme = getColorTheme;
        var getTabName = function () {
            return new Promise( (resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockLibrary_Interop.getTabName',
                    functionParams: {}
                }), (result) => {
                    if (typeof(resolve) !== "function") {
                        return;
                    }
                    var resObj = JSON.parse(result);
                    if (typeof (apiVersion) === 'function' && apiVersion() > 2) {
                        resolve(resObj.retValue);
                    } else {
                        if (resObj.hasOwnProperty("retValue"))
                            resolve(JSON.parse(resObj.retValue));
                        else
                            resolve(resObj);
                    }
                }, (result) => {
                    if (typeof(reject) !== 'function') {
                        return;
                    }
                    reject(JSON.stringify(result.retValue));
                });
            })
        };
        Interop.getTabName = getTabName;
        var getLanguage = function () {
            return new Promise( (resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockLibrary_Interop.getLanguage',
                    functionParams: {}
                }), (result) => {
                    if (typeof(resolve) !== "function") {
                        return;
                    }
                    var resObj = JSON.parse(result);
                    if (typeof (apiVersion) === 'function' && apiVersion() > 2) {
                        resolve(resObj.retValue);
                    } else {
                        if (resObj.hasOwnProperty("retValue"))
                            resolve(JSON.parse(resObj.retValue));
                        else
                            resolve(resObj);
                    }
                }, (result) => {
                    if (typeof(reject) !== 'function') {
                        return;
                    }
                    reject(JSON.stringify(result.retValue));
                });
            })
        };
        Interop.getLanguage = getLanguage;        
        var launchHelp = function (strHelpId) {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockLibrary_Interop.launchHelp',
                    functionParams: {helpId: strHelpId}
                }), (result) => {     
                    if (typeof(resolve) !== "function") {
                        return;
                    }               
                    resolve();
                }, (result) => {
                    if (typeof(reject) !== "function") {
                        return;
                    }
                    reject();
                });
            });
        };
        Interop.launchHelp = launchHelp;        
        var showOpenDialog = (strTitle, strDefault) => {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: "AcBlockLibrary_Interop.showOpenDialog",
                    functionParams: {title: strTitle, default: strDefault}
                }), (result) => {
                    if (typeof(resolve) !== "function")
                        return;
                    var jsonObj = JSON.parse(result);
                    if (typeof(apiVersion) === 'function' && apiVersion() > 2) {
                        jsonObj = jsonObj.retValue;
                    } else {
                        if (jsonObj.retValue) {
                            jsonObj = JSON.parse(jsonObj.retValue);
                        }
                    }
                    resolve(jsonObj);
                }, (result) => {
                    if (typeof(reject) !== "function")
                        return;
                    reject(result);
                });
            });
        };
        Interop.showOpenDialog = showOpenDialog;
        var setBlockLibrary = (strPath) => {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: "AcBlockLibrary_Interop.setBlockLibrary",
                    functionParams: {"BlockLibrary": strPath}
                }), (result) => {
                    if (typeof(resolve) !== 'function') {
                        return;
                    }
                    var resObj = JSON.parse(result);
                    if (typeof (apiVersion) === 'function' && apiVersion() > 2) {
                        resolve(resObj.retValue);
                    } else {
                        if (resObj.hasOwnProperty("retValue"))
                            resolve(JSON.parse(resObj.retValue));
                        else
                            resolve(resObj);
                    }
                }, (result) => {
                    if (typeof(reject) !== 'function') {
                        return;
                    }
                    reject(JSON.stringify(result.retValue));
                });
            });
        };
        Interop.setBlockLibrary = setBlockLibrary;        
    })(Interop || (Interop = {}));
    AcBlockLibrary.Interop = Interop;
})(AcBlockLibrary || (AcBlockLibrary={}));