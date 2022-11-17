
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
var AcBlockFavorites;
(function (AcBlockFavorites) {
    var Interop;
    (function (Interop) {
        var getIsLoggedIn = function() {
            return new Promise( (resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockFavorites_Interop.getIsLoggedIn',
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
        Interop.getIsLoggedIn = getIsLoggedIn;        
        var getColorTheme = function () {
            return new Promise( (resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockFavorites_Interop.getColorTheme',
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
                    functionName: 'AcBlockFavorites_Interop.getTabName',
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
                    functionName: 'AcBlockFavorites_Interop.getLanguage',
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
            });
        };
        Interop.getLanguage = getLanguage;
        var signIn = function () {
            return invokeAcadFunction({
                functionName: 'AcBlockFavorites_Interop.signIn',
                functionParams: {},
                invokeAsCommand: false
            });
        };
        Interop.signIn = signIn;
        var launchHelp = function (strHelpId) {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockFavorites_Interop.launchHelp',
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
        var getIsBlockSyncFolderSetToCloudStorage = () => {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'AcBlockFavorites_Interop.getIsBlockSyncFolderSetToCloudStorage',
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
            });
        };
        Interop.getIsBlockSyncFolderSetToCloudStorage = getIsBlockSyncFolderSetToCloudStorage;
        var showStorageDialog = function (title, default_str) {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: "AcBlockFavorites_Interop.showStorageDialog",
                    functionParams: {title: title, default: default_str}
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
        Interop.showStorageDialog = showStorageDialog;
        var setBlockSyncFolder = function (strPath) {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: "AcBlockFavorites_Interop.setBlockSyncFolder",
                    functionParams: {"BLOCKSYNCFOLDER": strPath}
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
        Interop.setBlockSyncFolder = setBlockSyncFolder;
        var getIsStateLocked = () => {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: "AcBlockFavorites_Interop.getIsStateLocked",
                    functionParams: {}
                }), result => {
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
                }, result => { 
                    if (typeof(reject) !== "function")
                        return;
                    reject(result);
                });
            });
        };
        Interop.getIsStateLocked = getIsStateLocked;
        var setIsStateLocked = (lock) => {
            return new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: "AcBlockFavorites_Interop.setIsStateLocked",
                    functionParams: {"LOCK": lock}
                }), result => {
                    if (typeof (resolve) !== "function")
                        return;
                    var jsonObj = JSON.parse(result);
                    if (typeof (apiVersion) === 'function' && apiVersion() > 2) {
                        jsonObj = jsonObj.retValue;
                    } else {
                        if (jsonObj.retValue) {
                            jsonObj = JSON.parse(jsonObj.retValue);
                        }
                    }
                    resolve(jsonObj);
                }, result => {
                    if (typeof (reject) !== "function")
                        return;
                    reject(result);
                });
            });
        };
        Interop.setIsStateLocked = setIsStateLocked;               
        const showGuideDialog = () => {
            return invokeAcadFunction({
                functionName: "AcBlockFavorites_Interop.showGuideDialog",
                functionParams: {},
                invokeAsCommand: false
            });
        };
        Interop.showGuideDialog = showGuideDialog;
        const setIsOnBoardingCompleted = () => {
            return invokeAcadFunction({
                functionName: "AcBlockFavorites_Interop.setOnBoardingCompleted",
                functionParams: {},
                invokeAsCommand: false
            });
        };
        Interop.setIsOnBoardingCompleted = setIsOnBoardingCompleted;
        const getBlockSyncFolderFromServer = (timeout) => {
            return invokeAcadFunction({
                functionName: "AcBlockFavorites_Interop.getBlockSyncFolderFromServer",
                functionParams: {"timeout": timeout},
                invokeAsCommand: false
            });
        };
        Interop.getBlockSyncFolderFromServer = getBlockSyncFolderFromServer;
        const showConsentDialog = () => {
            return invokeAcadFunction({
                functionName: "AcBlockFavorites_Interop.showConsentDialog",
                functionParams: {},
                invokeAsCommand: false
            });
        };
        Interop.showConsentDialog = showConsentDialog;
        const getConsentStatus = () => {
            return invokeAcadFunction({
                functionName: "AcBlockFavorites_Interop.getConsentStatus",
                functionParams: {},
                invokeAsCommand: false
            });
        };
        Interop.getConsentStatus = getConsentStatus;
        const setConsentStatus = (status) => {
            return invokeAcadFunction({
                functionName: "AcBlockFavorites_Interop.setConsentStatus",
                functionParams: {"CONSENT": status},
                invokeAsCommand: false
            });
        };
        Interop.setConsentStatus = setConsentStatus;
        const getIsBlockSyncFolderCustomizedToLocal = () => {
            return invokeAcadFunction({
                functionName: "AcBlockFavorites_Interop.getIsBlockSyncFolderCustomizedToLocal",
                functionParams: {},
                invokeAsCommand: false
            });
        };
        Interop.getIsBlockSyncFolderCustomizedToLocal = getIsBlockSyncFolderCustomizedToLocal;
        Interop.walkOnBoarding = () => {
            return invokeAcadFunction({
                functionName: "AcBlockFavorites_Interop.walkOnBoarding",
                functionParams: {},
                invokeAsCommand: false
            });
        };
    })(Interop || (Interop = {}));
    AcBlockFavorites.Interop = Interop;
})(AcBlockFavorites || (AcBlockFavorites={}));