var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
//Common declarations referenced by generated code
var Autodesk;
(function (Autodesk) {
    let JavaScript;
    (function (JavaScript) {
        let ErrorStatus;
        (function (ErrorStatus) {
            ErrorStatus[ErrorStatus["eOk"] = 0] = "eOk";
        })(ErrorStatus = JavaScript.ErrorStatus || (JavaScript.ErrorStatus = {}));
    })(JavaScript = Autodesk.JavaScript || (Autodesk.JavaScript = {}));
})(Autodesk || (Autodesk = {}));
//---start of generated typescript---
// 
//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright 2020 by Autodesk, Inc.
//
// The information contained herein is confidential, proprietary to Autodesk,
// Inc., and considered a trade secret as defined in section 499C of the
// penal code of the State of California.  Use of this information by anyone
// other than authorized employees of Autodesk, Inc. is granted only under a
// written non-disclosure agreement, expressly prescribing the scope and
// manner of such use.
//
//////////////////////////////////////////////////////////////////////////////
//---end of generated typescript---
//---start of generated typescript---
// 
//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright 2020 by Autodesk, Inc.
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
var JavaScript = Autodesk.JavaScript;
var help;
(function (help) {
    let Api;
    (function (Api) {
        function handlePromiseSuccess(resolve, result) {
            if (typeof (resolve) == 'function') {
                var resObj = JSON.parse(result);
                if (typeof (apiVersion) == 'function') {
                    resolve(resObj.retValue);
                }
            }
        }
        Api.handlePromiseSuccess = handlePromiseSuccess;
        function handlePromiseError(reject, result) {
            var error = JSON.parse(result);
            //retcode must be present
            if (error.retCode == undefined)
                throw TypeError("Internal error: retCode is not present.");
            //and it must be non-eOk
            if (error.retCode == JavaScript.ErrorStatus.eOk)
                throw "Internal error: retCode is eOk in error callback.";
            //errorMessage is optional
            if (error.retErrorString == undefined)
                reject(error.retCode);
            else
                reject(error.retCode, error.retErrorString);
        }
        Api.handlePromiseError = handlePromiseError;
        function login() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.login', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.login = login;
        function logout() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.logout', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.logout = logout;
        function isLoggedIn() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.isLoggedIn', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.isLoggedIn = isLoggedIn;
        function getUserInfo() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.getUserInfo', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.getUserInfo = getUserInfo;
        function getUserName() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.getUserName', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.getUserName = getUserName;
        function getUserFirstName() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.getUserFirstName', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.getUserFirstName = getUserFirstName;
        function getUserLastName() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.getUserLastName', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.getUserLastName = getUserLastName;
        function getUserId() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.getUserId', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.getUserId = getUserId;
        function getUserEmail() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.getUserEmail', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.getUserEmail = getUserEmail;
        function HomeUrl(Online) {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.HomeUrl', functionParams: { Online: Online }
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.HomeUrl = HomeUrl;
        function OfflineInstalled() {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.OfflineInstalled', functionParams: {}
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.OfflineInstalled = OfflineInstalled;
        function HelpFindUI(ID, IsTopic) {
            var promise = new Promise((resolve, reject) => {
                execAsync(JSON.stringify({
                    functionName: 'help_Api.HelpFindUI', functionParams: { ID: ID, IsTopic: IsTopic }
                }), function (result) {
                    handlePromiseSuccess(resolve, result);
                }, function (result) {
                    handlePromiseError(reject, result);
                });
            });
            return promise;
        } //end of function
        Api.HelpFindUI = HelpFindUI;
        var userLoggedInEvent_handlers = [];
        //called by native code: fires event to callback supplied by the user
        function userLoggedInEventCallback(args) {
            return __awaiter(this, void 0, void 0, function* () {
                var obj = JSON.parse(args);
                //unpack all event arguments
                var userInfo = obj.userInfo;
                //call each handler
                userLoggedInEvent_handlers.forEach((handler) => {
                    handler(userInfo);
                });
            });
        }
        //allows user to supply a callback
        function addHandler_userLoggedInEvent(userFunction) {
            var separator = (typeof (apiVersion) == 'function' && apiVersion() > 2) ? '.' : '_';
            if (userLoggedInEvent_handlers.indexOf(userFunction) < 0) {
                registerCallback('help_Api' + separator + 'userLoggedInEvent', userLoggedInEventCallback);
                userLoggedInEvent_handlers.push(userFunction);
            }
            return Promise.resolve();
        }
        Api.addHandler_userLoggedInEvent = addHandler_userLoggedInEvent;
        //removes the callback
        function removeHandler_userLoggedInEvent(userFunction) {
            for (var i = 0; i < userLoggedInEvent_handlers.length; i++) {
                if (userLoggedInEvent_handlers[i] === userFunction) {
                    userLoggedInEvent_handlers.splice(i, 1);
                    break;
                }
            }
            return Promise.resolve();
        }
        Api.removeHandler_userLoggedInEvent = removeHandler_userLoggedInEvent;
        var userLoggedOutEvent_handlers = [];
        //called by native code: fires event to callback supplied by the user
        function userLoggedOutEventCallback() {
            return __awaiter(this, void 0, void 0, function* () {
                //call each handler
                userLoggedOutEvent_handlers.forEach((handler) => {
                    handler();
                });
            });
        }
        //allows user to supply a callback
        function addHandler_userLoggedOutEvent(userFunction) {
            var separator = (typeof (apiVersion) == 'function' && apiVersion() > 2) ? '.' : '_';
            if (userLoggedOutEvent_handlers.indexOf(userFunction) < 0) {
                registerCallback('help_Api' + separator + 'userLoggedOutEvent', userLoggedOutEventCallback);
                userLoggedOutEvent_handlers.push(userFunction);
            }
            return Promise.resolve();
        }
        Api.addHandler_userLoggedOutEvent = addHandler_userLoggedOutEvent;
        //removes the callback
        function removeHandler_userLoggedOutEvent(userFunction) {
            for (var i = 0; i < userLoggedOutEvent_handlers.length; i++) {
                if (userLoggedOutEvent_handlers[i] === userFunction) {
                    userLoggedOutEvent_handlers.splice(i, 1);
                    break;
                }
            }
            return Promise.resolve();
        }
        Api.removeHandler_userLoggedOutEvent = removeHandler_userLoggedOutEvent;
    })(Api = help.Api || (help.Api = {})); //end module Api
})(help || (help = {}));
//---end of generated typescript---
//# sourceMappingURL=Autodesk.AutoCAD.Help.js.map