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
const invokeAcadFunction = invokeInfo => {
    if (typeof invokeInfo === 'undefined') {
        return null;
    } else if (typeof invokeInfo === 'object') {
        invokeInfo = JSON.stringify(invokeInfo);
    }
    if (typeof execAsync === 'undefined') {
        return Promise.reject('Function "execAsync" not available. ');
    }
    return new Promise((resolve, reject) => {
        execAsync(invokeInfo, data => {
            const result = JSON.parse(data);
            resolve(result.retValue);
        }, data => {
            if (typeof reject !== 'function') {
                return;
            }
            const result = JSON.parse(data);
            try {
                const errorMessage = JSON.parse(result.retErrorString);
                reject(new Error(errorMessage))
            } catch {
                reject(result.retErrorString);
            }
        });
    });
};