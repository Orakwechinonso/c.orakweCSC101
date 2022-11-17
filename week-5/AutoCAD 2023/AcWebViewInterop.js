//////////////////////////////////////////////////////////////////////////////
//
//  Copyright 2021 Autodesk, Inc.  All rights reserved.
//
// This computer source code and related instructions and comments
// are the unpublished confidential and proprietary information of
// Autodesk, Inc. and are protected under applicable copyright and
// trade secret law.  They may not be disclosed to, copied or used
// by any third party without the prior written consent of Autodesk, Inc.
//
//////////////////////////////////////////////////////////////////////////////



// execAsync
window.execAsyncHandlerID = 1;
window.execAsyncHandlers = {};

// generate unique ID for each frame
const FrameID = window === window.top ? "MainFrame" : Math.random().toString(16).slice(2);

function trace(message) {
  const trace = true; //set this to true if for tracing
  if (trace)
    console.log(`INTEROP(${FrameID}): ${message}`);
}

window.addEventListener("message", e => {
  if (e.data.isFromIFrame && window === window.top) {
    trace(`main frame receives message from iframe ${JSON.stringify(e.data)}`);
    postMessageToHost(e.data);
  } else if (e.data.isFromMainFrame && window !== window.top) {
    trace(`iframe receives message from main frame ${JSON.stringify(e.data)}`);
    const func = window[e.data.api];
    func.apply(null, e.data.args);
  }
});

function postTaskToIFrames(func, args) {
  if (window === window.top && window.frames.length > 0) {
    let message = {
      api: func.name,
      args,
      isFromMainFrame: true
    };
    trace(`broadcast function call from main frame to iframe ${JSON.stringify(message)}`);
    for (let i = 0; i < window.frames.length; i++) {
      window.frames[i].postMessage(message, "*");
    }
  }
}

function postMessageToHost(value) {
  if (window.webkit) {
    window.webkit.messageHandlers[value["api"]].postMessage(value);
  } else if (window !== window.top) { // iframe
    window.top.postMessage({ ...value, isFromIFrame: true }, "*");
  } else if (window.chrome.webview) {
    const json = JSON.stringify(value);
    trace(`postMessageToHost(${json}).`);
    window.chrome.webview.postMessage(json);
  } 
}

function execAsync(message, successCallback, errorCallback) {
  var promise = new Promise((resolve, reject) => {
    const handle = 'HANDLE' + FrameID + window.execAsyncHandlerID++;
    window.execAsyncHandlers[handle] = { resolve, reject };
    postMessageToHost({ message: message, handle: handle, api: "execAsync" });
  });
  promise.then(successCallback)
    .catch(errorCallback);
}

// to receive results of execAsync
function onMessageReceive(handle, error, data) {
  if (!window.execAsyncHandlers[handle]) {
    postTaskToIFrames(onMessageReceive, [handle, error, data]);
    return;
  }
  if (!error) {
    window.execAsyncHandlers[handle].resolve(data);
  } else {
    window.execAsyncHandlers[handle].reject(data);
  }
  delete window.execAsyncHandlers[handle];
}


// registerCallback, unregisterCallback
window.registerCallbackHandlers = {};
function registerCallback(name, callback) {
  window.registerCallbackHandlers[name] = callback;
}

function unregisterCallback(name) {
  delete window.registerCallbackHandlers[name];
}

let topLevelCommandPromise = null;

function onCallbackReceive(name, data, handle) {
  // need to do this weirdness because WebKit on Mac does not like top level async functions (or so I've been told) -szilvaa 4/14/2021
  (async () => {
    // not perfect, but should be good enough for AppHome insight iframe
    postTaskToIFrames(onCallbackReceive, [name, data, 0 /* iframe shouldn't call finishCallback */]);

    const msg = { api: "finishCallback", handle };
    let inTopLevelAsyncCommand = false;
    try {
      trace(`onCallbackReceive(name:${name}, data:${data}, handle:${handle}) starts. topLevelCommandPromise=${topLevelCommandPromise}`);
      const result = window.registerCallbackHandlers[name](data);
      if (result?.then) {
        const args = JSON.parse(data);
        if (!topLevelCommandPromise && args.isCommandDefinition) {
          // we just starting a top level command and it is async. Let's store
          // its promise because we don't want any of the executeCommandAsync continuations
          // to return until the top level promise is fulfilled.
          // For example:
          // async function myTestCommand()
          // {
          //    await executeCommandAsync("_ZOOM", "_EXTENTS");
          //    await someOtherAsync();
          // }
          // In the above code, executeCommandAsync() results in acedCmdC() on the native side.
          // Then we unwind the native stack so that we can execute _ZOOM _EXTENTS.
          // Then the native side calls a callback that was passed to acedCmdC(). When this callback
          // returns the native side will consider myTestCommand to be complete.
          //
          // This callback enters Javascript here and executes await someOtherAsync() which returns
          // immediately due to the await. This is where topLevelCommandPromise comes handy. We 
          // want to make sure that we await topLevelCommandPromise before we return from the callback!
          trace(`onCallbackReceive(name:${name}, data:${data}, handle:${handle}): Detected top level async command definition. Storing promise.`)
          topLevelCommandPromise = result;
          inTopLevelAsyncCommand = true;
        }
        
        msg.result = await result;

        // check if we are dealing with an acedCmdC callback
        if (topLevelCommandPromise && name.startsWith("ac.eca_")) {
          trace(`onCallbackReceive(name:${name}, data:${data}, handle:${handle}): Detected acedCmdC callback. await top level promise.`)
          await topLevelCommandPromise;
        }
      } else {
        msg.result = result;
      }
    } catch (e) {
      trace(`onCallbackReceive(name:${name}, data:${data}, handle:${handle}): exception=${e}.`);
      msg.error = e ?? "unknown error";
    } finally {
      if (handle) {
        postMessageToHost(msg);
      }
      if (inTopLevelAsyncCommand) {
        trace(`onCallbackReceive(name:${name}, data:${data}, handle:${handle}): ends. It was a top level command.`)
        topLevelCommandPromise = null;
      }
      else
      {
        trace(`onCallbackReceive(name:${name}, data:${data}, handle:${handle}): ends.`)
      }
    }
  })();
}


// apiVersion
function apiVersion() {
    return 3;
}
