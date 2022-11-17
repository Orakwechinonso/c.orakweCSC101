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

let CACHENAME = "INSTALLED-COMPONENT";
let SERVER = "initial";

let url = new URL(self.serviceWorker.scriptURL);
let searchParams = url.searchParams;
if (searchParams.has("componenturl")) {
    SERVER = decodeURIComponent(searchParams.get("componenturl"));
    console.debug("Setting SERVER to ", SERVER);
}

self.addEventListener('install', (event) => {
    console.log("Service Worker Installing");
    event.waitUntil(onInstall());
    self.skipWaiting();
});

self.addEventListener('activate', (event) => {
    console.log("Service Worker Activated");
    event.waitUntil(onActivate());
    // to claim the clients immediately
    // otherwise the sw will only control the current client on the next visit
    event.waitUntil(self.clients.claim());
});

async function returnFromCache(event) {
    const cache = await caches.open(CACHENAME);
    const cachedResponse = await cache.match(event.request);
    if (cachedResponse) {
        console.debug("Returning cached response: ", cachedResponse.url);
        return cachedResponse;
    }

    // If all else fails, finally, fall back to local folder
    try {
        const networkResponse = await fetch(event.request);
        console.log("Caching into SW: ", event.request);
        const cache = await caches.open(CACHENAME);
        cache.put(event.request, networkResponse.clone());

        return networkResponse;
    }
    catch(err) {
        return Promise.reject();
    }

    return Promise.reject();
}

if (navigator.onLine) {
    // If online, we'll use the fetch handler from the remote scripts
    importScripts(`${SERVER}/autocad.remotesw.js`)
}
else {
    function onInstall() {}
    function onActivate() {}

    self.addEventListener('fetch', (event) => {
        event.respondWith(async function() {
            return await returnFromCache(event);
        }());
    });
}