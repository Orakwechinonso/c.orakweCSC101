//////////////////////////////////////////////////////////////////////////////
//
//  Copyright 2017 by Autodesk, Inc.  All rights reserved.
//
// This computer source code and related instructions and comments
// are the unpublished confidential and proprietary information of
// Autodesk, Inc. and are protected under applicable copyright and
// trade secret law.  They may not be disclosed to, copied or used
// by any third party without the prior written consent of Autodesk, Inc.
//
//////////////////////////////////////////////////////////////////////////////
$.Class('MapServiceProvider',
//static methods
{
    isSupportedCultures: function (culture) {
        var supportCultures = ["nl-be", "en-ca", "en-in", "en-gb", "en-us", "fr-ca", "fr-fr", "de-de", "it-it", "ja-jp", "es-mx", "es-es", "es-us"];
        return supportCultures.indexOf(culture.toLowerCase()) != -1;
    },
    isMapLoaded: function () {
        return typeof (Microsoft) != 'undefined'
		    && typeof (Microsoft.Maps) != 'undefined'
		    && typeof (Microsoft.Maps.loadModule) != 'undefined';
    },
    getFullAddress: function (resource) {
        var fullname = resource.name;
        if (resource.address.formattedAddress) {
            fullname = resource.address.formattedAddress;
            if (resource.address.countryRegion) {
                fullname += ", " + resource.address.countryRegion;
            }
        }
        else if (resource.address.addressLine)
            fullname = resource.address.addressLine;
        return fullname;
    },
    moduleLoaded: function (self, id, callbackFinish) {
        return function () {
            var args = { 'functionName': 'Ac_Geo.getMapProviderKey',
                'invokeAsCommand': false
            };
            MarkerLocationController.executeNative(args, (jsonStr) => {
				jsonStr = jsonStr.replace("'", "\'");
				var res = JSON.parse(jsonStr).retValue;
				self.map = new Microsoft.Maps.Map(document.getElementById(id),
													{ credentials: res.mapproviderkey,
														mapTypeId: Microsoft.Maps.MapTypeId.aerial,
														zoom: 1,
														fixedMapPosition: false,
														enableSearchLogo: false,
														enableClickableLogo: false
													}
												  );
				if (typeof callbackFinish === 'function') {
					callbackFinish();
				}
			});
        }
    }
},
//instance methods
{

	init: function (id, callbackFinish) {
		this.contextMenu = null;

		this.map = null;
		this.infobox = null;
		this.pinLayer = new Microsoft.Maps.Layer();
		this.markerLayer = new Microsoft.Maps.Layer();
		Microsoft.Maps.loadModule("Microsoft.Maps.Search", { callback: MapServiceProvider.moduleLoaded(this, id, callbackFinish) });
	},
	destroy: function() {
		this.map.dispose();
	},
	removeEntity: function (entity) {
		if (entity != null) {
			this.map.entities.remove(entity);
		}
	},
	addMarker: function (location) {             
		// Add a pin to the center of the map, using a custom icon
		var pin = new Microsoft.Maps.Pushpin(location, {
			icon: "./assets/icon/bluepin.png",
			typeName: "markerType",
			anchor: new Microsoft.Maps.Point(8, 45),
			draggable: true
		});
		this.markerLayer.setZIndex(9999);
		this.markerLayer.add(pin);
		this.map.layers.insert(this.markerLayer);
		return pin;
	},
	addInfoBox: function (infoBox) {
		infoBox.setMap(this.map);
	},
	removeInfoBox: function (infoBox) {
		infoBox.setMap(null);
	},
	addPin: function (pinInfo, style) {
		var icon = style ? "./assets/icon/transparent.png" : "./assets/icon/macro_list.png";

		// Add a pin to the center of the map, using a custom icon
		var pin = new Microsoft.Maps.Pushpin(pinInfo.location, { text: pinInfo.id.toString(),
			width: 28, height: 28,
			icon: "./assets/icon/macro_list.png",
			anchor: new Microsoft.Maps.Point(14, 8),
			draggable: false
		});
		this.pinLayer.add(pin);
		this.pinLayer.setZIndex(1000);
		this.map.layers.insert(this.pinLayer)
		return pin;
	},
		setPinLocation: function (pin, location) {
		pin.setLocation(location);
	},
		getPinLocation: function (pin) {
		return pin.getLocation();
	},
	getHeightPin: function (pin) {
		return 0;
	},
	bingLocationRequest: function (query, numResults, callbackResult, networkerrorCallback) {
		return function (credentials) {

		//we do not need to fall back here if culture is not supported, server handle that for us
		var geocodeRequest = "https://dev.virtualearth.net/REST/v1/Locations?query=" + query + "&output=json&c=" + SELECTED_LANG;
			$.ajax({
				type: 'GET',
					url: geocodeRequest,
				dataType: 'jsonp',
				timeout: TIME_OUT,
					data: {
						key: credentials
			},
					success: function (response) {
						callbackResult(response);
			},
					error: function (jqXHR) {
					var textStatus = jqXHR.statusText;
					if (textStatus == 'timeout' || textStatus == 'error' || textStatus == 'abort' || textStatus == 'parsererror') {
						networkerrorCallback(textStatus);
				}
			},
			jsonp: 'jsonp'
			});
		};
    },
    queryLocation: function (query, numResults, callbackResult, networkerrorCallback) {
        this.map.getCredentials(this.bingLocationRequest(query, numResults, callbackResult, networkerrorCallback));
        /*this.map.getCredentials(function (c) {
            sessionKey = c;
        });
        this.bingLocationRequest(query, numResults, callbackResult, networkerrorCallback)*/
    },
	panMapTo: function (location) {
		this.map.setView({ center: location, zoom: 15 });
    },
    markPinStyle: function (pin, style) {
		var pinLocation = pin.getLocation();
		var pinInfo = { id: pin.getText(), location: pinLocation };
		if (pin)
		{
			pin.setOptions({
				icon: "./assets/icon/transparent.png"
			});
		}
		this.map.entities.remove(pin);
		return this.addPin(pinInfo, style);
    },
	addHandler: function (objOnMap, eventName, callback) {
		Microsoft.Maps.Events.addHandler(objOnMap, eventName, callback);
    },
	removeHandler: function (objOnMap) {
		Microsoft.Maps.Events.removeHandler(objOnMap);
    },
    getMapType: function () {
        return this.map.getMapTypeId();
    },
    getTargetBounds: function () {
        return this.map.getBounds();
    },
    createLocation: function (x, y) {
        return new Microsoft.Maps.Location(x, y);
    },
	createPoint: function (x, y) {
		return new Microsoft.Maps.Point(x, y);
    },
	createInfoBox: function (location, options) {
		return new Microsoft.Maps.Infobox(location, options);
    },
	convertMapTypeToString: function (mapTypeId) {
		var mapType = 'none';
		switch (mapTypeId) {
            case Microsoft.Maps.MapTypeId.aerial:
                mapType = 'aerial';
                break;
            case Microsoft.Maps.MapTypeId.auto:
                mapType = 'auto';
                break;
            case Microsoft.Maps.MapTypeId.birdseye:
                mapType = 'birdseye';
                break;
            case Microsoft.Maps.MapTypeId.collinsBart:
                mapType = 'collinsBart';
                break;
            case Microsoft.Maps.MapTypeId.mercator:
                mapType = 'mercator';
                break;
            case Microsoft.Maps.MapTypeId.ordnanceSurvey:
                mapType = 'ordnanceSurvey';
                break;
            case Microsoft.Maps.MapTypeId.road:
                mapType = 'road';
                break;
        }
        return mapType;
    }
});


