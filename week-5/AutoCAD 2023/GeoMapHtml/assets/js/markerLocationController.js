//////////////////////////////////////////////////////////////////////////////
//
//  Copyright 2013 by Autodesk, Inc.  All rights reserved.
//
// This computer source code and related instructions and comments
// are the unpublished confidential and proprietary information of
// Autodesk, Inc. and are protected under applicable copyright and
// trade secret law.  They may not be disclosed to, copied or used
// by any third party without the prior written consent of Autodesk, Inc.
//
//////////////////////////////////////////////////////////////////////////////

$.Controller('MarkerLocationController',
    {
        InputState: { ReadOnlyState: 0, EdittingState: 1 },
        location: { longitude: null, latitude: null, elevation: 0.0 },
        existingCurrentCSId: null,
        currentCSId: null,
        //static methods
        executeNativeSync: function (param) {
            alert("SYNC CALLS NOT SUPPORTED");
            return exec(JSON.stringify(param));
        },
        executeNative: function executeNative(param, callback) {
            execAsync(JSON.stringify(param),
            function (result) {
                if (callback) {
                    callback(result);
                }
            },
            function (result) {
                setLoadingState(1);
                alert($.localize("ID_CS_LIBRARY_NOT_AVAILABLE", SELECTED_LANG));
            });
        },            
        enableNextButton: function () {
            var s = $('input.invalid');
              if (s.length > 0)
                $("#buttonsRow").data("controllers")['buttons_row'].setNextButtonEnabled(false);
            else
                $("#buttonsRow").data("controllers")['buttons_row'].setNextButtonEnabled(true);
        },
        disableNextButton: function () {
            $("#buttonsRow").data("controllers")['buttons_row'].setNextButtonEnabled(false);
        },
        disablePanthere: function (disable) {
            if (IS_ONLINE_MODE) {
                $('#panthere').attr("disabled", disable);
                if(disable)
                    $('#panthere').attr("tabIndex", -1);
                else
                    $('#panthere').removeAttr("tabIndex");
            } else {
                //offline mode - force to disable
                $('#panthere').attr("disabled", true);
                $('#panthere').attr("tabIndex", -1);
            }
        },
        getAngleFormat: function (angle, callback) {
            if (typeof angle === 'undefined' || angle == null || angle.toString().trim().length == 0) return angle;
            MarkerLocationController.validateAngleFormat(angle, -360, 360, (res) => {
                if (res.isValid)
                    callback(res.degreevaluegeolatlongformat.toString());
                else 
                    callback(angle.toString());
            });
        },
        getFloatFormat: function (floatStr, callback) {
            if (typeof angle === 'undefined' || angle == null || angle.toString().trim().length == 0) return angle;
            MarkerLocationController.validateAngleFormat(angle, -360, 360, (res) => {
                if (res.isValid)
                    callback(res.floatvalue.toFixed(LUPREC));
                else 
                    callback(floatStr);
            });
        },
        validateAngleFormat: function (angle, minimumValueInDegree, maximumValueInDegree, callback) {

            if (typeof angle === 'undefined' || angle == null || angle.length == 0 || angle == $.localize("ID_NOT_DEFINED", SELECTED_LANG))
                return { isValid: false };

            var args = { 'functionName': 'Ac_Geo.convertAngleStringToFloat',
                'invokeAsCommand': false,
                'functionParams': {
                    'angle': angle.toString(),
                    'maxmiumInDegree': maximumValueInDegree,
                    'minimumInDegree': minimumValueInDegree
                }
            };
            var jsonStr = MarkerLocationController.executeNative(args, (jsonStr) => {
                jsonStr = jsonStr.replace("'", "\'");
                var res = JSON.parse(jsonStr).retValue;
    
                var funcRes = {
                    isValid: res.isvalid,
                    degreevalue: res.degreevalue,
                    degreevaluegeolatlongformat: res.degreevaluegeolatlongformat
                };
                callback(funcRes);
            });
        },
        validateFloatFormat: function(floatStr, callback) {
            if (typeof floatStr === 'undefined' || floatStr == null || floatStr.length == 0 || floatStr == $.localize("ID_NOT_DEFINED", SELECTED_LANG))
                return { isValid: false };

            var args = { 'functionName': 'Ac_Geo.convertFloatStringToFloat',
                'invokeAsCommand': false,
                'functionParams': {
                    'floatstr': floatStr.toString(),
                }
            };
            var jsonStr = MarkerLocationController.executeNative(args, (jsonStr) => {
                jsonStr = jsonStr.replace("'", "\'");
                var res = JSON.parse(jsonStr).retValue;
    
                var funcRes = {
                    isValid: res.isvalid,
                    floatvalue: res.floatvalue,
                };
                callback(funcRes);
            });
        },
        geoLocationMapDlgInvokeTermsLink: function () {
            var args = JSON.stringify({ 'functionName': 'Ac_Geo.geoLocationMapDlgInvokeTermsLink', 'invokeAsCommand': false });
            execAsync(args, function (result) { }, function (result) { });
        },
        hideTermsHyperlinks: function () {
            $("#termsContainer").hide();
        },

        // Meant for handling keydown input events. Returns true if event was handled
        handleKeyDownEvent: function(event) {
            // When ESC within lat/long/elevation edit boxes, do not close the dialog!
            // When ENTER within lat/long/elevation edit boxes, we commit the values.
            if ($("#lat").is(":focus") ||
                $("#long").is(":focus") ||
                $("#elevation").is(":focus")) {
                if (event.keyCode == 13) { // ENTER
                    $('#markerInfoAndListCS').marker_location('validateAcceptInputs');
                    return true;
                }
            }
            return false;
        }
    },
//instance methods
    {
    init: function (element, options) {
        this.mapServiceHandler = options.mapServiceHandler;
        this.markerPin = null;
        this.locationChanged = false;

        this.element.html('marker_info_template', {});
        this.setInputState(MarkerLocationController.InputState.ReadOnlyState);

        $('label').marktext();
        $('#elevation').val((0.0).toFixed(LUPREC)).change();
        $('#latLabel').html($.localize("ID_LATITUDE", SELECTED_LANG));
        $('#longLabel').html($.localize("ID_LONGITUDE", SELECTED_LANG));
        $('#elevationLabel').html($.localize("ID_ELEVATION", SELECTED_LANG));
        $('#metersLabel').html($.localize("ID_METERS", SELECTED_LANG));
        $('#termsContainer').html($.localize("ID_TERMS", SELECTED_LANG));

        MarkerLocationController.disablePanthere(true);

        $('#buttonsRow').buttons_row({ mapService: this.mapServiceHandler });

        MarkerLocationController.location = { longitude: null, latitude: null, elevation: 0.0 };
        //populate existing coordinate system and long/lat values
        if (globalQueries.hasOwnProperty('lat') && globalQueries.hasOwnProperty('long')) {
            MarkerLocationController.location.latitude = parseFloat(globalQueries["lat"]);
            MarkerLocationController.location.longitude = parseFloat(globalQueries["long"]);
        }
        if (globalQueries.hasOwnProperty('elevation')) {
            MarkerLocationController.location.elevation = parseFloat(globalQueries["elevation"]);
            $('#elevation').val(MarkerLocationController.location.elevation).change();
        }
        MarkerLocationController.existingCurrentCSId = null;
        if (globalQueries.hasOwnProperty('curCSId')) {
            MarkerLocationController.existingCurrentCSId = globalQueries['curCSId'];
        }
        if (MarkerLocationController.location.latitude != null) {
            this.dropMarker(MarkerLocationController.location);
            this.locationChanged = false; // dropMarker will set it to true
        }

        // Enable "Continue..." button if data is populated from KMLKMZ file
        if (globalQueries.hasOwnProperty('fromKML')) {
            if (globalQueries['fromKML'] > 0)
                MarkerLocationController.enableNextButton();
        }

        if (!IS_ONLINE_MODE) {
            this.setInputState(MarkerLocationController.InputState.EdittingState);
            MarkerLocationController.hideTermsHyperlinks();
        }
        else {
            // we don't want to show the access key for lat/long labels if online,
            // since the corresponding edit boxes are disabled.
            $('#latLabel span').removeClass('accesskey');
            $('#longLabel span').removeClass('accesskey');
        }
        
        CAN_CANCEL = true;
    },
    destroy: function () {
        if (this.mapServiceHandler) {
            if (this.markerPinDragEndHandler) {
                this.mapServiceHandler.removeHandler(this.markerPinDragEndHandler);
            }
            if (this.markerPin) {
                this.mapServiceHandler.removeEntity(this.markerPin);
                this.markerPin.setOptions({
                    icon: "./assets/icon/transparent.png"
                });
            }
        }
        //destroy button row
        if ($('#buttonsRow').controllers().length != 0) {
            $('#buttonsRow').buttons_row('destroy');
        }
        $('#buttonsRow').html("");
        this._super();
    },
    panThere: function () {
        try {
            if (this.mapServiceHandler) {
                this.mapServiceHandler.panMapTo(this.mapServiceHandler.getPinLocation(this.markerPin));
            }
        }catch(err) {}
    },
    dropMarkerWithoutGetCoordinateSystem: function (location) {
        if (!location) return;
        MarkerLocationController.location = location;

        if (this.mapServiceHandler) {
            if (!this.markerPin) {
                this.markerPin = this.mapServiceHandler.addMarker(location);
                this.markerPinDragEndHandler = this.mapServiceHandler.addHandler(this.markerPin, "dragend", this.updateLocationAfterDrag(this));
            } else {
                this.mapServiceHandler.setPinLocation(this.markerPin, location)
            }
        }
    },
    dropMarker: function (location, fromLatLongElevInputs) {
        if (!location) return;
        if (!this.markerPin) {
            this.setLocation(location, fromLatLongElevInputs)
            if (this.mapServiceHandler) {
                this.markerPin = this.mapServiceHandler.addMarker(location);
                this.markerPinDragEndHandler = this.mapServiceHandler.addHandler(this.markerPin, "dragend", this.updateLocationAfterDrag(this));
            }
        } else {
            this.mapServiceHandler.setPinLocation(this.markerPin, location)
            this.setLocation(location, fromLatLongElevInputs);
        }
        
        this.locationChanged = true;
    },
    updateLocationAfterDrag: function (self) {
        return function (event) {
            if (event) {
                try {                
                    var pinLoc = self.mapServiceHandler.getPinLocation(self.markerPin);
                    self.setLocation(pinLoc);
                    MarkerLocationController.enableNextButton();
                }catch(err) {}
            }
        }
    },
    setLocation: function (location, fromLatLongElevInputs) {       
        var self = this;
        setTimeout(function () {

            MarkerLocationController.location.longitude = location.longitude;
            MarkerLocationController.location.latitude = location.latitude;

            if(!fromLatLongElevInputs) {
                MarkerLocationController.getAngleFormat(location.latitude, (format) => {
                    $('#lat').val(format).change();
                });
                MarkerLocationController.getAngleFormat(location.longitude, (format) => {
                    $('#long').val(format).change();
                });
            }

            if (fromLatLongElevInputs && !(typeof location.elevation === 'undefined') && location.elevation != null && !isNaN(location.elevation)) {            
                MarkerLocationController.location.elevation = location.elevation;
            }            

            MarkerLocationController.disablePanthere(false);
            MarkerLocationController.enableNextButton();
            $('#elevation').attr("disabled", false);            

        }, 200);
    },
    setInputState: function (state) {
        if (state == MarkerLocationController.InputState.ReadOnlyState) {
            $('#lat').attr("disabled", true);
            $('#long').attr("disabled", true);
            $('#elevation').attr("disabled", true);

            // in online mode, the lat/long/elevation are supposed to be always non-editable
            // and hence supposed to look like labels.
            $('#latValue').addClass('latLongElevDisabled');
            $('#longValue').addClass('latLongElevDisabled');

        } else if (state == MarkerLocationController.InputState.EdittingState) {
            $('#lat').attr("disabled", false);
            $('#lat').focus();
            $('#long').attr("disabled", false);
            $('#elevation').attr("disabled", false);

            $('#latValue').removeClass('latLongElevDisabled');
            $('#longValue').removeClass('latLongElevDisabled');

            //save button is disabled too
            MarkerLocationController.disableNextButton();
        }
        $('#lat').removeClass("invalid");
        $('#long').removeClass("invalid");
        $('#elevation').removeClass("invalid");

    },
    '#lat keypress': function (el, ev) {

    },
    '#long keypress': function (el, ev) {

    },

    validateLat: function (callback) {
        var latStr = $('#lat').val();
        
        // validate only if string contains non-whitespace
        if(/\S/.test(latStr))
        {
            MarkerLocationController.validateAngleFormat(latStr, -90.0, 90.0, (resLat) => {
                if (resLat.isValid) {
                    $('#lat').removeClass('invalid');
                    if (!(typeof resLat.degreevaluegeolatlongformat === 'undefined')) {
                        $('#lat').val(resLat.degreevaluegeolatlongformat.toString());
                    }
                }
                else {
                    $('#lat').addClass('invalid');
                }
                callback(resLat);
            });
        }
        else {
        	callback({'isValid':0,'degreevalue':0});
        }
    },
    validateLong: function (callback) {
        var longStr = $('#long').val();
        
        // validate only if string contains non-whitespace
        if(/\S/.test(longStr))
        {
            MarkerLocationController.validateAngleFormat(longStr, -180.0, 180.0, (resLong) => {
                if (resLong.isValid) {
                    $('#long').removeClass('invalid');
                    if (!(typeof resLong.degreevaluegeolatlongformat === 'undefined')) {
                        $('#long').val(resLong.degreevaluegeolatlongformat.toString());
                    }
                }
                else {
                    $('#long').addClass('invalid');
                }
                callback(resLong);
            });
        }
		else {
        	callback({'isValid':0,'degreevalue':0});
		}
    },
    validateElevation: function (callback) {
        var elevStr = $('#elevation').val();
        
        // validate only if string contains non-whitespace
        if(/\S/.test(elevStr))
        {
            MarkerLocationController.validateFloatFormat(elevStr, (resElevation) => {

                if (resElevation.isValid){
                    $('#elevation').removeClass('invalid');
                    if (!(typeof resElevation.floatvalue === 'undefined')) {                
                       $('#elevation').val(resElevation.floatvalue.toFixed(LUPREC));
                    }
               } else{
                    $('#elevation').addClass('invalid');
               }
               callback(resElevation);
   
            });
        }
        else {
            // automatically change empty to zero
            $('#elevation').val((0.0).toFixed(LUPREC)).change();
            callback({'isValid':0,'degreevalue':0});
        }
    },
    validateInputs: function (callback) {
        this.validateLat((resLat) => {
            this.validateLong((resLong) => {
                this.validateElevation((resElevation) => {
                    callback({
                        isValid: resLat.isValid && resLong.isValid && resElevation.isValid,
                        lat: resLat.degreevalue,
                        long: resLong.degreevalue,
                        elevation: resElevation.floatvalue
                    })
                });  
            });
        });
    },
    validateAcceptInputs: function () {
        this.validateInputs((res) => {
            if (res.isValid) {
                var longitude = res.long;
                var latitude = res.lat;
                var elevation = res.elevation;
                var location = { longitude: longitude, latitude: latitude, elevation: elevation };
                this.dropMarker(location, true);
    
                MarkerLocationController.enableNextButton();
            } else {
                MarkerLocationController.disableNextButton();
            }
        });
    },
    '#lat change': function (el, ev) {
        this.validateAcceptInputs();
    },
    '#long change': function (el, ev) {
        this.validateAcceptInputs();
    },
    '#elevation change': function (el, ev) {
        this.validateAcceptInputs();
    } 
});


