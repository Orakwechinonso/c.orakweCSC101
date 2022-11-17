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

$.Class('util',
//static methods
{
getUrlVars: function (urlPath) {
    var vars = {}, hash;
    var hashes = urlPath.slice(urlPath.indexOf('?') + 1).split('&');
    for (var i = 0; i < hashes.length; i++) {
        hash = hashes[i].split('=');
        vars[hash[0]] = hash[1];
    }

    return vars;
}
},
//instance methods
{
});

(function ($) {
    $.watermark = function(target, label, options){
        var self = this;
        self.labelVisible = true;
        if (target.val() !== "" )
        {
                label.hide();
                self.labelVisible = false;
        }

        target.focus( function(){
            if (self.labelVisible)
                self.setOpacityForLabel(0.5);
        });

        target.blur( function(){
             self.checkIfTargetEmpty();
        });
        target.bind("keydown.watermark", function(e) {
            self.handleKeydownEvent(e);
        });
        target.bind("keyup.watermark", function(e){
            self.checkIfTargetEmpty();
        });
        target.bind("paste", function(e) {
            //paste can never be emptied so we shall always hide watermark
            self.hideWatermark();
        });
        target.bind("cut", function(e){
            /*this event is triggered before the text is changed
            so using setTimeout 9to delay the checking whether textbox state is empty to
            subsequent event loop. This cut event is handled because 'change' event does not work always.
            */
            setTimeout(self.checkIfTargetEmpty, 0);
        });
        target.bind("change", function (e) {
            self.checkIfTargetEmpty();
        });
        target.bind('onpropertychange', function (e) {
            self.checkIfTargetEmpty(e);
        });

        self.setOpacityForLabel = function(opacity) {
            label.stop().animate({opacity: opacity}, 300);
            self.labelVisible = opacity > 0.0;
        };

        self.checkIfTargetEmpty = function(){
          if (target.val() === ""){
                label.css({opacity:0.0}).show();
                self.setOpacityForLabel( 0.5);
          } else{
                self.setOpacityForLabel(0.0);
          }
        };

        self.handleKeydownEvent= function(e){
            if ( e.keyCode === 9 ||  //shift
                 e.keyCode === 16)   //tab
                 return;
            self.hideWatermark();
        };

        self.hideWatermark = function()
        {
            if (self.labelVisible)
            {
                label.hide();
                self.labelVisible = false;
            }
        };
        self.setOpacityForLabel(0.5);
    };

    $.fn.marktext = function (options) {
        return this.each(
			function () {

                var target_id = $(this).attr('target');
                if (!target_id) {
                    return ;
                }
                var label = $(this);
                $(this).siblings().each(function(){
                    if ($(this).attr('id') != target_id)
                        return;
                    return new $.watermark($(this), label, options);
                });
			}
		);
    };
})(jQuery);

/*Global Variables*/
    var globalQueries = util.getUrlVars(window.location.href);
    var SELECTED_LANG = 'en-US';
    var IS_ONLINE_MODE = navigator.onLine;
    var LUPREC = 6;
    var TIME_OUT = 16000;
    var CAN_CANCEL = false;
/****End of global variables***///

//register callback so acgeolocation arx can call this function
if (typeof (registerCallback) === 'function') {
    registerCallback("AcGeoLocation.updateDialogWithoutReload", updateDialogWithoutReload);
	registerCallback("AcGeoLocation.closeDialog", closeDialog);
}

function closeDialog() {
    if(CAN_CANCEL) {
        $('#cancelButton').focus(); //need to change the focus out of lat/lang/elevation text boxes that will trigger validation check
        ButtonsRowController.geoLocationMapDlgInvokeCancel();
    }
}

function getParamForceOffline() {
    var is_forceoffline = 0;
    if (globalQueries.hasOwnProperty('force_offline')) {
        is_forceoffline = parseInt(globalQueries['force_offline']);
    }
    return is_forceoffline;
}

function refreshDialog() {
    //destroy marker
    if ($('#markerInfoAndListCS').controllers().length != 0) {
        $('#markerInfoAndListCS').marker_location('destroy');
    }
    $('#markerInfoAndListCS').html("");

    //destroy map
    if ($('#addressSearchSection').controllers().length != 0) {
        $('#addressSearchSection').search_address_map('destroy');
    }
    $('#addressSearchSection').html("");

    //destroy coord sys
    if ($('#coordSysSection').controllers().length != 0) {
        $('#coordSysSection').coord_sys('destroy');
    }

    //start all over again
    initMap();
}

function updateDialogWithoutReload(newURL) {
    if (!newURL) {
        return;
    }
    newURL = newURL.replace("'", "\'");
    var paramObj = JSON.parse(newURL);
    if (paramObj && paramObj.url && typeof (paramObj.url) === 'string') {
        var is_online_prev = IS_ONLINE_MODE;
        globalQueries = util.getUrlVars(paramObj.url);
        IS_ONLINE_MODE = 1 - getParamForceOffline();
        if (!is_online_prev && IS_ONLINE_MODE) { //last time is offline, now is online, we better refresh all controllers
            refreshDialog();
        } else {
            $('#addressSearchSection').search_address_map("populateCurrentLocation");
        }
    }
}

function setLoadingState(state) {
    if (0 == state) { //waiting
        $('#loading').removeClass('loading-invisible');
        $('#loading').addClass('loading-visible');
    } else {
        $('#loading').removeClass('loading-visible');
        $('#loading').addClass('loading-invisible');
    }
}

function initializeController() {
    setLoadingState(1);
	$('#addressSearchSection').search_address_map();
}

function initializeOfflineMode() {
    IS_ONLINE_MODE = false;
    setLoadingState(1);
    $('#addressSearchSection').search_address_map();
}

function initMap() {

    setLoadingState(0);
    if (IS_ONLINE_MODE) {

        var MAP_CULTURE = SELECTED_LANG;
        //if map does not support the culture, fall back to default en-US
        if (!MapServiceProvider.isSupportedCultures(MAP_CULTURE)) {
            MAP_CULTURE = 'en-US';
        }
        var mapPath = "https://www.bing.com/api/maps/mapcontrol";
        TIME_OUT = 16000;
        if (globalQueries.hasOwnProperty('time_out')) {
            var timeStr = globalQueries['time_out'];
            TIME_OUT = parseInt(timeStr);
        }
        if (TIME_OUT == 0)
            TIME_OUT = 16000;

        var params = { fnIsComponentLoaded: MapServiceProvider.isMapLoaded,
            path: mapPath,
            num_try: TIME_OUT / LoadJSDynamic.TimeOut,
            callback: initializeController,
            failedLoadCallback: initializeOfflineMode
        };
        LoadJSDynamic.LoadJS(params);
    } else {
        initializeOfflineMode();
    }
}

function main() {
    var culture = 'en-US';
    if (globalQueries.hasOwnProperty('culture')) {
        culture = globalQueries['culture'];
    }

    if (globalQueries.hasOwnProperty('luprec')) {
        LUPREC = parseInt(globalQueries['luprec']);
    }

    IS_ONLINE_MODE = 1 - getParamForceOffline();

    SELECTED_LANG = culture;
    var localFolder = "./assets/js/local/" + SELECTED_LANG + "/";
    var cssFile = localFolder + "geo_map.css";
    $('#styleSheetForDialog').attr('href', cssFile);

    // prevent right click context menu from showing unless
    // the element has 'contextmenu' class defined
    $(document).bind("contextmenu", function(e) {
        return $(e.target).hasClass('contextmenu');
    });

    var langFile = localFolder + "strings.js";
    LoadJSDynamic.AppendJSfile(langFile, initMap);
    
    // just a failsafe to make sure we don't get into the situation whereby
    // the dialog can't be cancelled
    setTimeout(function() {
                    CAN_CANCEL = true;
                }, IS_ONLINE_MODE ? 8000 : 1000);
}

//Register event handlers after DOM is loaded.
$(document).ready(function () {
    $('body').keydown(function (event) {
        if (MarkerLocationController.handleKeyDownEvent(event))
            return;

        if (SearchAddressMapController.handleKeyDownEvent(event))
            return;
            
        if (CoordSysController.handleKeyDownEvent(event))
            return;
            
        if (event.keyCode == 27) { // ESC
            closeDialog();
        }
    });

    $('body').mousedown(function(e){
		if(e.button==1) //middle button is disable
		    return false;
	});
});

