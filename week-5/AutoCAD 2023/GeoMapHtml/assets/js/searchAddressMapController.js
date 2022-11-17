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

$.Controller('SearchAddressMapController',
//static methods
    {
        ResultPanelState: { "collapsed": 0, "expanding": 1, "expanded": 2, "collapsing": 3 },

        // This function is meant to set the focus on the search address edit box such
        // that user can directly type in a location to search.
        focusAddress: function () {
            $('#address').focus();
        },
        handleKeyDownEvent: function (event) {
            //check if pop-up menu is visible - if it is visible, then we need to hide that pop-up menu when pressing ESC
            if ($('#addressSearchSection').controllers().length != 0) {
                var searchAddresscontroller = $('#addressSearchSection').controllers()[0];
                if (searchAddresscontroller.isPopUpMenuVisible()) {
                    if (event.keyCode == 27) { // ESC
                        searchAddresscontroller.hidePopUpMenu();
                        return true;
                    }
                }
            }
            return false;
        },
        isCloudSupported: function (callback) {
            var args = { 'functionName': 'Ac_Geo.isCloudSupported',
                         'invokeAsCommand': false,
                         'functionParams': {} };

            execAsync(JSON.stringify(args), (jsonStr) => {
                    jsonStr = jsonStr.replace("'", "\'");
                    var res = JSON.parse(jsonStr).retValue;
                    if (callback) {
                        callback(res.supported);
                    }
                },
                (err) => {
                    setLoadingState(1);
                    alert($.localize("ID_CS_LIBRARY_NOT_AVAILABLE", SELECTED_LANG));
                }
            );
        }
},
//instance methods
    {
    init: function () {
        this.element.html('template_address_search_with_map', {});
        this.mapSection = $('#map_section', this.element);

        if (MapServiceProvider.isMapLoaded() && IS_ONLINE_MODE) {
            this.mapServiceHandler = new MapServiceProvider(this.mapSection.attr("id"), this.initMapEventHandler(this));
        } else {
            //consider as offline mode here
            this.mapServiceHandler = null;
            $('#markerInfoAndListCS').marker_location({ mapServiceHandler: null });
        }
        this.mapPinResult = {};
        this.curPanelResultState = -1;
        this.switchResultPanelState();
		$('#popupmenu').pop_up_menu();
        $('#popupmenu').pop_up_menu().bind('click', this.contextMenuClick(this));
        $('#searchAddress').attr("disabled", true);
        $('#searchAddress').attr("tabIndex", -1);

        $('#address').keyup(function () {
            if ($(this).val() != '') {
                $('#searchAddress').attr("disabled", false);
                $('#searchAddress').removeAttr("tabIndex");
            } else {
                $('#searchAddress').attr("disabled", true);
                $('#searchAddress').attr("tabIndex", -1);
            }
        });

        var self = this;
        $('body').click(function () {
            self.hidePopUpMenu();
        });

        $('body').keyup(function (e) {
            if (e.keyCode === 27) {
                self.hidePopUpMenu();
            }
        });

        $('#addressLabel').html($.localize("ID_ADDRESS_LABEL", SELECTED_LANG));
        $('#useMapsButton').html($.localize("ID_USE_MAPS", SELECTED_LANG));

        if (!IS_ONLINE_MODE) {
            $('#address').addClass("offline");
            $('#address').attr("disabled", true);
            $('#searchAddress').attr("disabled", true);
            $('#map_section').addClass("offline");
            $('#offlineMsg').css("visibility", "visible");

            // disable sign in button if cloud not available
            SearchAddressMapController.isCloudSupported((supported) => {
                if (!supported) {
                    $('#useMapsButton').addClass('displayNone');
                    $('#signInOfflineMsg1').addClass('displayNone');
                    $('#signInOfflineMsg2').addClass('displayNone');
                }
            }); 
        }
    },
    initMapEventHandler:  function (self) {
        return function () {
            self.mapRightClickHandler = self.mapServiceHandler.addHandler(self.mapServiceHandler.map, 'rightclick', self.showPopupMenu(self));
            self.mapViewChangeHandler = self.mapServiceHandler.addHandler(self.mapServiceHandler.map, 'viewchangestart', self.hidePopUpMenu);

            // show the current location(if exist) in the lat/long/elevation widgets
            self.populateCurrentLocation();
        };
    },
    populateCurrentLocation: function () {
        if ($('#markerInfoAndListCS').controllers().length != 0) {
            $('#markerInfoAndListCS').marker_location('destroy');
        }
        $('#markerInfoAndListCS').html(""); //destroy the old

        $('#markerInfoAndListCS').marker_location({ mapServiceHandler: this.mapServiceHandler });

        //populate the search field if there is any long/lat in query
        if (globalQueries.hasOwnProperty('lat') && globalQueries.hasOwnProperty('long')) {
            MarkerLocationController.location.latitude = parseFloat(globalQueries["lat"]);
            MarkerLocationController.location.longitude = parseFloat(globalQueries["long"]);
            $('#address').val(MarkerLocationController.location.latitude + "," + MarkerLocationController.location.longitude).change();
            $('#searchAddress').attr("disabled", false); ;
            this.searchAddress();
        } else {
            $('#address').val('').change();
            this.removeAllResults();
            $('#searchResult').append("<p style='color:grey;'>" + $.localize('ID_NO_SEARCH_RESULT', SELECTED_LANG) + "</p>");
            $('#searchResult').append("<p style='color:grey;'>" + $.localize('ID_MSG_GUIDE_RIGHT_CLICK', SELECTED_LANG) + "</p>");
            //force to collapse search panel
            if (this.curPanelResultState != SearchAddressMapController.ResultPanelState.collapsed &&
                this.curPanelResultState != SearchAddressMapController.ResultPanelState.collapsing) {
                this.toggleSearchResult();
            }
        }

        // Upon launch, focus should be within search. We do this only after bing map is initialized
        // because loading of the bing map appears to grap the focus.
        SearchAddressMapController.focusAddress();
    },
    destroy: function () {
        if (this.mapServiceHandler) {
            this.mapServiceHandler.destroy();
        }
        //destroy pop-up menu
        if ($('#popupmenu').controllers().length != 0) {
            $('#popupmenu').pop_up_menu().unbind();
            $('#popupmenu').pop_up_menu('destroy');
        }
        $('#popupmenu').html("");

        this._super();
    },
    isPopUpMenuVisible: function () {
        if ($('#popupmenu').controllers().length != 0) {
            var popUpController = $('#popupmenu').controllers()[0];
            return popUpController.isVisible();
        } else {
            return false;
        }
    },
    hidePopUpMenu: function () {
        $('#popupmenu').pop_up_menu("hide");
    },

    contextMenuClick: function (self) {
        return function (el) {
            var funcName = el.target.id;
            self.dropMarker();
            self.hidePopUpMenu();
            MarkerLocationController.enableNextButton();
        };
    },

    dropMarker: function () {
        $('#markerInfoAndListCS').marker_location("dropMarker", this.locRightClick);
    },

    showPopupMenu: function (self) {
        return function (e) {

            if (typeof e.target.tryPixelToLocation != 'function') return;

            //show context menu
            var x = e.pageX;
            var y = e.pageY;
            var txtDropOrMove;
            if (MarkerLocationController.location.latitude == null) {
                txtDropOrMove = $.localize("ID_USE_LOCATION", SELECTED_LANG);
            }
            else {
                txtDropOrMove = $.localize("ID_MOVE_MAKER", SELECTED_LANG); ;
            }

     		$('#popupmenu').pop_up_menu("show", x, y, txtDropOrMove);
            //get location of right click
            var point = self.mapServiceHandler.createPoint(e.getX(), e.getY());
            self.locRightClick = e.target.tryPixelToLocation(point);
        }
    },

    removeAllResults: function () {
        $('#searchResult').empty();
    },
    searchNetworkErrorCallback: function (self) {
        return function (textStatus) {
            $('#loadingProgress').hide();
            //force to show panel
            if (self.curPanelResultState == SearchAddressMapController.ResultPanelState.collapsed) {
                self.toggleSearchResult();
            }
            //Show no result
            self.removeAllResults();
            var txt = $.localize("ID_NETWORK_ERROR", SELECTED_LANG);
            $('#resultheader').hide();
            var errorHtml = $('<span>' + txt + '<span>');
            $('#searchResult').append(errorHtml);
        };
    },
    populateSearchResult: function (self) {
        return function (result) {
            $('#loadingProgress').hide();
            //force to show panel
            if (self.curPanelResultState == SearchAddressMapController.ResultPanelState.collapsed) {
                self.toggleSearchResult();
            }

            if (!result || !result.resourceSets || result.resourceSets.length == 0 ||
                    !result.resourceSets[0].resources || result.resourceSets[0].resources.length == 0) {
                //Show no result
                var txt = $.localize("ID_NO_RESULT_FOUND", SELECTED_LANG);
                $('#resultheader').hide();
                var errorHtml = $('<span>' + txt + '<span>');
                $('#searchResult').append(errorHtml);
                return;
            }

            var resources = result.resourceSets[0].resources;
            $('.result_row').remove();
            var j = 0;
            var resultHeaderTxt = $.localize("ID_FOUND_RESULTS", SELECTED_LANG);
            resultHeaderTxt = resultHeaderTxt.replace("%d", resources.length);
            $('#resultheader').text(resultHeaderTxt);
            $('#searchResult').append("<p style='color:grey;'>" + $.localize('ID_MSG_SEARCH_HINT', SELECTED_LANG) + "</p>");

            //for(var v = 0; v < 5 * resources.length; ++v) {
            for (var i = 0; i < resources.length; ++i) {
                //var i = v % resources.length;

                var resource = resources[i];
                if (!resource.name) continue;

                ++j;
                var resultObj = { id: j, name: resource.name, fullname: MapServiceProvider.getFullAddress(resource) };
                var htmlTemplate = $($.View("result_row", resultObj));
                $('#searchResult').append(htmlTemplate);
                htmlTemplate.pin_row({ id: j, resource: resources[i], mapServiceHandler: self.mapServiceHandler });

                if (1 == j) {
                    htmlTemplate.addClass("active");
                    htmlTemplate.find('#useLocation').show();
                    htmlTemplate.pin_row("panToPin");
                }
            }
        }
    },
    switchResultPanelState: function () {
        this.curPanelResultState = (this.curPanelResultState + 1) % 4;
        switch (this.curPanelResultState) {
            case SearchAddressMapController.ResultPanelState.collapsed:
                $('#searchResultContainer').css('left', -Math.abs($('#searchResultContainer').outerWidth()));
                $('#hidePanel').hide();
                $("#expandPanel").show();
                break;
            case SearchAddressMapController.ResultPanelState.collapsing:
                $("#expandPanel").hide();
                $('#hidePanel').show();
                break;
            case SearchAddressMapController.ResultPanelState.expanding:
                $("#expandPanel").hide();
                $('#hidePanel').show();
                break;
            case SearchAddressMapController.ResultPanelState.expanded:
                $('#searchResultContainer').css('left', 0);
                $('#hidePanel').show();

                break;
        }
    },
    toggleSearchResult: function () {
        var $s = $('#searchResultContainer');
        this.switchResultPanelState();
        (function (self) {
            $s.animate(
            //properties animate
                    {
                    left: parseInt($s.css('left'), 10) == 0 ? -$s.outerWidth() : 0
                },
            //options of animate
                    {
                    complete: function () {
                        self.switchResultPanelState();
                        if (parseInt($s.css('left'), 10) != 0) {
                            //self.mapSection.css('left', 0);
                            $('.spanright').css('margin-left', 0);
                        }
                        else {
                            //self.mapSection.css('left', $s.outerWidth());
                            $('.spanright').css('margin-left', $s.outerWidth());
                        }
                    },
                    duration: 700,
                    step: function (now, fx) {
                        var movingLeft = 250 + now;
                        //self.mapSection.css('left', movingLeft);
                        $('.spanright').css('margin-left', movingLeft);
                        $('#hidePanel').css('left', movingLeft);
                    }
                }//end of options of animate
                ) //end animate
        })(this);
    },

    searchAddress: function () {
        var queryStr = $('#address').val();
        var maxResultShow = 20;
        this.removeAllResults();
        $("#searchResult").append('<li class="nav-header" id="resultheader">' + $.localize("ID_SEARCH_INPROGRESS", SELECTED_LANG) + '</li>');
        $("#searchResult").append('<li class="nav-header" style="left:20px;" id="loadingProgress"><img src="./assets/icon/ajax-loader.gif"/></li>');
        this.mapServiceHandler.queryLocation(queryStr, maxResultShow, this.populateSearchResult(this), this.searchNetworkErrorCallback(this));
    },
    '#address keypress': function (el, ev) {
        var $addressTxt = $('#address');
        if ($addressTxt.val()) {
            if (event.which == 13) {
                event.preventDefault();
                this.searchAddress();
            }
        }

    },
    "#searchAddress click": function (el, ev) {
        this.searchAddress();
    },
    '#hidePanel click': function (el, ev) {
        this.toggleSearchResult();
    },
    '#expandPanel click': function (el, ev) {
        if (IS_ONLINE_MODE) {
            this.toggleSearchResult();
        }
    },
    "#panthere click": function (el, ev) {
        $('#markerInfoAndListCS').marker_location('panThere');
    },
    "#TermsLink click": function (el, ev) {
        MarkerLocationController.geoLocationMapDlgInvokeTermsLink();
    },
    "#useMapsButton click": function (el, ev) {
        CAN_CANCEL = false;
        
        var args = { 'functionName': 'Ac_Geo.forceUseMap',
                'invokeAsCommand': false,
                'functionParams': {}
            };

        execAsync(JSON.stringify(args), (jsonStr) => {
                jsonStr = jsonStr.replace("'", "\'");
                var res = JSON.parse(jsonStr).retValue;
                if(res.canUseMap) {
                    IS_ONLINE_MODE = true;
                    globalQueries['force_offline'] = "0";
        
                    refreshDialog();
                }
                else {
                    CAN_CANCEL = true;
                }
                    },
            (err) => {
                setLoadingState(1);
                alert($.localize("ID_CS_LIBRARY_NOT_AVAILABLE", SELECTED_LANG));
            }
        );
    }
});

