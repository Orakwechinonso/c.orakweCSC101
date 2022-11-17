//
///////////////////////////////////////////////////////////////////////////////
//
//                 (C) Copyright 2013 by Autodesk, Inc.
//
// The information contained herein is confidential, proprietary to Autodesk,
// Inc., and considered a trade secret as defined in section 499C of the
// penal code of the State of California.  Use of this information by anyone
// other than authorized employees of Autodesk, Inc. is granted only under a
// written non-disclosure agreement, expressly prescribing the scope and
// manner of such use.
//
///////////////////////////////////////////////////////////////////////////////

$.Controller('PinRowController',
//static methods
    {
},
//instance methods
    {
    init: function (element, options) {
        this.id = options.id;
        this.resource = options.resource;
        this.mapServiceHandler = options.mapServiceHandler;
        this.pinInfobox = null;

        // Set the map view using the returned bounding box

        //Create pin in the map
        var pinLocation = this.mapServiceHandler.createLocation(this.resource.point.coordinates[0], this.resource.point.coordinates[1]);
        var pinInfo = { id: this.id, location: pinLocation };
        this.pin = this.mapServiceHandler.addPin(pinInfo);
        //Add hover event for pin
        this.addHoverEventForPin(this.pin);
    },
    destroy: function () {
        this.removeHoverEventForPin();
        this.mapServiceHandler.removeEntity(this.pinInfo);
        this.mapServiceHandler.removeEntity(this.pin);
        this.pin.setOptions({
            icon: "./assets/icon/transparent.png"
        });
    },

    panToPin: function () {
        this.mapServiceHandler.panMapTo(this.mapServiceHandler.getPinLocation(this.pin));
    },

    removeHoverEventForPin: function () {
        this.mapServiceHandler.removeHandler(this.pinMouseOverHandler);
        this.mapServiceHandler.removeHandler(this.pinMouseOutHandler);
    },

    addHoverEventForPin: function () {
        // Add handler for the pushpin click event.
        this.pinMouseOverHandler = this.mapServiceHandler.addHandler(this.pin, 'mouseover', this.showInfobox(this));
        this.pinMouseOutHandler = this.mapServiceHandler.addHandler(this.pin, 'mouseout', this.hideInfobox(this));
    },

    showInfobox: function (self) {
        return function (e) {

            /*Make result row as hover*/
            var icon = self.find("#icon" + self.id);
            if (icon) {
                icon.removeClass("bullet_result");
                icon.addClass("bullet_result_hover");
            }

            /*Show info box*/
            var pin = e.target;
            if (pin == null) return;
            // Create the info box for the pushpin
            var location = self.mapServiceHandler.getPinLocation(pin);
            var options = {
                id: 'infoBox' + self.id,
                title: self.resource.name,
                description: MapServiceProvider.getFullAddress(self.resource),
                height: 100,
                width: 200,
                visible: true,
                showPointer: true,
                showCloseButton: true,
                // offset the infobox enough to keep it from overlapping the pin.
                offset: self.mapServiceHandler.createPoint(0, 38),
                zIndex: 999
            };
            if (this.pinInfobox != null) {
                self.mapServiceHandler.removeEntity(self.pinInfobox);
                self.pinInfobox = null;
            }
            // create the infobox
            self.pinInfobox = self.mapServiceHandler.createInfoBox(location, options);

            // add it to the map.
            self.mapServiceHandler.addInfoBox(self.pinInfobox);
        }
    },
    hideInfobox: function (self) {
        return function (e) {

            /*Make result row not hover*/
            var icon = self.find("#icon" + self.id);
            if (icon) {
                icon.removeClass("bullet_result_hover");
                icon.addClass("bullet_result");
            }

            /*trigger icon  mouseout on result icon*/
            self.find(".left_col_result_icon").trigger("mouseout");

            if (self.pinInfobox != null) {
                self.mapServiceHandler.removeInfoBox(self.pinInfobox);
                self.pinInfobox = null;
            }
        };
    },
    isCurrentRowActive: function () {
        return this.element.hasClass('active');
    },
    makeCurrentRowActive: function () {
        this.element.addClass("active");
    },
    makeAllRowInActive: function () {
        $(".result_row").removeClass("active");
    },
    hideAllUseLocationButton: function () {
        $(".result_row").find('#useLocation').hide();
    },
    showUseLocationButton: function () {
        this.element.find('#useLocation').show();
    },
    hideUseLocationButton: function () {
        this.element.find('#useLocation').hide();
    },
    showUseLocationOnActiveRow: function () {
        $(".result_row.active").find('#useLocation').show();
    },
    'click': function (el, ev) {
        this.makeAllRowInActive();
        this.hideAllUseLocationButton();
        this.showUseLocationButton();
        this.makeCurrentRowActive();
        this.mapServiceHandler.panMapTo(this.mapServiceHandler.getPinLocation(this.pin));
    },
    '.left_col_result_icon mouseover': function (el, ev) {
        this.removeHoverEventForPin();
        this.pin = this.mapServiceHandler.markPinStyle(this.pin, "bullet_result_hover");
        this.addHoverEventForPin();
    },

    '.left_col_result_icon mouseout': function (el, ev) {
        this.removeHoverEventForPin();
        this.pin = this.mapServiceHandler.markPinStyle(this.pin, "bullet_result");
        this.addHoverEventForPin();
    },
    'mouseover': function (el, ev) {
        this.hideAllUseLocationButton();
        this.showUseLocationButton();
    },
    'mouseout': function (el, ev) {
        if (!this.isCurrentRowActive()) {
            this.hideUseLocationButton();
        }
        this.showUseLocationOnActiveRow();
    },
    '#useLocation click': function (el, ev) {
        var pinLocation = this.mapServiceHandler.getPinLocation(this.pin);
        $('#markerInfoAndListCS').marker_location("dropMarker", pinLocation);
        MarkerLocationController.enableNextButton();
        CoordSysController.suggestTimeZone(pinLocation.longitude);
    }

});


