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

$.Controller('ButtonsRowController',
    {
        //static methods
        geoLocationMapDlgInvokeOk: function (mapService) {
            var timeZone = parseInt($('#timeZone').val());
            if (isNaN(timeZone) || timeZone == 'undefined') timeZone = 0; //UTC time

            var unit = parseInt($('#listDrawingUnits').val());
            if (isNaN(unit) || unit == 'undefined') unit = 6; //Meter AcDb::kUnitsMeters

            var mapType = 'none';
            if (mapService != null) {
                var mapTypeId = mapService.getMapType();
                mapType = mapService.convertMapTypeToString(mapTypeId);
            }
            var northwest = { "latitude": 77.61770905279676, "longitude": -180 };
            var southeast = { "latitude": -77.61770905279676, "longitude": 180 };
            if (mapService != null) {
                var targetBounds = mapService.getTargetBounds();
                northwest = targetBounds.getNorthwest();
                southeast = targetBounds.getSoutheast();
            }
            var options = {
                'timezone': timeZone,
                'unit': unit,
                'mapType': mapType,
                'northwestLat': northwest.latitude,
                'northwestLong': northwest.longitude,
                'southeastLat': southeast.latitude,
                'southeastLong': southeast.longitude
            };
            var args = JSON.stringify({ 'functionName': 'Ac_Geo.geoLocationMapDlgInvokeOk', 'invokeAsCommand': false, 'functionParams': options });
            execAsync(args, function (result) { }, function (result) { });

            // make sure the next time dialog is invoked, we start at page 1
            $('#buttonsRow').buttons_row('gotoPage1');
        },

        geoLocationMapDlgInvokeCancel: function () {
            var args = JSON.stringify({ 'functionName': 'Ac_Geo.geoLocationMapDlgInvokeCancel', 'invokeAsCommand': false });
            execAsync(args, function (result) { }, function (result) { });

            // make sure the next time dialog is invoked, we start at page 1
            $('#buttonsRow').buttons_row('gotoPage1');
        },

        geoLocationMapDlgInvokeHelp: function () {
            var args = JSON.stringify({ 'functionName': 'Ac_Geo.geoLocationMapDlgInvokeHelp', 'invokeAsCommand': false });
            execAsync(args, function (result) { }, function (result) { });
        },

        setDialogTitle: function (dialogTitleId) {
            var dialogTitle = $.localize(dialogTitleId, SELECTED_LANG)

            var options = {
                'dialogTitle': dialogTitle,
            };

            var args = JSON.stringify({ 'functionName': 'Ac_Geo.setDialogTitle', 'invokeAsCommand': false, 'functionParams': options });
            execAsync(args, function (result) { }, function (result) { });
        }
    },
    //instance methods
    {
        init: function (element, options) {
            this.mapService = options.mapService;
            this.currentPage = 1; // page 1 is bing map, page 2 is coordinate systems

            this.element.html('buttons_row_template', {});
            this.setNextButtonEnabled(false);
            this.setBackButtonEnabled(false);

            $('#backButton').html($.localize("ID_BACK", SELECTED_LANG));
            $('#nextButton').html($.localize("ID_NEXT", SELECTED_LANG));
            $('#helpButton').html($.localize("ID_HELP", SELECTED_LANG));
            gButtonsRowController = this;

            ButtonsRowController.setDialogTitle("ID_DIALOG_TITLE_PAGE_1");
        },

        setNextButtonEnabled: function (enabled) {
            $("#nextButton").attr("disabled", !enabled);
        },

        setBackButtonEnabled: function (enabled) {
            $backButton = $('#backButton');

            if (enabled)
                $backButton.removeClass('displayNone');
            else
                $backButton.addClass('displayNone');
        },

        gotoPage1: function () {
            this.currentPage = 1;

            $('#addressSearchSection').removeClass('displayNone');
            $('#markerInfoAndListCS').removeClass('displayNone');

            $('#coordSysSection').addClass('displayNone');

            this.setBackButtonEnabled(false);
            this.setNextButtonEnabled(true);

            ButtonsRowController.setDialogTitle("ID_DIALOG_TITLE_PAGE_1");
        },

        gotoPage2: function () {
            this.currentPage = 2;

            $('#addressSearchSection').addClass('displayNone');
            $('#markerInfoAndListCS').addClass('displayNone');

            $('#coordSysSection').coord_sys('destroy');
            $('#coordSysSection').coord_sys();

            $('#coordSysSection').removeClass('displayNone');

            this.setBackButtonEnabled(true);

            ButtonsRowController.setDialogTitle("ID_DIALOG_TITLE_PAGE_2");
        },

        ".rowButtons contextmenu": function (el, ev) {
            return false;
        },

        "#backButton click": function (el, ev) {
            this.gotoPage1();
        },

        "#nextButton click": function (el, ev) {
            // it is possible to receive click event even when the button is disabled
            // when click on child element (span)
            if($(el).attr('disabled'))
                return;

            if (this.currentPage == 1) {
                this.gotoPage2();
            }
            else if (this.currentPage == 2) {
                // exiting wizard, save the dialog data
                ButtonsRowController.geoLocationMapDlgInvokeOk(this.mapService);
            }
        },

        "#cancelButton click": function (el, ev) {
            ButtonsRowController.geoLocationMapDlgInvokeCancel();
        },

        "#helpButton click": function (el, ev) {
            ButtonsRowController.geoLocationMapDlgInvokeHelp();
        }
    }
);
