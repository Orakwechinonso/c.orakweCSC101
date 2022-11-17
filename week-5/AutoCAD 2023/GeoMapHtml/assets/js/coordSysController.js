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

$.Controller('CoordSysController',    // coord_sys
    {
        InputState: { ReadOnlyState: 0, EdittingState: 1 },
        location: { longitude: null, latitude: null, elevation: 0.0 },
        existingCurrentCSId: null,
        currentCSId: null,
        $originalTableRows: null,
        tableRowsSortFunc: null,
        timeZoneSuggest: false,
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
        // Meant for handling keydown input events. Returns true if event was handled
        handleKeyDownEvent: function(event) {
            if (event.keyCode == 38) { // ARROW UP
                if(document.activeElement == document.body || document.activeElement == $('#csSearch').get(0)) {
                    CoordSysController.handleCsTableArrowUp();
                    event.preventDefault();
                }
                return true;
            } else if (event.keyCode == 40) { // ARROW DOWN
                if(document.activeElement == document.body || document.activeElement == $('#csSearch').get(0)) {
                    CoordSysController.handleCsTableArrowDown();
                    event.preventDefault();
                }
                return true;
            }

            return false;
        },
        handleCsTableArrowUp: function() {
            var $selectedRow = $('#csTableDiv tbody tr.tableRowSelected');
            var $prevRow = $selectedRow.prev();
            if($prevRow.length > 0) {
                // deselect current row and select previous row
                $selectedRow.removeClass('tableRowSelected');
                $prevRow.addClass('tableRowSelected');
                
                // display newly selected value
                var csNameSelected = $prevRow.find('td:first').text();                
                $('#coordSysSection').coord_sys('setCurrentCoordinateSystem', csNameSelected);
            
                // if previous row is out of table display, scroll it into view!
                var $csTableDiv = $('#csTableDiv');
                var scrollTop = $csTableDiv.scrollTop()

                var relativeRowOffset = $prevRow.offset().top - $csTableDiv.offset().top;
                var rowHeight = $prevRow.height();

                if(relativeRowOffset < 0) {
                    $csTableDiv.scrollTop(scrollTop - rowHeight * 2.0);
                }
            }
        },
        handleCsTableArrowDown: function() {
            var $selectedRow = $('#csTableDiv tbody tr.tableRowSelected');
            var $nextRow = $selectedRow.next();
            if($nextRow.length > 0) {
                // deselect current row and select next row
                $selectedRow.removeClass('tableRowSelected');
                $nextRow.addClass('tableRowSelected');
                
                // display newly selected value
                var csNameSelected = $nextRow.find('td:first').text();
                $('#coordSysSection').coord_sys('setCurrentCoordinateSystem', csNameSelected);  

                // if next row is out of table display, scroll it into view!
                var $csTableDiv = $('#csTableDiv');
                var scrollTop = $csTableDiv.scrollTop()

                var relativeRowOffset = $nextRow.offset().top - $csTableDiv.offset().top;
                var rowHeight = $nextRow.height();

                if(relativeRowOffset + rowHeight > $csTableDiv.height()) {
                    $csTableDiv.scrollTop(scrollTop + rowHeight * 2.0);
                }
            }
        },
        getCoordinateSystem: function (lat, long, callback) {
            var args = { 'functionName': 'Ac_Geo.filterCoordinateSystem', 'invokeAsCommand': false, 'functionParams': { 'longitude': long, 'latitude': lat} };
            CoordSysController.executeNative(args, (jsonStr) => {
				jsonStr = jsonStr.replace("'", "\'");
                var res = JSON.parse(jsonStr).retValue;
                callback(res);
            });
        },
        updateLocationandCoordinateSystem: function () {
            // inform native side of the coordinate system change
            var latitude = CoordSysController.location.latitude == null ? 0 : CoordSysController.location.latitude;
            var longitude = CoordSysController.location.longitude == null ? 0 : CoordSysController.location.longitude;
            var elevation = parseFloat($('#elevation').val());
            if (isNaN(elevation))
                elevation = 0.0;

            var args = { 'functionName': 'Ac_Geo.updateLocationAndCoordinateSystem',
                'invokeAsCommand': false,
                'functionParams': {
                    'latitude': latitude,
                    'longitude': longitude,
                    'elevation': elevation,
                    'coordinateSystemId': CoordSysController.currentCSId
                }
            };
            CoordSysController.executeNative(args);
        },
        populateCoordinateSystem: function (res) {
            var tableRows = "";

            if (res && res.coords_system) {
                for (var i = 0; i < res.coords_system.length; ++i) {
                    var coord = res.coords_system[i];
                    if (coord.id && coord.unit && coord.referenceto) {
                        var id = coord.id;
                        var epsg = coord.epsg;

                        // show epsg as dash if it's value is zero'
                        if(epsg == 0)
                            epsg = '-';

                        var selectedClass = "";

                        // if drawing already has a coordinate system, we show it as selected
                        if (CoordSysController.existingCurrentCSId != null && CoordSysController.existingCurrentCSId == coord.id)
                        {
                            selectedClass = 'tableRowSelected';
                        }

                        // append row to table
                        var eachTableRow = '<tr class="' + selectedClass + '"><td class="csColumnName">' + id + '</td><td class="csColumnReference">' + coord.referenceto +
                                           '</td><td class="csColumnUnit">' + coord.unit + '</td><td class="csColumnEPSG">' + epsg + '</td></tr>';
                        tableRows += eachTableRow;
                    }
                }
            }

            $('#csTableDiv tbody').html(tableRows);

            CoordSysController.$originalTableRows = $(tableRows);

            CoordSysController.tableRowsSortFunc = null; // default no sorting

            if (CoordSysController.existingCurrentCSId == null) {
                CoordSysController.disableSaveButton();
            }
            else {
                CoordSysController.updateLocationandCoordinateSystem();
                CoordSysController.enableTimeZoneSel();
                CoordSysController.enableUnitsSel();
                CoordSysController.enableNextButton();
            }

            // compute the time zone if location was explicitly changed
            var markerLocController = $('#markerInfoAndListCS').controllers(MarkerLocationController)[0];
            if(markerLocController.locationChanged) {
                CoordSysController.suggestTimeZone(CoordSysController.location.longitude);
            }

            setLoadingState(1);

            $('#csSearch').focus();
        },
        populateTimezones: function (res) {
            if (res && res.alltimezones) {
                $('#timeZone').empty();
                for (var i = 0; i < res.alltimezones.length; ++i) {
                    var opt = $('<option>' + res.alltimezones[i].desc + '</option>');
                    opt.attr('value', Math.floor(res.alltimezones[i].timezone));
                    opt.attr('offset', Math.floor(res.alltimezones[i].offset));
                    $('#timeZone').append(opt);
                }

                if (globalQueries.hasOwnProperty('timezone')) {
                    var timeZone = globalQueries['timezone'].toString();
                    $('#timeZone').val(timeZone);
                }
            }
        },
        getTimeZones: function () {
            var args = { 'functionName': 'Ac_Geo.getTimeZones', 'invokeAsCommand': false, 'functionParams': {} };
            CoordSysController.executeNative(args, (jsonStr) => {
				jsonStr = jsonStr.replace("'", "\'");
                var res = JSON.parse(jsonStr).retValue;
                CoordSysController.populateTimezones(res);
            });
        },

        populateUnits: function (res) {
            if (res && res.units) {
                $('#listDrawingUnits').empty();
                for (var i = 0; i < res.units.length; ++i) {
                    var opt = $('<option>' + res.units[i].unitdesc + '</option>');
                    opt.attr('value', res.units[i].unitvalue.toString());
                    $('#listDrawingUnits').append(opt);
                }
                if (res.defaultunit)
                    $('#listDrawingUnits').val(res.defaultunit.toString());

                $('#listDrawingUnits').change(function () {
                    // Call native to check whether unit is mismatch with INSUNIT and prompt alert if yes
                    var unit = $('#listDrawingUnits').val();
                    var args = { 'functionName': 'Ac_Geo.listDrawingUnitsOnChanged', 'invokeAsCommand': false, 'functionParams': { 'unit': unit } };
                    CoordSysController.executeNative(args, (jsonStr) => {
                        jsonStr = jsonStr.replace("'", "\'");
                        var res = JSON.parse(jsonStr).retValue;
                        if (res.unit > 0)
                            $('#listDrawingUnits').val(res.unit);
                    });
                });
            }
        },
        getUnits: function () {
            var args = { 'functionName': 'Ac_Geo.getUnits', 'invokeAsCommand': false, 'functionParams': {} };
            CoordSysController.executeNative(args, (jsonStr) => {
				jsonStr = jsonStr.replace("'", "\'");
                var res = JSON.parse(jsonStr).retValue;
                CoordSysController.populateUnits(res);
            });
        },
        enableNextButton: function () {
            var s = $('input.invalid');
            var isEdittingState = $('#btneditok').is(":visible");
            if (s.length > 0 || isEdittingState)
                $("#buttonsRow").data("controllers")['buttons_row'].setNextButtonEnabled(false);
            else
                $("#buttonsRow").data("controllers")['buttons_row'].setNextButtonEnabled(true);
        },
        disableSaveButton: function () {
            $("#buttonsRow").data("controllers")['buttons_row'].setNextButtonEnabled(false);
        },
        enableTimeZoneSel: function () {
            $('#timeZone').attr("disabled", false);
        },
        enableUnitsSel: function () {
            $('#listDrawingUnits').attr("disabled", false);
        },
        suggestTimeZone: function (longitude) {
            var offset = Math.floor(CoordSysController.getTimeZone(longitude));
            $('#timeZone').find('option[offset="' + offset + '"]:first').attr("selected", true);
            CoordSysController.enableTimeZoneSel();
        },
        getTimeZone: function (longitude) {
            var tz;
            if (longitude <= 0) {
                tz = -Math.floor((-longitude + 7.5) / 15);
            }
            else {
                tz = Math.floor((longitude + 7.5) / 15);
            }
            return tz;
        },
        sortFilterTable: function () {
            var $sortedTableRows = CoordSysController.$originalTableRows;
                                
            // 1st - we sort
            // we use original rows to keep the sort from changing when keeping clicking on the header
            if(CoordSysController.tableRowsSortFunc)
                $sortedTableRows = CoordSysController.$originalTableRows.sort(CoordSysController.tableRowsSortFunc);
                
            // because originalTableRows has the initial coordinate system selected, we need to clear it
            // and select the row corresponding to the current coordinate system
            $sortedTableRows.removeClass('tableRowSelected');
            $sortedTableRows.filter(function(index) {
                                          return $(this).find('td:first').text() == CoordSysController.currentCSId;
                                      }).addClass('tableRowSelected');                  

            // 2nd - we filter sorted list by search string
            var searchStr = $('#csSearch').val().toLowerCase();
            var filterFunc = function(index) {
                                return true; // no filtering
                             };

            if (searchStr) {
                // filter the coordinate systems rows
                filterFunc = function(index) {
                    // 'this' instanceof tr
                    var name = $('td.csColumnName', this).text().toLowerCase();
                    var ref = $('td.csColumnReference', this).text().toLowerCase();
                    var unit = $('td.csColumnUnit', this).text().toLowerCase();
                    var epsg = $('td.csColumnEPSG', this).text().toLowerCase();

                    if(searchStr.length == 1 && /^[a-z]+$/i.test(searchStr)) {
                        if(name.indexOf(searchStr) == 0)
                            return true;
                    }
                    else {
                        if(name.indexOf(searchStr) >= 0)
                            return true;
                        else if(ref.indexOf(searchStr) >= 0)
                            return true;
                        else if(unit.indexOf(searchStr) >= 0)
                            return true;
                        else if(epsg.indexOf(searchStr) >= 0)
                            return true;
                    }

                    return false;
                };
            }


            var $filteredTableRows = $sortedTableRows.filter(filterFunc);                      

            $('#csTableDiv tbody').html($filteredTableRows);
        }
    },
//instance methods
    {
    init: function (element, options) {
        $('#coordSysSection').html('coord_sys_template', {});

        $('#coordinateSystemLabel').html($.localize("ID_COORDINATE_SYSTEM", SELECTED_LANG));
        $('#timeZoneLabel').html($.localize("ID_TIMEZONE", SELECTED_LANG));
        $('#drawingUnitLabel').html($.localize("ID_DRAWING_UNIT", SELECTED_LANG));

        CoordSysController.existingCurrentCSId = null;
        if (globalQueries.hasOwnProperty('curCSId')) {
            CoordSysController.existingCurrentCSId = globalQueries['curCSId'];

            $('#coordinateSystemText').html(CoordSysController.existingCurrentCSId);

            CoordSysController.currentCSId = CoordSysController.existingCurrentCSId;
        }

        //retrieve timezone and units from autocad
        CoordSysController.getTimeZones();
        CoordSysController.getUnits();

        // Enable "Continue..." button if data is populated from KMLKMZ file
        if (globalQueries.hasOwnProperty('fromKML')) {
            if (globalQueries['fromKML'] > 0)
                CoordSysController.enableNextButton();
        }

        // this enables the watermark to disappear when type into search box
        $('label').marktext();

        // populate coordinate systems table
        this.setLocation(MarkerLocationController.location);
    },
    destroy: function () {
        this._super();
    },
    setCurrentCoordinateSystem: function (csName) {
        this.find('#coordinateSystemText').html(csName);

        CoordSysController.currentCSId = csName;

        CoordSysController.updateLocationandCoordinateSystem();
        CoordSysController.enableTimeZoneSel();
        CoordSysController.enableUnitsSel();
        CoordSysController.enableNextButton();            
    },        
    setLocation: function (location) {
        setLoadingState(0);
        var self = this;
        setTimeout(function () {
            CoordSysController.location.longitude = location.longitude;
            CoordSysController.location.latitude = location.latitude;
            CoordSysController.location.elevation = location.elevation;

            CoordSysController.getCoordinateSystem(location.latitude, location.longitude, CoordSysController.populateCoordinateSystem);

        }, 200);
    },
    '#csSearch keyup': function (el, ev) {
        if ( ev.keyCode === 9  ||  //shift
             ev.keyCode === 16 ||  //tab
             ev.keyCode === 37 ||  //arrow left
             ev.keyCode === 38 ||  //arrow up
             ev.keyCode === 39 ||  //arrow right
             ev.keyCode === 40)    //arrow down
             return;
             
        CoordSysController.sortFilterTable();
    },
    '#csSearch cut': function (el, ev) {        
        setTimeout(function () {
            CoordSysController.sortFilterTable();        
        }, 100);                
    },      
    '#csSearch paste': function (el, ev) {        
        setTimeout(function () {
            CoordSysController.sortFilterTable();        
        }, 100);                
    },    
    // selection of coordinate system through table row mouse click
    '#csTableDiv tr click': function (el, ev) {
        $('#csTableDiv tr').removeClass('tableRowSelected');
        el.addClass('tableRowSelected');

        var csNameSelected = el.find('td:first').text();

        this.setCurrentCoordinateSystem(csNameSelected);
    },
    '#csTableHeaderDiv thead th click': function (el, ev) {
        var columnIndex = el.index();

        $spans = $('#csTableHeaderDiv thead th span');
        $spans.removeClass('csHeaderSortUp');
        $spans.removeClass('csHeaderSortDown');

        if(el.hasClass('csHeaderSortedGreater')) {
            el.removeClass('csHeaderSortedGreater');
            el.children('span').addClass('csHeaderSortDown');

            CoordSysController.tableRowsSortFunc = function (a,b) {
                                                        var aCsName = $(a).find('td').eq(columnIndex).text();
                                                        var bCsName = $(b).find('td').eq(columnIndex).text();

                                                        return aCsName.toLowerCase() < bCsName.toLowerCase() ? 1 : -1;
                                                    };
        }
        else {
            el.addClass('csHeaderSortedGreater');
            el.children('span').addClass('csHeaderSortUp');

            CoordSysController.tableRowsSortFunc = function (a,b) {
                                                        var aCsName = $(a).find('td').eq(columnIndex).text();
                                                        var bCsName = $(b).find('td').eq(columnIndex).text();

                                                        return aCsName.toLowerCase() > bCsName.toLowerCase() ? 1 : -1;
                                                     };
        }

        CoordSysController.sortFilterTable();
    }
});


