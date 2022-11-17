$.Controller('PopUpMenuController',
//static methods
    {

},
//instance methods
    {
    init: function () {
        this.element.html('pop_up_menu', {});
    },
    destroy: function () {
        this._super();
    },
    show: function (x, y, txt) {
        this.element.css("display", "block"); //Showing the menu


        //Handle the case where the pop-up menu near the right edge
        //Added 5 pixels different to make sure that the menu only show in one line
        if (x + $('#popupmenu').width() - 5  > $(window).width()) {
            x = $(window).width() - $('#popupmenu').width() - 5;
        }
        this.element.css("left", x); //Positioning the menu
        this.element.css("top", y);
        this.element.find('#dropMarker').html(txt);
    },
    hide: function () {
        this.element.css("display", "none");
    }, 
    isVisible: function () {
        return this.element.css("display") === 'block';
    }
});


