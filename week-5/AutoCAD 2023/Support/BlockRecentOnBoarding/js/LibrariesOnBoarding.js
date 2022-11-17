(function (LibrariesOnBoarding) {
    var tab_util = utils.library;
    var library_strings = null;
    var clear_colortheme_class = () => {
        $("body").removeClass("dark").removeClass("dark-blue");
        $("#txt_subhead").removeClass("dark").removeClass("dark-blue");
        $("#sign_in_link").removeClass("dark").removeClass("dark-blue");
        $("#learn_about_link").removeClass("dark").removeClass("dark-blue");
        $("#legal_sentence").removeClass("dark").removeClass("dark-blue");
        $("#btn_open_dialog").removeClass("dark").removeClass("dark-blue");
    };
    var fillImage = async () => {
        var theme = await tab_util.getColorTheme();
        theme = theme || 'Light';
        var img_suffix = theme === "Light" ? "light" : "dark";
        var img_name = "NoLibrary";
        $("#icon_blockplaceholder").attr("src", "imgs/" + img_name + "_" + img_suffix + ".png");
    };
    var fillTheme = async () => {
        var theme = await tab_util.getColorTheme();
        theme = theme || 'Light';
        clear_colortheme_class();
        if (theme !== 'Dark' && theme !== 'DarkBlue') {
            return;
        }
        var class_name = theme === 'Dark' ? "dark" : "dark-blue";
        $("body").addClass(class_name);
        $("#txt_subhead").addClass(class_name);
        $("#sign_in_link").addClass(class_name);
        $("#learn_about_link").addClass(class_name);
        $("#legal_sentence").addClass(class_name);
        $("#btn_open_dialog").addClass(class_name); 
    };
    var fillContent = async () => {
        var obj = await tab_util.loadLanguage();
        if (!obj || !obj['Library'])
            return;
        library_strings = obj['Library'];
        // txt_subhead
        $("#txt_subhead").html(library_strings["subhead"]);
        // btn_open_dialog
        $("#btn_open_dialog").removeClass("btn-collapse");
        $("#btn_open_dialog").html(library_strings["btn-title"]);
        // learn_about_link
        $("#learn_about_link").html(library_strings["external_help_link"]);
        $("#legal_sentence").html(library_strings["legal_sentence"]);
        
    };
    var refreshPanel = () => {
        fillImage();
        fillTheme();
        fillContent();
    };
    LibrariesOnBoarding.refreshPanel = refreshPanel;
    var selectBlockLibrary = async () => {
        try {
            var result = await tab_util.showOpenDialog(library_strings['open_dialog_title'], library_strings['open_dialog_default']);
            if (result.status && result.status === 'Done') {
                await tab_util.setBlockLibrary(result.value);
                tab_util.closeBrowser();
            }
        } catch (err) {}
    };
    LibrariesOnBoarding.selectBlockLibrary = selectBlockLibrary;
    var launchHelp = () => {
        tab_util.launchHelp("PaletteHelp");
    };
    LibrariesOnBoarding.launchHelp = launchHelp;
})(window.LibrariesOnBoarding || (window.LibrariesOnBoarding = {}));

