
(function onLoadRecentOnBoarding(RecentOnBoarding) {
    var locale_strings = null;
    var tab_util = utils.recent;
    var load_local_strings = async () => {
        if (locale_strings === null) {
            locale_strings = await tab_util.loadLanguage();
        }
        return locale_strings;
    };
    var clear_colortheme_class = () => {
        $("body").removeClass("dark").removeClass("dark-blue");
        $("#txt_subhead").removeClass("dark").removeClass("dark-blue");
        $("#txt_subhead_paragraph").removeClass("dark").removeClass("dark-blue");
        $("#sign_in_link").removeClass("dark").removeClass("dark-blue");
        $("#learn_about_link").removeClass("dark").removeClass("dark-blue");
        $("#legal_sentence").removeClass("dark").removeClass("dark-blue");
        $("#btn_open_dialog").removeClass("dark").removeClass("dark-blue");
    };
    var _fill_image_for_page = (theme, img_name) => {
        theme = theme || "Light";
        var img_suffix = theme === "Light" ? "light" : "dark";
        var img_name = img_name || "No-Sync";
        $("#icon_blockplaceholder").attr("src", "imgs/" + img_name + "_" + img_suffix + ".png");
    };    
    var fillImage = async (state) => {
        var theme = await tab_util.getColorTheme();
        if (state === "allset") {
            _fill_image_for_page(theme, "StartUse-AllSet");
        } else if (state === "notsynced") {
            _fill_image_for_page(theme, "StartUse-NotSynced");
        } else {
            _fill_image_for_page(theme, "No-Sync");
        }
    };
    var fillTheme = async () => {
        var theme = await tab_util.getColorTheme();
        /// tab_util.getColorTheme().then((theme) => {        
        theme = theme || "Light";
        clear_colortheme_class();
        if (theme !== 'Dark' && theme !== 'DarkBlue') {
            return;
        }
        var class_name = theme === 'Dark' ? "dark" : "dark-blue";
        $("body").addClass(class_name);
        $("#txt_subhead").addClass(class_name);
        $("#txt_subhead_paragraph").addClass(class_name);
        $("#sign_in_link").addClass(class_name);
        $("#learn_about_link").addClass(class_name);
        $("#legal_sentence").addClass(class_name);
        $("#btn_open_dialog").addClass(class_name);            
        /// });
    };
    var _fill_content_for_page = (state, recent_string) => {
        if (state === 'allset') {
            var help_id = "RemoteRecentBlocks";
            $("#txt_subhead").html(recent_string["subhead_startuse_allset"]);
            $("#txt_subhead_paragraph").removeClass('txt-collapse');
            $("#txt_subhead_paragraph").html(recent_string["subhead_paragraph_startuse_allset"]);
            document.querySelector("#learn_about_link").dataset.helpId = help_id;
            $("#learn_about_link").html(recent_string["external_help_link_sync_success"]);
        } else if (state === 'notsynced') {
            var help_id = "RemoteRecentBlocks";
            $("#txt_subhead").html(recent_string["subhead_startuse_notsynced"]);
            $("#txt_subhead_paragraph").removeClass('txt-collapse');
            $("#txt_subhead_paragraph").html(recent_string["subhead_paragraph_startuse_notsynced"]);
            document.querySelector("#learn_about_link").dataset.helpId = help_id;
            $("#learn_about_link").html(recent_string["external_help_link_sync_success"]);
        } else {
            // txt_headline
            var help_id = "RemoteRecentBlocks";
            $("#sign_in_link").removeClass("sign-in-collapse");
            $("#sign_in_link").html(recent_string["headline"]);
            // txt_subhead
            $("#txt_subhead").html(recent_string["subhead"]);
            $("#learn_about_link").data("helpId", help_id);
            $("#learn_about_link").html(recent_string["external_help_link_sign_in"]);
        }
    };
    var fillContent = async (state) => {
        locale_strings = await tab_util.loadLanguage();
        
        if (!locale_strings || !locale_strings['Recent']) {
            return;
        }
        var recent_string = locale_strings['Recent'];
        _fill_content_for_page(state, recent_string);
        $("#legal_sentence").html(recent_string["legal_sentence"]);
    };
    var refreshPanel = () => {
        var state = utils.getUrlParameter('state');
        fillImage(state);
        fillTheme();
        fillContent(state);
    };
    RecentOnBoarding.refreshPanel = refreshPanel;
    var show_guide_dialog = async () => {
        var res = await tab_util.showGuideDialog();
        return (!res || typeof res['status'] === 'undefined' || res['status'] !== 'Done') ? false : true;
    };
    const show_storage_dialog = async () => {
        try {            
            var res = await tab_util.getIsBlockSyncFolderSetToCloudStorage();
            if (res) {
                return true;
            }
            await load_local_strings();
            var recent_strings = locale_strings['Recent'];
            var result = await tab_util.showStorageDialog(recent_strings['storage_dialog_title'], recent_strings['storage_dialog_default']);
            if (result.status && result.status === 'Done') {
                await tab_util.setBlockSyncFolder(result.value);
                return true;
            }
            return false;
        } catch (err) {
            return false;
        }
    };    
    const show_consent_dialog = async () => {
        try {
            var timeout = 3000;
            var consent = await tab_util.getConsentStatus(timeout);
            if (consent && consent['status'] && consent['status'] !== 0) {
                return true;
            }
            var res = await tab_util.showConsentDialog();
            if (!res || typeof res['status'] === 'undefined' || res['status'] !== 'Done') {
                tab_util.setConsentStatus(2); // canceled means no
                return true;
            }
            return true;
        } catch (err) {
            return false;
        }
    };    
    
    var walkOnBoarding = () => {
        tab_util.walkOnBoarding();
    };
    RecentOnBoarding.walkOnBoarding = walkOnBoarding;
    var selectStorage = async () => {
        var is_locked = await tab_util.getIsStateLocked();
        if (is_locked) { return; }
        try {
            await tab_util.setIsStateLocked(true);
            var res = await show_guide_dialog();
            if (!res) { return; }
            await show_consent_dialog();
            await show_storage_dialog();
            // We will set on-boarding touched regardless of it was done to specify cloud storage
            tab_util.setIsOnBoardingCompleted();
        } catch (err) {            
        } finally {
            await tab_util.setIsStateLocked(false);
        }
    };
    RecentOnBoarding.selectStorage = selectStorage;
    var launchHelp = (help_id) => {
        tab_util.launchHelp(help_id);
    };
    RecentOnBoarding.launchHelp = launchHelp;
})(window.RecentOnBoarding || (window.RecentOnBoarding = {}));