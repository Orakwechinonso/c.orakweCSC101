
var init_tab_util = (interop) => {
    var tab_util = {};
    /*
     * return value: boolean
     */
    tab_util.getIsLoggedIn = function () {
        return interop.getIsLoggedIn();
    };
    tab_util.loadLanguage = () => {
        return lang.loadLanguage(interop);
    };
    tab_util.getColorTheme = function () {
        var promise = new Promise((resolve, reject) => {
            interop.getColorTheme().then((value) => {
                resolve(value);
            }).catch (() => {
                resolve("dark-blue");
            });
        });
        return promise;
    };
    tab_util.registerRefreshPanelCallback = function (cb) {                          
        registerCallback("AcBlockRecent_Interop.refreshPanel", cb);
    };
    tab_util.registerUserSignedInStateChangedCallback = function (cb) {
        registerCallback("AcBlockRecent_Interop.userSignedInStateChanged", cb);
    }
    /*
     * return value: {"status": "Done" or "Canceled"}
     */
    tab_util.signIn = function () {
        return interop.signIn();
    };
    tab_util.launchHelp = function(helpId) {
        return interop.launchHelp(helpId);
    };
    /*
     * return value: {"status": "Done" or "Canceled", value: "path to folder"}
     */
    tab_util.showStorageDialog = function (strTitle, strDefault) {
        return interop.showStorageDialog(strTitle, strDefault);
    };
    tab_util.closeBrowser = function () {
        interop.closeBrowser();
    };
    tab_util.setBlockSyncFolder = function (strPath) {
        return interop.setBlockSyncFolder(strPath);
    };
    /*
     * return value: boolean
     */
    tab_util.getIsBlockSyncFolderSetToCloudStorage = () => {
        return interop.getIsBlockSyncFolderSetToCloudStorage();
    };
    tab_util.updatePalette = () => {
        return interop.updatePalette();
    };
    /*
     * return value: boolean
     */
    tab_util.getIsStateLocked = () => {
        return interop.getIsStateLocked();
    };
    tab_util.setIsStateLocked = (lock) => {
        return interop.setIsStateLocked(lock);
    };
    /*
     * return value: {"status": "Done" or "Canceled"}
     */ 
    tab_util.showGuideDialog = () => {
        return interop.showGuideDialog();
    };
    tab_util.setIsOnBoardingCompleted = () => {
        return interop.setIsOnBoardingCompleted();
    };
    /*
     * return {"status": "Done" or "Failed", "value": "path to folder" or null}
     */
    tab_util.getBlockSyncFolderFromServer = (timeout) => {
        return interop.getBlockSyncFolderFromServer(timeout);
    }
    /*
     * return value: {"status": "Done" or "Canceled"}
     */ 
    tab_util.showConsentDialog = () => {
        return interop.showConsentDialog();
    };
    /* 
     * return value: {"status": consent status (int: 0 - un-decided, 1 - yes, 2 - no)}
     */
    tab_util.getConsentStatus = (timeout) => {
        return interop.getConsentStatus(timeout);
    };
    tab_util.setConsentStatus = (status) => {
        return interop.setConsentStatus(status);
    };
    tab_util.getIsBlockSyncFolderCustomizedToLocal = () => {
        return interop.getIsBlockSyncFolderCustomizedToLocal();
    };
    tab_util.walkOnBoarding = () => {
        return interop.walkOnBoarding();
    };
    return tab_util;
};

utils = {};

(function (utils) {
    utils.getUrlParameter = function (name) {
        name = name.replace(/[\[]/, '\\[').replace(/[\]]/, '\\]');
        var regex = new RegExp('[\\?&]' + name + '=([^&#]*)');
        var results = regex.exec(location.search);
        return results === null ? '' : decodeURIComponent(results[1].replace(/\+/g, ' '));
    };
    utils.getBaseFile = function () {
        var base_file = "";
        try{
			base_file = (/([^\/]+)(?=\.\w+)\.html/).exec(window.location)[1];
		} catch (e) {
			console.error('Utils could not parse base file');
        }
        return base_file;
    };
    utils.getTabName = function () {
        var name = this.getUrlParameter("tab");
        if (name === 'recent') {
            return 'Recent';
        } else if (name === 'favorites') {
            return "Favorites";
        } else {
            return 'Library';
        }
    };
    function using(resource, func) {
        try {
            func(resource);
        }
        finally {
            if (resource.dispose && typeof resource.dispose === 'function') {
                resource.dispose();
            }
        }
    }
    utils.library = {};
    utils.library.getIsBlockLibraryExisted = () => {
        return AcBlockLibrary.Interop.getIsBlockLibrariesExisted();
    };
    utils.library.closeBrowser = () => {
        AcBlockLibrary.Interop.closeBrowser();
    };
    /*
     * return value: "color theme" // "DarkBlue" or "Light"
     */
    utils.library.getColorTheme = () => {
        return AcBlockLibrary.Interop.getColorTheme();
    };
    utils.library.loadLanguage = () => {
        return lang.loadLanguage(AcBlockLibrary.Interop);
    };
    utils.library.registerRefreshPanelCallback = function (cb) {                          
        registerCallback("AcBlockLibrary_Interop.refreshPanel", cb);
    };
    utils.library.setBlockLibrary = function (strPath) {
        return AcBlockLibrary.Interop.setBlockLibrary(strPath);
    };
    utils.library.showOpenDialog = function (strTitle, strDefault) {
        return AcBlockLibrary.Interop.showOpenDialog(strTitle, strDefault);
    };    
    utils.library.launchHelp = function(helpId) {
        return AcBlockLibrary.Interop.launchHelp("OrganizeBlocksAsLibrary");
    };


    utils.recent = init_tab_util(AcBlockRecent.Interop);
    utils.favorites = init_tab_util(AcBlockFavorites.Interop);
    
})(utils || (utils={}));

