var LoadJSDynamic = new function () {
    var callbackOnLoaded = null, isFirstTime = true, fnIsComponentLoaded = null;
    var failedLoadCallback = null;
    var num_try = 0;
    function appendJSFile(file, callbackComplete) {

        var script_tag = $('<script><\/script>');
        script_tag.attr('type', 'text/javascript');
        script_tag.bind('load', function (e) {
            if (callbackComplete) {
                callbackComplete();
            }
        });
        script_tag.attr('src', file);
        $('head')[0].appendChild(script_tag[0]);
    }
    this.LoadJS = function (params) {
        fnIsComponentLoaded = params.fnIsComponentLoaded;
        var filePath = params.path;
        callbackOnLoaded = params.callback;
        failedLoadCallback = params.failedLoadCallback;
        if (isFirstTime && !fnIsComponentLoaded()) {
            isFirstTime = false;
            appendJSFile(filePath);
            num_try = 0;
            if (params.num_try)
                num_try = params.num_try;
            if (num_try == 0)
                num_try = 8;
            setTimeout('LoadJSDynamic.tryAgain();', this.TimeOut);
        } else if (fnIsComponentLoaded()) {
            callbackOnLoaded();
        }
    };
    this.TimeOut = 200;
    this.IsLoaded = function () {
        return (fnIsComponentLoaded != null) && fnIsComponentLoaded();
    };
    this.AppendJSfile = function (file, callbackComplete) {
        return appendJSFile(file, callbackComplete);
    };
    this.tryAgain = function () {
        if (this.IsLoaded()) {
            callbackOnLoaded();
            isFirstTime = false;
        } else {
            --num_try;
            if (num_try > 0) {
                setTimeout('LoadJSDynamic.tryAgain();', this.TimeOut);
            } else {
                if (failedLoadCallback)
                    failedLoadCallback();
            }
        }
    };
};
