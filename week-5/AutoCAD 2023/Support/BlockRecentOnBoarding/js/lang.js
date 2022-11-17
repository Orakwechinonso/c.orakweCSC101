
lang = {};

var process_language = (param_lang) => {
    var supported_langs = ['en-US', 'en-GB', 'cs-CZ', 'de-DE', 'es-ES', 'fr-FR', 'hu-HU', 'it-IT', 'ja-JP', 'ko-KR', 'pl-PL', 'pt-BR', 'ru-RU', 'zh-CN', 'zh-TW'];
    if (!supported_langs.includes(param_lang)) {
        param_lang = "en-US";
    }
    var lang_file = 'lang/' + param_lang + "/" + utils.getBaseFile() + '.json';
    var promise = new Promise(function (resolve, reject) {            
        $.getJSON(lang_file, function (data) {  
            console.log("language " + param_lang + ' loaded.');
            resolve(data);
        }).fail(function () {
            reject();
        });
    });
    return promise;
};

(function (lang) {
    lang.loadLanguage = function (interop) {
        var promise = new Promise((resolve, reject) => {
            interop.getLanguage().then((value) => {
                process_language(value).then((data) => {
                    resolve(data);
                }).catch(() => {
                    reject();
                });
            }).catch((value) => {
                process_language("en-US").then(resolve);
            });
        });
        return promise;
    };
})(lang);

