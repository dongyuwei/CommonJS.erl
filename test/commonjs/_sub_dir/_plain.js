require('./_data_binding.js')

console.log('I have no exports, but other module can require me as normal module');

if (typeof document !== 'undefined'){
    var onClicked = function(){
        console.log('you clicked the document!2');
    };
    document.addEventListener('click', onClicked, false);

    //dispose the old event listener if any module changed
    function onCodeUpdated(event) {
        window.removeEventListener('message', onCodeUpdated);

        var data = event.data;
        if (data.eventType === 'sourceCodeChanged') {
            document.removeEventListener('click', onClicked);

            // Rerun the module's new source code, should bubble the process to entry file.
            // My assumption is that: The parent or ancestor module should take care of themselves.
            // Here just a simple demo of HMR.
            var moduleName = data.moduleName;
            delete require.cache[moduleName];
            require.sourceCache[moduleName] = data.moduleContent;
            require.cache[moduleName] = require(moduleName);
        }
    }
    window.addEventListener("message", onCodeUpdated, false);
}