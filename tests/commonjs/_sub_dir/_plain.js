require('./_data_binding.js')

console.log('I have no exports, but other module can require me as normal module');

if (typeof document !== 'undifined'){
    var onClicked = function(){
        console.log('you clicked the document!');
    };
    document.addEventListener('click', onClicked, false);

    //dispose the old event listener if any module changed
    window.addEventListener("message", function(event){
        if (event.data === 'source_code_changed') {
            document.removeEventListener('click', onClicked);
        }
    }, false);
}