var bar = require('./sub_dir/_bar.js');

exports.hi = function(){
    console.log('hi, foo')
    
    return 'foo ' + bar();
}
exports.name = 'foofoo';
