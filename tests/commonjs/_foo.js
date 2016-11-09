var bar = require('./_sub_dir/_bar');

exports.hi = function(){
    console.log('hi, foo')
    
    return 'foo ' + bar();
}
exports.name = 'foofoo';
