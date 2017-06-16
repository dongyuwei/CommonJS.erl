var foo = require('./_foo.js');
console.log(foo);
console.log(foo.hi());
console.log('foobar');

console.assert(foo.hi() === 'foo bar' );
console.assert(foo.name === 'foofoo');

// test single line comment

/* test block comments
 require('./_foobar.js');
 * test foo
*/

var a = 1;// test
var b = 2; /* test */ var c = 3;