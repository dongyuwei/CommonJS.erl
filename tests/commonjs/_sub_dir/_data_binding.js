var formInputValue = 'foo@example.com';

var userModel = {email: '', name: 'luke'};
Object.defineProperty(userModel, 'email', {
    configurable: true,
    enumerable: true,
    get: function () {
        return formInputValue;
    },
    set: function (value) {
        console.log(value);
        formInputValue = value;
    }
});

console.assert(userModel.email === formInputValue);
userModel.email = 'bar@example.com';
console.assert(formInputValue === 'bar@example.com');

module.exports = userModel;