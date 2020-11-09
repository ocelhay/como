// Test if an array contains '_'

console.log('Test x');
let x = ['_', '_', '_'];
console.log(x.indexOf('_'));


// console.log(x.every( (val, i, arr) => val === '_' ));

// console.log(x.every( (val, i) => val === '_' ));

console.log(x.every( (val) => { return val === '_';} ));

