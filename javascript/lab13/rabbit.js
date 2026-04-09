// name = "Monty";
// function Rabbit(name) {
//   this.name = name;
// }
// var r = Rabbit("Python");

// console.log(r.name);  // ERROR!!!
// console.log(name);    // Prints "Python"


var name = "Monty";
var r;
function Rabbit(name) {
  this.name = name;
}
r = new Rabbit("Python");

console.log(r.name); 
console.log(name); 