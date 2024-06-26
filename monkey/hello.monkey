// Integers & arithmetic expressions...
let version = 1 + (50 / 2) - (8 * 3);

// ... and strings
let name = "The Monkey programming language";

// ... booleans
let isMonkeyFastNow = true;

// ... arrays & hash maps
let people = [{"name": "Anna", "age": 24}, {"name": "Bob", "age": 99}];

// User-defined functions...
let getName = fn(person) { person["name"]; };
puts(getName(people[0])); // => "Anna"
puts(getName(people[1])); // => "Bob"

// and built-in functions
puts(len(people))  // prints: 2

// `newAdder` returns a closure that makes use of the free variables `a` and `b`:
let newAdder = fn(a, b) {
    fn(c) { a + b + c };
};
// This constructs a new `adder` function:
let adder = newAdder(1, 2);

puts(adder(8)); // => 11