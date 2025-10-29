const person = {
    name: {
        first: "Bob",
        last: "Smith",
    },
    age: 32,
    bio() {
        console.log(`${this.name[0]} ${this.name[1]} is ${this.age} years old`);
    },

    introduceSelf() {
        console.log(`Hi! I'm ${this.name[0]}`);
    },
};


function Person(name) {
    this.name = name;
    this.introduceSelf = function () {
        console.log(`Hi!, I'm ${this.name}.`);
    };
}