
val a = 1;
val b = "2";
val c = lambda () {
    print 2;
}

var car = struct {
    make: "Porsche";
    printMake: lambda () { print this.make };
}

var carFactory = lambda (_make) {
    struct {
        make: _make
    }
}

// This is a struct with make = "Volvo"
var volvoCar = carFactory("Volvo");

var animal = struct {
    type: "Animal;
}

var horse = struct {
    numLegs: 4;
    prototype: animal;
}

// Prints "Animal"
print horse.type;

var count = 4;
// Any value != 0 is truthy
while (count){
    print count;
    if (count == 4) {
        count = count - 2; 
    }
    count = count - 1;
}

	