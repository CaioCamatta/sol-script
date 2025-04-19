// struct with methods using "this"
var point = struct {
    x: 10;
    y: 20;
    
    // Basic this reference to access fields
    printCoords: lambda() {
        print "Coordinates:";
        print this.x;
        print this.y;
    };
    
    // Method that modifies fields with this
    move: lambda(dx, dy) {
        this.x = this.x + dx;
        this.y = this.y + dy;
        print "Moved to:";
        this.printCoords();  // Method call using this
    };
    
    // Method that returns this for chaining
    scale: lambda(factor) {
        this.x = this.x * factor;
        this.y = this.y * factor;
        this;  // Return this for method chaining
    };
    
    // Method that calculates using multiple this references
    distance: lambda() {
        var d = (this.x * this.x) + (this.y * this.y);
        return d;
    };
    
    // Chained methods
    scaleAndMove: lambda() {
        this.scale(2).move(5, 5);
    };
};

// Test basic field access
print "Initial state:";
point.printCoords();

// Test method that modifies fields
point.move(5, 10);

// Test method chaining
print "After scaling:";
point.scale(2).printCoords();

// Test returning values calculated from this
print "Distance from origin:";
print point.distance();

// Test passing this to external function
val printStruct = lambda(obj) {
    print "External function received:";
    print obj.x;
    print obj.y;
};
printStruct(point);

// Test field direct access and mutation
print "Direct field access:";
print point.x;
point.x = 100;
print point.x;

// Test complex nested this usage
print "Complex chained usage:";
point.scaleAndMove();
point.printCoords();
