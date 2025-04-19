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
print "";
print "Initial state:";
point.printCoords();

// Test method that modifies fields
point.move(5, 10);

// Test method chaining
print "";
print "After scaling:";
point.scale(2).printCoords();

// Test returning values calculated from this
print "";
print "Distance from origin:";
print point.distance();

// Test passing this to external function
val printStruct = lambda(obj) {
    print "";
    print "External function received:";
    print obj.x;
    print obj.y;
};
printStruct(point);

// Test field direct access and mutation
print "";
print "Direct field access:";
print point.x;
point.x = 100;
print point.x;

// Test complex nested this usage
print "";
print "Complex chained usage:";
point.scaleAndMove();
point.printCoords();



// Test this with nested structs
var nested = struct {
    inner: struct {
        value: 42;
        getValue: lambda() {
            return this.value;
        };
    };
    
    getInnerValue: lambda() {
        return this.inner.getValue();
    };
};

print "";
print "Nested struct this test:";
print nested.getInnerValue();

// Test this in constructors
var makeRect = lambda(width, height) {
    return struct {
        w: width;
        h: height;
        
        area: lambda() {
            return this.w * this.h;
        };
        
        scale: lambda(factor) {
            this.w = this.w * factor;
            this.h = this.h * factor;
            return this;
        };
    };
};

var rect = makeRect(5, 10);
print "";
print "Rectangle area:";
print rect.area();
print "";
print "Scaled rectangle area:";
print rect.scale(2).area();

// Test method calling another method that uses this
var counter = struct {
    count: 0;
    
    increment: lambda() {
        this.count = this.count + 1;
        return this.count;
    };
    
    incrementTwice: lambda() {
        this.increment();
        return this.increment();
    };
};

print "";
print "Counter tests:";
print counter.increment();
print counter.incrementTwice();
