var point = struct {
    x: 1;
    y: 2;
    
    printCoords: lambda () {
        print this.x; // Access works
        print this.y;
    };

    move: lambda (dx, dy) {
        this.x = this.x + dx;   // Re-assignment works
        this.y = this.y + dy;
    
        this.printCoords();    // Method calls work
    };

    // Todo add declaration with this.field
};

point.printCoords();
point.move(3, 4);