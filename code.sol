val a = struct {
    a_a: 4;
    a_b: this.a;
    a_c: {
        val b = struct {
            b_a: 7;
            b_b: {
                print this.b_a;
            };
        };
        print this.a_a;
    };
};


// var point = struct {
//     x: 0;
//     y: 0;
    
//     printCoords: lambda () {
//         print this.x; // Access works
//         print this.y;
//     };

//     move: lambda (dx, dy) {
//         this.x = this.x + dx;  // Re-assignment works
//         this.y = this.y + dy;
        
//         this.printCoords();    // Method calls work
//     };

//     // Todo add declaration with this.field
// };

// point.printCoords();
// point.move(1,2);