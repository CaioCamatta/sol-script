val a = struct {
    a_a: 4;
    a_b: this.a_a; // This access in top level struct works
    a_c: {
        val b = struct {
            b_a: 7;
            b_b: {
                print this.b_a; // This access in nested struct works
            };
        };
        print this.a_b; // Access to pointer field works
    };
    a_d: {
        this.a_e = 5; // Setting outside normal declaration works
    };
    a_f: {
        print this.a_e; // Confirm settings works
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