// No else branch and no blocks
if (true) print "Should print";
if (false) print "Should NOT print";

// Neither if nor else use blocks
if (true)
    print "Should print";
else
    print "Should NOT print";

if (false)
    print "Should NOT print";
else
    print "Should print";


// One of if or else use blocks
if (false) {
    print "Should NOT print";
}
else
    print "Should print";

if (false)
    print "Should NOT print";
else {
    print "Should print";
}


// Both if and else use blocks
val a = 20;
if (a > 15) {
    print "Should print";
} else {
    print "Should NOT print";
}

// Multiple statements in block
if (true) {
    print "Should print";
    print "Should print";
}
if (true)
    print "Should print";
    print "Should print";

if (false)
    print "Should NOT print";
    print "Should print";