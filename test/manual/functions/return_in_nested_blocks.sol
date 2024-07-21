print "should print 11";

val test = lambda() { 
    var x = 0;
    while (true) { 
        if (x > 10) { 
            return x; 
        } 
        x = x + 1;
    } 
    return 0; 
};
print test();  // Should print 11