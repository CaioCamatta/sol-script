print "should print 1 and -1";

val test = lambda(x) { 
    if (x > 0) { 
        return 1; 
    } else { 
        return -1; 
    } 
};
print test(5);   // Should print 1
print test(-5);  // Should print -1