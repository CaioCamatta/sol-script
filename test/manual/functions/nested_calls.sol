print "should print 26";

val multiply = lambda (a, b) { a * b; };
val add = lambda (a, b) { a + b; };
print add(multiply(2, 3), multiply(4, 5));  // Should print 26