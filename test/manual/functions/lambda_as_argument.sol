print "should print 10";

val applyFunc = lambda (f, x) { f(x); };
val double = lambda (x) { x * 2; };
print applyFunc(double, 5);  // Should print 10