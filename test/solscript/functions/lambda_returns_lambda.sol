print "should print 10";

val makeIdentiy = lambda () {
    lambda (y) { y; };
};
val id = makeIdentiy();
print id(10);  // Should print 10