print "should print 16";

val outer = lambda() {
    val inner = lambda(x) {
        return x * x;
    };
    return inner(4);
};
print outer();  // Should print 16