print "should print 120";

val factorial = lambda (n) {
    if (n <= 1) {
        1;
    } else {
        n * factorial(n - 1);
    };
};
print factorial(5);  // Should print 120