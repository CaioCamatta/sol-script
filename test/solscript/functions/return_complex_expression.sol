print "should print -1";

val complex = lambda(a, b, c, d) {
    return a + b * (c - d);
};
print complex(1, 2, 3, 4);  // Should print -1