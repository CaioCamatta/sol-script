val factorial = lambda (n) {
    var result;
    if (n <= 1) {
        result = 1;
    } else {
        result = n * factorial(n - 1);
    };
    result;
};
print factorial(3);