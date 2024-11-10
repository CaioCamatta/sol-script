val factorial = lambda (n) {
    var result;
    if (n <= 1) {
        result = 1;
    } else {
        result = n * factorial(n - 1);
    };
    return result;
};
print factorial(5);