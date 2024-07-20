val addTwo = lambda (n) {
    n + 2;
};

val multiplyByThree = lambda (n) {
    n * 3;
};

val result = multiplyByThree(addTwo(5));
print result;