val applyOperation = lambda (a, operation) {
    operation(a);
};

val result = applyOperation(5, lambda(x) { x * x; });
print result;