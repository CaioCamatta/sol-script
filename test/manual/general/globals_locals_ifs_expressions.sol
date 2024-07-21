// Global variable declarations
var globalVar;
val globalVal = 10;

// Main program
globalVar = 42;
print globalVar;

// Block expressions and local variables
val result1 = {
    val a = 5;
    val b = 3;
    a * b - 2;
};
print "Result of block expression 'result1' is: ";
print result1;

val result2 = {
    val x = 10;
    val y = {
        val inner = 7;
        x + inner;
    };
    y * 2;
};
print "Result of block expression 'result2' is: ";
print result2;

// Control flow statements
var num = 15;
if (num > 10) {
    print "15 is greater than 10.";
} else {
    print "15 is not greater than 10.";
}