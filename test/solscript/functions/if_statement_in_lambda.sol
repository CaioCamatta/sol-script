print "should print 13";

val calculate = lambda (x) { val result = x * 2; if (result > 10) { result + 1; } else { result - 1; } };
print calculate(6);