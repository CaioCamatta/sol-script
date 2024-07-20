print "Expecting 9 and 10 to be printed:";
val f = lambda(b){ b+2; };
print f(7);
print f(8);

print "";
print "Expecting 5 and 25 to be printed:";
val a = lambda(b, c){ print b+c; };
a({2;}, 3);
a({12;}, 13);

print "";
print "Expecting 12 and 16 to be printed:";
val b = lambda(b, c){ b+c; };
val c = lambda(d, e){
    b(d, e) + 1;
};
print c(5, 6);
print c(7,8);
