val a = lambda(b,c){print b+c;};
a({2;}, 3);
a({12;}, 13);

val b = lambda(b,c){b+c;};
print b(b({12;}, 13), 2);

//val a = lambda(){print 1;};