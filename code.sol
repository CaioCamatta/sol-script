//val a = lambda(b, c){ print b+c; };
//a({2;}, 3);
//a({12;}, 13);

val b = lambda(b, c){ b+c; };
// print b(b({12;}, 13), 2);

val c = lambda(d, e){
    b(d, e) + 1;
};
print c(5, 6);
//print c(7,8);