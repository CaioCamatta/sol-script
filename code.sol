var array = struct { 
    length: 3;
    get: lambda(index) {
        if (index == 0) { return 10; }
        if (index == 1) { return 20; }
        if (index == 2) { return 30; }
        "OutOfBounds"; 
    };
};
print array.length;
print array.get(0);
print array.get(1);
print array.get(2);
print array.get(3); 