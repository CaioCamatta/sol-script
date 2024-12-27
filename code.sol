val s = struct {
    a: 4;
    f: lambda(){
        print this.a;
    };
};
print s.f();