val a = { 
    val b = 2; 
    print "In block 1";
    3;
} // should NOT error

2 + { 
    print "In block 2"; 
    4;    
} // Expression statement; SHOULD error due to lack of semicolon after ExpressionStatement.