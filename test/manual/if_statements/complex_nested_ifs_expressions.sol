// This is a complex scenario testing not only if statements but also expressions and blocks

// Start with a simple condition
if (5 > 3) {
    print "5 is greater than 3";  // True path

    // Nested if with a logical AND
    if (true && (10 >= 5)) {
        print "Both conditions are true";
        
        // Deeply nested if with a comparison
        if ((5 + 5) == 10) {
            print "5 + 5 is indeed 10";
        } else {
            print "This shouldn't print";
        }
    } else {
        print "This shouldn't print either";
    }

    // Nested if with a logical OR
    if (false || (7 <= 8)) {
        print "At least one condition is true";
        
        // Nested if-else inside an OR condition
        if ((3 * 3) != 9) {
            print "This shouldn't print";
        } else {
            print "3 * 3 is still 9";
            
            // Further nesting with another layer of logic
            if (!(false)) {
                print "Negation of false is true";
            }
        }
    }
} else {
    print "This shouldn't print";
}

// Let's add a contrasting condition to see else in action
if (1 > 2) {
    print "This won't print because 1 is not greater than 2";
} else {
    print "As expected, 1 is not greater than 2";
    
    // A nested if with a straightforward condition
    if (2 == 2) {
        print "2 is equal to 2, as expected";
    }
}