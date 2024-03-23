if (true) { 
    print "true-outer";
    if (true) { 
        print "true-inner";
    } else { 
        print "false-inner";
    }
} else { 
    print "false-outer";
}
