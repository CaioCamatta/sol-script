print "SolScript doesn't support immediately-invoked function expressions yet";

print (lambda () { "IIFE"; })();  // Should print "IIFE"