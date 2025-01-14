    open QGenerator Random

fun run a =
    (print "Run all tests: \n";
     print "test 1: make_init_columns ([X, Y], 2)\n";
     print ("Expected: " ^ (pp_column_list [[X, X], [Y, Y], [X, Y], [Y, X]]) ^ "\n");
     print ("Result: " ^ (pp_column_list (make_init_columns ([X, Y], 2))) ^ "\n\n");
     print "test 2: pp_tile_lst [[[X, X], [I, I]], [[I, X], [X, I]]] \n";
     print "Expected: [[[X, X], [I, I]], [[I, X], [X, I]]] \n";
     print ("Result: "^ (pp_tile_list [[[X, X], [I, I]], [[I, X], [X, I]]]) ^ "\n\n");
     print "test 3: make_tiles ([I, X], 2, 2)\n";
     print "Expected: [[[I I], [I I]],[[I I], [I X]],[[I I], [X I]],[[I I], [X X]],[[I X], [I I]],[[I X], [I X]],[[I X], [X I]],[[I X], [X X]],[[X I], [I I]],[[X I], [I X]],[[X I], [X I]],[[X I], [X X]],[[X X], [I I]],[[X X], [I X]],[[X X], [X I]],[[X X], [X X]]]\n";
     print ("Result: " ^ (pp_tile_list (make_tiles ([I, X], 2, 2))) ^ "\n\n");
     print "test 4: print cost of tile\n";
     print ("Result: " ^ Int.toString (cost [[X, I], [I, I], [I, X]]) ^ "\n\n");
     print "test 5: convert tile to unitary matrix\n";
     print ("Semantics of circiut:\n" ^ Semantics.pp_mat(tile_to_matrix [[X, I], [I, I], [X, Y]] ) ^ "\n");
     print "test 6: Column to circuit works\n";
     print ("Column to circuit\n" ^ (Circuit.draw (column_to_circuit [X, Y, Z])) ^ "\n");
     print "test 7: Test tile to circuit works\n";
     print ("Tile to circuit\n" ^ (Circuit.draw (tile_to_circuit [[X, Y, Z], [I, X, Z]])) ^ "\n");
     print "test 8 generate random complex \n";
     print (Complex.toString  (gen_random_complex (Random.newgen ()) ) ^ "\n\n");
     print "test 9: generate list of random complexes \n" ;
     print (pp_list (gen_random_complex_list (Random.newgen (), 10), Complex.toString) ^ "\n\n");
     print "test 10: normalize list of complex numbers \n";
     print (pp_list (normalize_complex_list (gen_random_complex_list (Random.newgen (), 10)), Complex.toString) ^ "\n\n");
     print "test 11: normalize list and check sums to 1 \n";
     print (Real.toString ( Math.sqrt (foldl (fn (elm, acc) => Complex.re (Complex.* (elm, Complex.conj elm)) + acc ) 0.0 (normalize_complex_list (gen_random_complex_list (Random.newgen (), 10))))) ^ "\n\n")
    )

val () = run 42