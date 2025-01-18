open QGenerator Random Optimizer Time

fun run a =
    (
    let val circuit = [[H, I, X], [Y, Z, X], [H, I, X], [I, I, H], [Z, I, H]]
        val height = 2 
        val depth = 2
        val gate_set = [I, X, Y, Z, H]
    in
        optimize_circuit ([[X, Y, Z], [X, Y, Z], [X, Y, Z], [X, Y, Z], [X, Y, Z], [X, Y, Z], [X, Y, Z], [X, Y, Z], [X, Y, Z],[X, Y, Z],[X, Y, Z],[X, Y, Z],[X, Y, Z],[X, Y, Z],[X, Y, Z], [X,Y,Z] , [X, Y, Z]], [I, X, Y, Z], 2, 2, 2)
    end
    (*
    val optimized_circuit = optimize_circuit (circuit, gate_set, 2, height, depth)
    in print ("Original circuit:\n" ^ pp_tile circuit ^ "\n");
       print ("Optimized circuit: \n" ^ pp_tile optimized_circuit ^ "\n\n")
    end
    *)
    )

val _ = run 42