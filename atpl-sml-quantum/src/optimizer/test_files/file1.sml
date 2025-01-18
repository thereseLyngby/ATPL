open QGenerator Random Optimizer Constants

fun run a =
    (
    let val gate_set = Constants.small_gate_set ()
        val circuit = Constants.small_circuit gate_set
        val (height, depth) = Constants.small_tile_dim ()
    (*in
        optimize_circuit (circuit, gate_set, 2, height, depth)
    end*)
        val optimized_circuit = optimize_circuit (circuit, gate_set, 2, height, depth)
    in print ("Original circuit:\n" ^ pp_tile circuit ^ "\n");
       print ("Optimized circuit: \n" ^ pp_tile optimized_circuit ^ "\n\n")
    end

    )

val _ = run 42