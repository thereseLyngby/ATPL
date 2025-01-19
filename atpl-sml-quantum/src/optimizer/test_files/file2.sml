open QGenerator Random Optimizer Constants

fun run a =
    (
    let val gate_set = Constants.medium_gate_set ()
        val circuit = Constants.med_circuit gate_set
        val (height, depth) = Constants.med_tile_dim ()
    in
        optimize_circuit (circuit, gate_set, 3, height, depth)
    end)


val _ = run 42