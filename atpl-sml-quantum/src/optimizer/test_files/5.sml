open QGenerator Random Optimizer Constants

fun run a =
    (
    let val gate_set = Constants.small_gate_set ()
        val (height, depth) = Constants.med_tile_dim ()
    in
        generator(gate_set, height, depth)
    end)


val _ = run 42