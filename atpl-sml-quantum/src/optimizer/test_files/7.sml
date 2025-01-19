open QGenerator Random Optimizer Constants

fun run a =
    (
    let val gate_set = Constants.medium_gate_set ()
        val (height, depth) = Constants.small_tile_dim ()
    in
        generator(gate_set, height, depth)
    end)


val _ = run 42