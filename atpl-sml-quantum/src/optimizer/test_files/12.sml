open QGenerator Random Optimizer Constants

fun run a =
    (
    let val gate_set = Constants.large_gate_set ()
        val (height, depth) = Constants.large_tile_dim ()
    in
        generator(gate_set, height, depth)
    end)


val _ = run 42