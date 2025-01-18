signature CONSTANTS = sig

    val small_tile_dim : unit -> QGenerator.height * QGenerator.depth
    val med_tile_dim : unit -> QGenerator.height * QGenerator.depth
    val large_tile_dim : unit -> QGenerator.height * QGenerator.depth

    val small_circuit : QGenerator.gate list -> QGenerator.tile
    val med_circuit : QGenerator.gate list -> QGenerator.tile
    val big_circuit : QGenerator.gate list -> QGenerator.tile

    val small_gate_set : unit -> QGenerator.gate list 
    val medium_gate_set : unit -> QGenerator.gate list 
    val large_gate_set : unit -> QGenerator.gate list 

end
