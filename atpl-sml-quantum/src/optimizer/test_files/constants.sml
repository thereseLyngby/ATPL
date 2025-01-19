structure Constants : CONSTANTS = struct
    structure QG = QGenerator
(*    
    val small_tile_dim : unit -> QGenerator.height * QGenerator.depth
    val med_tile_dim : unit -> QGenerator.height * QGenerator.depth
    val large_tile_dim : unit -> QGenerator.height * QGenerator.depth

    val small_circuit : unit -> QGenerator.tile
    val med_circuit : unit -> QGenerator.tile
    val big_circuit : unit -> QGenerator.tile

    val small_gate_set : unit -> QGenerator.tile list 
    val medium_gate_set : unit -> QGenerator.tile list 
    val large_gate_set : unit -> QGenerator.tile list 
*)

    fun small_tile_dim (a : unit) : QG.height * QG.depth = (1, 2)
    fun med_tile_dim (a : unit) : QG.height * QG.depth = (2, 2)
    fun large_tile_dim (a : unit) : QG.height * QG.depth = (2, 3)

    (*For tile size 3x4 and max gates we segfault always*)
    fun small_gate_set (a : unit) : QG.gate list =
        [QG.I, QG.X]
    fun medium_gate_set (a : unit) : QG.gate list =
        [QG.I, QG.X, QG.H]
    fun large_gate_set (a : unit) : QG.gate list =
        [QG.I, QG.X, QG.Y, QG.Z, QG.H]

    (*3 x 3*)
    fun small_circuit (gS : QG.gate list) : QG.tile =
        case gS of
            [QG.I, QG.X]                        =>
                [[QG.X, QG.I, QG.I], [QG.X, QG.X, QG.X], [QG.I, QG.I, QG.X]]
            | [QG.I, QG.X, QG.H]                =>
                [[QG.X, QG.H, QG.I], [QG.X, QG.X, QG.X], [QG.I, QG.I, QG.H]]
            | [QG.I, QG.X, QG.Y, QG.Z, QG.H]    =>
                [[QG.X, QG.H, QG.I], [QG.Z, QG.Y, QG.X], [QG.Y, QG.Z, QG.H]]
            | _ => raise Fail "Illegal gate set"

    (*5 x 7*)
    fun med_circuit (gS : QG.gate list) : QG.tile =
        case gS of
            [QG.I, QG.X]                        =>
                [[QG.X, QG.I, QG.I, QG.X, QG.X], [QG.X, QG.X, QG.X, QG.I, QG.I], [QG.I, QG.I, QG.X, QG.I, QG.X],[QG.I, QG.I, QG.X, QG.I, QG.X], [QG.I, QG.I, QG.X, QG.I, QG.X], [QG.I, QG.I, QG.X, QG.I, QG.X], [QG.I, QG.I, QG.X, QG.I, QG.X]]
            | [QG.I, QG.X, QG.H]                =>
                [[QG.X, QG.H, QG.I, QG.I, QG.H], [QG.X, QG.X, QG.X, QG.H, QG.I], [QG.X, QG.H, QG.H, QG.H, QG.I],[QG.I, QG.X, QG.H, QG.I, QG.X],[QG.X, QG.I, QG.I, QG.I, QG.X],[QG.H, QG.I, QG.H, QG.X, QG.I], [QG.H, QG.I, QG.I, QG.H, QG.X]]
            | [QG.I, QG.X, QG.Y, QG.Z, QG.H]    =>
                [[QG.X, QG.H, QG.Y, QG.I, QG.H], [QG.Y, QG.Z, QG.X, QG.H, QG.Y], [QG.I, QG.H, QG.Y, QG.I, QG.I],[QG.Z, QG.X, QG.I, QG.I, QG.Z],[QG.X, QG.Z, QG.Y, QG.H, QG.Y],[QG.Y, QG.H, QG.H, QG.X, QG.I], [QG.H, QG.I, QG.Z, QG.H, QG.X]]
            | _ => raise Fail "Illegal gate set"

    (*10 x 20*)
    fun big_circuit (gS : QG.gate list) : QG.tile =
        case gS of
            [QG.I, QG.X]                        =>
                [[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X], [QG.I, QG.X, QG.I, QG.X, QG.X, QG.X, QG.I, QG.X, QG.I, QG.I],[QG.I, QG.I, QG.I, QG.I, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X, QG.I],[QG.X, QG.X, QG.X, QG.X, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X],[QG.I, QG.X, QG.I, QG.X, QG.X, QG.I, QG.I, QG.I, QG.X, QG.I],[QG.X, QG.X, QG.I, QG.I, QG.I, QG.X, QG.I, QG.I, QG.X, QG.I],[QG.I, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.X, QG.I, QG.X],[QG.X, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.I, QG.I, QG.X],[QG.I, QG.X, QG.I, QG.X, QG.I, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.I, QG.X, QG.X, QG.X, QG.I, QG.X, QG.I, QG.X, QG.I, QG.X],[QG.I, QG.I, QG.I, QG.X, QG.X, QG.I, QG.I, QG.I, QG.I, QG.X],[QG.I, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.X]]
            | [QG.I, QG.X, QG.H]                =>
                [[QG.X, QG.H, QG.I, QG.X, QG.X, QG.X, QG.I, QG.H, QG.X, QG.X], [QG.I, QG.H, QG.I, QG.X, QG.X, QG.X, QG.H, QG.H, QG.I, QG.I],[QG.H, QG.I, QG.I, QG.H, QG.X, QG.X, QG.H, QG.I, QG.X, QG.X],[QG.H, QG.I, QG.X, QG.X, QG.H, QG.I, QG.I, QG.H, QG.H, QG.H],[QG.H, QG.X, QG.X, QG.X, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X],[QG.I, QG.H, QG.H, QG.X, QG.X, QG.I, QG.I, QG.H, QG.H, QG.I],[QG.X, QG.X, QG.I, QG.I, QG.I, QG.X, QG.I, QG.I, QG.X, QG.I],[QG.H, QG.I, QG.H, QG.H, QG.X, QG.X, QG.I, QG.X, QG.H, QG.X],[QG.X, QG.H, QG.H, QG.X, QG.H, QG.I, QG.I, QG.I, QG.I, QG.X],[QG.I, QG.X, QG.I, QG.X, QG.H, QG.X, QG.I, QG.I, QG.X, QG.X],[QG.H, QG.X, QG.X, QG.H, QG.H, QG.X, QG.I, QG.X, QG.I, QG.X],[QG.I, QG.I, QG.I, QG.X, QG.X, QG.I, QG.I, QG.H, QG.H, QG.H],[QG.H, QG.I, QG.I, QG.X, QG.X, QG.X, QG.H, QG.I, QG.X, QG.H],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.H, QG.X, QG.H],[QG.H, QG.H, QG.I, QG.X, QG.H, QG.X, QG.H, QG.I, QG.X, QG.X],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.H, QG.X, QG.X],[QG.X, QG.H, QG.I, QG.X, QG.X, QG.H, QG.I, QG.I, QG.X, QG.H],[QG.H, QG.I, QG.I, QG.H, QG.X, QG.X, QG.H, QG.I, QG.X, QG.H],[QG.X, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.H],[QG.H, QG.I, QG.I, QG.H, QG.X, QG.H, QG.H, QG.I, QG.X, QG.X]]
            | [QG.I, QG.X, QG.Y, QG.Z, QG.H]    =>
                [[QG.X, QG.H, QG.Y, QG.Z, QG.Y, QG.X, QG.Y, QG.H, QG.X, QG.Z], [QG.Y, QG.Y, QG.I, QG.X, QG.X, QG.X, QG.H, QG.H, QG.I, QG.Z],[QG.H, QG.I, QG.Z, QG.H, QG.X, QG.X, QG.H, QG.I, QG.X, QG.X],[QG.H, QG.I, QG.Y, QG.X, QG.H, QG.I, QG.Y, QG.Z, QG.Y, QG.H],[QG.Y, QG.Y, QG.X, QG.X, QG.X, QG.X, QG.Z, QG.I, QG.Z, QG.X],[QG.Y, QG.H, QG.H, QG.X, QG.X, QG.Z, QG.I, QG.H, QG.Y, QG.I],[QG.X, QG.Z, QG.I, QG.I, QG.I, QG.X, QG.Z, QG.Z, QG.Y, QG.Z],[QG.H, QG.I, QG.H, QG.Y, QG.X, QG.Y, QG.Z, QG.X, QG.H, QG.X],[QG.Y, QG.H, QG.H, QG.X, QG.Z, QG.I, QG.I, QG.I, QG.Y, QG.X],[QG.I, QG.Y, QG.Z, QG.Z, QG.H, QG.X, QG.Y, QG.Y, QG.X, QG.Y],[QG.Z, QG.X, QG.X, QG.Z, QG.H, QG.X, QG.Y, QG.X, QG.I, QG.Y],[QG.I, QG.Y, QG.Z, QG.Y, QG.X, QG.I, QG.I, QG.Z, QG.H, QG.H],[QG.H, QG.I, QG.I, QG.Y, QG.Z, QG.Z, QG.Y, QG.Y, QG.Y, QG.Z],[QG.Y, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.H, QG.Z, QG.H],[QG.H, QG.H, QG.I, QG.X, QG.H, QG.X, QG.Y, QG.I, QG.X, QG.X],[QG.X, QG.Y, QG.I, QG.X, QG.X, QG.X, QG.Y, QG.Z, QG.X, QG.X],[QG.Y, QG.H, QG.I, QG.X, QG.X, QG.Y, QG.I, QG.Z, QG.X, QG.Y],[QG.H, QG.I, QG.I, QG.H, QG.Y, QG.X, QG.H, QG.I, QG.X, QG.H],[QG.Y, QG.I, QG.I, QG.X, QG.X, QG.X, QG.I, QG.I, QG.X, QG.H],[QG.Z, QG.Z, QG.I, QG.H, QG.Y, QG.Z, QG.H, QG.I, QG.Z, QG.Y]]
            | _ => raise Fail "Illegal gate set"



end