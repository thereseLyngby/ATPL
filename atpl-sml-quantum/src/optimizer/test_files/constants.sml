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
    fun med_tile_dim (a : unit) : QG.height * QG.depth = (2, 3)
    fun large_tile_dim (a : unit) : QG.height * QG.depth = (3, 4)

    fun small_gate_set (a : unit) : QG.gate list =
        [QG.I, QG.X]
    fun medium_gate_set (a : unit) : QG.gate list =
        [QG.I, QG.X, QG.H]
    fun large_gate_set (a : unit) : QG.gate list =
        [QG.I, QG.X, QG.Y, QG.Z, QG.H]

    fun small_circuit (gS : QG.gate list) : QG.tile =
        case gS of
            [QG.I, QG.X]                        =>
                [[QG.X, QG.I, QG.I], [QG.X, QG.X, QG.X], [QG.I, QG.I, QG.X]]
            | [QG.I, QG.X, QG.H]                =>
                [[QG.X, QG.H, QG.I], [QG.X, QG.X, QG.X], [QG.I, QG.I, QG.H]]
            | [QG.I, QG.X, QG.Y, QG.Z, QG.H]    =>
                [[QG.X, QG.H, QG.I], [QG.Z, QG.Y, QG.X], [QG.Y, QG.Z, QG.H]]
            | _ => raise Fail "Illegal gate set"

    fun med_circuit (gS : QG.gate list) : QG.tile =
        case gS of
            [QG.I, QG.X]                        =>
                [[QG.X, QG.I, QG.I], [QG.X, QG.X, QG.X], [QG.I, QG.I, QG.X]]
            | [QG.I, QG.X, QG.H]                =>
                [[QG.X, QG.H, QG.I], [QG.X, QG.X, QG.X], [QG.I, QG.I, QG.H]]
            | [QG.I, QG.X, QG.Y, QG.Z, QG.H]    =>
                [[QG.X, QG.H, QG.I], [QG.Z, QG.Y, QG.X], [QG.Y, QG.Z, QG.H]]
            | _ => raise Fail "Illegal gate set"


    fun big_circuit (gS : QG.gate list) : QG.tile =
        case gS of
            [QG.I, QG.X]                        =>
                [[QG.X, QG.I, QG.I], [QG.X, QG.X, QG.X], [QG.I, QG.I, QG.X]]
            | [QG.I, QG.X, QG.H]                =>
                [[QG.X, QG.H, QG.I], [QG.X, QG.X, QG.X], [QG.I, QG.I, QG.H]]
            | [QG.I, QG.X, QG.Y, QG.Z, QG.H]    =>
                [[QG.X, QG.H, QG.I], [QG.Z, QG.Y, QG.X], [QG.Y, QG.Z, QG.H]]
            | _ => raise Fail "Illegal gate set"



end