signature QGENERATOR = sig 
    datatype gate = I | X | Y | Z | H (*For now only these 1-qubit gates are handled*)

    type ('key, 'val) hash_map = ('key, 'val) Table.table

    type column = gate list
    type tile = column list
    type fingerprint = real
    type depth = int
    type height = int
 
    type circuit_identities = (tile, fingerprint) hash_map 

    type fingerprints = (fingerprint, (tile * depth)) hash_map (* should also store the cost of the tile (here the depth) *)
    type database = circuit_identities * fingerprints

    val generator : gate list * height * depth -> database
    val hash : tile -> fingerprint
    val gate_to_circuit_gate : gate -> Circuit.t
    val circuit_gate_to_gate : Circuit.t -> gate
    val make_init_columns : gate list * height -> column list
    val make_tiles : gate list * height * depth -> tile list
    (* Only here for testing puposes*)
    val column_to_circuit : column -> Circuit.t
    val tile_to_circuit : tile -> Circuit.t

    (* Here not only for testing purposes *)
    val tile_to_matrix : tile -> Semantics.mat

    (*Fingerprints dimser*)
    val gen_random_complex : Random.generator -> Complex.complex
    val gen_random_complex_list : Random.generator * int -> Complex.complex list
    val normalize_complex_list : Complex.complex list -> Complex.complex list
    val gen_fingerprint : tile * Complex.complex list * Complex.complex list -> real
    val tile_to_word : tile -> word
    val fingerprint_equality : fingerprint * fingerprint -> bool

    val cost : tile -> depth

    val pp_list : 'a list * ('a -> string) -> string
    val pp_gate : gate -> string 
    val pp_column : column -> string
    val pp_column_list: column list -> string
    val pp_tile_list : tile list -> string
    val pp_database : database -> string
end