signature OPTIMIZER = sig

    val get_fingerprint : QGenerator.tile * QGenerator.circuit_identities -> QGenerator.fingerprint

    (* val split_column : QGenerator.column * QGenerator.height * QGenerator.height-> QGenerator.column list

    val split_all_cols : QGenerator.tile * QGenerator.height -> QGenerator.column list list

    val split_tile_depth : QGenerator.tile * QGenerator.height * QGenerator.depth * QGenerator.depth -> QGenerator.tile list

    val create_depth_lists : QGenerator.tile list -> QGenerator.tile list

    val split_all_depths : QGenerator.tile list * QGenerator.depth -> QGenerator.tile list *)

    val optimize_tile_partition : QGenerator.tile list * QGenerator.database -> QGenerator.tile list

    val circuit_to_tile_partition: QGenerator.tile * QGenerator.height * QGenerator.depth -> QGenerator.tile list

    val remove_I_columns: QGenerator.tile -> QGenerator.tile

    val tile_partition_to_circuit : QGenerator.tile list * QGenerator.height * QGenerator.depth -> QGenerator.tile

    val optimization_pass : QGenerator.tile * QGenerator.database * QGenerator.height * QGenerator.depth -> QGenerator.tile  

    (*val optimize_circuit : QGenerator.tile * QGenerator.gate list * int *QGenerator.height * QGenerator.depth -> QGenerator.tile*)
end
