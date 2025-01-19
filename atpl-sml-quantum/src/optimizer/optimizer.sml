structure Optimizer : OPTIMIZER = struct
  
  structure QG = QGenerator


  fun get_fingerprint (tile : QG.tile, c_ids : QG.circuit_identities) : QG.fingerprint =
    case Table.lookup c_ids tile of 
        NONE => raise Fail "Missing Tile in Circuit Identities"
      | SOME fp => fp 

  fun get_best_tile (fp : QG.fingerprint, fps : QG.fingerprints) : QG.tile =
    case Table.lookup fps fp of
        NONE => raise Fail "Missing Fingerprint in Fingerprints"
      | SOME (tile, _) => tile 

  fun best_tile (tile : QG.tile, (circuit_ids : QG.circuit_identities, 
                 fps : QG.fingerprints) : QG.database) : QG.tile =
    let val fp = get_fingerprint (tile, circuit_ids)  
    in get_best_tile (fp, fps) end      

  fun rep_identity_gate (height : QG.height) : QG.gate list =
    case height of
        0 => []
      | n => QG.I :: rep_identity_gate (n - 1)

  fun split_column (col : QG.column, 
                    og_height : QG.height, 
                    grid_height : QG.height) : QG.column list =
    if og_height < grid_height then 
        if og_height < 1 then
          []
        else
          [col @ rep_identity_gate (grid_height - og_height)]
    else 
      List.take (col, grid_height) :: split_column (List.drop (col, grid_height), 
                                               og_height - grid_height, 
                                               grid_height)

  fun rep_col (col : QG.column, depth : QG.depth) : QG.column list =
    case depth of 
        0 => []
      | n => col :: rep_col (col, depth - 1)  

  fun split_tile_depth (tile : QG.tile, (* Entire circuit, represented as a big tile *)
                        height : QG.height, (* Tile height*)
                        og_depth : QG.depth, (* Depth of entire circuit *)
                        grid_depth : QG.depth) : QG.tile list = (* Tile depth *)
    if og_depth < grid_depth then 
      if og_depth < 1 then 
        []
      else
        [tile @ rep_col (rep_identity_gate (height), grid_depth - og_depth)]
    else 
      List.take (tile, grid_depth) :: split_tile_depth (List.drop (tile, grid_depth),
                                                   height,
                                                   og_depth - grid_depth,
                                                   grid_depth)

  fun split_all_cols (tile : QG.tile, 
                      height: QG.height) : QG.column list list =
    List.map (fn col => (split_column (col, List.length col, height))) tile

  fun create_depth_lists (split_columns : QG.tile list) : QG.tile list =
    let val transposed_cols = Matrix.listlist (Matrix.transpose (Matrix.fromListList split_columns))
    in transposed_cols end


  fun split_all_depths (depth_lists : QG.tile list, grid_depth : QG.depth) : QG.tile list =
      let val og_depth = List.length (List.hd depth_lists)
          val height = List.length (List.hd (List.hd depth_lists))
      in
        List.foldl (fn (tile, acc) => acc @ split_tile_depth (tile,  height, og_depth, grid_depth)) [] depth_lists
      end
  
  fun find_optimal_tile (tile : QG.tile, db : QG.database) : QG.tile =
    let val (ci_db, fp_db) = db
        val tile_cost = QG.cost tile
        in
          case Table.lookup ci_db tile of
              NONE => raise Fail "Incorrect generation of circuit identities"
            | SOME fp => 
              case Table.lookup fp_db fp of
                  NONE => raise Fail "Incorrect fingerprint database generation"
                | SOME (t_new, depth) =>
                  t_new
    end

        
  fun optimize_tile_partition (c_tiles : QG.tile list, db : QG.database) : QG.tile list =
    List.map (fn tile => find_optimal_tile (tile, db)) c_tiles

  (* Create list of non-overlapping tiles of dimension tile_height x tile_depth in circuit represented as a tile *)
  fun circuit_to_tile_partition (circuit : QG.tile, tile_height : QG.height, tile_depth : QG.depth) : QG.tile list =
    let val column_split = split_all_cols (circuit, tile_height)
        val row_split = create_depth_lists column_split
        in split_all_depths (row_split, tile_depth)
    end
  
  fun move_Is_right (column : QG.column) : QG.column =
    List.foldr (fn (gate, acc) =>
      case gate of
          QG.I => QG.I :: acc 
        | t => acc @ [t]
    ) [] column

  (* Gather I's at end of circuit *)
  fun move_Is_in_circuit (circuit : QG.tile ) : QG.tile =
    let val circuit_transpose = Matrix.listlist (Matrix.transpose (Matrix.fromListList circuit))
        val circuit_Is_moved_right = List.map (fn col => move_Is_right col) circuit_transpose
    in Matrix.listlist (Matrix.transpose (Matrix.fromListList circuit_Is_moved_right))
    end
  
  (* Idea: to improve further, move all gates as far left as possible, and all I's to the far right*)
  fun remove_I_columns (circuit : QG.tile) : QG.tile =
    let val circuit_Is_moved_right = move_Is_in_circuit circuit
        val column_height = List.length (List.hd circuit_Is_moved_right)
        val circuit_Is_removed = 
          List.foldl (fn (col, acc) => 
            if List.all (fn gate => gate = QG.I) col then
              acc
            else
              col :: acc
            ) [] circuit_Is_moved_right
        in
          case circuit_Is_removed of
              [] => [List.tabulate (column_height, fn i => QG.I)]
            | circuit => circuit
    end

  (* Gather all columns in column cur_column from tile partitioning *)
  fun helper (columns : QG.column list, num_columns : int, cur_column : int) : QG.column list =
    case columns of
       [] => []
      | _ => 
        if num_columns <= List.length columns then
          let val row = List.take (columns, num_columns)
          in 
            if cur_column < List.length (row) then
              if num_columns <= List.length columns then
                (List.nth (row, cur_column)) :: (helper (List.drop (columns, num_columns), num_columns, cur_column))
              else
                raise Fail "Cannot drop more columns than number of existing columns."
            else raise Fail "Try to take an entry from a list that does not exist."
          end
        else
          raise Fail ("Length of columns= " ^ (Int.toString (List.length columns)) ^ ", num_columns =" ^(Int.toString num_columns))
 
  fun tile_partition_to_circuit (tiles : QG.tile list, original_height : QG.height,original_depth : QG.depth) : QG.tile =
    let val num_horzontal_tiles = original_height div (List.length (List.hd tiles)) (* Should always have no remainder *)
        val flatten_tiles = List.concat tiles
    in
      List.tabulate (original_depth,
        fn col_idx => 
          List.concat(helper(flatten_tiles, original_depth, col_idx))
      )
    end

  (* A single optimization pass of a circuit*)
  
  fun optimization_pass (circuit : QG.tile, tile_db : QG.database, tile_height : QG.height, tile_depth : QG.depth) : QG.tile =
    let 
        val og_height = List.length (List.hd circuit)
        val og_depth = List.length circuit
        val circuit_tile_partition = circuit_to_tile_partition (circuit, tile_height, tile_depth)
        val optimized_tile_partition = optimize_tile_partition (circuit_tile_partition, tile_db)
        val optimized_circuit = 
          tile_partition_to_circuit (
            optimized_tile_partition 
            ,og_height + ((tile_height - (og_height mod tile_height)) mod tile_height)
            , og_depth + ((tile_depth - ((og_depth mod tile_depth))) mod tile_depth))
        val optimized_circuit_column_padding_removed =
          List.map (fn col => List.take (col, og_height)) optimized_circuit
        val optimized_circuit_I_cols_removed = remove_I_columns (optimized_circuit_column_padding_removed)
    in optimized_circuit_I_cols_removed
    end
    
  fun loop (circuit : QG.tile, db: QG.database, tile_height : QG.height, tile_depth : QG.depth, iterations : int) : QG.tile =
    case iterations of
        0 => circuit
      | _ =>
        loop(optimization_pass(circuit, db, tile_height, tile_depth), db, tile_height, tile_depth, iterations-1)

  fun optimize_circuit (circuit : QG.tile, gate_set : QG.gate list, iterations : int, tile_height : QG.height, tile_depth : QG.depth) : QG.tile =
    let val db = QG.generator(gate_set, tile_height, tile_depth)
    in loop (circuit, db, tile_height, tile_depth, iterations) end

end