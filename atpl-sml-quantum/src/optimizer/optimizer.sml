open QGenerator

structure Optimizer : OPTIMIZER = struct

  type circuit = (tile, depth, height)

  fun get_fingerprint (tile : tile, c_ids : circuit_identities) : fingerprint =
    case Table.lookup c_ids tile of 
        NONE => raise Fail "Missing Tile in Circuit Identities"
      | SOME fp => fp 

  fun get_best_tile (fp : fingerprint, fps : fingerprints) : tile =
    case Table.lookup fps fp of
        NONE => raise Fail "Missing Fingerprint in Fingerprints"
      | SOME (tile, _) => tile 

  fun best_tile (tile : tile, (circuit_ids : circuit_identities, 
                 fps : fingerprints) : database) : tile =
    let val fp = get_fingerprint (tile, circuit_ids)  
    in get_best_tile (fp, fps) end      

  fun rep_identity_gate (height : height) : gate list =
    case height of
        0 => []
      | n => I :: rep_identities (n - 1)

  fun split_column (col : column, 
                    og_height : height, 
                    grid_height : height) : column list =
    if og_height < grid_height then 
      [col @ rep_identity_gate (grid_height - og_height)]
    else 
      take (col, grid_height) :: split_column (drop (col, grid_height), 
                                               og_height - grid_height, 
                                               grid_height)

  fun rep_col (col : column, depth : depth) : column list =
    case depth of 
        0 => []
      | n => col :: rep_col (col, depth - 1)  

  fun split_tile_depth (tile : tile, 
                        height : height
                        og_depth : depth,
                        grid_depth : depth) : tile list =
    if og_depth < grid_depth then 
      [tile @ rep_col (rep_identity_gate height, grid_depth - og_depth)]
    else 
      take (tile, grid_depth) :: split_tile_depth (drop (tile, grid_depth),
                                                   og_depth - grid_depth,
                                                   grid_depth)

  fun gather (tiles : 'a list list) : 'a list list =
    
  fun foo (list_list : 'a list list) : ('a list, 'a list list) = 
    case list_list of 
      (list::lists) => map  
  list_list => ('head_list, tail_list)
    
  fun split_circuit ((og_t, og_height, og_depth) : circuit, 
                   grid_height : height, 
                   grid_depth : depth) : tile list =
    let val split_depth = 
      split_tile_depth (og_t, og_height, og_depth, grid_depth)
        val split_height = 
          map (fn col => split_column (col, og_height, grid_height)) split_depth
    in gather split_height end




end