structure QGenerator : QGENERATOR = struct
  datatype gate = I | X | Y | Z | H  (*For now only these 1-qubit gates are handled*)

  datatype ('key, 'val) hash_map = Hash_map of 'key -> 'val

  type column = gate list
  type tile = column list
  type fingerprint = real
  type depth = int
  type height = int
 
  type circuit_identities = (tile, fingerprint) hash_map 

  type fingerprints = (fingerprint, (tile * depth)) hash_map (* should also store the cost of the tile (here the depth) *)
  type database = circuit_identities * fingerprints 

  fun generator (gates,height,depth) : database = raise Fail "not implemented"

  fun hash file = raise Fail "not implemented"

  fun gate_to_circuit_gate I = Circuit.I
    | gate_to_circuit_gate X = Circuit.X
    | gate_to_circuit_gate Y = Circuit.Y
    | gate_to_circuit_gate Z = Circuit.Z
    | gate_to_circuit_gate H = Circuit.H

  fun circuit_gate_to_gate (t : Circuit.t) =
      case t of
           Circuit.I => I
         | Circuit.X => X
         | Circuit.Y => Y
         | Circuit.Z => Z
         | Circuit.H => H
         | _ => raise Fail "Stoopid, bad gate"

  fun make_init_columns (gates, height) : column list =
    case height of 
        0 => raise Fail "height must be positive"
      | 1 => map (fn g => [g]) gates
      | n => foldl (fn (gate, res) => (map (fn list => gate :: list) (make_init_columns (gates, height-1))) @ res) [] gates 

  (* wait we need to keep all depths up to depth *)
  fun make_tiles (gates, height, depth) : tile list =
    let fun make_tiles1 (d : depth, init_col : column list) : tile list =
      case d of 
          0 => raise Fail "depth must be positive"
        | 1 => map (fn t => [t]) init_col 
        | n => let val prev_step = make_tiles1 (d-1, init_col)
               in (foldl (fn (col, acc) => (map (fn tile => col :: tile) prev_step) @ acc) [] init_col)
               end
    in make_tiles1 (depth, make_init_columns (gates, height))
    end

  fun cost (tile : tile) : depth =
    foldl (fn (col, acc) => if (List.all (fn g => g = I) col) then acc else acc + 1) 0 tile

  fun column_to_circuit (col : column) : Circuit.t =
    case col of
        [] => raise Fail "Empty Columns not allowed\n"
      | g::gs => foldl (fn (g0, g_acc) => Circuit.Tensor(g_acc, (gate_to_circuit_gate g0))) (gate_to_circuit_gate g) gs


  fun tile_to_circuit (tile : tile) : Circuit.t =
    case tile of
        [] => raise Fail "Empty Tiles not allowed\n"
      | [[]] => raise Fail "Empty Tiles not allowed\n"
      | col::cols => foldl (fn (c0, c_acc) => Circuit.Seq(c_acc,(column_to_circuit c0))) (column_to_circuit col) cols

  fun tile_to_matrix (tile : tile) : Semantics.mat =
    Semantics.sem (tile_to_circuit tile)
  
  fun gen_random_complex (rand_gen : Random.generator) : Complex.complex =
    let val a = Random.random rand_gen
        val b = Random.random rand_gen
    in Complex.mk (a, b) end
  
  fun gen_random_complex_list (rand_gen : Random.generator, n : int) =
    let val alist = Random.randomlist (n, rand_gen)
        val blist = Random.randomlist (n, rand_gen)
        in ListPair.map Complex.mk (alist, blist) end

  fun normalize_complex_list (comp_list : Complex.complex list) : Complex.complex list =
    let val norm = Math.sqrt (foldl (fn (elm, acc) => Complex.re (Complex.* (elm, Complex.conj elm)) + acc ) 0.0 comp_list)
    in map (fn c_val => Complex./ (c_val, Complex.fromRe norm) ) comp_list end

(* REALM OF THE PP *)
  fun pp_list (list : 'a list, pp_a : 'a -> string) =
    let fun pp_list1 (lst : 'a list) =
      case lst of
          [] => ""
        | [elm] => pp_a elm
        | elm::tail => (pp_a elm) ^ ", " ^ (pp_list1 tail)
    in "[" ^ (pp_list1 list) ^ "]"
    end

  fun pp_gate gate = Circuit.pp (gate_to_circuit_gate gate)

  fun pp_column (col : column) =
    pp_list (col, pp_gate)

  (* Note, column list = tile *)
  fun pp_column_list (columns : column list) =  pp_list (columns, pp_column)

  fun pp_tile (t : tile) = pp_column_list t

  fun pp_tile_list (tiles : tile list) =  pp_list (tiles, pp_tile)
end