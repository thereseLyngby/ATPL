structure QGenerator : QGENERATOR = struct
  datatype gate = I | X | Y | Z | H  (*For now only these 1-qubit gates are handled*)

  type ('key, 'val) hash_map = ('key, 'val) Table.table

  type column = gate list
  type tile = column list
  type fingerprint = real
  type depth = int
  type height = int
 
  type circuit_identities = (tile, fingerprint) hash_map 

  type fingerprints = (fingerprint, (tile * depth)) hash_map (* should also store the cost of the tile (here the depth) *)
  type database = circuit_identities * fingerprints 

  fun pow2 (n : int) : int =
    case n of
        0 => 1
      | _ => 2 * (pow2 (n-1))
  

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
  
  (* Code for Fingerprinting starts here *) 

  (* Generate random complex numbers *)
  fun gen_random_complex (rand_gen : Random.generator) : Complex.complex =
    let val a = Random.random rand_gen
        val b = Random.random rand_gen
    in Complex.mk (a, b) end
  
  (*For generating psi_1, psi_0 for fingerprints*)
  (* n=2^(num qubits in tile) *)
  fun gen_random_complex_list (rand_gen : Random.generator, n : int) =
    let val alist = Random.randomlist (n, rand_gen)
        val blist = Random.randomlist (n, rand_gen)
        in ListPair.map Complex.mk (alist, blist) end

  (* Normalize vector, s.t. complex vector is a complex state *)
  fun normalize_complex_list (comp_list : Complex.complex list) : Complex.complex list =
    let val norm = Math.sqrt (foldl (fn (elm, acc) => Complex.re (Complex.* (elm, Complex.conj elm)) + acc ) 0.0 comp_list)
    in map (fn c_val => Complex./ (c_val, Complex.fromRe norm) ) comp_list end

  (*  *)
  fun gen_fingerprint (tile : tile, psi_0 : Complex.complex list, psi_1 : Complex.complex list) : real =
    let val psi_1_state = Vector.fromList psi_1 
        val res_1 = Semantics.interp (tile_to_circuit tile) psi_1_state
        fun complex_dot_product (avec : Complex.complex list, bvec : Complex.complex list) : Complex.complex =
          case (avec, bvec) of
              ([], [])  => Complex.fromInt 0
            | ([], _)   => raise Fail "Vectors not same length"
            | (_, [])   => raise Fail "Vectors not same length"
            | (a::atail, b::btail) =>
               Complex.+ (Complex.* (a, b), complex_dot_product(atail, btail))
        in Complex.mag (complex_dot_product (psi_0, Vector.foldl(fn (elm, acc) => elm :: acc ) [] res_1))
        end
  
  fun gate_to_num (gate : gate) : string =
    case gate of
        I => "1"
      | X => "2"
      | Y => "3"
      | Z => "4"
      | H => "5"

  fun tile_to_word (tile : tile) : word =
    let val str = (
      Word.fromString (
        foldr (fn (col, acc) => (
          foldr (fn (gate, acc2) => gate_to_num (gate) ^ acc2) "" col) ^ acc)
        "" tile))
    in
      case str of 
          NONE => raise Fail "Stoopid, badie baddie u fuck up. Could not convert from int to str"
        | SOME(s) => s
    end
  
  (*fun fingerprint_to_word (fp : fingerprint) : word =
    let val str = Real.toString fp
    in case Word.fromString of
        NONE => "Dummy dummy"
      | Some
  *)
  (*fun fingerprint_to_word (fp:real) : word = (Word8Vector.foldl word8Hash 0w0 o PackReal.toBytes) fp
    hvor word8Hash : Word8.word * word -> word*)

  fun fingerprint_to_word (fp : fingerprint) : word =
    Word.fromInt (Real.floor (fp / 0.000000000000002))


  fun fingerprint_equality (f0 : fingerprint, f1 : fingerprint) : bool =
    Real.abs (f0-f1) < 0.000000000000001 (* 10^-15 as in Quartz paper *)

(* Make databases *)
  fun make_identities (tlst : tile list, psi0 : Complex.complex list, psi1 : Complex.complex list) : circuit_identities =
    let val identities_db = Table.new {hash= tile_to_word, eq= fn (t0, t1) => t0 = t1}
        val _ = map (fn tile => Table.add (tile, gen_fingerprint(tile, psi0, psi1), identities_db)) tlst
    in
      identities_db
    end

  fun update_fingerprints_db (fp_db : fingerprints, cur_tile : tile, cur_fp : fingerprint) : unit =
    let val cost_cur_tile = cost cur_tile
    in case Table.lookup fp_db cur_fp of
        NONE => Table.add (cur_fp, (cur_tile, cost_cur_tile), fp_db)
      | SOME (old_tile, old_cost) =>
        if cost_cur_tile < old_cost then
          Table.add (cur_fp, (cur_tile, cost_cur_tile), fp_db)
        else
          ()
    end

  fun make_fingerprints(circuit_identities : circuit_identities) : fingerprints =
    let val fingerprints_db = Table.new {hash= fingerprint_to_word, eq = fingerprint_equality}
        val _ = Table.Map (fn (tile, fp) => update_fingerprints_db(fingerprints_db, tile, fp)) circuit_identities 
    in
      fingerprints_db
    end


  fun generator (gates : gate list, height : height, depth : depth) : database =
    let val tiles = make_tiles (gates, height, depth)
        val rand_gen = Random.newgen ()
        val n = pow2 height
        val psi0 = normalize_complex_list (gen_random_complex_list (rand_gen, n))
        val psi1 = normalize_complex_list (gen_random_complex_list (rand_gen, n)) 
        val circuit_identities = make_identities(tiles, psi0, psi1)
        val fingerprints = make_fingerprints(circuit_identities)
    in
      raise Fail "Hi Jóhann :3"
    end

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