val () = print "Testing: Tile to Matrix\n"

structure G = Generator

val res = tile_to_matrix [[G.I, G.I], [G.I, G.I]]

val matrix = Matrix.fromList [[1, 0, 0, 0],
                             [0, 1, 0, 0],
                             [0, 0, 1, 0],
                             [0, 0, 0, 1]]

val () = print (Bool.toString (res = matrix) ^ "\n")