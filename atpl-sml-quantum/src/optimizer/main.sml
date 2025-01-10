open Generator
(*val () = print (pp_column_list (make_init_columns ([X, Y], 2)))*)
val () = print (pp_column_list [[X, X], [Y, Y], [X, Y], [Y, X]])