open Core.Std

let print_indent (out: out_channel) (indent: int) =
  output_string out (String.make indent ' ')

let print_statements
    (out: out_channel)
    (stmts: 'a list)
    ~(f: out_channel -> 'a -> indent:int -> unit)
    ~(indent: int) =
  let print_statement_indented out stmt = f out stmt ~indent in
  List.iter stmts ~f: (fun stmt ->
      fprintf out "%a%a\n"
        print_indent indent
        print_statement_indented stmt
    )
