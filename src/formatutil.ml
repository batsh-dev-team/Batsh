open Core_kernel.Std

let print_indent (buf : Buffer.t) (indent : int) =
  Buffer.add_string buf (String.make indent ' ')

let print_statements
    (buf : Buffer.t)
    (stmts : 'a list)
    ~(f : Buffer.t -> 'a -> indent:int -> unit)
    ~(indent : int) =
  let print_statement_indented buf stmt = f buf stmt ~indent in
  let num_stmts = List.length stmts in
  List.iteri stmts ~f: (fun i stmt ->
      print_statement_indented buf stmt;
      if i < num_stmts - 1 then
        Buffer.add_string buf "\n"
    )

let print_separate_list
    (buf : Buffer.t)
    (elements : 'a list)
    ~(f : Buffer.t -> 'a -> unit)
    ~(separator : string) =
  let num_elements = List.length elements in
  List.iteri elements ~f: (fun i element ->
      f buf element;
      if i < num_elements - 1 then
        Buffer.add_string buf separator
    )

let escaper = Staged.unstage (
    String.Escaping.escape_gen_exn
      ~escapeworthy_map: [
        ('\n', 'n');
        ('\r', 'r');
        ('\"', '"')]
      ~escape_char: '\\'
  )

let escape (str : string) : string =
  escaper str
