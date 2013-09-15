val print_indent : out_channel -> int -> unit

val print_statements :
  out_channel -> 'a list -> f:(out_channel -> 'a -> indent:int -> unit)
  -> indent:int -> unit

val print_separate_list :
  out_channel -> 'a list -> f:(out_channel -> 'a -> unit)
  -> separator:string -> unit

val escape : string -> string
