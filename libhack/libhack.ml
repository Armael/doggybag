type file_descr

external memfd_create : unit -> file_descr = "caml_memfd_create"
external outchannel_of_file_descr : file_descr -> out_channel = "caml_outchannel_of_file_descr"
external execv_memfd : file_descr -> string array -> 'a = "caml_execv_memfd"
external execv : string -> string array -> 'a = "caml_my_execv"
external putenv : string -> string -> unit = "caml_my_putenv"

let with_open_in fn f =
  let ic = open_in_bin fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> f ic)
