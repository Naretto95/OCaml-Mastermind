(** Small terminal helpers shared by the CLI. *)

let clear_screen () =
  let command = if Sys.win32 then "cls" else "clear" in
  ignore (Sys.command command)

let pause message =
  print_string message;
  ignore (read_line ())
