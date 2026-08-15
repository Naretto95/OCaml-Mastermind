(** Minimal helpers for coloring terminal output with ANSI escape codes. *)

let wrap code text = "\027[" ^ code ^ "m" ^ text ^ "\027[0m"

let red = wrap "31"
let green = wrap "32"
let blue = wrap "34"
let yellow = wrap "33"
let cyan = wrap "36"
let magenta = wrap "35"
