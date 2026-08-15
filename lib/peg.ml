(** The six peg colors a Mastermind code is built from. *)

type t = Red | Blue | Green | Yellow | Orange | Purple

let all = [ Red; Blue; Green; Yellow; Orange; Purple ]

let to_string = function
  | Red -> "rouge"
  | Blue -> "bleu"
  | Green -> "vert"
  | Yellow -> "jaune"
  | Orange -> "orange"
  | Purple -> "violet"

let of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "rouge" -> Some Red
  | "bleu" -> Some Blue
  | "vert" -> Some Green
  | "jaune" -> Some Yellow
  | "orange" -> Some Orange
  | "violet" -> Some Purple
  | _ -> None

(* Matches the palette the original game used: not a "true" 6-color ANSI
   set, but kept as-is since it's part of the game's look. *)
let ansi_code = function
  | Red -> "31"
  | Blue -> "34"
  | Green -> "32"
  | Yellow -> "36"
  | Orange -> "33"
  | Purple -> "35"

let colorize peg = Ansi.wrap (ansi_code peg) (to_string peg)
