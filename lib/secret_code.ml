(** A code is a fixed-length sequence of pegs: the thing that must be
    guessed, and the shape a guess takes. *)

type t = Peg.t list

let length = 4

(* Every possible code, built as the cartesian product of [Peg.all] with
   itself [length] times. *)
let all : t list =
  let rec combinations n =
    if n = 0 then [ [] ]
    else
      let tails = combinations (n - 1) in
      List.concat_map (fun peg -> List.map (fun tail -> peg :: tail) tails) Peg.all
  in
  combinations length

let random () = List.nth all (Random.int (List.length all))

let to_string code = String.concat " " (List.map Peg.colorize code)

let of_string s =
  let words = String.split_on_char ' ' (String.trim s) |> List.filter (( <> ) "") in
  if List.length words <> length then None
  else
    let rec parse = function
      | [] -> Some []
      | word :: rest -> (
          match (Peg.of_string word, parse rest) with
          | Some peg, Some tail -> Some (peg :: tail)
          | _ -> None )
    in
    parse words

(* (black, white): black pegs are the right color in the right spot, white
   pegs are the right color in the wrong spot. Computed as "how many pegs
   match by color, ignoring position" minus the exact matches, which is the
   standard way to score Mastermind without the "remove a peg once it has
   been matched" bookkeeping. *)
let feedback ~secret ~guess =
  if List.length secret <> List.length guess then None
  else
    let black = List.fold_left2 (fun n s g -> if s = g then n + 1 else n) 0 secret guess in
    let occurrences code peg = List.length (List.filter (( = ) peg) code) in
    let color_matches =
      List.fold_left
        (fun n peg -> n + min (occurrences secret peg) (occurrences guess peg))
        0 Peg.all
    in
    Some (black, color_matches - black)

(* Every (black, white) pair a code of this length can produce.
   (length - 1, 1) is impossible: if all but one peg are exact matches, the
   last one has nowhere else to go but to also match. *)
let all_feedbacks =
  List.init (length + 1) (fun black -> black)
  |> List.concat_map (fun black ->
         List.init (length + 1) (fun white -> white)
         |> List.filter_map (fun white ->
                if black + white <= length && not (black = length - 1 && white = 1) then
                  Some (black, white)
                else None))
