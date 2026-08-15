(** The AI's code-breaking strategies. *)

type strategy = Random | Minimax

let strategies = [ Random; Minimax ]

let name = function
  | Random -> "aleatoire"
  | Minimax -> "Knuth (minimax)"

let consistent_with ~guess ~feedback candidates =
  List.filter (fun code -> Secret_code.feedback ~secret:code ~guess = Some feedback) candidates

(* Knuth's classic opening guess: two pegs of one color, two of another. *)
let opening_guess =
  match Peg.all with
  | first :: second :: _ -> [ first; first; second; second ]
  | _ -> failwith "Peg.all must have at least two colors"

(* For each remaining candidate guess, find the response an adversarial
   code-setter would give to keep as many secrets alive as possible, then
   pick the guess that minimizes that worst case. This is Knuth's minimax
   algorithm for Mastermind. *)
let best_minimax_guess ~history ~candidates =
  let untried = List.filter (fun code -> not (List.mem code history)) Secret_code.all in
  let worst_case_remaining guess =
    List.fold_left
      (fun worst feedback -> max worst (List.length (consistent_with ~guess ~feedback candidates)))
      0 Secret_code.all_feedbacks
  in
  let better (guess, score) (candidate_guess, candidate_score) =
    if candidate_score < score then (candidate_guess, candidate_score) else (guess, score)
  in
  match untried with
  | [] -> List.hd candidates
  | first :: rest ->
      List.fold_left
        (fun best guess -> better best (guess, worst_case_remaining guess))
        (first, worst_case_remaining first)
        rest
      |> fst

let choose strategy ~history ~candidates =
  match strategy with
  | Random -> List.nth candidates (Random.int (List.length candidates))
  | Minimax -> ( match history with [] -> opening_guess | _ -> best_minimax_guess ~history ~candidates )

let filter strategy ~guess ~feedback candidates =
  match strategy with
  | Random -> candidates
  | Minimax -> consistent_with ~guess ~feedback candidates
