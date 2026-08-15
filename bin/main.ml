(* Mastermind: a game of Player vs AI, each guessing the other's secret
   code in turn. See lib/ for the game rules and AI strategies; this file
   is the terminal interface tying them together. *)

open Mastermind

(* ---- Welcome screen ------------------------------------------------- *)

let print_banner () =
  let bar = Ansi.blue " -----------------------------------------" in
  print_string bar;
  print_newline ();
  print_string (Ansi.blue "[  ");
  print_string (Ansi.red "M A S T E R M I N D");
  print_string (Ansi.blue "  ]");
  print_newline ();
  print_string bar;
  print_newline ()

(* ---- Small input prompts, each retrying until the answer is valid --- *)

let prompt_non_empty label =
  let rec loop () =
    print_string label;
    let answer = String.trim (read_line ()) in
    if answer = "" then loop () else answer
  in
  loop ()

let prompt_int_in_range ~label ~min ~max =
  let rec loop () =
    print_string label;
    match try Some (read_int ()) with _ -> None with
    | Some n when n >= min && n <= max -> n
    | _ -> loop ()
  in
  loop ()

let prompt_bool label =
  let rec loop () =
    print_string label;
    match String.lowercase_ascii (String.trim (read_line ())) with
    | "true" -> true
    | "false" -> false
    | _ -> loop ()
  in
  loop ()

let round_up_to_even n = if n mod 2 = 0 then n else n + 1

type settings = { player_name : string; attempts : int; rounds : int; auto_answer : bool }

let prompt_settings () =
  print_banner ();
  print_newline ();
  print_string "Bienvenue dans le Mastermind !!!";
  print_newline ();
  print_newline ();
  print_string "Pour lancer le jeu vous devez renseigner vos préférences ci-dessous :";
  let player_name = prompt_non_empty "\n- Votre pseudo : " in
  let attempts =
    prompt_int_in_range
      ~label:"\n- Le nombre de tentatives pour résoudre le code par partie (entier entre 2 et 20) : "
      ~min:2 ~max:20
  in
  let rounds =
    prompt_int_in_range
      ~label:
        "\n\
         - Le nombre de parties (entier entre 2 et 20, arrondi au nombre pair supérieur si besoin) : "
      ~min:2 ~max:20
    |> round_up_to_even
  in
  let auto_answer =
    prompt_bool
      "\n\
       - Entrez true pour laisser le programme répondre à l'IA à votre place, false pour répondre \
       vous-même : "
  in
  { player_name; attempts; rounds; auto_answer }

(* ---- Reading a code from the player ---------------------------------- *)

let color_legend () = String.concat ", " (List.map Peg.colorize Peg.all)

let print_format_hint () =
  print_string
    "pour que votre réponse soit compréhensible, veuillez laisser un seul espace entre chaque nom de \
     couleur : ";
  print_newline ();
  Printf.printf "Les couleurs possibles sont le {%s} : " (color_legend ())

let rec read_code () =
  match Secret_code.of_string (read_line ()) with
  | Some code -> code
  | None ->
      print_string "Entrée invalide, veuillez réessayer : ";
      read_code ()

let rec read_feedback () =
  print_string "blancs = ";
  let white = try read_int () with _ -> -1 in
  print_string "noirs = ";
  let black = try read_int () with _ -> -1 in
  if black >= 0 && white >= 0 && black + white <= Secret_code.length then (black, white)
  else begin
    print_string "Réponse invalide, veuillez réessayer.\n";
    read_feedback ()
  end

let print_attempt (guess, (black, white)) =
  Printf.printf "\n%s | noirs: %s, blancs: %s" (Secret_code.to_string guess)
    (Ansi.green (string_of_int black))
    (Ansi.red (string_of_int white))

let print_board history =
  List.iter print_attempt (List.rev history);
  print_newline ()

(* ---- A single round -------------------------------------------------- *)

type outcome = Player_wins | Ai_wins

let rec player_breaks_code ~secret ~max_attempts ~attempt ~history =
  if history <> [] then begin
    print_string "Pas de chance ! Essayez de nouveau ; votre plateau :";
    print_board history;
    Printf.printf "Vous en êtes au tour %s\n" (Ansi.cyan (string_of_int attempt))
  end;
  print_newline ();
  print_string "Entrez ci-dessous votre réponse ;";
  print_newline ();
  print_format_hint ();
  let guess = read_code () in
  Ui.clear_screen ();
  if guess = secret then begin
    Printf.printf "Vous avez trouvé le code de l'IA, bravo !!!\n";
    Printf.printf "Le code était bien le : [ %s ]\n" (Secret_code.to_string secret);
    Printf.printf "Vous avez gagné en %s coups !!\n" (Ansi.green (string_of_int attempt));
    Player_wins
  end
  else if attempt >= max_attempts then begin
    print_string "Vous avez perdu... vous n'êtes pas arrivé à trouver le code de l'IA\n";
    Ai_wins
  end
  else
    let feedback = Option.get (Secret_code.feedback ~secret ~guess) in
    player_breaks_code ~secret ~max_attempts ~attempt:(attempt + 1) ~history:((guess, feedback) :: history)

let rec ai_breaks_code ~secret ~strategy ~auto_answer ~remaining_attempts ~history ~candidates =
  if remaining_attempts = 0 then begin
    print_string "L'IA n'a pas pu casser votre code !\n";
    Player_wins
  end
  else
    let guess = Solver.choose strategy ~history ~candidates in
    let feedback = Option.get (Secret_code.feedback ~secret ~guess) in
    let feedback_confirmed =
      if auto_answer then true
      else begin
        Printf.printf "\nVoici votre code et le code proposé par l'IA : [ %s ] | [ %s ]\n"
          (Secret_code.to_string secret) (Secret_code.to_string guess);
        print_string
          "Veuillez saisir la réponse correspondante ci-dessous, en premier le nombre de pions noirs \
           et en second le nombre de pions blancs :\n";
        read_feedback () = feedback
      end
    in
    if not feedback_confirmed then begin
      print_string "\nVotre réponse n'est pas correcte !! vous perdez donc cette manche :)\n";
      Ai_wins
    end
    else if feedback = (Secret_code.length, 0) then begin
      Printf.printf "\ncode testé : [ %s ]\n" (Secret_code.to_string guess);
      print_string "L'IA a pu casser votre code !\n";
      Ai_wins
    end
    else begin
      Printf.printf "\ncode testé : [ %s ]\n" (Secret_code.to_string guess);
      let candidates = Solver.filter strategy ~guess ~feedback candidates in
      ai_breaks_code ~secret ~strategy ~auto_answer ~remaining_attempts:(remaining_attempts - 1)
        ~history:(guess :: history) ~candidates
    end

type side = Ai_guesses | Player_guesses

let role_message = function
  | Ai_guesses -> "C'est à l'IA de deviner votre code"
  | Player_guesses -> "C'est à vous de deviner le code de l'IA"

let flip_side = function Ai_guesses -> Player_guesses | Player_guesses -> Ai_guesses

let play_round settings ~side =
  match side with
  | Ai_guesses ->
      let strategy = List.nth Solver.strategies (Random.int (List.length Solver.strategies)) in
      Printf.printf "\nLa méthode utilisée par l'IA est la méthode %s" (Ansi.cyan (Solver.name strategy));
      if strategy = Solver.Minimax then
        print_string " donc l'ordinateur peut mettre du temps à calculer";
      print_newline ();
      print_newline ();
      print_string "Entrez ci-dessous le code que vous souhaitez faire deviner à l'IA :";
      print_newline ();
      print_format_hint ();
      let secret = read_code () in
      Ui.clear_screen ();
      ai_breaks_code ~secret ~strategy ~auto_answer:settings.auto_answer
        ~remaining_attempts:settings.attempts ~history:[] ~candidates:Secret_code.all
  | Player_guesses ->
      let secret = Secret_code.random () in
      player_breaks_code ~secret ~max_attempts:settings.attempts ~attempt:1 ~history:[]

(* ---- The full match, alternating who guesses whom -------------------- *)

type score = { player : int; ai : int }

let apply_outcome score = function
  | Player_wins -> { score with player = score.player + 1 }
  | Ai_wins -> { score with ai = score.ai + 1 }

let rec play_rounds settings ~round_number ~side ~score =
  Ui.clear_screen ();
  print_string (role_message side);
  print_newline ();
  Printf.printf "\n%s Plus que %s parties %s\n" (Ansi.cyan "/")
    (Ansi.blue (string_of_int (settings.rounds - round_number + 1)))
    (Ansi.cyan "/");
  let score = apply_outcome score (play_round settings ~side) in
  if round_number >= settings.rounds then score
  else begin
    Ui.pause "\nappuyez sur ENTER\n";
    play_rounds settings ~round_number:(round_number + 1) ~side:(flip_side side) ~score
  end

let announce_winner settings score =
  Ui.clear_screen ();
  if score.ai > score.player then
    Printf.printf "%s cette partie de Mastermind avec un score de %s contre %s, dommage pour vous :)\n"
      (Ansi.red "l'IA a gagné")
      (Ansi.green (string_of_int score.ai))
      (Ansi.red (string_of_int score.player))
  else if score.player > score.ai then
    Printf.printf "%s vous avez %s avec un score de : %s contre %s pour l'IA. Bravo !!!\n"
      settings.player_name (Ansi.green "gagné !!!")
      (Ansi.green (string_of_int score.player))
      (Ansi.red (string_of_int score.ai))
  else print_string (Ansi.blue "Vous êtes à égalité !!!" ^ "\n")

let () =
  Random.self_init ();
  let settings = prompt_settings () in
  Ui.clear_screen ();
  Printf.printf "@@ %s, vous avez choisi de faire %s parties\n\n" (Ansi.green settings.player_name)
    (Ansi.green (string_of_int settings.rounds));
  Printf.printf "Vous et l'IA aurez à chaque fois %s tentatives pour trouver le code de l'adversaire\n\n"
    (Ansi.green (string_of_int settings.attempts));
  if settings.auto_answer then
    print_string "Vous n'aurez pas à fournir les réponses aux tentatives de l'IA\n\n"
  else print_string "Vous allez aussi devoir fournir à l'IA les réponses à ses tentatives.\n\n";
  Ui.pause "appuyez sur ENTER pour lancer le jeu\n";
  Ui.clear_screen ();
  let starting_side = if Random.int 2 = 0 then Ai_guesses else Player_guesses in
  let score = play_rounds settings ~round_number:1 ~side:starting_side ~score:{ player = 0; ai = 0 } in
  Ui.pause "\nappuyez sur ENTER pour voir le score final\n";
  announce_winner settings score
