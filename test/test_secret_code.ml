open Mastermind

let () =
  assert (Peg.of_string "Rouge" = Some Peg.Red);
  assert (Peg.of_string "bleu" = Some Peg.Blue);
  assert (Peg.of_string "inconnu" = None);
  assert (Peg.to_string Peg.Red = "rouge");

  let code = [ Peg.Red; Peg.Blue; Peg.Green; Peg.Yellow ] in
  assert (Secret_code.feedback ~secret:code ~guess:code = Some (4, 0));
  assert (
    Secret_code.feedback ~secret:code ~guess:[ Peg.Blue; Peg.Red; Peg.Green; Peg.Orange ] = Some (1, 2));
  assert (Secret_code.feedback ~secret:code ~guess:[ Peg.Orange; Peg.Orange; Peg.Purple; Peg.Purple ]
         = Some (0, 0));

  assert (List.length Secret_code.all = 1296);
  assert (Secret_code.of_string "rouge bleu vert jaune" = Some code);
  assert (Secret_code.of_string "rouge bleu vert" = None);
  assert (Secret_code.of_string "rouge bleu vert inconnu" = None);

  assert (not (List.mem (Secret_code.length - 1, 1) Secret_code.all_feedbacks));
  assert (List.mem (4, 0) Secret_code.all_feedbacks);
  assert (List.mem (0, 0) Secret_code.all_feedbacks);

  print_endline "All tests passed."
