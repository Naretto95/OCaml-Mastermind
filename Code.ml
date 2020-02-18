module Code : sig
  type pion = Rouge | Bleu | Vert | Jaune | Orange | Violet

  type t = pion list

  val nombre_pions : int

  val couleurs_possibles : pion list

  val compare : t -> t -> int

  val string_of_code : t -> string

  val code_of_string : string -> t option

  val tous : t list

  val toutes_reponses : (int * int) list

  val reponse : t -> t -> (int * int) option
end = struct
  type pion = Rouge | Bleu | Vert | Jaune | Orange | Violet

  type t = pion list

  let nombre_pions = 4

  let couleurs_possibles = [Rouge; Bleu; Vert; Jaune; Orange; Violet]

  let rec compare c1 c2 =
    match (c1, c2) with
    | [], [] -> 0
    | p1 :: r1, p2 :: r2 when p1 > p2 = true -> 1 + compare r1 r2
    | p1 :: r1, p2 :: r2 when p1 < p2 = true -> -1 + compare r1 r2
    | p1 :: r1, p2 :: r2 when p1 = p2 = true -> 0 + compare r1 r2
    | _, _ -> failwith "Cela n'est pas possible"

  let string_of_code2 x =
    match x with
    | Rouge -> "Rouge "
    | Bleu -> "Bleu "
    | Vert -> "Vert "
    | Jaune -> "Jaune "
    | Orange -> "Orange "
    | Violet -> "Violet "

  let rec string_of_code code =
    match code with
    | [] -> ""
    | p :: reste -> string_of_code2 p ^ string_of_code reste

  let code_of_string2 x =
    match x with
    | "Rouge" -> Rouge
    | "Bleu" -> Bleu
    | "Vert" -> Vert
    | "Jaune" -> Jaune
    | "Orange" -> Orange
    | "Violet" -> Violet
    | "rouge" -> Rouge
    | "bleu" -> Bleu
    | "vert" -> Vert
    | "jaune" -> Jaune
    | "orange" -> Orange
    | "violet" -> Violet
    | _ -> failwith "Cette chaine de caractère n'est pas correcte"

  let rec code_of_string3 x = Str.split (Str.regexp " ") x

  let rec code_of_string4 x =
    match x with
    | p :: reste -> code_of_string2 p :: code_of_string4 reste
    | _ -> []

  let code_of_string code = Some (code_of_string4 (code_of_string3 code))

  let rec ajout a b c d n =
    match (a, b, c, d) with
    | x, y, z, t when x = y && y = z && z = t && t = n - 1 ->
        [[n - 1; n - 1; n - 1; n - 1]]
    | x, y, z, t when x = y && y = z && z = n - 1 ->
        [x; y; z; t] :: ajout 0 0 0 (t + 1) n
    | x, y, z, t when x = y && y = n - 1 ->
        [x; y; z; t] :: ajout 0 0 (z + 1) t n
    | x, y, z, t when x = n - 1 -> [x; y; z; t] :: ajout 0 (y + 1) z t n
    | x, y, z, t -> [x; y; z; t] :: ajout (x + 1) y z t n

  let tous4 = ajout 0 0 0 0 (List.length couleurs_possibles)

  let code_of_int x =
    match x with
    | 0 -> Rouge
    | 1 -> Bleu
    | 2 -> Vert
    | 3 -> Jaune
    | 4 -> Orange
    | 5 -> Violet
    | _ -> failwith "xD"

  let rec tous3 x =
    match x with p :: reste -> code_of_int p :: tous3 reste | _ -> []

  let rec tous2 x =
    match x with p :: reste -> tous3 p :: tous2 reste | _ -> []

  let tous = tous2 tous4

  let noir l1 l2 =
    List.split
      (List.fold_right
         (fun (b1, b2) a -> if b1 <> b2 = true then [(b1, b2)] @ a else a)
         (List.combine l1 l2) [])

  let rec blanc l1 l2 =
    List.length l1
    - List.length
        (List.fold_left
           (fun l a ->
             fst (List.split (List.remove_assoc a (List.combine l l))) )
           l1 l2)

  let reponse code test =
    match (code, test) with
    | _ when List.length code = List.length test = true ->
        Some
          ( List.length code - List.length (fst (noir code test))
          , blanc (fst (noir code test)) (snd (noir code test)) )
    | _ -> None

  let rec ajout2 x y n =
    match (x, y, n) with
    | x, y, n when x = n && y = n -> []
    | x, y, n when x = n && x + y <= n -> (x, y) :: ajout2 0 (y + 1) n
    | x, y, n when x = n -> ajout2 0 (y + 1) n
    | x, y, n when x + y <= n -> (x, y) :: ajout2 (x + 1) y n
    | x, y, n -> ajout2 (x + 1) y n

  let toutes_reponses = List.filter (fun x -> x <> (3, 1)) (ajout2 0 0 4)
end
