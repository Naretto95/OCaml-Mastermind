open Code

module IA : sig
  val nombre_methodes : int

  val choix : int -> Code.t list -> Code.t list -> Code.t

  val filtre : int -> Code.t * (int * int) option -> Code.t list -> Code.t list
end = struct
  let nombre_methodes = 2

  let f2 x = match x with Some c -> c | None -> (0, 0)

  let listeS1 test rep liste =
    snd
      (List.partition
         (fun x -> x = test)
         (List.filter (fun y -> f2 (Code.reponse test y) = rep) liste))

  let listesupprime l1 l2 =
    List.filter
      (fun x ->
        List.fold_left (fun n y -> if y = x then n + 1 else n) 0 l2 > 0 = false
        )
      l1

  let choix n lt lp =
    match n with
    | 0 -> List.nth lp (Random.int (List.length lp))
    | 1 ->
        if lt = [] = true then List.nth Code.tous 7
        else
          let s = listesupprime Code.tous lt in
          fst
            (List.fold_left
               (fun n sa ->
                 if
                   List.length
                     (listeS1 sa
                        (List.fold_left
                           (fun l rep ->
                             if
                               List.length (listeS1 sa rep lp)
                               > List.length (listeS1 sa l lp)
                             then rep
                             else l )
                           (0, 0) Code.toutes_reponses)
                        lp)
                   < List.length (listeS1 (fst n) (snd n) lp)
                 then
                   ( sa
                   , List.fold_left
                       (fun l rep ->
                         if
                           List.length (listeS1 sa rep lp)
                           > List.length (listeS1 sa l lp)
                         then rep
                         else l )
                       (0, 0) Code.toutes_reponses )
                 else n )
               ([], (0, 0))
               s)
    | _ -> failwith "choix ne marche pas"

  let filtre n cr l =
    match n with
    | 0 -> l
    | 1 -> listeS1 (fst cr) (f2 (snd cr)) l
    | _ -> failwith "filtre ne marche pas"
end
