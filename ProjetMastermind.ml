open Str
open Code
open IA

(*ce module permet de créer une "racine" de random afin de randomiser les autres utilisations du module Random*)

;;
Random.self_init (print_string "")

(*gagnant permet d'afficher le nom du gagnant avec le son score*)
let gagnant x nom =
  match x with
  | a, b when a < b ->
      "\027[31ml'IA a gagné\027[0m cette partie de Mastermind avec un score \
       de \027[32m" ^ string_of_int b ^ "\027[0m contre \027[31m"
      ^ string_of_int a ^ "\027[0m, dommage pour vous :)"
  | a, b when a > b ->
      nom ^ " vous avez \027[32mgagné !!!\027[0m avec un score de : \027[32m"
      ^ string_of_int a ^ "\027[0m contre \027[31m" ^ string_of_int b
      ^ "\027[0m pour l'IA. Bravo !!!"
  | a, b when a = b -> "Vous êtes à \027[34mégalité\027[0m !!!"
  | _ -> failwith "gagnant ne marche pas"

(*findugame permet d'appeler gagnant lorsque c'est la fin du jeu*)
let findugame score nom =
  print_string (String.sub (read_line ()) 0 0) ;
  print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
  print_string (gagnant score nom)

(*paire prend en entré un entier et ressort cet entier si il est paire, dans le cas écheant, il renvoye cet entier+1*)
let paire a =
  match a with
  | a when a mod 2 = 0 = true -> a
  | a when a mod 2 = 0 = false -> a + 1
  | _ -> failwith "paire a un problème"

(*f sert à renvoyer la liste d'une entrée Some liste*)
let f x = match x with Some c -> c | None -> []

(*f2 sert à renvoyer le doublet d'une entrée Some doublet*)
let f2 x = match x with Some c -> c | None -> (0, 0)

(*demandecode va demander à l'utilisateur d'entrer un code jusqu'à ce que celui-ci est une syntaxe correcte et va renvoyer ce code*)
let rec demandecode b =
  let a = try Code.code_of_string (read_line ()) with _ -> Some [] in
  match a with
  | a when List.length (f a) <> 4 = false ->
      snd
        ( print_newline
            (print_string
               (String.sub (string_of_int (Sys.command "clear")) 0 0))
        , f a )
  | a when a = Some [] = true || List.length (f a) <> 4 = true -> demandecode b
  | _ -> failwith "demandecode ne marche pas"

(*lirerep va demander à l'utilisateur d'entrer la réponse au code de l'IA*)
let rec lirerep b =
  let a =
    try
      ( (print_string "blancs = " ; read_int ())
      , (print_string "noirs = " ; read_int ()) )
    with _ -> (4, 4)
  in
  match a with
  | a
    when ( fst a + snd a <= 4
         && (fst a >= 0 && fst a <= 4)
         && snd a >= 0
         && snd a <= 4 )
         = true ->
      snd
        ( print_newline
            (print_string
               (String.sub (string_of_int (Sys.command "clear")) 0 0))
        , (snd a, fst a) )
  | a -> lirerep b

(*affichageplat va afficher le plateau du joueur (codes proposés et réponses associées) lors des parties où le joueur doit deviner le code de l'IA*)
let rec affichageplat l =
  match l with
  | [] -> print_newline ()
  | p :: r ->
      print_newline () ;
      print_string
        ( Code.string_of_code (fst p)
        ^ "| noirs:\027[32m"
        ^ string_of_int (fst (snd p))
        ^ "\027[34m,blancs:\027[31m"
        ^ string_of_int (snd (snd p))
        ^ "\027[34m" ) ;
      affichageplat r

(*affichage va afficher les différentes étapes dans les parties du jeu où le joueur doit deviner le code de l'IA*)
let rec affichage code reponse nbrt liste joueur tent part ch score =
  match (code, reponse) with
  | code, reponse when code = reponse = true ->
      ( print_newline
          ( print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
            print_string "Vous avez trouvé le code de l'IA, bravo !!!" ;
            print_newline () ;
            print_string
              ( "Le code était bien le : \027[32m[ " ^ Code.string_of_code code
              ^ "]\027[0m" ) ;
            print_newline () ;
            print_string
              ( "Vous avez gagnez en \027[32m" ^ string_of_int nbrt
              ^ "\027[0m coups !!" ) )
      , (part - 1, (fst score + 1, snd score)) )
  | code, reponse when List.length reponse <> 4 = true ->
      print_newline () ;
      print_string "Entrez ci-dessous votre réponse ;" ;
      print_newline () ;
      print_string
        "pour que votre réponse soit compréhensible, veuillez laisser un \
         seul espace entre chaque nom de couleur: " ;
      print_newline () ;
      print_string
        "Les couleurs possibles sont le {\027[31mrouge\027[0m, \
         \027[34mbleu\027[0m, \027[32mvert\027[0m, \027[36mjaune\027[0m, \
         \027[33morange\027[0m, \027[35mviolet\027[0m} : " ;
      let n = read_line () in
      snd
        ( print_newline ()
        , affichage code
            (let a = try Code.code_of_string n with _ -> Some [] in
             if
               a = Some [] = true
               || List.length (f (Code.code_of_string n)) <> 4 = true
             then reponse
             else f (try Code.code_of_string n with _ -> Some []))
            nbrt
            (let a = try Code.code_of_string n with _ -> Some [] in
             if
               a = Some [] = true
               || List.length (f (Code.code_of_string n)) <> 4 = true
             then liste
             else
               ( f (Code.code_of_string n)
               , f2 (Code.reponse code (f (Code.code_of_string n))) )
               :: liste)
            joueur tent part ch score )
  | code, reponse when code = reponse = false && nbrt < tent = true ->
      print_newline
        ( print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
          print_string
            "Pas de chance ! essayez de nouveau ; votre plateau :\027[34m" ;
          print_newline () ;
          affichageplat (List.rev liste) ;
          print_newline () ;
          print_string
            ( "\027[0mVous en êtes au tour \027[36m"
            ^ string_of_int (nbrt + 1)
            ^ "\027[0m" ) ) ;
      let n = read_line () in
      affichage code
        (f (try Code.code_of_string n with _ -> Some []))
        (nbrt + 1)
        (let a = try Code.code_of_string n with _ -> Some [] in
         if
           a = Some [] = true
           || List.length (f (Code.code_of_string n)) <> 4 = true
         then liste
         else
           ( f (Code.code_of_string n)
           , f2 (Code.reponse code (f (Code.code_of_string n))) )
           :: liste)
        joueur tent part ch score
  | code, reponse when nbrt < tent = false ->
      ( print_newline
          ( print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
            print_string
              "Vous avez \027[31mperdu...\027[0m vous n'êtes pas arrivé à \
               trouver le code de l'IA" )
      , (part - 1, (fst score, snd score + 1)) )
  | _ -> failwith "ca marche po"

(*affichage va afficher les différentes étapes du jeu dans les parties où l'IA doit deviner le code du joueur*)
let rec affichageIA jcode lcodepossibles codespropos joueur tent part ch score
    meth =
  match ch with
  | true ->
      if tent = 0 = true then
        ( print_newline
            (print_string "L'IA n'a \027[31mpas pu casser\027[0m votre code !")
        , (part - 1, (fst score + 1, snd score)) )
      else
        let ab = IA.choix meth codespropos lcodepossibles in
        if f2 (Code.reponse jcode ab) = (4, 0) = true then
          ( print_newline
              ( print_string
                  ( "code testé : \027[32m[ " ^ Code.string_of_code ab
                  ^ "]\027[0m" ) ;
                print_newline () ;
                print_string "L'IA \027[32ma pu casser\027[0m votre code !" )
          , (part - 1, (fst score, snd score + 1)) )
        else (
          print_newline
            (print_string
               ( "code testé : \027[32m[ " ^ Code.string_of_code ab
               ^ "]\027[0m" )) ;
          affichageIA jcode
            (IA.filtre meth (ab, Code.reponse jcode ab) lcodepossibles)
            (ab :: codespropos) joueur (tent - 1) part ch score meth )
  | false ->
      if tent = 0 = true then
        ( print_newline
            (print_string "L'IA \027[31mpas pu casser\027[0m votre code !")
        , (part - 1, (fst score + 1, snd score)) )
      else
        let ab = IA.choix meth codespropos lcodepossibles in
        print_newline
          ( print_string
              ( "Voici \027[32mvotre code\027[0m et le \027[31mcode proposé \
                 par l'IA\027[0m : \027[32m[ " ^ Code.string_of_code jcode
              ^ "]\027[0m | \027[31m[ " ^ Code.string_of_code ab
              ^ "]\027[0m veuillez saisir la réponse correspondante \
                 ci-dessous," ) ;
            print_newline () ;
            print_string
              "en premier le nombre de pions noirs et en second le nombre de \
               pions blancs :" ) ;
        if lirerep "" = f2 (Code.reponse jcode ab) = true then
          if f2 (Code.reponse jcode ab) = (4, 0) then
            ( print_newline
                (print_string "L'IA \027[32ma pu casser\027[0m votre code !")
            , (part - 1, (fst score, snd score + 1)) )
          else
            affichageIA jcode
              (IA.filtre meth (ab, Code.reponse jcode ab) lcodepossibles)
              (ab :: codespropos) joueur (tent - 1) part ch score meth
        else
          ( print_newline
              (print_string
                 "Votre réponse \027[31mn'est pas correcte\027[0m !! vous \
                  perdez donc cette manche :)")
          , (part - 1, (fst score, snd score + 1)) )

(*humainvsIA lance la partie où l'IA doit deviner le code de l'humain*)
let humainvsIA joueur tentatives part ch score =
  print_newline () ;
  print_newline
    (print_string
       ( "\027[36m/\027[0m Plus que \027[34m" ^ string_of_int part
       ^ "\027[0m parties \027[36m/\027[0m" )) ;
  let a = List.nth Code.tous (Random.int (List.length Code.tous)) in
  (print_string "", snd (affichage a [] 1 [] joueur tentatives part ch score))

(*iavsHUMAIN lance la partie où l'humain doit deviner le code de l'IA*)
let iavsHUMAIN joueur tentatives part ch score =
  let metho = Random.int 2 in
  print_newline () ;
  print_newline
    (print_string
       ( "\027[36m/\027[0m Plus que \027[34m" ^ string_of_int part
       ^ "\027[0m parties \027[36m/\027[0m" )) ;
  print_newline () ;
  print_newline
    ( print_string
        ( "La méthode utilisée par l'IA est la méthode "
        ^
        if metho = 0 then "\027[36maléatoire\027[0m "
        else
          "de \027[36mKNUTH\027[0m donc l'ordinateur peut mettre du temps à \
           calculer " ) ;
      print_newline () ;
      print_newline
        (print_string
           "Entrez ci-dessous le code que vous souhaitez faire deviner à \
            l'IA :") ;
      print_newline () ;
      print_string
        "--Pour que votre réponse soit compréhensible, veuillez laisser un \
         seul espace entre chaque nom de couleur: " ;
      print_newline () ;
      print_string
        "-Les couleurs possibles sont le {\027[31mrouge\027[0m, \
         \027[34mbleu\027[0m, \027[32mvert\027[0m, \027[36mjaune\027[0m, \
         \027[33morange\027[0m, \027[35mviolet\027[0m} : " ) ;
  let c = demandecode "" in
  ( print_newline (print_string "")
  , snd (affichageIA c Code.tous [] joueur tentatives part ch score metho) )

(*num fait le jeu en alternant le lancement de la fonction humainvsIA et iavsHUMAIN*)
let rec num n joueur tentatives parties choix score =
  match n with
  | n when n mod 2 = 0 = true ->
      print_newline
        ( print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
          print_string "C'est \027[31mà l'IA de deviner\027[0m votre code " ) ;
      let double1 = snd (iavsHUMAIN joueur tentatives parties choix score) in
      if fst double1 = 0 = true then (
        print_string "appuyez sur \027[32mENTER\027[0m pour continuer" ;
        print_string (String.sub (read_line ()) 0 0) ;
        print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
        print_string "\027[36m@@\027[0m fin du jeu \027[36m@@\027[0m" ;
        print_newline () ;
        print_newline
          (print_string
             "appuyez sur \027[32mENTER\027[0m pour voir le score final") ;
        findugame (snd double1) joueur )
      else (
        print_string "appuyez sur \027[32mENTER\027[0m" ;
        print_newline (print_string (String.sub (read_line ()) 0 0)) ;
        num (n + 1) joueur tentatives (fst double1) choix (snd double1) )
  | n when n mod 2 = 0 = false ->
      print_newline
        ( print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
          print_string
            "C'est \027[32mà vous de deviner\027[0m le code de l'IA " ) ;
      let double2 = snd (humainvsIA joueur tentatives parties choix score) in
      if fst double2 = 0 = true then (
        print_string "appuyez sur \027[32mENTER\027[0m pour continuer" ;
        print_string (String.sub (read_line ()) 0 0) ;
        print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
        print_string "\027[36m@@\027[0m fin du jeu \027[36m@@\027[0m" ;
        print_newline () ;
        print_newline
          (print_string
             "appuyez sur \027[32mENTER\027[0m pour voir le score final") ;
        findugame (snd double2) joueur )
      else (
        print_string "appuyez sur \027[32mENTER\027[0m" ;
        print_newline (print_string (String.sub (read_line ()) 0 0)) ;
        num (n + 1) joueur tentatives (fst double2) choix (snd double2) )
  | _ -> failwith "num ne marche pas"

(*mastermind récapitule les choix du joueur et lance le jeu*)
let mastermind joueur tentatives parties choix =
  print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
  print_newline
    (print_string
       ( "\027[36m@@\027[0m \027[32m" ^ joueur
       ^ "\027[0m, vous avez choisi de faire \027[32m"
       ^ string_of_int (paire parties)
       ^ "\027[0m parties" )) ;
  print_newline () ;
  print_newline
    (print_string
       ( "Vous et l'IA aurez à chaque fois \027[32m" ^ string_of_int tentatives
       ^ "\027[0m tentatives pour trouver le code de l'adversaire" )) ;
  print_newline () ;
  if choix = true then
    print_newline
      (print_string
         "Vous n'aurez pas à fourir les réponses aux tentatives de l'IA ")
  else
    print_newline
      (print_string
         "Vous allez aussi devoir fournir à l'IA les réponses à ses \
          tentatives.") ;
  print_newline () ;
  print_newline
    (print_string "appuyez sur \027[32mENTER\027[0m pour lancer le jeu") ;
  print_string (String.sub (read_line ()) 0 0) ;
  print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
  num (Random.int 2) joueur tentatives (paire parties) choix (0, 0)

(*start demande au joueur plusieurs paramètres et lance le jeu grâce à la fonction mastermind*)
let rec start j t p ch =
  print_string (String.sub (string_of_int (Sys.command "clear")) 0 0) ;
  print_string " \027[34m---------------------------------------------\027[0m" ;
  print_newline () ;
  Printf.printf "%1s%3s%3s%4s%5s%6s%3s%3s%7s%2s%4s%3s%3s" "\027[34m[\027[0m  "
    "\027[31mMM\027[0m" " \027[31mMM\027[0m" "  \027[33mA\027[0m" "SS"
    " \027[36mTTTTT\027[0m" " \027[32mEE\027[0m" "RR" "  \027[31mMM MM\027[0m"
    " \027[33mI\027[0m" "N n" " \027[36mDD\027[0m" "  \027[34m]\027[0m" ;
  print_newline () ;
  Printf.printf "%1s%1s%3s%3s%4s%3s%5s%4s%4s%7s%2s%4s%4s%1s"
    "\027[34m[\027[0m  " "\027[31mM\027[0m " "\027[31mM\027[0m "
    "\027[31mM\027[0m" " \027[33mAAA\027[0m" "S" "    \027[36mT\027[0m"
    "   \027[32mE\027[0m" "RR" "  \027[31mM M M\027[0m" " \027[33mI\027[0m"
    "nNn" " \027[36mD D\027[0m" " \027[34m]\027[0m" ;
  print_newline () ;
  Printf.printf "%1s%1s%3s%3s%4s%3s%5s%5s%4s%6s%2s%4s%3s%3s"
    "\027[34m[\027[0m  " "\027[31mM\027[0m " "\027[31mM\027[0m "
    "\027[31mM\027[0m" " \027[33mA A\027[0m" "SS" "    \027[36mT\027[0m"
    "   \027[32mEE\027[0m" "R R" " \027[31mM M M\027[0m" " \027[33mI\027[0m"
    "n N" " \027[36mDD\027[0m" "  \027[34m]\027[0m" ;
  print_newline () ;
  print_string " \027[34m---------------------------------------------\027[0m" ;
  print_newline () ;
  print_newline () ;
  print_string "Bienvenue dans le \027[36mMastermind\027[0m !!!" ;
  print_newline () ;
  print_newline () ;
  print_newline () ;
  print_string
    "Pour lancer le jeu vous devez renseigner vos préférences ci-dessous :" ;
  if j = "" then
    let a =
      print_newline () ;
      print_string "- Votre pseudo : " ;
      read_line ()
    in
    start a t p ch
  else if t = -1 then
    let b =
      print_newline () ;
      print_string "\027[36mpseudo :\027[0m \027[32mOK\027[0m" ;
      print_newline () ;
      print_string
        "- Le nombre de tentatives pour résoudre le code par partie \
         (\027[33mentier entre 2 et 20\027[0m) : " ;
      try read_int () with _ -> -1
    in
    if b < 2 && b > 20 then start j t p ch else start j b p ch
  else if p = -1 then
    let c =
      print_newline () ;
      print_string "\027[36mpseudo :\027[0m \027[32mOK\027[0m" ;
      print_newline () ;
      print_string "\027[36mtentatives :\027[0m \027[32mOK\027[0m" ;
      print_newline () ;
      print_string
        "- Le nombre de parties (\027[33mentier paire entre 2 et 20\027[0m, \
         sera arrondi à un entier paire supérieur si c'est un nombre \
         impaire) : " ;
      try read_int () with _ -> -1
    in
    if c < 2 && c > 20 then start j t p ch else start j t c ch
  else if ch = "" then
    let d =
      print_newline () ;
      print_string "\027[36mpseudo :\027[0m \027[32mOK\027[0m" ;
      print_newline () ;
      print_string "\027[36mtentatives :\027[0m \027[32mOK\027[0m" ;
      print_newline () ;
      print_string "\027[36mparties :\027[0m \027[32mOK\027[0m" ;
      print_newline () ;
      print_string
        "- Entrez \027[33mtrue\027[0m pour laisser le programme répondre à \
         l'IA et \027[33mfalse\027[0m pour répondre vous-même : " ;
      read_line ()
    in
    if d = "true" || d = "True" || d = "false" || d = "False" then
      start j t p d
    else start j t p ch
  else (
    print_newline () ;
    print_string "\027[36mpseudo :\027[0m \027[32mOK\027[0m" ;
    print_newline () ;
    print_string "\027[36mtentatives :\027[0m \027[32mOK\027[0m" ;
    print_newline () ;
    print_string "\027[36mparties :\027[0m \027[32mOK\027[0m" ;
    print_newline () ;
    print_string "\027[36mréponses :\027[0m \027[32mOK\027[0m" ;
    print_newline () ;
    print_newline () ;
    print_string "Appuyer sur \027[32mENTER\027[0m pour lancer le jeu ..." ;
    print_string (String.sub (read_line ()) 0 0) ;
    mastermind j t p
      ( if ch = "true" || ch = "True" then true
      else if ch = "false" || ch = "False" then false
      else false ) )

;;
start "" (-1) (-1) ""

;;
print_newline ()
