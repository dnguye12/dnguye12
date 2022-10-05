(* Compiler avec ocamlopt -o hanoi_list unix.cmxa hanoi_list.ml *)

(* On rappelle la fonction de hanoi du TD 2 *)
let hanoi n =
  let rec hanoi_aux dep mil arr n =
    if n > 0 then begin
      hanoi_aux dep arr mil (n - 1);
      Printf.printf "%s -> %s\n" dep arr;
      hanoi_aux mil dep arr (n - 1)
    end
  in
  hanoi_aux "dep" "mil" "arr" n
;;

(* On modélise un  piquet de la façon suivante : *)
type piquet = string * int list (* le nom, la liste de disques *)

type jeu = piquet list

let choix_piquet piquets nom = (nom, List.assoc nom piquets)

let affiche_piquet p =
  let affiche_disque n = Printf.printf "%d-" n in 
  let nom, l = p in
    Printf.printf "%s|" nom;
    List.iter affiche_disque (List.rev l);
    Printf.printf "\n"
;;

let affiche_jeu piquets =
  Printf.printf "\x1b[2J\x1b[H";
  affiche_piquet (choix_piquet piquets "dep");
  affiche_piquet (choix_piquet piquets "mil");
  affiche_piquet (choix_piquet piquets "arr");
  Printf.printf "%!"
;;


let deplace_sommet (nd, ld) (na, la) =
  match (ld, la) with
  | [], _ -> failwith (Printf.sprintf "Liste de départ vide: %s" nd)
  | d :: lld, a :: _ ->
      if d > a then
        failwith
          (Printf.sprintf "Plateau trop petit sur la liste d'arrivée %d > %d" d
             a)
      else ((nd, lld), (na, d :: la))
  | d :: lld, [] -> ((nd, lld), (na, [ d ]))
;;

let joue piquets src dst autre =
  let p_s = choix_piquet piquets src in
  let p_d = choix_piquet piquets dst in
  let p_a = choix_piquet piquets autre in
  let p_s, p_d = deplace_sommet p_s p_d in
  [ p_s; p_d; p_a ]
;;

let gen_list n =
  let rec loop n acc = if n = 0 then acc else loop (n - 1) (n :: acc) in
  loop n []
;;

let hanoi_list n =
  let rec hanoi_aux piquets dep mil arr n =
    if n > 0 then (
      let piquets = hanoi_aux piquets dep arr mil (n - 1) in
      let piquets = joue piquets dep arr mil in
      affiche_jeu piquets;
      Printf.printf "%s -> %s\n%!" dep arr;
      Unix.sleepf 0.1;
      hanoi_aux piquets mil dep arr (n - 1) )
    else piquets
  in
  let final =
    hanoi_aux [ ("dep", gen_list n); 
                ("mil", []);
                ("arr", []) ] "dep" "mil" "arr" n
  in
  affiche_jeu final
;;

hanoi_list 10
;;
