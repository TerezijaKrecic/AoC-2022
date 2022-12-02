let dan = "2"

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

let vsebina_datoteke = preberi_datoteko ("resevanje/"^dan^"dan/input.txt")
let seznam = String.split_on_char '\n' vsebina_datoteke
(* ustvari string seznam vseh vrstic iz datoteke "input.in" *)

(*
let seznam_parov = List.map (String.split_on_char ' ') seznam
ta funkcija ustvari seznam seznamov z 2 elementoma
*)

(*
A - Rock
B - Paper
C - Scissors

X - Rock
Y - Paper
Z - Scissors
*)

let pristej x stevec =
  match x with
  | "A X" -> stevec + 3 + 1
  | "A Y" -> stevec + 6 + 2
  | "A Z" -> stevec + 0 + 3
  | "B X" -> stevec + 0 + 1
  | "B Y" -> stevec + 3 + 2
  | "B Z" -> stevec + 6 + 3
  | "C X" -> stevec + 6 + 1
  | "C Y" -> stevec + 0 + 2
  | "C Z" -> stevec + 3 + 3
  | x -> failwith ("Ta par je čuden: "^x)

  let pristej_pravo x stevec =
    match x with
    | "A X" -> stevec + 0 + 3
    | "A Y" -> stevec + 3 + 1
    | "A Z" -> stevec + 6 + 2
    | "B X" -> stevec + 0 + 1
    | "B Y" -> stevec + 3 + 2
    | "B Z" -> stevec + 6 + 3
    | "C X" -> stevec + 0 + 2
    | "C Y" -> stevec + 3 + 3
    | "C Z" -> stevec + 6 + 1
    | x -> failwith ("Ta par je čuden: "^x)

let sestej_tocke sez_parov y =
  let rec sestej_tocke_aux acc sez =
    match sez with
    | [] -> acc
    | x :: xs -> sestej_tocke_aux (if y = "prav" then pristej_pravo x acc else pristej x acc) xs
  in
  sestej_tocke_aux 0 sez_parov

(*
let izpisi_prvi_par seznam =
  match seznam with
  | [] -> "Seznam je prazen"
  | x :: xs -> x
to je samo poskusna funkcija, da izpiše prvi par
*)

let odgovor1 = sestej_tocke seznam "napačno"
let odgovor2 = sestej_tocke seznam "prav"


let _ = izpisi_datoteko ("resevanje/"^dan^"dan/odgovor1.out") (Int.to_string odgovor1)
let _ = izpisi_datoteko ("resevanje/"^dan^"dan/odgovor2.out") (Int.to_string odgovor2)

(* če je odgovor v tipu int, popravi odgovor1 -> (Int.to_string odgovor1) *)