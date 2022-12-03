let dan = "3"

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

let vsebina_datoteke = preberi_datoteko ("resevanje/"^dan^"dan/day_"^dan^".in")
let seznam = String.split_on_char '\n' vsebina_datoteke
(* to je seznam nizov posameznih vrstic*)

let seznam_crk s = List.map Char.escaped (List.init (String.length s) (String.get s))
(* to iz niza naredi seznam njegovih črk *)

let rec najdi_skupno_crko sez1 sez2 = match sez1 with
(* najde skupno črko v dveh seznamih črk *)
    | [] -> "Ni skupnega elementa"
    | x :: xs -> if List.mem x sez2 = true then x else najdi_skupno_crko xs sez2

let prioriteta niz =
  (* vrne črko, ki se pojavi v obeh polovicah niza *)
  let dolzina = String.length niz in
  let sez_crk1 = seznam_crk (String.sub niz 0 (dolzina/2)) in
  let sez_crk2 = seznam_crk (String.sub niz (dolzina/2) (dolzina/2)) in
  najdi_skupno_crko sez_crk1 sez_crk2

let seznam_prioritet = List.map prioriteta seznam
(* vrne seznam podvojenih črk *)

let vrednost p = match p with
  | "a" -> 1
  | "b" -> 2
  | "c" -> 3
  | "d" -> 4
  | "e" -> 5
  | "f" -> 6
  | "g" -> 7
  | "h" -> 8
  | "i" -> 9
  | "j" -> 10
  | "k" -> 11
  | "l" -> 12
  | "m" -> 13
  | "n" -> 14
  | "o" -> 15
  | "p" -> 16
  | "q" -> 17
  | "r" -> 18
  | "s" -> 19
  | "t" -> 20
  | "u" -> 21
  | "v" -> 22
  | "w" -> 23
  | "x" -> 24
  | "y" -> 25
  | "z" -> 26
  | "A" -> 27
  | "B" -> 28
  | "C" -> 29
  | "D" -> 30
  | "E" -> 31
  | "F" -> 32
  | "G" -> 33
  | "H" -> 34
  | "I" -> 35
  | "J" -> 36
  | "K" -> 37
  | "L" -> 38
  | "M" -> 39
  | "N" -> 40
  | "O" -> 41
  | "P" -> 42
  | "Q" -> 43
  | "R" -> 44
  | "S" -> 45
  | "T" -> 46
  | "U" -> 47
  | "V" -> 48
  | "W" -> 49
  | "X" -> 50
  | "Y" -> 51
  | "Z" -> 52
  | _ -> failwith "To ni črka."

let seznam_vrednosti_prioritet = List.map vrednost seznam_prioritet
(* vrne seznam vrednosti prioritet *)
let sestej_prioritete sez =
  (* sešteje elemente int seznama *)
  let rec sestej_aux stevec sez = match sez with
    | [] -> stevec
    | x :: xs -> sestej_aux (stevec + x) xs
  in
  sestej_aux 0 sez

let odgovor1 = sestej_prioritete seznam_vrednosti_prioritet

let rec najdi_skupno sez1 sez2 sez3 =
  (* najde skupno črko v treh seznamih*)
  match sez1 with
  | [] -> failwith "Ti trije seznami nimajo nič skupnega"
  | x :: xs -> if (List.mem x sez2 = true && List.mem x sez3 = true) then x else najdi_skupno xs sez2 sez3

let poisci_badge seznam =
  let rec poisci_badge_aux acc sez = match sez with
    | x :: y :: z :: xs -> poisci_badge_aux ((najdi_skupno (seznam_crk x) (seznam_crk y) (seznam_crk z)) :: acc) xs
    | _ -> List.rev acc
  in
  poisci_badge_aux [] seznam

let seznam_badge = poisci_badge seznam

let odgovor2 = sestej_prioritete (List.map vrednost seznam_badge)


let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_1.out") (Int.to_string odgovor1)
let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_2.out") (Int.to_string odgovor2)