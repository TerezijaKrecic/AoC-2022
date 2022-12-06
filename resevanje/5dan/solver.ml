let dan = "5"

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

(* day_5.in malo preuredimo, ker ne znam prebrat prvih 9 vrstic vseznam stolpcev, zato to naredim na roke
   seznam je urejen tako: [vrhnji_elemente; ... ; spodnji_element] *)
let list1 = ["W"; "L"; "S"]
let list2 = ["Q"; "N"; "T"; "J"]
let list3 = ["J"; "F"; "H"; "C"; "S"]
let list4 = ["B"; "G"; "N"; "W"; "M"; "R"; "T"]
let list5 = ["B"; "Q"; "H"; "D"; "S"; "L"; "R"; "T"]
let list6 = ["L"; "R"; "H"; "F"; "V"; "B"; "J"; "M"]
let list7 = ["M"; "J"; "N"; "R"; "W"; "D"]
let list8 = ["J"; "D"; "N"; "H"; "F"; "T"; "Z"; "B"]
let list9 = ["T"; "F"; "B"; "N"; "Q"; "L"; "H"]
let list1_9 = [list1; list2; list3; list4; list5; list6; list7; list8; list9]
let vsebina_datoteke = preberi_datoteko ("resevanje/"^dan^"dan/day_"^dan^".in")
let seznam = String.split_on_char '\n' vsebina_datoteke
(* to je seznam nizov posameznih vrstic npr. "move 5 from 4 to 5"*)

let el1 sez =
  (* vrne prvi element seznama *)
  match sez with
  | x :: xs -> x
  | _ -> failwith "Seznam je prazen"

  let preberi niz =
  (* iz niza "move 5 from 4 to 5" naredi seznam [5;4;5] *)
  match String.split_on_char ' ' niz with
  | [_; a; _; b; _; c] -> [Stdlib.int_of_string a; Stdlib.int_of_string b; Stdlib.int_of_string c]
  | _ -> failwith "Neveljavna vrstica"

let rec preuredi_seznam seznam_kupov prvilist drugilist prvi drugi =
  (* v seznam_kupov se seznam na mestu "prvi" spremeni v "prvilist" in drugi podobno *)
  if drugi < prvi then preuredi_seznam seznam_kupov drugilist prvilist drugi prvi
  else
    let rec preuredi_seznam_aux acc stevec1 stevec2 seznam_kupov =
      match (seznam_kupov, stevec1, stevec2) with
      | ([], 0, 0) -> List.rev acc
      | (x :: xs, 0, st2) ->
          if st2 = 1 then preuredi_seznam_aux (drugilist::acc) 0 0 xs
          else if st2 > 1 then preuredi_seznam_aux (x::acc) 0 (st2-1) xs
          else preuredi_seznam_aux (x::acc) 0 0 xs
      | (x :: xs, 1, st2) -> preuredi_seznam_aux (prvilist::acc) 0 (st2-1) xs
      | (x :: xs, st1, st2) -> preuredi_seznam_aux (x::acc) (st1-1) (st2-1) xs
      | _ -> failwith "Napaka"
    in
    preuredi_seznam_aux [] prvi drugi seznam_kupov

let rec preuredi1 seznam_kupov stevec prvi drugi =
  (* sprejme list [list1; list2; ...; list9] in stevec (število premikov) prvi (s katerega kupa jemljemo) drugi (na kateri kup odlagamo) iz seznama [5;4;5] (iz niza "move 5 from 4 to 5")
  TA JE ZA POŠTIMAT, OSTALE ŠTIMAJO!!   
  *)
  match stevec with
  | 0 -> seznam_kupov
  | _ ->
    let prvikup = List.nth seznam_kupov (prvi-1) in
    let drugikup = List.nth seznam_kupov (drugi-1) in
    match (prvikup, drugikup) with
      | (x :: xs, ys) ->
        let prvilist = xs in let drugilist = x :: ys in
        preuredi1 (preuredi_seznam seznam_kupov prvilist drugilist prvi drugi) (stevec-1) prvi drugi
      | _ -> failwith "Napakica"

let podniz x seznam =
  (* naredi seznam prvih x elementov seznama x in njegov preostanek *)
  let rec podniz_aux acc x sez =
    if x = 0 then (List.rev acc, sez) else
    match sez with
    | y :: ys -> podniz_aux (y :: acc) (x-1) ys
    | _ -> failwith "predolg seznam"
  in
  podniz_aux [] x seznam

let rec preuredi2 seznam_kupov stevec prvi drugi =
  (* vzameš seznam prvih stevec elementov iz seznama na mestu "prvi" in ga pripopaš pred seznam na mestu "drugi" *)
  let prvikup = List.nth seznam_kupov (prvi-1) in
  let drugikup = List.nth seznam_kupov (drugi-1) in
  let premik = podniz stevec prvikup in
  match premik with
  |  (x, y) -> preuredi_seznam seznam_kupov y (List.append x drugikup)  prvi drugi


let rec po_vrsticah f seznam seznam_kupov = match seznam with
  (* IDEJA: gre po vrsticah seznama. Za vsako vrstico preuredi določena seznama črk in gre na naslednjo vrstico. Na koncu vrne prve elemente vseh 9-ih seznamov v obliki enega niza
  f je preuredi1 (za 1. nalogo) ali preuredi2 (za 2. nalogo)   
  *)
  | [] -> List.map el1 seznam_kupov (* vrne prve elemente vseh devetih seznamov *)
  | x :: xs ->
    match preberi x with
    | [stevec; prvi; drugi] -> po_vrsticah f xs (f seznam_kupov stevec prvi drugi)
    | _ -> failwith "Napaka"

let odgovor1 = po_vrsticah preuredi1 seznam list1_9
let odgovor2 = po_vrsticah preuredi2 seznam list1_9


let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_1.out") (String.concat "" odgovor1)
let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_2.out") (String.concat "" odgovor2)