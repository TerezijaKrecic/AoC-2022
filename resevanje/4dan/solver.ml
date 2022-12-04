let dan = "4"

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

let popolna_vsebovanost a b c d =
  (* pogleda, če je interval [a, b] vsebovan v [c, d] *)
  if a >= c && a <= d && b >= c && b <= d then true
  else if c >= a && c <= b && d >= a && d <= b then true
  else false

let vsebovanost a b c d = 
  (* pogleda, če se intervala [a, b] in [c, d] prekrivata *)
  if (a >= c && a <= d) || (b >= c && b <= d) || (c >= a && c <= b) || (d >= a && d <= b) then true
  else false

let poglej niz acc f =
  (* sprejme niz oblike "33-62,26-62" in če se eno območje nahaja v drugem, prišteje acc 1*)
  let stringsez = List.map (String.split_on_char '-') (String.split_on_char ',' niz) in (* dobimo [["33"; "62"]; ["26"; "62"]]*)
  let intsez = List.map (List.map Stdlib.int_of_string) stringsez in (* dobimo [[33; 62]; [26; 62]] *)
  match intsez with
  | [[a; b]; [c; d]] -> if (f a b c d = true) then (acc + 1) else acc
    | _ -> failwith "Nekaj je narobe."

let naloga seznam f =
  (* sprejme seznam nizov in za vsak niz pogleda, če je en interval vsebovan v drugem *)
  let rec naloga_aux acc seznam =
    match seznam with
    | [] -> acc
    | x :: xs -> naloga_aux (poglej x acc f) xs
  in
  naloga_aux 0 seznam
let odgovor1 = naloga seznam popolna_vsebovanost
let odgovor2 = naloga seznam vsebovanost

let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_1.out") (Int.to_string odgovor1)
let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_2.out") (Int.to_string odgovor2)