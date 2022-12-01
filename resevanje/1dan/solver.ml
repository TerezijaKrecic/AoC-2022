let dan = "1"

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina
  
let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

let vsebina_datoteke = preberi_datoteko ("resevanje/"^dan^"dan/input.in")

let seznam = String.split_on_char '\n' vsebina_datoteke
(* ustvari string seznam vseh vrstic iz datoteke "input.in" *)

let seznam_vsot =
  let rec loci_sezname seznam_vsot stevec sez = match sez with
    | [] -> seznam_vsot
    | "" :: xs -> loci_sezname (stevec :: seznam_vsot) 0 xs
    | x :: xs -> loci_sezname seznam_vsot (stevec + Stdlib.int_of_string x) xs
  in
  loci_sezname [] 0 seznam
(* funkcija sprejme zgornji seznam in naredi integer seznam vsot kalorij za vsakega škrata *)

let naloga1 vsebina =
  (* poišče max. element seznama vsot*)
  let rec max_el_seznam sez acc = match sez with
    | [] -> acc
    | x :: xs -> if x > acc then max_el_seznam xs x else max_el_seznam xs acc
  in
  max_el_seznam seznam_vsot 0


let naloga2 vsebina =
  (* poišče tri največje elemente seznama vsot *)
  let padajoce = List.rev (List.sort compare seznam_vsot) in
  let rec stej sez stevec acc = match (sez, stevec) with
    | (_, 0) -> acc
    | (x :: xs, _) -> stej xs (stevec - 1) (acc + x)
    | ([], _) -> failwith "Seznam ima manj kot tri elemente."
  in
  stej padajoce 3 0



let odgovor1 = naloga1 vsebina_datoteke
let odgovor2 = naloga2 vsebina_datoteke


let _ = izpisi_datoteko ("resevanje/"^dan^"dan/odgovor1.out") (Int.to_string odgovor1)
let _ = izpisi_datoteko ("resevanje/"^dan^"dan/odgovor2.out") (Int.to_string odgovor2)