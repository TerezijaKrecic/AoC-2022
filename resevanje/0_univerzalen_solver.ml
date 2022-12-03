let dan = ""

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

let naloga1 vsebina =
  "TODO"

let naloga2 vsebina =
  "TODO"

let odgovor1 = naloga1 vsebina_datoteke
let odgovor2 = naloga2 vsebina_datoteke

(*
let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_1.out") odgovor1
let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_2.out") odgovor2
*)

(* če je odgovor v tipu int, popravi odgovor1 -> (Int.to_string odgovor1) *)