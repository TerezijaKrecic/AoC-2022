let dan = "6"

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
  (* niz vsebine datoteke *)
let seznam_crk s = List.map Char.escaped (List.init (String.length s) (String.get s))
  (* to iz niza naredi seznam njegovih črk *)
let sez_crk = seznam_crk vsebina_datoteke

let podniz x seznam =
  (* naredi seznam prvih x elementov seznama *)
  let rec podniz_aux acc x sez =
    if x = 0 then List.rev acc else
    match sez with
    | y :: ys -> podniz_aux (y :: acc) (x-1) ys
    | _ -> failwith "predolg seznam"
  in
  podniz_aux [] x seznam

let rec naloga1 sez_crk stevec =
  (* gre po seznamu črk, jih vzame po 4 naenkrat v svoj seznam, v tem seznamu se pobriše dvojnike in če se dolžina ne spremeni, je to to in vrne števec (ta teče od 4 naprej, za vsak korak se poveča za 1) *)
  match sez_crk with
  | a::b::[c] -> failwith "Ni nobenega primernega vzorca dolžine 4"
  | a::b::c::d::xs ->
    if List.length (List.sort_uniq compare [a;b;c;d]) = 4 (*seznama sta enaka*) then stevec
    else naloga1 (b::c::d::xs) (stevec + 1)
  | _ -> failwith "Seznam ni primeren"


let rec naloga2 sez_crk stevec =
  match sez_crk with
  | a::b::c::d::e::f::g::h::i::j::k::l::[m] -> failwith "Ni nobenega primernega vzorca dolžine 14"
  | a::b::c::d::e::f::g::h::i::j::k::l::m::n::xs ->
    if List.length (List.sort_uniq compare [a;b;c;d;e;f;g;h;i;j;k;l;m;n]) = 14 (*seznama sta enaka*) then stevec
    else naloga2 (b::c::d::e::f::g::h::i::j::k::l::m::n::xs) (stevec + 1)
  | _ -> failwith "Seznam ni primeren"

let odgovor1 = naloga1 sez_crk 4
let odgovor2 = naloga2 sez_crk 14

let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_1.out") (Int.to_string odgovor1)
let _ = izpisi_datoteko ("resevanje/"^dan^"dan/day_"^dan^"_2.out") (Int.to_string odgovor2)