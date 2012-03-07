(** Implements several frequently used functions. *)

(** Type to describe a point in a source file. *)
type pos = string*int*int

(** Returns the second element of a tuple of 3 elements. For values of type {!pos}, it correspond to the line number in source file. *)
let ln (_,i,_) = i
(** Returns the third element of a tuple of 3 elements. For values of type {!pos}, it correspond to the column number in source file. *)
let cn (_,_,j) = j
(** Returns the first element of a tuple of 3 elments. For values of type {!pos}, it correspond to the source-file name. *)
let fn (n,_,_) = n
(** Defines a null {!pos}. *)
let no_pos = ("",-1,-1)

(** Returns a string representation of a {!pos} value. *)
let string_of_pos = fun (n,i,j) -> ("(in "^n^") line "^(string_of_int i)^", char "^(string_of_int j)^": ")
(** Prints the elements of a set, separated by commas.
    @param f Function used to build a string representation of each element in the set.
    @param fold Function used to reduce the set.
    @param set A set. *)
let string_of_set f fold set = 
  let l = 
    fold (fun i cont -> (f i)::cont) set [] 
  in
    Printf.sprintf "{%s}" (String.concat "," l)
(** Prints the entries of a map, separated by commas.
    @param f1 Function used to build a string representation of each entry's key.
    @param f2 Function used to build a string representation of each entry's value.
    @param swap If true, swap keys and values of the map. Optional, default is false.
    @param map A map. *)
let string_of_map ?(swap=false) f1 f2 fold map = 
  let l = 
    fold (fun i j cont -> 
			if swap then ((f2 j)^"->"^(f1 i))::cont
			else ((f1 i)^"->"^(f2 j))::cont) map [] 
  in
    Printf.sprintf "[%s]" (String.concat "," l)
(** Returns a string representation of an array.
    @param f Function used to get a string representation of each element in the array. 
    @param ar An array. *)
let string_of_array f ar =
	let l = ref [] in 
		Array.iteri (fun i e -> l:=(((string_of_int i)^":"^(f e))::!l)) ar ;
		"[|"^(String.concat ";" (List.rev !l))^"|]"
(** Returns a string representation of a list.
    @param f Function used to get a string representation of each element in the list.
    @param l A list. *)		
let string_of_list f l =
	"["^(String.concat ";" (List.map f l))^"]"
	
let pow x n =
	let rec aux x n acc =
		if n = 0 then acc
		else
			aux x (n-1) (x*acc)
	in
	aux x n 1

(** Returns the number of bits needed to represent n in base 2.
    @param n An integer. *)
let bit_rep_size n = 
	let rec aux p acc = 
		if p = 0 then acc
		else
			let p' = p/2 in
			aux p' (acc+1)
	in
		aux n 0  

(** Replace spaces by underscores in a given string and returns it.
    @param str A string. *)
let replace_space str = 
	let cpt = ref 0 in
	String.iter (fun c -> if c=' ' then String.set str !cpt '_' ; cpt := !cpt+1) str ;
	str

(** Reads standard input and returns it without spaces. *)
let read_input () = 
	let rec parse acc input =
		match Stream.next input with
			| '\n' -> acc
			| c -> parse (Printf.sprintf "%s%c" acc c) input
	in
	try
		let user_input = Stream.of_channel stdin in
		parse "" user_input
	with
		| Stream.Failure -> invalid_arg "Tools.Read_input: cannot read stream"
