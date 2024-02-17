(* @author Alisha Varma (av523) *)
(*Game Logic File*)

(**[create_word_bank] is a list of the words contained in the text file
   [diction]. Requires: [diction] is a test file*)
let create_word_bank diction = BatList.of_enum (BatFile.lines_of diction)

(**[valid_input] is a check for whether [value] is contained in [lst], returning
   true if it is and false if not. Requires: [lst] is any list type; [value] is
   the type matching the list's type*)
let rec valid_input value lst =
  match lst with
  | [] -> false
  | h :: t -> if h = value then true else valid_input value t

let%test "contained" = valid_input "hello" [ "hello"; "no" ] = true
let%test "contained" = valid_input "adventure" [ "hello"; "drone" ] = false

(**[valid_input2] is a check for whether [user_input] is a length of 5,
   returning true if it is and false if not. Requires: [user_input] is of type
   string*)
let valid_input2 user_input =
  if String.length user_input = 5 then true else false

let%test "5letters" = valid_input2 "hello" = true
let%test "5letters" = valid_input2 "adventure" = false

(**[lst_contains] is a check for whether the list [lst] contains the char
   [chari], returning true if it does and false if not. Requires: [lst] is of
   type char list; [chari] is of type char*)
let rec lst_contains lst chari =
  match lst with
  | [] -> false
  | h :: t -> if h = chari then true else lst_contains t chari

let%test "lstcontains" = lst_contains [ 'h'; 'e'; 'l'; 'l'; 'o' ] 'o' = true
let%test "lstcontains" = lst_contains [ 'h'; 'e'; 'l'; 'l'; 'o' ] 'z' = false

(**[delete_instance] is a list with the first instance of [char_delete] removed
   from it if [lst] contains [char_delete]. If it doesn't contain [char_delete],
   the original list [lst] is returned. Requires: [char_delete] is of type char;
   [lst] is of type char list*)
let rec delete_instance char_delete lst =
  match lst with
  | [] -> []
  | h :: t -> if h = char_delete then t else h :: delete_instance char_delete t

let%test "delinstance" =
  delete_instance 'l' [ 'h'; 'e'; 'l'; 'l'; 'o' ] = [ 'h'; 'e'; 'l'; 'o' ]

let%test "delinstance" =
  delete_instance 'e' [ 'h'; 'e'; 'l'; 'l'; 'o' ] = [ 'h'; 'l'; 'l'; 'o' ]

let%test "delinstance" =
  delete_instance 'z' [ 'h'; 'e'; 'l'; 'l'; 'o' ] = [ 'h'; 'e'; 'l'; 'l'; 'o' ]

(**[lst_construct] is a list that stores each character of string [str] as each
   element. Requires: [str] is of type string; [i] is of type int*)
let rec lst_construct str i =
  if i >= String.length str then [] else str.[i] :: lst_construct str (i + 1)

let%test "lstcons" = lst_construct "flies" 0 = [ 'f'; 'l'; 'i'; 'e'; 's' ]
