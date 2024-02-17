(** @author Alisha Varma (av523) *)

(**Dictionary Citation:
   https://gist.github.com/scholtes/94f3c0303ba6a7768b47583aff36654d*)
open A2.Wordle
(**Interface*)

(**This initializes the random number generator*)
let () = Random.self_init ()

(**This stores the dictionary text file in [dict]*)
let dict = "data/dictionary.txt"

(**This creates a word_bank variable that stores the list of the dictionary in
   [word_bank]*)

let word_bank = create_word_bank dict

(**This creates a variable storing an index that is a random integer that is a
   valid index of [word_bank]*)
let true_ans_index = 1 + Random.int (List.length word_bank - 1)

(**This creates a string variable, which represents the string at the random
   index previously generated. This string will be the true answer of the game.*)

let true_ans = List.nth word_bank true_ans_index

(**[color_match] is a single element string (representing a character) that is
   colored in green, yellow, or red. The colors correspond to how the game is
   played. Requires: [user_input] is of type string; [true_ans] is of type
   string; [i] is of type int; [newmem] is of type list*)
let color_match user_input true_ans i newmem =
  let charstr = String.make 1 user_input.[i] in
  let str =
    if user_input.[i] = true_ans.[i] && lst_contains newmem user_input.[i] then
      ANSITerminal.sprintf
        [ ANSITerminal.green; ANSITerminal.on_black ]
        "%s" charstr
    else if lst_contains newmem user_input.[i] then
      ANSITerminal.sprintf
        [ ANSITerminal.yellow; ANSITerminal.on_black ]
        "%s" charstr
    else
      ANSITerminal.sprintf
        [ ANSITerminal.red; ANSITerminal.on_black ]
        "%s" charstr
  in
  str

(**[color_match_concat] is print statement and a string representing the
   characters of [user_input] in the correct corresponding colors of green,
   yellow, and red. Requires: [user_input] is of type string; [true_ans] is of
   type string*)
let color_match_concat user_input true_ans =
  let memoryta = lst_construct true_ans 0 in
  let str0 = color_match user_input true_ans 0 memoryta in
  let newmemta0 = delete_instance user_input.[0] memoryta in
  let str1 = color_match user_input true_ans 1 newmemta0 in
  let newmemta1 = delete_instance user_input.[1] newmemta0 in
  let str2 = color_match user_input true_ans 2 newmemta1 in
  let newmemta2 = delete_instance user_input.[2] newmemta1 in
  let str3 = color_match user_input true_ans 3 newmemta2 in
  let newmemta3 = delete_instance user_input.[3] newmemta2 in
  let str4 = color_match user_input true_ans 4 newmemta3 in
  let () = print_endline (str0 ^ str1 ^ str2 ^ str3 ^ str4) in
  str0 ^ str1 ^ str2 ^ str3 ^ str4

(**[prompt_guess] is a prompting message for user input asking for the user to
   enter a 5-letter word. If the user does not enter a valid input, then the
   function is called again recursively so that the user may enter a valid
   input.*)
let rec prompt_guess () =
  let () = print_string "Enter a 5-letter word for a guess: " in
  let user_input = read_line () in
  if valid_input user_input word_bank && valid_input2 user_input then
    if user_input = true_ans then user_input
    else color_match_concat user_input true_ans
  else
    let () = print_endline "Invalid Input. Please enter again. " in
    prompt_guess ()

(**[prompt_cheat] is a prompting message for the user asking the user if they
   would like to enable cheat mode, which reveals the answer to the user. If the
   user enters an invalid input (i.e. input that is not "Y" or "N"), the user is
   prompted to re-enter an answer to the question.*)
let rec prompt_cheat () =
  let () = print_string "Would you like to enable cheat mode? (Y/N): " in
  let user_cheat = read_line () in
  if user_cheat = "Y" then
    print_endline ("Cheat enabled: The answer is " ^ true_ans)
  else if user_cheat = "N" then print_endline "Cheat is not enabled. Proceed: "
  else
    let () = print_endline "Invalid input. Please enter again. " in
    prompt_cheat ()

(**[play] is a checker tracking the number of attempts the user has. If the user
   runs out of attempts, the correct string output is printed. If not, the user
   may start an attempt if they have not already identified the correct answer.
   Requires: [a] is of type int; [b] is of type int*)
let rec play a b =
  if a > b then
    print_endline
      ("You have run out of tries. Thanks for playing! \n\
       \ The correct answer is: " ^ true_ans)
  else if prompt_guess () = true_ans then
    print_endline
      (ANSITerminal.sprintf
         [ ANSITerminal.green; ANSITerminal.on_black ]
         "%s" true_ans
      ^ "\nYou have succesfully completed the game! Congratulations")
    (* "You have succesfully completed the game! Congratulations" *)
  else play (a + 1) b

(**This is the output message to the user signaling the beginning of the game.*)

let () = print_endline "Welcome to Wordle! Here are the rules:"

(**This is output for the user to see that explains the rules of the game. *)

let () =
  print_endline
    "You have to try to guess the correct five-letter word according to a\n\
     series of hints after each of your attempts. \n\
     A green letter means a letter in the correct place. \n\
     A yellow letter means a correct letter in the wrong place. \n\
     A red letter means that letter is not contained in the word. \n\
     You have 6 attempts to correctly guess."

(**This begins prompts the cheat function, which asks the suer if they would
   like to cheat.*)
let () = prompt_cheat ()

(**This starts a play for the user, in which they have 6 attempts.*)
let () = play 1 6
