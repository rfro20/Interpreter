open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let rec tokenize input = 

let possibly_id = Str.regexp "[a-zA-Z0-9]+" in

let len = String.length input in
let rec helper pos = if pos >= len then []

else if Str.string_match (Str.regexp "\\^") input pos then
Tok_Concat :: helper (pos + 1)

else if Str.string_match (Str.regexp "[0-9]+") input pos then
    let first_end = Str.match_end() in
    let matched_int = Str.matched_string input in 
    if (Str.string_match possibly_id input first_end) then 
        raise(InvalidInputException "Invalid ID (Leading number)")
    else
Tok_Int(int_of_string matched_int) :: helper (pos + (String.length matched_int))

else if Str.string_match (Str.regexp "(\\(-[0-9]+\\))") input pos then 
    let matched_int = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
        raise(InvalidInputException "Invalid ID (Leading Number)")
    else
    let raw_int = Str.replace_first (Str.regexp "(\\(-[0-9]+\\))") "\\1" matched_int in
Tok_Int (int_of_string raw_int) :: helper (pos+(String.length matched_int))

else if Str.string_match (Str.regexp "(") input pos then
Tok_LParen :: helper (pos + 1)

else if Str.string_match (Str.regexp ")") input pos then
Tok_RParen :: helper (pos + 1)

else if Str.string_match (Str.regexp "<>") input pos then
Tok_NotEqual :: helper (pos + 2)

else if Str.string_match (Str.regexp ">=") input pos then
Tok_GreaterEqual :: helper (pos + 2)

else if Str.string_match (Str.regexp "<=") input pos then
Tok_LessEqual :: helper (pos + 2)

else if Str.string_match (Str.regexp "=") input pos then
Tok_Equal :: helper (pos + 1)

else if Str.string_match (Str.regexp ">") input pos then
Tok_Greater :: helper (pos + 1)

else if Str.string_match (Str.regexp "<") input pos then
Tok_Less :: helper (pos + 1)

else if Str.string_match (Str.regexp "||") input pos then
Tok_Or :: helper (pos + 2)

else if Str.string_match (Str.regexp "&&") input pos then
Tok_And :: helper (pos + 2)

else if Str.string_match (Str.regexp "!") input pos then
Tok_Not :: helper (pos + 1)

else if (Str.string_match (Str.regexp "if") input pos) then 
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_If)::(helper first_end)

else if (Str.string_match (Str.regexp "not") input pos) then 
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Not)::(helper first_end)

else if Str.string_match (Str.regexp "then") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Then)::(helper first_end)
    
else if Str.string_match (Str.regexp "else") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Else)::(helper first_end)

else if Str.string_match (Str.regexp "->") input pos then
Tok_Arrow :: helper (pos + 2)

else if Str.string_match (Str.regexp "+") input pos then
Tok_Add :: helper (pos + 1)

else if Str.string_match (Str.regexp "-") input pos then
Tok_Sub :: helper (pos + 1)

else if Str.string_match (Str.regexp "*") input pos then
Tok_Mult :: helper (pos + 1)

else if Str.string_match (Str.regexp "/") input pos then
Tok_Div :: helper (pos + 1)

else if Str.string_match (Str.regexp "let") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Let)::(helper first_end)

else if Str.string_match (Str.regexp "rec") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Rec)::(helper first_end)

else if Str.string_match (Str.regexp "in") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_In)::(helper first_end)

else if Str.string_match (Str.regexp "def") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Def)::(helper first_end)

else if Str.string_match (Str.regexp "fun") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Fun)::(helper first_end)

else if Str.string_match (Str.regexp "true") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Bool(true))::(helper first_end)

else if Str.string_match (Str.regexp "false") input pos then
let curr_key = Str.matched_string input in
    let first_end = Str.match_end() in
    if (Str.string_match possibly_id input first_end) then 
    let id = Str.matched_string input in
    (Tok_ID (curr_key^id))::(helper (Str.match_end()))
    else 
    (Tok_Bool(false))::(helper first_end)

else if Str.string_match (Str.regexp "\"\\([^\"]*\\)\"") input pos then
    let matched_str = Str.matched_string input in
    let raw_str = Str.replace_first (Str.regexp "\"\\([^\"]*\\)\"") "\\1" matched_str in
Tok_String(raw_str) :: helper (pos + String.length matched_str)

else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
    let matched_id = Str.matched_string input in
Tok_ID(matched_id) :: helper (pos + String.length matched_id)

else if Str.string_match (Str.regexp " ") input pos then
    helper (pos + 1)

else if Str.string_match (Str.regexp ";;") input pos then
Tok_DoubleSemi :: helper (pos + 2)

else raise(InvalidInputException "Tokenize Failure")

in helper 0;;