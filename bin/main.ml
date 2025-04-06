exception SyntaxError of string

type token =
  | Let                        (* "let" keyword *)
  | If                         (* "if" keyword *)
  | Then                       (* "then" keyword *)
  | Else                       (* "else" keyword *)
  | True
  | False
  | Identifier of string       (* Variable names, function names, etc. *)
  | Int of int                 (* Integer literals *)
  | Float of float             (* Float literals *)
  | String of string           (* String literals *)
  | Equals                     (* "="  symbol *)
  | Equality                   (* "==" symbol *)
  | NotEquality                (* "!=" symbol *)
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Plus                       (* "+" operator *)
  | Minus                      (* "-" operator *)
  | Star                       (* "*" operator *)
  | Slash                      (* "/" operator *)
  | Lparen                     (* "(" symbol *)
  | Rparen                     (* ")" symbol *)
  | Lbrace                     (* "{" symbol *)
  | Rbrace                     (* "}" symbol *)
  | Semicolon                  (* ";" symbol *)
  | Bang                       (* "!" symbol *)
  | Nil                        (* nil keyword *)
  | Eof                        (* End of file *)
;;

(*type literal =*)
(*  | Number*)
(*  | String*)
(*  | True*)
(*  | False*)
(*  | Nil*)
(*;;*)

(*type operator =*)
(*  | Equal*)
(*  | NotEqual*)
(*  | Less*)
(*  | LessEqual*)
(*  | Greater*)
(*  | GreaterEqual*)
(*  | Add*)
(*  | Subtract*)
(*  | Multiply*)
(*  | Divide*)
(*;;*)

let tokenize input =
  let len = String.length input in
  let rec aux pos tokens =
    if pos >= len then List.rev (Eof :: tokens)
    else
      match input.[pos] with
      | ' ' | '\n' | '\t' -> aux (pos + 1) tokens
      | '+' -> aux (pos + 1) (Plus :: tokens)
      | '-' -> aux (pos + 1) (Minus :: tokens)
      | '*' -> aux (pos + 1) (Star :: tokens)
      | '/' -> aux (pos + 1) (Slash :: tokens)
      | '=' -> let start = pos in
               let rec find_end end_pos =
                  if end_pos < len && input.[end_pos] = '=' then find_end (end_pos + 1)
                  else end_pos
               in
               let end_pos = find_end start in
               let token = String.sub input start (end_pos - start) in
               (match token with
                | "==" -> aux (end_pos) (Equality :: tokens)
                | "=" -> aux (start + 1) (Equals :: tokens)
                | _ -> raise (SyntaxError (Printf.sprintf "Invalid token in position %d, token: %s" start token))
               )
      | '!' -> if pos + 1 < len && input.[pos + 1] = '='
               then aux (pos + 2) (NotEquality :: tokens)
               else aux (pos + 1) (Bang :: tokens)
      | '<' -> if pos + 1 < len && input.[pos + 1] = '='
              then aux (pos + 2) (LessEqual :: tokens)
              else aux (pos + 1) (Less :: tokens)
      | '>' -> if pos + 1 < len && input.[pos + 1] = '='
              then aux (pos + 2) (GreaterEqual :: tokens)
              else aux (pos + 1) (Greater :: tokens)
      | '(' -> aux (pos + 1) (Lparen :: tokens)
      | ')' -> aux (pos + 1) (Rparen :: tokens)
      | '{' -> aux (pos + 1) (Lbrace :: tokens)
      | '}' -> aux (pos + 1) (Rbrace :: tokens)
      | ';' -> aux (pos + 1) (Semicolon :: tokens)
      | '"' ->
          let start = pos in
          let rec find_end pos =
            if pos + 1 < len && input.[pos + 1] != '"' then
            find_end (pos + 1)
            else
            pos + 1  (* Include the closing quote position *)
          in
          let end_pos = find_end pos in
          let str_contents = String.sub input (start + 1) (end_pos - start - 1) in
          aux (end_pos + 1) (String str_contents :: tokens)

      | '0' .. '9' ->
          let start = pos in
          let rec find_end pos is_float =
            if pos < len then
              match input.[pos] with
                | c when c >= '0' && c <= '9' -> find_end (pos + 1) is_float
                | '.' -> find_end (pos + 1) true
                | _ -> (pos, is_float)
            else (pos, is_float)
          in
          let (end_pos, is_float) = find_end pos false in
          let num_string = String.sub input start (end_pos - start) in
          if is_float then aux end_pos (Float (float_of_string num_string) :: tokens)
          else aux end_pos (Int (int_of_string num_string) :: tokens)

      | 'a' .. 'z' | 'A' .. 'Z' ->
        let start = pos in
        let rec find_end end_pos =
          if end_pos < len then
            let c = Char.code input.[end_pos] in
            if (65 <= c && c <= 90) ||    (* A-Z *)
               (97 <= c && c <= 122) ||   (* a-z *)
               (48 <= c && c <= 57) ||    (* 0-9 *)
               c = 95                    (* _ *)
            then find_end (end_pos + 1)
            else end_pos
          else end_pos
        in
        let end_pos = find_end start in
        let token = String.sub input start (end_pos - start) in
        (match token with
          | "if" -> aux end_pos (If :: tokens)
          | "else" -> aux end_pos (Else :: tokens)
          | "then" -> aux end_pos (Then :: tokens)
          | "let" -> aux end_pos (Let :: tokens)
          | "true" -> aux end_pos (True :: tokens)
          | "false" -> aux end_pos (False :: tokens)
          | "nil" -> aux end_pos (Nil :: tokens)
          | _ -> aux end_pos (Identifier token :: tokens)
        )
      | _ -> aux (pos + 1) tokens
    in aux 0 [];;

let rec print_token_list tokens =
  match tokens with
  | [] -> print_endline ""
  | token :: rest ->
      (match token with
      | Int n -> print_string ("INT(" ^ string_of_int n ^ ")")
      | Plus -> print_string "+"
      | Minus -> print_string "-"
      | Star -> print_string "*"
      | Slash -> print_string "/"
      | Eof -> print_string ""
      | Equals -> print_string "="
      | Equality -> print_string "=="
      | If -> print_string "IF"
      | Else -> print_string "ELSE"
      | Then -> print_string "THEN"
      | Let -> print_string "LET"
      | Lparen -> print_string "("
      | Rparen -> print_string ")"
      | Lbrace -> print_string "{"
      | Rbrace -> print_string "}"
      | String string -> print_string ("\"" ^ string ^ "\"")
      | Semicolon -> print_string("SEMICOLON")
      | Float float -> print_string("FLOAT(" ^ string_of_float float ^ ")")
      | Identifier string -> print_string ("IDENTIFIER(" ^ string ^ ")")
      | Nil -> print_string "NIL"
      | _ -> print_string "wait lil bro"
    );
      print_string " ";
      print_token_list rest;;

let syntax_string = {|2 + 2 - 4 / 6|};;
let tokens = tokenize syntax_string;;

print_token_list tokens
