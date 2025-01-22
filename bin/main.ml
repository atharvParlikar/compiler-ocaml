type token =
  | LET                        (* "let" keyword *)
  | IF                         (* "if" keyword *)
  | THEN                       (* "then" keyword *)
  | ELSE                       (* "else" keyword *)
  | IDENTIFIER of string       (* Variable names, function names, etc. *)
  | INT of int                 (* Integer literals *)
  | FLOAT of float             (* Float literals *)
  | STRING of string           (* String literals *)
  | EQUALS                     (* "=" symbol *)
  | EQUALITY                   (* "==" symbol *)
  | PLUS                       (* "+" operator *)
  | MINUS                      (* "-" operator *)
  | STAR                       (* "*" operator *)
  | SLASH                      (* "/" operator *)
  | LPAREN                     (* "(" symbol *)
  | RPAREN                     (* ")" symbol *)
  | LBRACE                     (* "{" symbol *)
  | RBRACE                     (* "}" symbol *)
  | SEMICOLON                  (* ";" symbol *)
  | EOF                        (* End of file *)
;;
exception SyntaxError of string

let tokanize input =
  let len = String.length input in
  let rec aux pos tokens =
    if pos >= len then List.rev (EOF :: tokens)
    else
      match input.[pos] with
      | ' ' | '\n' | '\t' -> aux (pos + 1) tokens
      | '+' -> aux (pos + 1) (PLUS :: tokens)
      | '-' -> aux (pos + 1) (MINUS :: tokens)
      | '*' -> aux (pos + 1) (STAR :: tokens)
      | '/' -> aux (pos + 1) (SLASH :: tokens)
      | '=' -> let start = pos in
               let rec find_end end_pos =
                  if end_pos < len && input.[end_pos] = '=' then find_end (end_pos + 1)
                  else end_pos
               in
               let end_pos = find_end start in
               let token = String.sub input start (end_pos - start) in
               (match token with
                | "==" -> aux (end_pos) (EQUALITY :: tokens)
                | "=" -> aux (start + 1) (EQUALS :: tokens)
                | _ -> raise (SyntaxError (Printf.sprintf "Invalid token in position %d, token: %s" start token))
               )
      | '(' -> aux (pos + 1) (LPAREN :: tokens)
      | ')' -> aux (pos + 1) (RPAREN :: tokens)
      | '{' -> aux (pos + 1) (LBRACE :: tokens)
      | '}' -> aux (pos + 1) (RBRACE :: tokens)
      | ';' -> aux (pos + 1) (SEMICOLON :: tokens)
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
          aux (end_pos + 1) (STRING str_contents :: tokens)

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
          if is_float then aux end_pos (FLOAT (float_of_string num_string) :: tokens)
          else aux end_pos (INT (int_of_string num_string) :: tokens)
        
      | 'a' .. 'z' | 'A' .. 'Z' ->
        let start = pos in
        let rec find_end end_pos =
          if end_pos < len && (
            (65 <= (Char.code input.[end_pos]) && (Char.code input.[end_pos]) <= 90)
            || (97 <= (Char.code input.[end_pos]) && (Char.code input.[end_pos]) <= 122)
          ) then find_end (end_pos + 1)
          else (end_pos)
        in
        let end_pos = find_end start in
        let token = String.sub input start (end_pos - start) in
        (match token with
          | "if" -> aux end_pos (IF :: tokens)
          | "else" -> aux end_pos (ELSE :: tokens)
          | "then" -> aux end_pos (THEN :: tokens)
          | "let" -> aux end_pos (LET :: tokens)
          | _ -> aux end_pos (IDENTIFIER token:: tokens)
        )
      | _ -> aux (pos + 1) tokens
    in aux 0 [];;

let rec print_token_list tokens =
  match tokens with
  | [] -> print_endline ""
  | token :: rest ->
      (match token with
      | INT n -> print_string ("INT(" ^ string_of_int n ^ ")")
      | PLUS -> print_string "+"
      | MINUS -> print_string "-"
      | STAR -> print_string "*"
      | SLASH -> print_string "/"
      | EOF -> print_string ""
      | EQUALS -> print_string "="
      | EQUALITY -> print_string "=="
      | IF -> print_string "IF"
      | ELSE -> print_string "ELSE"
      | THEN -> print_string "THEN"
      | LET -> print_string "LET"
      | LPAREN -> print_string "("
      | RPAREN -> print_string ")"
      | LBRACE -> print_string "{"
      | RBRACE -> print_string "}"
      | STRING string -> print_string ("\"" ^ string ^ "\"")
      (* | IDENTIFIER identifier -> print_string ("" ^ identifier ^ "\"") *)
      | _ -> print_string "OTHER_TOKEN");  (* Handle other tokens *)
      print_string " ";
      print_token_list rest;;

let syntax_string = {|"fuck you nigga" 2|};;
let tokens = tokanize syntax_string;;

print_token_list tokens;;
