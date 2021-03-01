module Stdio = Stdio
open Base

type token_type =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS_EQUAL
  | LESS
  | IDENTIFIER of string
  | STRING of string
  | NUMBER of int
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
[@@deriving show { with_path = false }, eq, ord]

let to_string s = Printf.sprintf "%s\n" (show_token_type s)

type location = { line : int; column : int } [@@deriving show, make, eq]

let start () = { line = 1; column = 0 }

let next_column x c = { x with column = x.column + c }

let next_line x = { line = x.line + 1; column = 0 }

(* let to_string = function
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "}"
  | RIGHT_BRACE -> "{"
  | COMMA -> ","
  | DOT -> "."
  | MINUS -> "-"
  | PLUS -> "+"
  | SEMICOLON -> ";"
  | SLASH -> "/"
  | STAR -> "*"
  | BANG -> "!"
  | BANG_EQUAL -> "!="
  | EQUAL -> "="
  | EQUAL_EQUAL -> "=="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | LESS_EQUAL -> "<="
  | LESS -> "<"
  | IDENTIFIER s -> s
  | STRING s -> s
  | NUMBER s -> Int.to_string s
  | AND -> "and"
  | CLASS -> "class"
  | ELSE -> "else"
  | FALSE -> "false"
  | FUN -> "fun"
  | FOR -> "for"
  | IF -> "if"
  | NIL -> "nil"
  | OR -> "or"
  | PRINT -> "print"
  | RETURN -> "return"
  | SUPER -> "super"
  | THIS -> "this"
  | TRUE -> "true"
  | VAR -> "var"
  | WHILE -> "while"
  | EOF -> "<eof>" *)

type token = { _type : token_type; location : location }
[@@deriving show { with_path = false }, make, eq]

let of_token_kind ~_type = { _type; location = start () }

type error = { message : string; location : location }

type result_token = { token : token list; error : error list }

let init_result_token () = { token = []; error = [] }

let is_word_end (s : char list) =
  match s with
  | [] -> true
  | c :: _ when Char.is_alphanum c -> false
  | _ -> true

let add_token _type location tokens = make_token ~_type ~location :: tokens

let rec scan_token (loc : location) (tokens : result_token) (source : char list)
    =
  match source with
  | [] -> ()
  | hd :: tl -> Printf.sprintf "%c\n" |> fun _ -> ()

let scan_tokens (source : string) =
  scan_token (start ()) (init_result_token ()) (String.to_list source)

let run_file file =
  let ic = open_in file in
  let file_list = Stdio.In_channel.input_lines ic in
  (* Printf.printf "%d\n" (Lis.length file_list); *)
  String.concat file_list |> Stdio.print_string

let run_prompt _ = ()

(* for now just think about the run_file funtion *)

(* let check_args = fun _ ->
  match (Array.length Sys.argv) with
  | 0 -> Result.error "There are not input arguments which should never be the case"
  | 1 -> run_prompt |> fun _ -> Result.ok ()
  | 2 -> run_file Sys.argv.(1) |> fun () -> Result.ok ()
  | _ -> Result.error "Usage: ocaml_lox [script]" *)

let _ = Sys.get_argv () |> fun s -> run_file s.(1)
